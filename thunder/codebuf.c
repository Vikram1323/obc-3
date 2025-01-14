/*
 * codebuf.c
 * 
 * This file is part of the Oxford Oberon-2 compiler
 * Copyright (c) 2006--2016 J. M. Spivey
 * All rights reserved
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "config.h"
#include "vm.h"
#include "vminternal.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* Define to forbid pages from being both writable and executable */
// #define NO_WRITEXEC 1

#ifdef LINUX
#define USE_MPROTECT 1
#endif

#ifdef MACOS
#define USE_MPROTECT 1
#endif

#define MARGIN 32	        /* Safety margin for swiching buffers */
#define MIN 128                 /* Min space at beginning of routine */

code_addr pc;                   /* Current assembly location */
static code_addr proc_beg, codebuf, limit;

/* byte -- contribute a byte to the object code */
void byte(int x) {
     *pc++ = x & 0xff;
}

/* modify -- modify previous byte */
void modify(int bit) {
     pc[-1] |= bit;
}

/* word -- contribute a whole word */
void word(int x) {
     * (int *) pc = x;
     pc += 4;
}

/* qword -- contribute a 64-bit quantity */
void qword(uint64 x) {
     // Assume unaligned store is OK
     * (uint64 *) pc = x;
     pc += 8;
}

/* vm_literal_align -- allocate aligned space in code buffer */
void *vm_literal_align(int n, int a) {
     vm_space(n+a);
     limit = (code_addr) (((ptr) limit - n) & ~(a-1));
     return limit;
}

#ifdef FREEBSD
#define USE_MPROTECT 1
#endif

#ifdef USE_MPROTECT
#include <sys/mman.h>

static void mprot(void *p, int flags) {
     if (mprotect(p, CODEPAGE, flags) < 0) {
          perror("mprotect failed");
          exit(2);
     }
}

#define prot_write(p)    mprot(p, PROT_READ|PROT_WRITE)
#define prot_exec(p)     mprot(p, PROT_READ|PROT_EXEC)
#define prot_writexec(p) mprot(p, PROT_READ|PROT_WRITE|PROT_EXEC)
#endif

#ifdef WINDOWS
#undef byte
#include <windows.h>

static void vprot(void *p, long unsigned flags) {
     long unsigned oldflags;

     if (VirtualProtect(p, CODEPAGE, flags, &oldflags) == 0)
          vm_panic("VirtualProtect failed");
}

#define prot_write(p)    vprot(p, PAGE_READWRITE)
#define prot_exec(p)     vprot(p, PAGE_EXECUTE_READ)
#define prot_writexec(p) vprot(p, PAGE_EXECUTE_READWRITE)
#endif

#ifdef NO_WRITEXEC
#define PAGES 8
static code_addr page[PAGES];
static int npages = -1;

static void set_write(void) {
     npages = 0;
     if (codebuf != NULL) {
          page[npages++] = codebuf;
          prot_write(codebuf);
     }
}

static void set_exec(void) {
     for (int i = 0; i < npages; i++) {
          prot_exec(page[i]);
     }
     npages = -1;
}
#endif

#ifdef USE_FLUSH
#define FRAGS 16
static code_addr fragbeg[FRAGS], fragend[FRAGS];
static int nfrags;

/* vm_flush -- clear code from data cache */
static void vm_flush(void) {
     // This is probably ARM-specific
     for (int i = 0; i < nfrags; i++)
	  __clear_cache(fragbeg[i], fragend[i]);
}
#endif

/* vm_begin -- begin new procedure */
unsigned vm_begin_locals(int n, int locs) {
     vm_space(MIN);
     proc_beg = pc;
#ifdef NO_WRITEXEC
     set_write();
#endif
#ifdef USE_FLUSH
     nfrags = 0; fragbeg[0] = pc;
#endif
     return vm_prelude(n, locs);
}

/* vm_space -- ensure space in code buffer */
void vm_space(int space) {
     if (codebuf == NULL || pc + space > limit - MARGIN) {
	  code_addr p = (code_addr) vm_alloc(CODEPAGE);
	  if (codebuf != NULL) vm_chain(p);
#ifdef NO_WRITEXEC
          if (npages == PAGES) vm_panic("too many pages");
          page[npages++] = p;
#else
          prot_writexec(p);
#endif
#if USE_FLUSH
	  fragend[nfrags++] = pc;
	  if (nfrags >= FRAGS) vm_panic("too many frags");
	  fragbeg[nfrags] = p;
#endif
	  codebuf = p; limit = p + CODEPAGE;
	  pc = codebuf;
     }
}

/* vm_end -- finish a procedure */
void vm_end(void) {
     vm_postlude();
     vm_reset();

#ifdef NO_WRITEXEC
     set_exec();
#endif
#ifdef USE_FLUSH
     fragend[nfrags++] = pc;
     vm_flush();
#endif
}

int vm_procsize(void) {
     return pc - proc_beg;
}

int vm_addr(void *p) {
     int r = (int) (ptr) p;
     if (p != (void *) (ptr) r)
          vm_panic("address overflow");
     return r;
}

#ifndef M64X32
funptr vm_func(int fun) { return (funptr) fun; }
int vm_wrap(funptr fun) { return (int) fun; }
#else
funptr vm_func(int fun) { return (funptr) (ptr) fun; }

#ifndef NO_WRITEXEC
int vm_wrap(funptr fun) { return vm_tramp(fun); }
#else
int vm_wrap(funptr fun) {
     if (npages >= 0)
          return vm_tramp(fun);
     else {
          set_write();
          int r = vm_tramp(fun);
          set_exec();
          return r;
     }
}
#endif
#endif
