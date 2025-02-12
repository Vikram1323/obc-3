/*
 * profile.c
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

#define PROFILE 1
#include "obx.h"
#include <string.h>
#include <stdlib.h>


/* STATE MACHINE */

typedef struct _state *state;
typedef struct _trans *trans;

struct _state {
     int s_num;			/* Serial number */
     int s_depth;		/* Number of procs in history */
     word *s_history;		/* The history list */
     unsigned s_calls;		/* No. of calls */
     unsigned s_rec;		/* No. of recursive calls */
     counter s_time;		/* Time charged so far */
     state s_next;		/* Next state for same procedure */
     state s_chain;		/* Next state in chain of all states */
};

struct _trans {			
     state t_from, t_to;
     word t_addr;
     trans t_hlink;
};

#define MBIT 0x1
#define mark(p)   ((p) | MBIT)
#define ptr(p)    ((p) & ~MBIT)
#define marked(p) ((p) & MBIT)

/* format_count -- format a counter as a decimal string */
static char *format_count(counter n) {
     static char buf[32];
#ifdef __MINGW32__
     const char *fmt = "%I64u";
#else
     const char *fmt = "%llu";
#endif

     sprintf(buf, fmt, n);
     return buf;
}

/* format_index -- format a list index as [n] */
static char *format_index(proc p) {
     static char buf[32];
     sprintf(buf, "[%d]", p->p_index);
     return buf;
}

#define SPONTANEOUS (-1)

static struct _proc no_proc = {
     "*no-proc*",		/* name */
     SPONTANEOUS, 		/* addr */
     0, 0, 0, 0, 0,		/* index calls rec self child */
     NULL, NULL			/* parents children */
};

static proc find_node(word p) {
     if (p == 0)
	  return &no_proc;
     else
	  return find_proc(p);
}

#ifdef DEBUG
static void dump_state(state s, mybool stats) {
     int n = s->s_depth, x = 3;
     word *hist = s->s_history;

     printf("> State %d:\n", s->s_num);
     printf(">   ");
     if (n == 0)
	  printf("(empty)\n");
     else {
	  for (int i = 0; i < n; i++) {
	       proc p = find_node(ptr(hist[i]));
               const char *item =
                    (marked(hist[i]) ? mysprintf("(%s)", p->p_name)
                     : p->p_name);
	       printf(" %s", item);
	       x += strlen(item)+1;

	       if (x > 60) {
		    printf("\n>    "); x = 5;
	       }
	  }
	  printf("\n");
     }
     if (stats)
	  printf(">   %u+%u calls, %s ticks\n",
		 s->s_calls, s->s_rec, format_count(s->s_time));
}
#endif

static int n_states = 0;
static state s_head = NULL, s_tail;

#define HSIZE 32771		/* Not a power of 2! */
static trans *trtable;
#define hash(s0, a) \
     (((((int) (ptrtype) (s0)) << 4) ^ (addr)) % HSIZE)

static state make_state(word addr, int n, word *history) {
     state s;
     unsigned h = hash(NULL, addr);
     trans t;

     /* All the states for a given procedure p are chained together;
	the first one can be found by looking up (NULL, p) in the
	transition table */

     /* Look to see if the state already exists */
     for (t = trtable[h]; t != NULL; t = t->t_hlink)
	  if (t->t_from == NULL && t->t_addr == addr)
	       break;

     if (t == NULL) {
	  /* Create the dummy transition for (NULL, p) */
	  t = scratch_alloc_atomic(sizeof(struct _trans), "dummy transition");
	  t->t_from = NULL;
	  t->t_to = NULL;
	  t->t_addr = addr;
	  t->t_hlink = trtable[h];
	  trtable[h] = t;
     }

     for (s = t->t_to; s != NULL; s = s->s_next)
	  if (s->s_depth == n 
	      && memcmp(s->s_history, history, n * sizeof(word)) == 0)
	       return s;

     /* If all else fails, make a new state */
     s = scratch_alloc_atomic(sizeof(struct _state), "profile state");
     s->s_num = n_states++;
     s->s_depth = n;
     s->s_history = scratch_alloc_atomic(n * sizeof(word), "profile history");
     memcpy(s->s_history, history, n * sizeof(word));
     s->s_calls = s->s_rec = s->s_time = 0;
     s->s_next = t->t_to;
     t->t_to = s;
     if (s_head == NULL) 
	  s_head = s_tail = s;
     else {
	  s_tail->s_chain = s;
	  s_tail = s;
     }

#ifdef DEBUG
     if (dflag) dump_state(s, FALSE);
#endif
     
     return s;
}

#define MAXDEPTH 128

static state next_state(state s0, word addr) {
     state s;
     word *h0 = s0->s_history;
     int n = 0, n0 = s0->s_depth;
     trans t;

     /* First look in the hash table of transitions */
     unsigned h = hash(s0, addr);
     for (t = trtable[h]; t != NULL; t = t->t_hlink)
	  if (t->t_from == s0 && t->t_addr == addr)
	       return t->t_to;

     /* A new transition will be needed */
     if (h0[n0-1] == addr) 
	  /* Direct recursion */
	  s = s0;
     else {
	  /* Compute the history list for the desired state */
          static word history[MAXDEPTH];
	  for (int k = 0; k < n0; k++)
	       history[k] = (ptr(h0[k]) == addr ? mark(h0[k]) : h0[k]);
	  history[n0] = addr;
	  
	  /* Eliminate marked entries that are surrounded by other
	     marked entries, and adjacent marked entries for the same proc */
	  if (n0 > 0) {
	       n = 1;
	       for (int k = 1; k < n0; k++) {
		    if (marked(history[n-1]) && marked(history[k])
			&& (marked(history[k+1]) 
			    || history[n-1] == history[k])) continue;
		    history[n++] = history[k];
	       }
	  }
	  if (n >= MAXDEPTH) panic("profile history too long");
	  history[n++] = history[n0];
	  s = make_state(addr, n, history);
     }

     /* Create the new transition */
     t = scratch_alloc_atomic(sizeof(struct _trans), "profiling transition");
     t->t_from = s0;
     t->t_to = s;
     t->t_addr = addr;
     t->t_hlink = trtable[h];
     trtable[h] = t;
     
#ifdef DEBUG
     if (dflag) {
	  proc p = find_node(addr);
	  printf("Transition: state %d, proc %s -> state %d\n",
		 s0->s_num, p->p_name, s->s_num);
     }
#endif

     return s;
}


/* MONITOR ROUTINE */

#define PSTACKSIZE 1000

static counter tot_ticks = 0;
static state *pstack = NULL;
static int psp;
static state prof_state;

static proc currproc;

#ifdef DEBUG
static void flat_charge(proc p, counter t) {
     if (dflag) printf("Charging %s to %s\n", format_count(t), p->p_name);
     p->p_self += t;
}
#else
#define flat_charge(p, t)  p->p_self += t
#endif

/* prof_enter -- record procedure entry */
void prof_enter(word addr, counter ticks, int why) {
     tot_ticks += ticks;

     if (gflag) {
	  state s = next_state(prof_state, addr);
	  prof_state->s_time += ticks;

#ifdef DEBUG
	  if (dflag) printf("Entering state %d\n", s->s_num);
#endif

	  if (s == prof_state)
	       s->s_rec++;
	  else
	       s->s_calls++;

	  switch (why) {
	  case PROF_CALL:
	       psp++;
	       if (psp >= PSTACKSIZE) panic("profile stack overflow");
	       pstack[psp] = prof_state = s;
	       break;

	  case PROF_TAIL:
	       /* Replace old state on the stack */
	       pstack[psp] = prof_state = s;
	       break;

	  case PROF_PRIM:
	       /* Keep old state */
	       break;

	  default:
	       panic("Bad argument to prof_enter");
	  }
     } else {
	  proc callee;

	  if (currproc != NULL) flat_charge(currproc, ticks);

	  callee = find_node(addr);

	  if (callee == currproc)
	       currproc->p_rec++;
	  else
	       callee->p_calls++;

	  if (why != PROF_PRIM) currproc = callee;
     }
}

/* prof_exit -- record procedure exit */
void prof_exit(word addr, counter ticks)  {
     tot_ticks += ticks;
     
     if (gflag) {
	  prof_state->s_time += ticks;
	  prof_state = pstack[--psp];

#ifdef DEBUG
	  if (dflag) printf("Returning to state %d\n", pstack[psp]->s_num);
#endif
     } else {
	  flat_charge(currproc, ticks);
	  if (addr != 0) currproc = find_node(addr);
     }
}

/* prof_init -- initialize profiling */
void prof_init(void) {
     if (gflag) {
	  if (pstack == NULL)
	       pstack = scratch_alloc_atomic(PSTACKSIZE * sizeof(state),
                                             "profile stack");
	  if (trtable == NULL) {
	       trtable = scratch_alloc_atomic(HSIZE * sizeof(trans),
                                              "profile hashtable");
	       for (int i = 0; i < HSIZE; i++) trtable[i] = NULL;
	  }
	       
	  state s = make_state(0, 0, NULL);
	  s->s_calls++;
	  psp = 0;
	  pstack[psp] = prof_state = s;
     } else {
	  currproc = NULL;
     }
}

void prof_reset(proc p) {
     currproc = p;
}


/* ANALYSIS PHASE */

struct _arc {
     proc a_src;		/* Calling proc */
     proc a_dst;		/* Called proc */
     unsigned a_count;		/* Call count */
     counter a_self1;		/* Time of dst when src unmarked */
     counter a_child1;		/* Time of children */
     counter a_self2;		/* Time of dst when dst unmarked */
     counter a_child2;		/* Time of children */
     arc a_plink;		/* Next arc for same dest */
     arc a_clink;		/* Next arc for same source */
};


/* find_arc -- find or create arc from SRC to DST */
static arc find_arc(word src, word dst) {
     proc psrc = find_node(src);
     proc pdst = find_node(dst);
     arc a;

     for (a = pdst->p_parents; a != NULL; a = a->a_plink)
	  if (a->a_src == psrc) break;

     if (a == NULL) {
	  a = scratch_alloc_atomic(sizeof(struct _arc), "profile arc");
	  a->a_count = a->a_self1 = a->a_child1 =
               a->a_self2 = a->a_child2 = 0;
	  a->a_src = psrc;
	  a->a_dst = pdst;
	  a->a_plink = pdst->p_parents;
	  a->a_clink = psrc->p_children;
	  pdst->p_parents = psrc->p_children = a;
     }

     return a;
}

/* graph_stats -- after execution, analyse accumulated statistics */
static void graph_stats(void) {
#ifdef DEBUG
     if (dflag) printf("\nAnalysis phase:\n");
#endif

     /* Find all the states we've created */
     for (state s = s_head; s != NULL; s = s->s_chain) {
	  int n = s->s_depth;
	  counter t = s->s_time;
	  word *hist = s->s_history;
	  proc p;

#ifdef DEBUG
	  if (dflag) dump_state(s, TRUE);
#endif

	  if (n == 0) continue;

	  p = find_node(ptr(hist[n-1]));
	  p->p_calls += s->s_calls;
	  p->p_rec += s->s_rec;
	  p->p_self += t;

	  for (int j = 0; j < n; j++) {
	       if (marked(hist[j])) continue;

	       if (j > 0) {
		    arc a = find_arc(ptr(hist[j-1]), ptr(hist[j]));
		    if (j == n-1)
			 a->a_count += s->s_calls;
		    if (a->a_dst == p)
			 a->a_self2 += t;
		    else
			 a->a_child2 += t;
	       }

	       if (j < n-1) {
		    arc a = find_arc(ptr(hist[j]), ptr(hist[j+1]));
		    a->a_src->p_child += t;
		    if (a->a_dst == p)
			 a->a_self1 += t;
		    else
			 a->a_child1 += t;
	       }
	  }
     }
}

/* OUTPUT ROUTINES */

static int cfsyms(proc *a, proc *b) {
     /* Descending order of self time */
     if ((*a)->p_self < (*b)->p_self)
	  return 1;
     else if ((*a)->p_self > (*b)->p_self)
	  return -1;
     else
	  return strcmp((*a)->p_name, (*b)->p_name);
}

static int cfsyms2(proc *a, proc *b) {
     /* Descending order of total time */
     counter at = (*a)->p_self + (*a)->p_child,
	  bt = (*b)->p_self + (*b)->p_child;
     if (at < bt)
	  return 1;
     else if (at > bt)
	  return -1;
     else if ((*a)->p_calls != (*b)->p_calls)
	  return (*b)->p_calls - (*a)->p_calls;
     else
	  return strcmp((*a)->p_name, (*b)->p_name);
}

static int cfsyms3(proc *a, proc *b) {
     /* Ascending alphabetical order */
     return strcmp((*a)->p_name, (*b)->p_name);
}

static int cfarcs1(arc *a, arc *b) {
     counter at = (*a)->a_self1 + (*a)->a_child1,
	  bt = (*b)->a_self1 + (*b)->a_child1;
     int r = 0;

     /* Descending order of total time 1 */
     if (at < bt) r = 1; else if (at > bt) r = -1;

     /* Descending order of call count */
     if (r == 0) r = (*b)->a_count - (*a)->a_count;

     /* Alphabetical order */
     if (r == 0) r = strcmp((*a)->a_dst->p_name, (*b)->a_dst->p_name);

     return r;
}

static int cfarcs2(arc *a, arc *b) {
     counter at = (*a)->a_self2 + (*a)->a_child2,
	  bt = (*b)->a_self2 + (*b)->a_child2;
     int r = 0;

     /* Descending order of total time 1 */
     if (at < bt) r = 1; else if (at > bt) r = -1;

     /* Descending order of call count */
     if (r == 0) r = (*b)->a_count - (*a)->a_count;

     /* Alphabetical order */
     if (r == 0) r = strcmp((*a)->a_src->p_name, (*b)->a_src->p_name);

     return r;
}

#define percent(t) ((float) (t) / tot_ticks * 100.0)

/* flat_profile -- print flat profile */
static void flat_profile(FILE *fp) {
     counter cumul = 0;

     /* Finished executing, so we can sort the proc table into a
        different order. */
     qsort(proctab, nprocs, sizeof(proc),
	   (int (*)(const void *, const void *)) cfsyms);

     fprintf(fp, "Execution profile:\n\n");

     fprintf(fp, "     Ticks    Frac     Cumul   Calls   Procedure\n");
     fprintf(fp, " ---------------------------------------------------\n");

     for (int i = 0; i < nprocs; i++) {
	  proc p = proctab[i];

	  cumul += p->p_self;

	  if (p->p_calls > 0)
	       fprintf(fp, "%10s   %5.1f%%   %5.1f%% %7u   %s\n", 
		       format_count(p->p_self), percent(p->p_self), 
		       percent(cumul), p->p_calls + p->p_rec, p->p_name);
     }

     fprintf(fp, "\nTotal of %s clock ticks\n", format_count(tot_ticks));
}

static void graph_profile(FILE *fp) {
     arc *abuf = scratch_alloc_atomic(256 * sizeof(arc), "profile buffer");

     /* Finished executing, so we can sort the proc table into
	a different order. */
     qsort(proctab, nprocs, sizeof(proc),
	   (int (*)(const void *, const void *)) cfsyms2);

     for (int i = 0; i < nprocs; i++) proctab[i]->p_index = i+1;

     fprintf(fp, "\nCall graph profile:\n\n");

     fprintf(fp, "index  total   self children   calls        name\n\n");

     for (int i = 0; i < nprocs && proctab[i]->p_calls > 0; i++) {
	  proc p = proctab[i]; 

#define FMT1 "              %5.1f%% %5.1f%% %7u/%-7u     %s\n"
#define FMT2 "%-6s %5.1f%% %5.1f%% %5.1f%% %-15s %s [%d]\n"
#define DIVR "----------------------------------------------------------------"

	  int n = 0;
	  for (arc a = p->p_parents; a != NULL; a = a->a_plink)
	       if (a->a_src != p) abuf[n++] = a;
	  qsort(abuf, n, sizeof(arc), 
		(int (*)(const void *, const void *)) cfarcs2);
	  for (int j = 0; j < n; j++) {
	       arc a = abuf[j];
               char *src = (a->a_src->p_addr == SPONTANEOUS
                            ? "<spontaneous>"
                            : mysprintf("%s %s", a->a_src->p_name, 
                                        format_index(a->a_src)));
	       fprintf(fp, FMT1,
		       percent(a->a_self2), percent(a->a_child2),
		       a->a_count, p->p_calls, src);
	  }
	  
          char *calls = (p->p_rec == 0
                         ? mysprintf("%7u", p->p_calls)
                         : mysprintf("%7u+%u", p->p_calls, p->p_rec));
	  fprintf(fp, FMT2,
		  format_index(p), percent(p->p_self + p->p_child),
		  percent(p->p_self), percent(p->p_child),
		  calls, p->p_name, i+1);

	  n = 0;
	  for (arc a = p->p_children; a != NULL; a = a->a_clink)
	       if (a->a_dst != p) abuf[n++] = a;
	  qsort(abuf, n, sizeof(arc), 
		(int (*)(const void *, const void *)) cfarcs1);
	  for (int j = 0; j < n; j++) {
	       arc a = abuf[j];
	       char *dest = mysprintf("%s %s", a->a_dst->p_name, 
                                      format_index(a->a_dst));
	       fprintf(fp, FMT1,
		       percent(a->a_self1), percent(a->a_child1), 
		       a->a_count, a->a_dst->p_calls, dest);
	  }

	  fprintf(fp, "%s\n", DIVR);
     }
}

/* graph_index -- print alphabetical index for call graph profile */
static void graph_index(FILE *fp) {
     /* Sort the procedures yet again */
     int n = 0, maxw = 1;
     while (n < nprocs && proctab[n]->p_calls > 0) {
	  int ww = strlen(proctab[n]->p_name);
	  if (ww > maxw) maxw = ww;
	  n++;
     }

     qsort(proctab, n, sizeof(proc),
	   (int (*)(const void *, const void *)) cfsyms3);

     /* The |n| procedures are displayed in |ncols| columns; the first
        |n % ncols| have size |floor(n/ncols)+1| and the rest of size
        |floor(n/ncols)|.  Working from left to right, if |r| entries
	remain to be put in |k| columns, the next column contains
        |ceil(r/k)| entries.  */

     fprintf(fp, "\nProcedure index:\n\n");
     int ncols = 70 / (maxw+8);
     if (ncols == 0) ncols = 1;

     for (int i = 0; i*ncols < n; i++) {
          int c;
	  for (int r = n, k = ncols; k > 0; r -= c, k--) {
	       c = (r+k-1)/k;
	       if (i >= c) break;
               proc p = proctab[n-r+i];
	       char *entry = mysprintf("%s [%d]", p->p_name, p->p_index);
	       fprintf(fp, "  %-*s", maxw+6, entry);
	  }
	  fprintf(fp, "\n");
     }
     fprintf(fp, "\n");
}

void profile(FILE *fp) {
     if (gflag) graph_stats();

     flat_profile(fp);
     if (gflag) {
	  graph_profile(fp);
	  graph_index(fp);
     }
}
