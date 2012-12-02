#
# iset.tcl
# 
# This file is part of the Oxford Oberon-2 compiler
# Copyright (c) 2006 J. M. Spivey
# All rights reserved
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
# 3. The name of the author may not be used to endorse or promote products
#    derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
# OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
# IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
# OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
# ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

# This workaround is needed with TCL 8.4.2 if output goes to an emacs
# compilation buffer.
fconfigure stdout -translation lf
fconfigure stderr -translation lf

if {[llength $argv] != 5} {
    puts stderr "usage: iset input.iset iskel.c header template interp"
    exit 1
}

set srcdir [file dirname $argv0]
source "$srcdir/util.tcl"
source "$srcdir/iparse.tcl"

if {[file exists "config.tcl"]} {source "config.tcl"}

lsplit $argv infile sfile hfile tfile ifile

# BUILD THE TRIE

# make_trie -- recursively build a trie for a set of strings
proc make_trie {n strings} {
    global charcode first trie taken check ntrie free

    # Assume the strings agree on the first n characters

    if {[llength $strings] == 0} {
	puts stderr "Empty trie!!!"
	return -9999
    }

    # Set chars to the set of n'th characters of the strings
    set chars [remdups [lsort [map {nth_char $n} $strings]]]
    set c1 $charcode([lindex $chars 0])

    # Find a place where a node for $chars will fit
    if {$n == 0} {
	# Force the root to be at position 0
	set q 0
    } else {
	# Otherwise try putting the smallest char in the first free slot
	set q [expr {$free - $c1}]
    }

    for {} {1} {incr q} {
	if {[info exists taken($q)]} continue

	set ok 1
	foreach c $chars {
	    set ix [expr {$q+$charcode($c)}]

	    while {$ntrie <= $ix} {
		set trie($ntrie) 0
		set check($ntrie) 128
		incr ntrie
	    }

	    if {$check($ix) != 128} {
		set ok 0; break
	    }
	}

	if {$ok} break
    }

    # Reserve the locations we will use by filling in check
    # (actual values in trie get filled in later)
    set taken($q) 1

    foreach c $chars {
	set ix [expr {$q+$charcode($c)}]
	set check($ix) $charcode($c)
    }

    while {$free < $ntrie && $check($free) != 128} {
	incr free
    }

    # Recursively build sub-tries
    foreach c $chars {
	if {$c == ""} {
	    set t [string range [lindex $strings 0] 0 [expr {$n-1}]]
	    set trie($q) $first($t)
	    set check($q) 0
	} else {
	    set ix [expr {$q+$charcode($c)}]
	    set subset [filter {nth_char_is $n $c} $strings]
	    set trie($ix) [make_trie [expr {$n+1}] $subset]
	    set check($ix) $charcode($c)
	}
    }

    return $q
}

# Build a packed trie for the instructions
proc build_trie {} {
    global ntrie instrs dirs free

    set ntrie 0; set free 0
    make_trie 0 [concat $instrs $dirs]
}


# GENERATE HEADER FILE

proc gen_header {name} {
    global ntempl maxargs instrs instrno dirs dirno ops action ntrie

    set f [open $name "w"]
    puts $f "/* Header file -- generated by iset.tcl */"
    puts $f ""
    puts $f "#define NTEMPLATES $ntempl"
    puts $f "#define NTRIE $ntrie"
    puts $f "#define MAXARGS $maxargs"
    puts $f ""
    puts $f "#define I_ILLEGAL 0"
    foreach i $instrs {
	puts $f "#define [csym I $i] $instrno($i)"
    }
    puts $f ""
    foreach dir $dirs {
	puts $f "#define [csym D $dir] $dirno($dir)"
    }
    puts $f ""
    puts $f "#define K_ILLEGAL 0"
    foreach op $ops {
	with $action($op) {base count length inst key act args} {
	    puts $f "#define [csym K $op] $base"
	}
    }
    close $f
}

# GENERATE TEMPLATE FILE

# make_code -- assemble equivalent code
proc make_code {op} {
    global ops dirs status

    if {$op == "NOP"} {
	return {}
    } elseif {[lmember $op $ops]} {
	return [csym K $op]
    } elseif {[lmember $op $dirs]} {
	return [csym D $op]
    } else {
	puts stderr "Code $op does not exist"
	set status 1
    }
}

proc quote {s} {return "\"$s\""}

proc gen_template {name} {
    global templates instrs dirs first ntrie trie check macro

    set f [open $name "w"]
    puts $f "/* Template file -- generated by iset.tcl */"
    puts $f ""
    puts $f "#include \"oblink.h\""
    puts $f "#include \"keiko.h\""
    puts $f ""

    set nt 0
    set fmt "{%-12s %-7s%3d, %2d, %2d, %2d, %2d, %s, { %s }},"
    puts $f "struct _template templates\[NTEMPLATES\] = {"
    foreach inst [concat $instrs $dirs] {
	set first($inst) $nt
	foreach templ $templates($inst) {
	    with $templ {patt bounds op argsz} {
		with $bounds {lo hi step} {
		    if {$nt == $first($inst)} {
			set icode "\"$inst\""
		    } else {
			set icode "   NULL"
		    }
		    if {[info exists macro($op)]} {
			set maclines [map quote $macro($op)]
			puts $f \
			    [format $fmt "$icode," "\"$patt\"," \
				 $lo $hi $step 0 0 0 [join $maclines ", "]]
		    } else {
			if {$op == "NOP"} {
			    set n 0; set c 0
			} else {
			    set n 1; set c [make_code $op]
			}
			set len [expr {$argsz >= 0 ? $n + $argsz : $argsz}]
			puts $f \
			    [format $fmt "$icode," "\"$patt\"," \
				 $lo $hi $step  $len $n $c "NULL"]
		    }
		}
	    }
	    incr nt
	}
    }
    puts $f "};";
    puts $f "";

    build_trie

    puts $f "short templ_trie\[NTRIE\] = {"
    for {set i 0} {$i < $ntrie} {incr i} {
	if {$i > 0 && $i % 10 == 0} {puts $f ""}
	puts -nonewline $f [format "%4d, " $trie($i)]
    }
    puts $f "\n};"    
    puts $f "";
    puts $f "uchar templ_check\[NTRIE\] = {"
    for {set i 0} {$i < $ntrie} {incr i} {
	if {$i > 0 && $i % 10 == 0} {puts $f ""}
	puts -nonewline $f [format "%4d, " $check($i)]
    }
    puts $f "\n};"    
    close $f
}

# GENERATE INTERPRETER

proc copy_some {f} {
    global skelf

    while {[gets $skelf line] >= 0} {
	if {[regexp {^\$\$} $line]} break
	puts $f $line
    }
}	     
	    
proc make_body {key action argv} {
    global err_op

    set body $action

    for {set i 0} {$i < [llength $argv]} {incr i} {
	set formal [string index "abcd" $i]
	regsub -all "\\\$$formal" $body [lindex $argv $i] body
    }

    regsub -all {\$s} $body "sp" body

    if {[regexp {\.(.)} $key _ suffix] && $suffix == "*"} {
	set suffix "i"
	regsub -all {\$t} $body "i" body
    }
    regsub -all {\$([123])\.\*} $body {$\1.i} body
    regsub -all {\$u[123]} $body "i" body

    switch -glob -- $key {
	B.d {
	    # Double from two doubles
	    regsub -all {\$1\.d} $body {getdbl(\&sp[0])} body
	    regsub -all {\$2\.d} $body {getdbl(\&sp[-2])} body
	    return "P(2); putdbl(&sp\[0\], $body); G;"
	}
	B.?dd {
	    # Value from two doubles
	    regsub -all {\$1\.d} $body {getdbl(\&sp[-1])} body
	    regsub -all {\$2\.d} $body {getdbl(\&sp[-3])} body
	    return "P(3); A(0).$suffix = $body;"
	}	    
	B.d?? {
	    # Double from two values
	    regsub -all {\$1} $body {sp[1]} body
	    regsub -all {\$2} $body {A(0)} body
    	    return "putdbl(&sp\[0\], $body); G;"
	}
	B.q {
	    # Long from two longs
	    regsub -all {\$1\.q} $body {getlong(\&sp[0])} body
	    regsub -all {\$2\.q} $body {getlong(\&sp[-2])} body
	    return "P(2); putlong(&sp\[0\], $body); G;"
	}
	B.?qq {
	    # Value from two longs
	    regsub -all {\$1\.q} $body {getlong(\&sp[-1])} body
	    regsub -all {\$2\.q} $body {getlong(\&sp[-3])} body
	    return "P(3); A(0).$suffix = $body;"
	}	    
	B.q?? {
	    # Long from two values
	    regsub -all {\$1} $body {sp[1]} body
	    regsub -all {\$2} $body {A(0)} body
    	    return "putlong(&sp\[0\], $body); G;"
	}
	B.? {
	    regsub -all {\$1} $body {sp[0]} body
	    regsub -all {\$2} $body {A(-1)} body
	    return "M(1); A(0).$suffix = $body;"
	}
	M.dq {
	    regsub -all {\$1\.q} $body {getlong(\&sp[0])} body
	    return "P(0); putdbl(&sp\[0\], $body); G;"
	}
	M.qd {
	    regsub -all {\$1\.d} $body {getdbl(\&sp[0])} body
	    return "P(0); putlong(&sp\[0\], $body); G;"
	}
	M.d {
	    regsub -all {\$1\.d} $body {getdbl(\&sp[0])} body
	    return "P(0); putdbl(&sp\[0\], $body); G;"
	}
	M.d? {
	    # Double from value
	    regsub -all {\$1} $body {A(1)} body
	    return "M(-1); putdbl(&sp\[0\], $body); G;"
	}
	M.?d {
	    # Value from double
	    regsub -all {\$1\.d} $body {getdbl(\&sp[-1])} body
	    return "P(1); A(0).$suffix = $body;"
	}	    
	M.q {
	    regsub -all {\$1\.q} $body {getlong(\&sp[0])} body
	    return "P(0); putlong(&sp\[0\], $body); G;"
	}
	M.q? {
	    # Long from value
	    regsub -all {\$1} $body {A(1)} body
	    return "M(-1); putlong(&sp\[0\], $body); G;"
	}
	M.?q {
	    # Value from long
	    regsub -all {\$1\.q} $body {getlong(\&sp[-1])} body
	    return "P(1); A(0).$suffix = $body;"
	}	    
	M.? {
	    regsub -all {\$1} $body {A(0)} body
	    return "A(0).$suffix = $body;"
	}
	V.d {
	    return "P(-2); putdbl(&sp\[0\], $body); G;"
	}
	V.q {
	    return "P(-2); putlong(&sp\[0\], $body); G;"
	}
	V.? {
	    return "P(-1); A(0).$suffix = $body;"
	}	    
	S0 {
	    return "{ $body }"
	}
	S[123] {
	    regexp {S(.)} $key _ x
	    for {set i 1} {$i < $x} {incr i} {
		regsub -all "\\\$$i" $body "sp\[-$i\]" body
	    }
	    regsub -all "\\\$$x" $body "A(-$x)" body
	    return "M($x); { $body } G;"
	}
	S1d {
    	    regsub -all {\$1\.d} $body {getdbl(\&sp[-2])} body
	    return "P(2); { $body } G;"
	}
	S2d? {
    	    regsub -all {\$1\.d} $body {getdbl(\&sp[-2])} body
	    regsub -all {\$2} $body {A(-3)} body
	    return "M(3); { $body } G;"
	}
	S3d?? {
    	    regsub -all {\$1\.d} $body {getdbl(\&sp[-2])} body
	    regsub -all {\$2} $body {sp[-3]} body
	    regsub -all {\$3} $body {A(-4)} body
	    return "M(4); { $body } G;"
	}
	S1q {
    	    regsub -all {\$1\.q} $body {getlong(\&sp[-2])} body
	    return "P(2); { $body } G;"
	}
	S2q? {
    	    regsub -all {\$1\.q} $body {getlong(\&sp[-2])} body
	    regsub -all {\$2} $body {A(-3)} body
	    return "M(3); { $body } G;"
	}
	S3q?? {
    	    regsub -all {\$1\.q} $body {getlong(\&sp[-2])} body
	    regsub -all {\$2} $body {sp[-3]} body
	    regsub -all {\$3} $body {A(-4)} body
	    return "M(4); { $body } G;"
	}
	T2 {
	    regsub -all {\$1} $body {sp[0]} body
	    regsub -all {\$2} $body {A(-1)} body
	    return "M(1); { $body } G;"
	}
	default {
	    error "Bad key $key for $err_op"
	}
    }
}

proc gen_interp {name sname} {
    global skelf ncodes opcode defs copy ops action input err_op
    
    set f [open $name "w"]
    set skelf [open $sname "r"]
    
    puts $f "/* Instruction interpreter -- generated by iset.tcl */"
    puts $f ""
		
    copy_some $f
		
    # iname array
    for {set i 0} {$i < $ncodes} {incr i} {
	with $opcode($i) {op inst patt arg len} {
	    puts $f "     { \"$inst\", \"$patt\", $arg, $len },"
	}
    }
    
    copy_some $f
    
    # macros used in action code
    puts $f $defs

    copy_some $f
    
    # jtable array
    for {set i 0} {$i < 256} {incr i} {
	if {$i < $ncodes} {
	    with $opcode($i) {op inst patt arg len} {
		puts $f "          &&lbl_$op,"
	    }
	} else {
	    puts $f "          &&lbl_ILLEGAL,"
	}
    }
    
    copy_some $f
    
    # action code
    foreach op $ops {
	set err_op $op
	with $action($op) {base count length inst key act argv} {
	    set act [make_body $key $act $argv]
	    puts $f "          ACTION($op)"
	    for {set j 1} {$j < $count} {incr j} {
		puts $f "          ALSO($op+$j)"
	    }
	    puts $f "               pc = pc0 + $length;"
	    puts $f "               $act"
	    puts $f "               NEXT;"
	    puts $f ""
	}
    }
    
    copy_some $f

    close $skelf
    close $f
}


# MAIN PROGRAM

readfile $infile

if {$status != 0} {exit $status}

gen_template $tfile
gen_interp $ifile $sfile
gen_header $hfile

# Print statistics
puts "Instr     Count  Opcodes"
set fmt "%-10s %3d    %3d"
set count(0) 0;
set count(1) 1; # Allow for ILLEGAL
set count(2) 0
foreach inst $instrs {
    if {$opcount($inst) <= 2} {
	incr count($opcount($inst))
    } else {
	puts [format $fmt $inst 1 $opcount($inst)]
    }
}
puts [format $fmt "singles" $count(1) $count(1)]
puts [format $fmt "doubles" $count(2) [expr {2 * $count(2)}]]
puts [format $fmt "Total" $ninstr $ncodes]

if {$ncodes > 256} {set status 1}

exit $status
