#
# makeprims.tcl
# 
# This file is part of the Oxford Oberon-2 compiler
# Copyright (c) 2006--2016 J. M. Spivey
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

# This script scans the files of Oberon code that make up the standard
# library, and extracts the C routines that implement primitives.

set prims {}

proc scanfile {fn} {
    global prims known

    set f [open $fn "r"]
    set lnum 0

    while {[gets $f line] >= 0} {
	incr lnum;

	if {[regexp {^PRIMDEF (.*) (.*) (.*)} $line _ name func patt]} {
            if {[info exists known($func)]} continue
            lappend prims $name $func $patt
            set known($func) 1
	}
    }

    close $f
}

foreach fn $argv {scanfile $fn}

puts "/* Generated by makeprims.tcl */"
puts ""
puts "#include \"obx.h\""
puts "#include <math.h>"
puts "#include <ctype.h>"
puts ""

proc map {f xs} {
    set r {}
    foreach x $xs {
        lappend r [uplevel $f $x]
    }
    return $r
}

proc fmt_type {c} {
    switch $c {
        C - I {return int}
        L {return longint}
        F {return float}
        D {return double}
        P - Q - X - Z {return "void *"}
        V {return void}
        default {error "type $c"}
    }
}

# Function declarations
foreach {name func patt} $prims {
    if {! [string is upper [string index $func 0]]} continue
    set restype [fmt_type [string index $patt 0]]
    set argtypes [map fmt_type [split [string range $patt 1 end] {}]]
    puts "$restype ${func}([join $argtypes ", "]);"
}
puts ""

# Wrappers

proc result {rtype val} {
    switch $rtype {
        I - C {return "ob_res.i = $val;"}
        L {return "put_long(&ob_res, $val);"}
        F {return "ob_res.f = $val;"}
        D {return "put_double(&ob_res, $val);"}
        P {return "ob_res.a = address($val);"}
        Q {return "put_long(&ob_res, (ptrtype) $val);"}
        V {return "$val;"}
        default {error "result $rtype"}
    }
}

proc args {atypes} {
    set res {}
    set i 0
    foreach a [split $atypes {}] {
        switch $a {
            I {set e "bp\[HEAD+$i\].i"; incr i}
            C {set e "align_byte(bp\[HEAD+$i\].i)"; incr i}
            L {set e "get_long(&bp\[HEAD+$i\])"; incr i 2}
            F {set e "bp\[HEAD+$i\].f"; incr i}
            D {set e "get_double(&bp\[HEAD+$i\])"; incr i 2}
            P {set e "pointer(bp\[HEAD+$i\])"; incr i}
            X {set e "pointer(bp\[HEAD+$i\])"; incr i 2}
            Q {set e "ptrcast(void, get_long(&bp\[HEAD+$i\]))"; incr i 2}
            Z {set e "bp"}
            default {error "arg $a"}
        }
        lappend res $e
    }
    return $res
}

proc call {func params} {
    set args [join $params ", "]
    return "${func}($args)"
}


foreach {name func patt} $prims {
    puts "void P_${func}(value *bp) {"
    puts "     FPINIT;"

    set rtype [string index $patt 0]
    set atypes [string range $patt 1 end]
    puts "     [result $rtype [call $func [args $atypes]]]"

    puts "}"
    puts ""
}
    
# Table
puts "#ifndef DYNLINK"
puts "struct primdef primtab\[\] = {"
foreach {name func patt} $prims {
    puts "     { \"$func\", P_$func },"
}
puts "     { NULL, NULL }"
puts "};"
puts "#endif"

exit


proc maketable {} {
    global prims

    puts "/* primtab.c */"
    puts ""
    puts "#include \"obx.h\""
    puts ""
    puts -nonewline "PRIMDEF primitive"
    set sep ""
    foreach p $prims {
	puts $sep
	puts -nonewline "     $p"
	set sep ","
    }
    puts ";"
    puts ""
}

if {[lindex $argv 0] == "-t"} {
    set mode "table"
    set argv [lrange $argv 1 end]
}

init



if {$mode == "table"} maketable
