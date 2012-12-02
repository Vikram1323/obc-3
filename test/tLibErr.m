MODULE tLibErr;

(*<<
Runtime error: string is not null-terminated
In procedure COMPARE
   called from tLibErr.%main
   called from MAIN
>>*)

VAR t: ARRAY 2 OF CHAR; b: BOOLEAN;

BEGIN
  t[0] := 'a'; t[1] := 'b';
  b := (t = 'ab')
END tLibErr.
  
(*[[
!! SYMFILE #tLibErr STAMP #tLibErr.%main 1
!! END STAMP
!! 
MODULE tLibErr STAMP 0
ENDHDR

PROC tLibErr.%main 0 20 0
!   t[0] := 'a'; t[1] := 'b';
CONST 97
STGC tLibErr.t
CONST 98
CONST tLibErr.t
CONST 1
STIC
!   b := (t = 'ab')
CONST 3
CONST tLibErr.%1
CONST 2
CONST tLibErr.t
CONST COMPARE
CALLW 4
CONST 0
EQ
STGC tLibErr.b
RETURN
END

! Global variables
GLOBAL tLibErr.t 2
GLOBAL tLibErr.b 1

! String "ab"
DEFINE tLibErr.%1
STRING 616200

! End of file
]]*)
