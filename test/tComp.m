MODULE tComp;

(*<<
1
>>*)

IMPORT Out;

VAR x, y: REAL; b: BOOLEAN;

BEGIN
  x := 3; y := 4;
  b := (x < 4);
  Out.Int(ORD(b), 0); Out.Ln
END tComp.

(*[[
!! SYMFILE #tComp STAMP #tComp.%main 1
!! END STAMP
!! 
MODULE tComp STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tComp.%main 0 12 0
!   x := 3; y := 4;
FCONST 3.
STGF tComp.x
FCONST 4.
STGF tComp.y
!   b := (x < 4);
LDGF tComp.x
FCONST 4.
FLT
STGC tComp.b
!   Out.Int(ORD(b), 0); Out.Ln
CONST 0
LDGC tComp.b
CONST Out.Int
CALL 2
CONST Out.Ln
CALL 0
RETURN
END

! Global variables
GLOBAL tComp.x 4
GLOBAL tComp.y 4
GLOBAL tComp.b 1

! End of file
]]*)
