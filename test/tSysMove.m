MODULE tSysMove;

IMPORT Out, SYSTEM;

VAR 
  a: ARRAY 10 OF INTEGER;
  i : INTEGER;

BEGIN
  FOR i := 0 TO 9 DO a[i]:= i END;
  SYSTEM.MOVE(SYSTEM.ADR(a[2]), SYSTEM.ADR(a[4]), 4*SIZE(INTEGER));
  FOR i := 0 TO 9 DO Out.Int(a[i], 2) END;
  Out.Ln
END tSysMove.

(*<<
 0 1 2 3 2 3 4 5 8 9
>>*)

(*[[
!! SYMFILE #tSysMove STAMP #tSysMove.%main 1
!! END STAMP
!! 
MODULE tSysMove STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tSysMove.%main 0 20 0
!   FOR i := 0 TO 9 DO a[i]:= i END;
CONST 0
STGW tSysMove.i
JUMP 2
LABEL 1
LDGW tSysMove.i
CONST tSysMove.a
LDGW tSysMove.i
CONST 10
BOUND 10
STIW
LDGW tSysMove.i
INC
STGW tSysMove.i
LABEL 2
LDGW tSysMove.i
CONST 9
JLEQ 1
!   SYSTEM.MOVE(SYSTEM.ADR(a[2]), SYSTEM.ADR(a[4]), 4*SIZE(INTEGER));
CONST 16
CONST tSysMove.a
CONST 16
PLUSA
CONST tSysMove.a
CONST 8
PLUSA
CONST MOVE
CALL 3
!   FOR i := 0 TO 9 DO Out.Int(a[i], 2) END;
CONST 0
STGW tSysMove.i
JUMP 4
LABEL 3
CONST 2
CONST tSysMove.a
LDGW tSysMove.i
CONST 10
BOUND 12
LDIW
CONST Out.Int
CALL 2
LDGW tSysMove.i
INC
STGW tSysMove.i
LABEL 4
LDGW tSysMove.i
CONST 9
JLEQ 3
!   Out.Ln
CONST Out.Ln
CALL 0
RETURN
END

! Global variables
GLOBAL tSysMove.a 40
GLOBAL tSysMove.i 4

! End of file
]]*)
