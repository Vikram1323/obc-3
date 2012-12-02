MODULE tEnum;

(*<<
4
red
blue 1
green 2
0
>>*)

IMPORT X := xPrelude, Out;

PROCEDURE Print(c: X.colour);
BEGIN
  IF c = X.red THEN
    Out.String("red")
  ELSE
    CASE c OF
        X.blue: Out.String("blue")
      | X.green: Out.String("green")
    END;
    Out.Int(ORD(c), 2)
  END
END Print;

VAR x: X.colour;
BEGIN
  Out.Int(SIZE(X.colour), 0); Out.Ln;
  FOR x := MIN(X.colour) TO MAX(X.colour) DO
    X.x := x; Print(x); Out.Ln
  END;
  Out.Int(ORD(X.x > MAX(X.colour)), 0); Out.Ln
END tEnum.

(*[[
!! SYMFILE #tEnum STAMP #tEnum.%main 1
!! END STAMP
!! 
MODULE tEnum STAMP 0
IMPORT xPrelude STAMP
IMPORT Out STAMP
ENDHDR

PROC tEnum.Print 0 12 0
! PROCEDURE Print(c: X.colour);
!   IF c = X.red THEN
LDLW 12
JNEQZ 5
!     Out.String("red")
CONST 4
CONST tEnum.%1
CONST Out.String
CALL 2
RETURN
LABEL 5
!     CASE c OF
LDLW 12
DEC
JCASE 2
CASEL 8
CASEL 9
JUMP 6
LABEL 8
!         X.blue: Out.String("blue")
CONST 5
CONST tEnum.%2
CONST Out.String
CALL 2
JUMP 7
LABEL 9
!       | X.green: Out.String("green")
CONST 6
CONST tEnum.%3
CONST Out.String
CALL 2
JUMP 7
LABEL 6
ERROR E_CASE 18
LABEL 7
!     Out.Int(ORD(c), 2)
CONST 2
LDLW 12
CONST Out.Int
CALL 2
RETURN
END

PROC tEnum.%main 0 12 0
!   Out.Int(SIZE(X.colour), 0); Out.Ln;
CONST 0
CONST 4
CONST Out.Int
CALL 2
CONST Out.Ln
CALL 0
!   FOR x := MIN(X.colour) TO MAX(X.colour) DO
CONST 0
STGW tEnum.x
JUMP 11
LABEL 10
!     X.x := x; Print(x); Out.Ln
LDGW tEnum.x
STGW xPrelude.x
LDGW tEnum.x
CONST tEnum.Print
CALL 1
CONST Out.Ln
CALL 0
!   FOR x := MIN(X.colour) TO MAX(X.colour) DO
LDGW tEnum.x
INC
STGW tEnum.x
LABEL 11
LDGW tEnum.x
CONST 2
JLEQ 10
!   Out.Int(ORD(X.x > MAX(X.colour)), 0); Out.Ln
CONST 0
LDGW xPrelude.x
CONST 2
GT
CONST Out.Int
CALL 2
CONST Out.Ln
CALL 0
RETURN
END

! Global variables
GLOBAL tEnum.x 4

! String "red"
DEFINE tEnum.%1
STRING 72656400

! String "blue"
DEFINE tEnum.%2
STRING 626C756500

! String "green"
DEFINE tEnum.%3
STRING 677265656E00

! End of file
]]*)
