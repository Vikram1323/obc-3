MODULE tFac;

(*<<
The factorial of 10 is 3628800
>>*)

IMPORT Out;

PROCEDURE fac(n: INTEGER): INTEGER;
BEGIN 
  IF n = 0 THEN 
    RETURN 1
  ELSE 
    RETURN n * fac(n-1)
  END
END fac;

CONST nnn = 10;

BEGIN
  Out.String("The factorial of "); Out.Int(nnn, 0);
  Out.String(" is "); Out.Int(fac(nnn), 0); Out.Ln
END tFac.

(*[[
!! SYMFILE #tFac STAMP #tFac.%main 1
!! END STAMP
!! 
MODULE tFac STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tFac.fac 0 12 0
! PROCEDURE fac(n: INTEGER): INTEGER;
!   IF n = 0 THEN 
LDLW 12
JNEQZ 4
!     RETURN 1
CONST 1
RETURNW
LABEL 4
!     RETURN n * fac(n-1)
LDLW 12
LDLW 12
DEC
CONST tFac.fac
CALLW 1
TIMES
RETURNW
END

PROC tFac.%main 0 12 0
!   Out.String("The factorial of "); Out.Int(nnn, 0);
CONST 18
CONST tFac.%1
CONST Out.String
CALL 2
CONST 0
CONST 10
CONST Out.Int
CALL 2
!   Out.String(" is "); Out.Int(fac(nnn), 0); Out.Ln
CONST 5
CONST tFac.%2
CONST Out.String
CALL 2
CONST 0
CONST 10
CONST tFac.fac
CALLW 1
CONST Out.Int
CALL 2
CONST Out.Ln
CALL 0
RETURN
END

! String "The factorial of "
DEFINE tFac.%1
STRING 54686520666163746F7269616C206F662000

! String " is "
DEFINE tFac.%2
STRING 2069732000

! End of file
]]*)
