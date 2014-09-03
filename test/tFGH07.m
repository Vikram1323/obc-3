MODULE tFGH07;

(*<<
17
>>*)

IMPORT Out;

PROCEDURE f(): INTEGER;

  VAR x: INTEGER;

  PROCEDURE g;
    PROCEDURE h(i: INTEGER); BEGIN x := i END h;
  BEGIN h(17) END g;

BEGIN
  g;
  RETURN x
END f;

BEGIN
  Out.Int(f(), 0); Out.Ln
END tFGH07.

(*[[
!! SYMFILE #tFGH07 STAMP #tFGH07.%main 1
!! END STAMP
!! 
MODULE tFGH07 STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tFGH07.%2.h 4 3 0
!     PROCEDURE h(i: INTEGER); BEGIN x := i END h;
SAVELINK
LDLW 12
LDEW -4
STNW -4
RETURN
END

PROC tFGH07.%1.g 4 3 0
!   PROCEDURE g;
SAVELINK
!   BEGIN h(17) END g;
CONST 17
LOCAL 0
LINK
GLOBAL tFGH07.%2.h
CALL 1
RETURN
END

PROC tFGH07.f 4 3 0
! PROCEDURE f(): INTEGER;
!   g;
LOCAL 0
LINK
GLOBAL tFGH07.%1.g
CALL 0
!   RETURN x
LDLW -4
RETURNW
END

PROC tFGH07.%main 0 3 0
!   Out.Int(f(), 0); Out.Ln
CONST 0
GLOBAL tFGH07.f
CALLW 0
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! End of file
]]*)

