MODULE tAssert07;

(*<<
Runtime error: assertion failed (0) on line 12 in module tAssert07
In procedure tAssert07.Fail
   called from tAssert07.%main
   called from MAIN
>>*)

PROCEDURE Fail;
BEGIN
  ASSERT(1 < 0)
END Fail;

BEGIN
  Fail
END tAssert07.

(*[[
!! (SYMFILE #tAssert07 STAMP #tAssert07.%main 1)
!! (CHKSUM STAMP)
!! 
MODULE tAssert07 STAMP 0
ENDHDR

PROC tAssert07.Fail 0 4 0
! PROCEDURE Fail;
CONST 0
CONST 12
GLOBAL EASSERT
CALL 2
RETURN
END

PROC tAssert07.%main 0 2 0
!   Fail
GLOBAL tAssert07.Fail
CALL 0
RETURN
END

! End of file
]]*)
