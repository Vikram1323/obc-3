MODULE tDive;

(*<<
Runtime error: stack overflow in module tDive
In procedure tDive.p
   called from tDive.p
   called from tDive.p
   called from tDive.p
   called from tDive.p
   called from tDive.p
   ... 87336 intervening frames omitted ...
   called from tDive.p
   called from tDive.p
   called from tDive.p
   called from tDive.%main
   called from MAIN
>>*)

PROCEDURE p;
BEGIN
  p; p
END p;

BEGIN
  p
END tDive.

(*[[
!! SYMFILE #tDive STAMP #tDive.%main 1
!! END STAMP
!! 
MODULE tDive STAMP 0
ENDHDR

PROC tDive.p 0 4 0
! PROCEDURE p;
!   p; p
CONST tDive.p
CALL 0
CONST tDive.p
CALL 0
RETURN
END

PROC tDive.%main 0 4 0
!   p
CONST tDive.p
CALL 0
RETURN
END

! End of file
]]*)
