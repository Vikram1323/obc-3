MODULE tCaseEmpty;

(*<<
>>*)

BEGIN
  CASE 0 OF ELSE END
END tCaseEmpty.

(*[[
!! SYMFILE #tCaseEmpty STAMP #tCaseEmpty.%main 1
!! END STAMP
!! 
MODULE tCaseEmpty STAMP 0
ENDHDR

PROC tCaseEmpty.%main 0 4 0
!   CASE 0 OF ELSE END
RETURN
END

! End of file
]]*)
