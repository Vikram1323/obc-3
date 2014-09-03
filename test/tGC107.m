MODULE tGC107;

(*<<
Done 2097152
>>*)

IMPORT Out, GC;

TYPE ptr = POINTER TO ARRAY 256 OF INTEGER;

VAR i: INTEGER; p: ptr;

BEGIN
  FOR i := 1 TO 128 * 1024 DO
    NEW(p)
  END;
  Out.String("Done "); Out.Int(GC.HeapSize(), 0); Out.Ln
END tGC107.

(*[[
!! SYMFILE #tGC107 STAMP #tGC107.%main 1
!! END STAMP
!! 
MODULE tGC107 STAMP 0
IMPORT Out STAMP
IMPORT GC STAMP
ENDHDR

PROC tGC107.%main 0 4 0
!   FOR i := 1 TO 128 * 1024 DO
CONST 1
STGW tGC107.i
LABEL 2
LDGW tGC107.i
CONST 131072
JGT 3
!     NEW(p)
CONST 1024
CONST 0
GLOBAL tGC107.p
GLOBAL NEW
CALL 3
LDGW tGC107.i
INC
STGW tGC107.i
JUMP 2
LABEL 3
!   Out.String("Done "); Out.Int(GC.HeapSize(), 0); Out.Ln
CONST 6
GLOBAL tGC107.%1
GLOBAL Out.String
CALL 2
CONST 0
GLOBAL GC.HeapSize
CALLW 0
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tGC107.i 4
GLOVAR tGC107.p 4

! Pointer map
DEFINE tGC107.%gcmap
WORD GC_BASE
WORD tGC107.p
WORD 0
WORD GC_END

! String "Done "
DEFINE tGC107.%1
STRING 446F6E652000

! End of file
]]*)
