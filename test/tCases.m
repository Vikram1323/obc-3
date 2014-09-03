MODULE tCases;

(*<<
1
4
7
8
10
abcdefghijklmnopqrstuvwxyz
>>*)

IMPORT Out;

VAR i: INTEGER; c: CHAR;

BEGIN
  i := 0;
  WHILE i < 10 DO
    CASE i OF
    |  2, 6: 
        i := i - 1;
    | 9..10:
	Out.String("Fail"); Out.Ln
    || 8:
        i := i + 2;
    | 1, 3, 4, 5:
	i := i + 1;
	i := i + 2
    ELSE
      i := i + 1
    END;
    Out.Int(i, 0); Out.Ln()
  END;

  FOR i := 0 TO 127 DO
    c := CHR(i);
    CASE c OF
       'a'..'z': Out.Char(c)
    ELSE
    END
  END;
  Out.Ln
END tCases.

(*[[
!! SYMFILE #tCases STAMP #tCases.%main 1
!! END STAMP
!! 
MODULE tCases STAMP 0
IMPORT Out STAMP
ENDHDR

PROC tCases.%main 0 3 0
!   i := 0;
CONST 0
STGW tCases.i
LABEL 2
!   WHILE i < 10 DO
LDGW tCases.i
CONST 10
JGEQ 4
!     CASE i OF
LDGW tCases.i
DEC
JCASE 10
CASEL 10
CASEL 7
CASEL 10
CASEL 10
CASEL 10
CASEL 7
CASEL 5
CASEL 9
CASEL 8
CASEL 8
JUMP 5
LABEL 7
!         i := i - 1;
LDGW tCases.i
DEC
STGW tCases.i
JUMP 6
LABEL 8
! 	Out.String("Fail"); Out.Ln
CONST 5
GLOBAL tCases.%1
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
JUMP 6
LABEL 9
!         i := i + 2;
LDGW tCases.i
CONST 2
PLUS
STGW tCases.i
JUMP 6
LABEL 10
! 	i := i + 1;
LDGW tCases.i
INC
STGW tCases.i
! 	i := i + 2
LDGW tCases.i
CONST 2
PLUS
STGW tCases.i
JUMP 6
LABEL 5
!       i := i + 1
LDGW tCases.i
INC
STGW tCases.i
LABEL 6
!     Out.Int(i, 0); Out.Ln()
CONST 0
LDGW tCases.i
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
JUMP 2
LABEL 4
!   FOR i := 0 TO 127 DO
CONST 0
STGW tCases.i
LABEL 11
LDGW tCases.i
CONST 127
JGT 12
!     c := CHR(i);
LDGW tCases.i
STGC tCases.c
!     CASE c OF
LDGC tCases.c
CONST 97
CONST 122
JRANGE 15
JUMP 13
LABEL 15
!        'a'..'z': Out.Char(c)
LDGC tCases.c
ALIGNC
GLOBAL Out.Char
CALL 1
LABEL 13
!     ELSE
LDGW tCases.i
INC
STGW tCases.i
JUMP 11
LABEL 12
!   Out.Ln
GLOBAL Out.Ln
CALL 0
RETURN
END

! Global variables
GLOVAR tCases.i 4
GLOVAR tCases.c 1

! String "Fail"
DEFINE tCases.%1
STRING 4661696C00

! End of file
]]*)
