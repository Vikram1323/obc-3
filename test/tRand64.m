MODULE tRand64;

(* Check the first few random numbers are as expected *)

IMPORT Random, Out;

(* An implementation of the same generator as in Random.m, but using
   LONGINT. *)

CONST a = 48271; m = 2147483647;

VAR u: LONGINT;

PROCEDURE next(): INTEGER;
BEGIN
  u := (a * u) MOD m;
  RETURN SHORT(u)
END next;

VAR x, y, z: INTEGER;

BEGIN
  u := 31415926;

  (* Check that Random.Random agrees with our slow implementation *)
  FOR x := 1 TO 100 DO 
    y := Random.Random();
    z := next();
    Out.Int(y, 10); Out.Int(z, 12); Out.Ln;
    ASSERT(y = z)
  END;

  (* A more thorough check *)
  FOR x := 1 TO 100000 DO ASSERT(Random.Random() = next()) END;
END tRand64.

(*<<
 354709164   354709164
 278937913   278937913
2037015380  2037015380
1935662791  1935662791
1512587038  1512587038
1792396945  1792396945
 824278112   824278112
 151732736   151732736
1371663186  1371663186
 337847102   337847102
 226645324   226645324
1114736986  1114736986
2118791974  2118791974
 151204932   151204932
1663840066  1663840066
1482911733  1482911733
1707341839  1707341839
1117989450  1117989450
 204691840   204691840
 107548793   107548793
1019812104  1019812104
 582432003   582432003
1866793936  1866793936
1448772889  1448772889
 911160364   911160364
   9356437     9356437
 673004557   673004557
1617842778  1617842778
1645913683  1645913683
1594387681  1594387681
1168808365  1168808365
 858212931   858212931
1836841671  1836841671
 879483505   879483505
2091535959  2091535959
 883580478   883580478
 140540471   140540471
 128234768   128234768
 972615474   972615474
 834054740   834054740
1780424231  1780424231
 562501661   562501661
1881929110  1881929110
1894317063  1894317063
 725258813   725258813
 689748929   689748929
 284088671   284088671
1561151746  1561151746
1007274289  1007274289
 959952592   959952592
1616917113  1616917113
2060295055  2060295055
 387423688   387423688
1041245372  1041245372
 100593777   100593777
 301683700   301683700
 487272393   487272393
1884780559  1884780559
2097658334  2097658334
  64000817    64000817
1301953021  1301953021
 465347236   465347236
  97481336    97481336
 384899479   384899479
1601720612  1601720612
 801918911   801918911
1035015706  1035015706
  36096871    36096871
 822822324   822822324
 746350539   746350539
 901205997   901205997
 538443908   538443908
 231303427   231303427
 480243964   480243964
1917900526  1917900526
 956268376   956268376
2017269278  2017269278
 106828770   106828770
 623320223   623320223
2044589963  2044589963
 348655147   348655147
 103259298   103259298
 120029071   120029071
  12406635    12406635
1880224219  1880224219
1201902188  1201902188
 602309596   602309596
1452895430  1452895430
 194357804   194357804
1636986788  1636986788
 180968536   180968536
1716208907  1716208907
1790983125  1790983125
1297249596  1297249596
1059585643  1059585643
 640552654   640552654
 647611728   647611728
2093756556  2093756556
 699835915   699835915
1861685655  1861685655
>>*)

(*[[
!! (SYMFILE #tRand64 STAMP #tRand64.%main 1 #tRand64.m)
!! (CHKSUM STAMP)
!! 
MODULE tRand64 STAMP 0
IMPORT Random STAMP
IMPORT Out STAMP
ENDHDR

PROC tRand64.next 0 4 0
! PROCEDURE next(): INTEGER;
!   u := (a * u) MOD m;
LDGQ tRand64.u
QCONST 48271
QTIMES
QCONST 2147483647
QMOD
STGQ tRand64.u
!   RETURN SHORT(u)
LDGQ tRand64.u
CONVQN
RETURNW
END

PROC tRand64.%main 0 3 0
!   u := 31415926;
QCONST 31415926
STGQ tRand64.u
!   FOR x := 1 TO 100 DO 
CONST 1
STGW tRand64.x
LABEL L1
LDGW tRand64.x
CONST 100
JGT L2
!     y := Random.Random();
GLOBAL Random.Random
CALLW 0
STGW tRand64.y
!     z := next();
GLOBAL tRand64.next
CALLW 0
STGW tRand64.z
!     Out.Int(y, 10); Out.Int(z, 12); Out.Ln;
CONST 10
LDGW tRand64.y
GLOBAL Out.Int
CALL 2
CONST 12
LDGW tRand64.z
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!     ASSERT(y = z)
LDGW tRand64.y
LDGW tRand64.z
JEQ L4
CONST 0
EASSERT 30
LABEL L4
!   FOR x := 1 TO 100 DO 
LDGW tRand64.x
INC
STGW tRand64.x
JUMP L1
LABEL L2
!   FOR x := 1 TO 100000 DO ASSERT(Random.Random() = next()) END;
CONST 1
STGW tRand64.x
LABEL L5
LDGW tRand64.x
CONST 100000
JGT L6
GLOBAL Random.Random
CALLW 0
GLOBAL tRand64.next
CALLW 0
JEQ L8
CONST 0
EASSERT 34
LABEL L8
LDGW tRand64.x
INC
STGW tRand64.x
JUMP L5
LABEL L6
RETURN
END

! Global variables
GLOVAR tRand64.u 8
GLOVAR tRand64.x 4
GLOVAR tRand64.y 4
GLOVAR tRand64.z 4

! End of file
]]*)
