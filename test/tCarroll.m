MODULE tCarroll;

IMPORT Out, Random;

CONST n = 5; N = LSL(1, n);

VAR
  first, second: ARRAY N OF ARRAY n+1 OF INTEGER;
  kith, kin: ARRAY N OF INTEGER;
  busy: ARRAY N OF ARRAY n+1 OF BOOLEAN;
  matches: INTEGER;

PROCEDURE min(a, b: INTEGER): INTEGER;
BEGIN
  IF a <= b THEN RETURN a ELSE RETURN b END
END min;

PROCEDURE max(a, b: INTEGER): INTEGER;
BEGIN
  IF a <= b THEN RETURN b ELSE RETURN a END
END max;

PROCEDURE play(a, b, t: INTEGER);
BEGIN
  Out.Int(a, 0); Out.String(" plays "); Out.Int(b, 0);
  Out.String(" at time "); Out.Int(t, 0); Out.Ln;
  IF busy[a,t] OR busy[b,t] THEN
    Out.String("Conflict"); Out.Ln; HALT(1)
  END;
  busy[a,t] := TRUE; busy[b,t] := TRUE;
  INC(matches)
END play;

PROCEDURE trial;
  VAR a, b, t, p, q, r: INTEGER;
BEGIN
  matches := 0;

  FOR a := 0 TO N-1 DO
    first[a,0] := a;
    FOR t := 0 TO n DO
      busy[a,t] := FALSE
    END
  END;

  FOR a := 1 TO N-1 DO
    b := Random.Roll(a+1);
    p := first[b,0];
    first[b,0] := first[a,0];
    first[a,0] := p
  END;

  FOR a := 0 TO N-1 DO
    Out.Char(' ');
    Out.Int(first[a,0], 0)
  END;
  Out.Ln;

  (* First round: play in pairs *)
  r := N DIV 2;
  FOR a := 0 TO r-1 DO
    p := first[2*a,0]; q := first[2*a+1,0];
    play(p, q, 0);
    first[a,1] := min(p, q);
    second[a,1] := max(p, q);
  END;

  FOR t := 1 TO n-1 DO
    (* Subsequent rounds: primary competition *)
    r := r DIV 2;
    FOR a := 0 TO r-1 DO
      p := first[2*a,t]; q := first[2*a+1,t];
      play(p, q, t);
      IF p <= q THEN
        first[a,t+1] := p;
        kith[a] := second[2*a,t]; kin[a] := q
      ELSE
        first[a,t+1] := q;
        kith[a] := p; kin[a] := second[2*a+1,t]
      END
    END;
    
    (* Playoff on next day *)
    FOR a := 0 TO r-1 DO
      p := kith[a]; q := kin[a];
      play(p, q, t+1); second[a,t+1] := min(p, q)
    END
  END;

  Out.String("Result "); Out.Int(second[0,n], 0); Out.Ln;
  Out.Int(matches, 0); Out.String(" matches"); Out.Ln
END trial;

BEGIN
  trial; trial; trial; trial; trial
END tCarroll.
        
(*<<
 26 20 1 7 5 6 22 3 27 18 21 25 15 13 17 9 24 31 4 14 23 28 29 30 10 16 8 2 0 12 19 11
26 plays 20 at time 0
1 plays 7 at time 0
5 plays 6 at time 0
22 plays 3 at time 0
27 plays 18 at time 0
21 plays 25 at time 0
15 plays 13 at time 0
17 plays 9 at time 0
24 plays 31 at time 0
4 plays 14 at time 0
23 plays 28 at time 0
29 plays 30 at time 0
10 plays 16 at time 0
8 plays 2 at time 0
0 plays 12 at time 0
19 plays 11 at time 0
20 plays 1 at time 1
5 plays 3 at time 1
18 plays 21 at time 1
13 plays 9 at time 1
24 plays 4 at time 1
23 plays 29 at time 1
10 plays 2 at time 1
0 plays 11 at time 1
20 plays 7 at time 2
5 plays 22 at time 2
27 plays 21 at time 2
13 plays 17 at time 2
24 plays 14 at time 2
28 plays 29 at time 2
10 plays 8 at time 2
12 plays 11 at time 2
1 plays 3 at time 2
18 plays 9 at time 2
4 plays 23 at time 2
2 plays 0 at time 2
7 plays 3 at time 3
18 plays 13 at time 3
14 plays 23 at time 3
2 plays 11 at time 3
1 plays 9 at time 3
4 plays 0 at time 3
3 plays 9 at time 4
4 plays 2 at time 4
1 plays 0 at time 4
1 plays 2 at time 5
Result 1
46 matches
 29 24 16 25 22 15 31 6 19 10 0 23 13 17 2 20 5 9 30 18 14 21 8 12 27 11 1 3 28 7 4 26
29 plays 24 at time 0
16 plays 25 at time 0
22 plays 15 at time 0
31 plays 6 at time 0
19 plays 10 at time 0
0 plays 23 at time 0
13 plays 17 at time 0
2 plays 20 at time 0
5 plays 9 at time 0
30 plays 18 at time 0
14 plays 21 at time 0
8 plays 12 at time 0
27 plays 11 at time 0
1 plays 3 at time 0
28 plays 7 at time 0
4 plays 26 at time 0
24 plays 16 at time 1
15 plays 6 at time 1
10 plays 0 at time 1
13 plays 2 at time 1
5 plays 18 at time 1
14 plays 8 at time 1
11 plays 1 at time 1
7 plays 4 at time 1
24 plays 25 at time 2
15 plays 31 at time 2
10 plays 23 at time 2
13 plays 20 at time 2
9 plays 18 at time 2
14 plays 12 at time 2
11 plays 3 at time 2
7 plays 26 at time 2
16 plays 6 at time 2
0 plays 2 at time 2
5 plays 8 at time 2
1 plays 4 at time 2
16 plays 15 at time 3
10 plays 2 at time 3
9 plays 8 at time 3
3 plays 4 at time 3
6 plays 0 at time 3
5 plays 1 at time 3
6 plays 2 at time 4
5 plays 3 at time 4
0 plays 1 at time 4
2 plays 1 at time 5
Result 1
46 matches
 22 21 29 19 9 17 14 25 7 0 5 4 13 24 2 15 6 3 26 8 11 23 28 20 30 1 31 12 16 27 10 18
22 plays 21 at time 0
29 plays 19 at time 0
9 plays 17 at time 0
14 plays 25 at time 0
7 plays 0 at time 0
5 plays 4 at time 0
13 plays 24 at time 0
2 plays 15 at time 0
6 plays 3 at time 0
26 plays 8 at time 0
11 plays 23 at time 0
28 plays 20 at time 0
30 plays 1 at time 0
31 plays 12 at time 0
16 plays 27 at time 0
10 plays 18 at time 0
21 plays 19 at time 1
9 plays 14 at time 1
0 plays 4 at time 1
13 plays 2 at time 1
3 plays 8 at time 1
11 plays 20 at time 1
1 plays 12 at time 1
16 plays 10 at time 1
21 plays 29 at time 2
17 plays 14 at time 2
7 plays 4 at time 2
13 plays 15 at time 2
6 plays 8 at time 2
23 plays 20 at time 2
30 plays 12 at time 2
16 plays 18 at time 2
19 plays 9 at time 2
0 plays 2 at time 2
3 plays 11 at time 2
1 plays 10 at time 2
19 plays 14 at time 3
4 plays 2 at time 3
6 plays 11 at time 3
12 plays 10 at time 3
9 plays 0 at time 3
3 plays 1 at time 3
9 plays 2 at time 4
3 plays 10 at time 4
0 plays 1 at time 4
2 plays 1 at time 5
Result 1
46 matches
 12 13 24 2 31 17 7 14 25 8 10 30 0 22 16 19 9 26 11 4 3 28 23 15 21 1 5 20 6 29 27 18
12 plays 13 at time 0
24 plays 2 at time 0
31 plays 17 at time 0
7 plays 14 at time 0
25 plays 8 at time 0
10 plays 30 at time 0
0 plays 22 at time 0
16 plays 19 at time 0
9 plays 26 at time 0
11 plays 4 at time 0
3 plays 28 at time 0
23 plays 15 at time 0
21 plays 1 at time 0
5 plays 20 at time 0
6 plays 29 at time 0
27 plays 18 at time 0
12 plays 2 at time 1
17 plays 7 at time 1
8 plays 10 at time 1
0 plays 16 at time 1
9 plays 4 at time 1
3 plays 15 at time 1
1 plays 5 at time 1
6 plays 18 at time 1
12 plays 24 at time 2
17 plays 14 at time 2
25 plays 10 at time 2
22 plays 16 at time 2
9 plays 11 at time 2
28 plays 15 at time 2
21 plays 5 at time 2
29 plays 18 at time 2
2 plays 7 at time 2
8 plays 0 at time 2
4 plays 3 at time 2
1 plays 6 at time 2
12 plays 7 at time 3
8 plays 16 at time 3
4 plays 15 at time 3
5 plays 6 at time 3
2 plays 0 at time 3
3 plays 1 at time 3
2 plays 8 at time 4
3 plays 5 at time 4
0 plays 1 at time 4
2 plays 1 at time 5
Result 1
46 matches
 19 29 27 1 23 21 11 31 17 0 26 18 7 30 20 14 15 12 10 8 3 2 6 25 24 9 22 16 28 13 4 5
19 plays 29 at time 0
27 plays 1 at time 0
23 plays 21 at time 0
11 plays 31 at time 0
17 plays 0 at time 0
26 plays 18 at time 0
7 plays 30 at time 0
20 plays 14 at time 0
15 plays 12 at time 0
10 plays 8 at time 0
3 plays 2 at time 0
6 plays 25 at time 0
24 plays 9 at time 0
22 plays 16 at time 0
28 plays 13 at time 0
4 plays 5 at time 0
19 plays 1 at time 1
21 plays 11 at time 1
0 plays 18 at time 1
7 plays 14 at time 1
12 plays 8 at time 1
2 plays 6 at time 1
9 plays 16 at time 1
13 plays 4 at time 1
19 plays 27 at time 2
21 plays 31 at time 2
17 plays 18 at time 2
30 plays 14 at time 2
12 plays 10 at time 2
3 plays 6 at time 2
24 plays 16 at time 2
13 plays 5 at time 2
1 plays 11 at time 2
0 plays 7 at time 2
8 plays 2 at time 2
9 plays 4 at time 2
19 plays 11 at time 3
17 plays 7 at time 3
8 plays 3 at time 3
9 plays 5 at time 3
1 plays 0 at time 3
2 plays 4 at time 3
1 plays 7 at time 4
3 plays 4 at time 4
0 plays 2 at time 4
1 plays 2 at time 5
Result 1
46 matches
>>*)

(*[[
!! (SYMFILE #tCarroll STAMP #tCarroll.%main 1 #tCarroll.m)
!! (CHKSUM STAMP)
!! 
MODULE tCarroll STAMP 0
IMPORT Out STAMP
IMPORT Random STAMP
ENDHDR

PROC tCarroll.min 0 2 0
! PROCEDURE min(a, b: INTEGER): INTEGER;
!   IF a <= b THEN RETURN a ELSE RETURN b END
LDLW 12
LDLW 16
JGT L8
LDLW 12
RETURN
LABEL L8
LDLW 16
RETURN
END

PROC tCarroll.max 0 2 0
! PROCEDURE max(a, b: INTEGER): INTEGER;
!   IF a <= b THEN RETURN b ELSE RETURN a END
LDLW 12
LDLW 16
JGT L11
LDLW 16
RETURN
LABEL L11
LDLW 12
RETURN
END

PROC tCarroll.play 0 5 0
! PROCEDURE play(a, b, t: INTEGER);
!   Out.Int(a, 0); Out.String(" plays "); Out.Int(b, 0);
CONST 0
LDLW 12
GLOBAL Out.Int
CALL 2
CONST 8
GLOBAL tCarroll.%1
GLOBAL Out.String
CALL 2
CONST 0
LDLW 16
GLOBAL Out.Int
CALL 2
!   Out.String(" at time "); Out.Int(t, 0); Out.Ln;
CONST 10
GLOBAL tCarroll.%2
GLOBAL Out.String
CALL 2
CONST 0
LDLW 20
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   IF busy[a,t] OR busy[b,t] THEN
GLOBAL tCarroll.busy
LDLW 12
CONST 32
BOUND 27
CONST 6
TIMES
LDLW 20
CONST 6
BOUND 27
PLUS
LDIC
JNEQZ L13
GLOBAL tCarroll.busy
LDLW 16
CONST 32
BOUND 27
CONST 6
TIMES
LDLW 20
CONST 6
BOUND 27
PLUS
LDIC
JEQZ L14
LABEL L13
!     Out.String("Conflict"); Out.Ln; HALT(1)
CONST 9
GLOBAL tCarroll.%3
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
CONST 1
GLOBAL HALT
CALL 1
LABEL L14
!   busy[a,t] := TRUE; busy[b,t] := TRUE;
CONST 1
GLOBAL tCarroll.busy
LDLW 12
CONST 32
BOUND 30
CONST 6
TIMES
LDLW 20
CONST 6
BOUND 30
PLUS
STIC
CONST 1
GLOBAL tCarroll.busy
LDLW 16
CONST 32
BOUND 30
CONST 6
TIMES
LDLW 20
CONST 6
BOUND 30
PLUS
STIC
!   INC(matches)
LDGW tCarroll.matches
INC
STGW tCarroll.matches
RETURN
END

PROC tCarroll.trial 36 5 0
! PROCEDURE trial;
!   matches := 0;
CONST 0
STGW tCarroll.matches
!   FOR a := 0 TO N-1 DO
CONST 0
STLW -4
LABEL L16
LDLW -4
CONST 31
JGT L17
!     first[a,0] := a;
LDLW -4
GLOBAL tCarroll.first
LDLW -4
CONST 32
BOUND 40
CONST 6
TIMES
STIW
!     FOR t := 0 TO n DO
CONST 0
STLW -12
LABEL L18
LDLW -12
CONST 5
JGT L19
!       busy[a,t] := FALSE
CONST 0
GLOBAL tCarroll.busy
LDLW -4
CONST 32
BOUND 42
CONST 6
TIMES
LDLW -12
CONST 6
BOUND 42
PLUS
STIC
!     FOR t := 0 TO n DO
INCL -12
JUMP L18
LABEL L19
!   FOR a := 0 TO N-1 DO
INCL -4
JUMP L16
LABEL L17
!   FOR a := 1 TO N-1 DO
CONST 1
STLW -4
LABEL L20
LDLW -4
CONST 31
JGT L21
!     b := Random.Roll(a+1);
LDLW -4
INC
GLOBAL Random.Roll
CALLW 1
STLW -8
!     p := first[b,0];
GLOBAL tCarroll.first
LDLW -8
CONST 32
BOUND 48
CONST 6
TIMES
LDIW
STLW -16
!     first[b,0] := first[a,0];
GLOBAL tCarroll.first
LDLW -4
CONST 32
BOUND 49
CONST 6
TIMES
LDIW
GLOBAL tCarroll.first
LDLW -8
CONST 32
BOUND 49
CONST 6
TIMES
STIW
!     first[a,0] := p
LDLW -16
GLOBAL tCarroll.first
LDLW -4
CONST 32
BOUND 50
CONST 6
TIMES
STIW
!   FOR a := 1 TO N-1 DO
INCL -4
JUMP L20
LABEL L21
!   FOR a := 0 TO N-1 DO
CONST 0
STLW -4
LABEL L22
LDLW -4
CONST 31
JGT L23
!     Out.Char(' ');
CONST 32
ALIGNC
GLOBAL Out.Char
CALL 1
!     Out.Int(first[a,0], 0)
CONST 0
GLOBAL tCarroll.first
LDLW -4
CONST 32
BOUND 55
CONST 6
TIMES
LDIW
GLOBAL Out.Int
CALL 2
!   FOR a := 0 TO N-1 DO
INCL -4
JUMP L22
LABEL L23
!   Out.Ln;
GLOBAL Out.Ln
CALL 0
!   r := N DIV 2;
CONST 16
STLW -24
!   FOR a := 0 TO r-1 DO
LDLW -24
DEC
STLW -28
CONST 0
STLW -4
LABEL L24
LDLW -4
LDLW -28
JGT L25
!     p := first[2*a,0]; q := first[2*a+1,0];
GLOBAL tCarroll.first
LDLW -4
CONST 2
TIMES
CONST 32
BOUND 62
CONST 6
TIMES
LDIW
STLW -16
GLOBAL tCarroll.first
LDLW -4
CONST 2
TIMES
INC
CONST 32
BOUND 62
CONST 6
TIMES
LDIW
STLW -20
!     play(p, q, 0);
CONST 0
LDLW -20
LDLW -16
GLOBAL tCarroll.play
CALL 3
!     first[a,1] := min(p, q);
LDLW -20
LDLW -16
GLOBAL tCarroll.min
CALLW 2
GLOBAL tCarroll.first
LDLW -4
CONST 32
BOUND 64
CONST 6
TIMES
INC
STIW
!     second[a,1] := max(p, q);
LDLW -20
LDLW -16
GLOBAL tCarroll.max
CALLW 2
GLOBAL tCarroll.second
LDLW -4
CONST 32
BOUND 65
CONST 6
TIMES
INC
STIW
!   FOR a := 0 TO r-1 DO
INCL -4
JUMP L24
LABEL L25
!   FOR t := 1 TO n-1 DO
CONST 1
STLW -12
LABEL L26
LDLW -12
CONST 4
JGT L27
!     r := r DIV 2;
LDLW -24
CONST 2
DIV
STLW -24
!     FOR a := 0 TO r-1 DO
LDLW -24
DEC
STLW -32
CONST 0
STLW -4
LABEL L28
LDLW -4
LDLW -32
JGT L29
!       p := first[2*a,t]; q := first[2*a+1,t];
GLOBAL tCarroll.first
LDLW -4
CONST 2
TIMES
CONST 32
BOUND 72
CONST 6
TIMES
LDLW -12
CONST 6
BOUND 72
PLUS
LDIW
STLW -16
GLOBAL tCarroll.first
LDLW -4
CONST 2
TIMES
INC
CONST 32
BOUND 72
CONST 6
TIMES
LDLW -12
CONST 6
BOUND 72
PLUS
LDIW
STLW -20
!       play(p, q, t);
LDLW -12
LDLW -20
LDLW -16
GLOBAL tCarroll.play
CALL 3
!       IF p <= q THEN
LDLW -16
LDLW -20
JGT L32
!         first[a,t+1] := p;
LDLW -16
GLOBAL tCarroll.first
LDLW -4
CONST 32
BOUND 75
CONST 6
TIMES
LDLW -12
INC
CONST 6
BOUND 75
PLUS
STIW
!         kith[a] := second[2*a,t]; kin[a] := q
GLOBAL tCarroll.second
LDLW -4
CONST 2
TIMES
CONST 32
BOUND 76
CONST 6
TIMES
LDLW -12
CONST 6
BOUND 76
PLUS
LDIW
GLOBAL tCarroll.kith
LDLW -4
CONST 32
BOUND 76
STIW
LDLW -20
GLOBAL tCarroll.kin
LDLW -4
CONST 32
BOUND 76
STIW
JUMP L30
LABEL L32
!         first[a,t+1] := q;
LDLW -20
GLOBAL tCarroll.first
LDLW -4
CONST 32
BOUND 78
CONST 6
TIMES
LDLW -12
INC
CONST 6
BOUND 78
PLUS
STIW
!         kith[a] := p; kin[a] := second[2*a+1,t]
LDLW -16
GLOBAL tCarroll.kith
LDLW -4
CONST 32
BOUND 79
STIW
GLOBAL tCarroll.second
LDLW -4
CONST 2
TIMES
INC
CONST 32
BOUND 79
CONST 6
TIMES
LDLW -12
CONST 6
BOUND 79
PLUS
LDIW
GLOBAL tCarroll.kin
LDLW -4
CONST 32
BOUND 79
STIW
LABEL L30
!     FOR a := 0 TO r-1 DO
INCL -4
JUMP L28
LABEL L29
!     FOR a := 0 TO r-1 DO
LDLW -24
DEC
STLW -36
CONST 0
STLW -4
LABEL L33
LDLW -4
LDLW -36
JGT L34
!       p := kith[a]; q := kin[a];
GLOBAL tCarroll.kith
LDLW -4
CONST 32
BOUND 85
LDIW
STLW -16
GLOBAL tCarroll.kin
LDLW -4
CONST 32
BOUND 85
LDIW
STLW -20
!       play(p, q, t+1); second[a,t+1] := min(p, q)
LDLW -12
INC
LDLW -20
LDLW -16
GLOBAL tCarroll.play
CALL 3
LDLW -20
LDLW -16
GLOBAL tCarroll.min
CALLW 2
GLOBAL tCarroll.second
LDLW -4
CONST 32
BOUND 86
CONST 6
TIMES
LDLW -12
INC
CONST 6
BOUND 86
PLUS
STIW
!     FOR a := 0 TO r-1 DO
INCL -4
JUMP L33
LABEL L34
!   FOR t := 1 TO n-1 DO
INCL -12
JUMP L26
LABEL L27
!   Out.String("Result "); Out.Int(second[0,n], 0); Out.Ln;
CONST 8
GLOBAL tCarroll.%4
GLOBAL Out.String
CALL 2
CONST 0
GLOBAL tCarroll.second
LDNW 20
GLOBAL Out.Int
CALL 2
GLOBAL Out.Ln
CALL 0
!   Out.Int(matches, 0); Out.String(" matches"); Out.Ln
CONST 0
LDGW tCarroll.matches
GLOBAL Out.Int
CALL 2
CONST 9
GLOBAL tCarroll.%5
GLOBAL Out.String
CALL 2
GLOBAL Out.Ln
CALL 0
RETURN
END

PROC tCarroll.%main 0 1 0
!   trial; trial; trial; trial; trial
GLOBAL tCarroll.trial
CALL 0
GLOBAL tCarroll.trial
CALL 0
GLOBAL tCarroll.trial
CALL 0
GLOBAL tCarroll.trial
CALL 0
GLOBAL tCarroll.trial
CALL 0
RETURN
END

! Global variables
GLOVAR tCarroll.first 768
GLOVAR tCarroll.second 768
GLOVAR tCarroll.kith 128
GLOVAR tCarroll.kin 128
GLOVAR tCarroll.busy 192
GLOVAR tCarroll.matches 4

! String " plays "
DEFINE tCarroll.%1
STRING 20706C6179732000

! String " at time "
DEFINE tCarroll.%2
STRING 2061742074696D652000

! String "Conflict"
DEFINE tCarroll.%3
STRING 436F6E666C69637400

! String "Result "
DEFINE tCarroll.%4
STRING 526573756C742000

! String " matches"
DEFINE tCarroll.%5
STRING 206D61746368657300

! End of file
]]*)
