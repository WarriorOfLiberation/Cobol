       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOLUTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 N        PIC 9(10)   VALUE ZEROES.
       01 foo.
           02 X-Coord occurs 55 times pic 9(10) VALUE ZEROES.
       01 foo2.
           02 Y-Coord occurs 55 times pic 9(10) VALUE ZEROES.
       01 STR      PIC X(10).
       01 i        PIC 9(10)   VALUE ZEROES.
       01 j        PIC 9(10)   VALUE ZEROES.
       01 k        PIC 9(10)   VALUE ZEROES.
       01 l        PIC 9(10)   VALUE ZEROES.
       01 tmp1     PIC 9(10)   VALUE ZEROES.
       01 tmp2     PIC 9(10)   VALUE ZEROES.
       01 res      PIC 9(10)V9(10) VALUE ZEROES.
       01 tmpres   PIC 9(10)V9(10) VALUE ZEROES.

       PROCEDURE DIVISION.
           ACCEPT STR
           MOVE STR TO N
           move 1 to i
           perform until i > N
               move 0 to tmp1
               move 0 to tmp2
               accept STR
               unstring STR delimited by "-" into tmp2, tmp1
               move 100 to X-Coord(i)
               add tmp2 to X-Coord(i)
               subtract tmp1 from X-Coord(i)
               move 0 to tmp1
               move 0 to tmp2
               accept STR
               unstring STR delimited by "-" into tmp2, tmp1
               move 100 to Y-Coord(i)
               add tmp2 to Y-Coord(i)
               subtract tmp1 from Y-Coord(i)
               add 1 to i
           end-perform
           move 0 to res
           move 1 to i
           perform until i > N
               move i to j
               add 1 to j
               perform until j > N
                   move 0 to k
                   multiply X-Coord(i) by X-Coord(i) giving k
                   multiply X-Coord(j) by X-Coord(j) giving l
                   add l to k
                   multiply X-Coord(i) by X-Coord(j) giving l
                   multiply l by 2 giving l
                   subtract l from k
                   multiply Y-Coord(i) by Y-Coord(i) giving l
                   add l to k
                   multiply Y-Coord(j) by Y-Coord(j) giving l
                   add l to k
                   multiply Y-Coord(i) by Y-Coord(j) giving l
                   multiply l by 2 giving l
                   subtract l from k
                   compute tmpres = function sqrt(k)
                   if tmpres > res
                       move tmpres to res
                   end-if
                   add 1 to j
               end-perform
               add 1 to i
           end-perform
           display res
           STOP RUN.
                                                                                                                                                                                                                                                                                                                                  
