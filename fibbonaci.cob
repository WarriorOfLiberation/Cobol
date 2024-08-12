       IDENTIFICATION DIVISION.
       PROGRAM-ID. Fibonacci.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  NUM-TERMS            PIC 9(02) VALUE 10.
       01  TERM-1               PIC 9(04) VALUE 0.
       01  TERM-2               PIC 9(04) VALUE 1.
       01  NEXT-TERM            PIC 9(04).
       01  COUNTER              PIC 9(02) VALUE 1.

       PROCEDURE DIVISION.

           DISPLAY "Enter the number of Fibonacci terms you want: "
           ACCEPT NUM-TERMS.

           IF NUM-TERMS LESS THAN 1
               DISPLAY "Number of terms must be greater than 0."
               STOP RUN
           END-IF.

           DISPLAY "Fibonacci sequence up to " NUM-TERMS " terms:"
           DISPLAY TERM-1
           DISPLAY TERM-2

           PERFORM VARYING COUNTER FROM 3 BY 1
               UNTIL COUNTER GREATER THAN NUM-TERMS
               COMPUTE NEXT-TERM = TERM-1 + TERM-2
               DISPLAY NEXT-TERM
               MOVE TERM-2 TO TERM-1
               MOVE NEXT-TERM TO TERM-2
           END-PERFORM.

           STOP RUN.
