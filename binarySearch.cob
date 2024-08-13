       IDENTIFICATION DIVISION.
       PROGRAM-ID. BINARY-SEARCH.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 ARRAY-SIZE       PIC 9(02) VALUE 10.
           01 SEARCH-ARRAY.
               05 ARRAY-ELEMENT OCCURS 10 TIMES PIC 9(02) VALUE ZEROS.
           01 SEARCH-VALUE      PIC 9(02).
           01 MID               PIC 9(02).
           01 LOWL               PIC 9(02) VALUE 1.
           01 HIGHH              PIC 9(02) VALUE 10.
           01 RESULT            PIC 9(02) VALUE 0.
           01 FOUND-FLAG        PIC X VALUE 'N'.

       PROCEDURE DIVISION.
           MOVE 1 TO ARRAY-ELEMENT (1)
           MOVE 2 TO ARRAY-ELEMENT (2)
           MOVE 3 TO ARRAY-ELEMENT (3)
           MOVE 4 TO ARRAY-ELEMENT (4)
           MOVE 5 TO ARRAY-ELEMENT (5)
           MOVE 6 TO ARRAY-ELEMENT (6)
           MOVE 7 TO ARRAY-ELEMENT (7)
           MOVE 8 TO ARRAY-ELEMENT (8)
           MOVE 9 TO ARRAY-ELEMENT (9)
           MOVE 10 TO ARRAY-ELEMENT (10)
           
           DISPLAY "Enter the number to search for:" WITH NO ADVANCING
           ACCEPT SEARCH-VALUE

           PERFORM BINARY-SEARCH

           IF FOUND-FLAG = 'Y'
               DISPLAY "Number " SEARCH-VALUE "found at position" RESULT
           ELSE
               DISPLAY "Number " SEARCH-VALUE " not found."
           END-IF

           STOP RUN.

       BINARY-SEARCH.
           MOVE 1 TO LOWL
           MOVE ARRAY-SIZE TO HIGHH
           PERFORM UNTIL LOWL > HIGHH
               COMPUTE MID = (LOWL + HIGHH) / 2
               IF SEARCH-VALUE = ARRAY-ELEMENT (MID)
                   MOVE MID TO RESULT
                   MOVE 'Y' TO FOUND-FLAG
                   EXIT PERFORM
               ELSE IF SEARCH-VALUE < ARRAY-ELEMENT (MID)
                   SUBTRACT 1 FROM MID
                   MOVE MID TO HIGHH
               ELSE
                   ADD 1 TO MID
                   MOVE MID TO LOWL
               END-IF
           END-PERFORM
           IF FOUND-FLAG = 'N'
               MOVE -1 TO RESULT
           END-IF.
