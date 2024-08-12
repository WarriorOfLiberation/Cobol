       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOLUTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BIN-VAR PIC X(64) VALUE ZEROES.                                    
       01 NUMER1 PIC 9(8).                                      
       01 I      PIC 99.  
       01 ACA    PIC 99 VALUE 0.                
       01 H      PIC 99 VALUE 1.                      
       01 J      PIC 9.
       01 STR    PIC X(8).
       01 RES    PIC ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ.

       PROCEDURE DIVISION.
         ACCEPT STR
         MOVE STR TO NUMER1                                        
         PERFORM VARYING I FROM 64 BY -1 UNTIL I < 1          
           DIVIDE NUMER1 BY 2 GIVING NUMER1 REMAINDER J
           MOVE J TO BIN-VAR(I:1)                              
         END-PERFORM
         PERFORM VARYING I FROM 1 BY +1 UNTIL BIN-VAR(I:1)>0
           ADD 1 TO ACA
         END-PERFORM
         ADD 1 TO ACA
         PERFORM VARYING I FROM ACA BY +1 UNTIL I>64
           MOVE BIN-VAR(I:1) TO RES(H:1)
           ADD 1 TO H
         END-PERFORM
         DISPLAY RES.
         STOP RUN.
