       IDENTIFICATION DIVISION.
       PROGRAM-ID. QUDIT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       77 MAX-QUDIT-SIZE          PIC 9 VALUE 10.
       77 NUM-AMPLITUDES           PIC 9 VALUE 0.
       77 OBSERVATION              PIC 9V9 VALUE 0.0.
       77 CUMULATIVE-PROB          PIC 9V9 VALUE 0.0.
       77 TOTAL-PROB               PIC 9V9 VALUE 0.0.
       77 I                        PIC 9 VALUE 1.
       77 RESULT                   PIC 9 VALUE -1.

       01 AMPLITUDES.
           05 AMPLITUDE OCCURS 10 TIMES.
               10 VALUE PIC 9V9 VALUE 0.0.

       PROCEDURE DIVISION.

       MAIN-LOGIC.
           MOVE 3 TO NUM-AMPLITUDES.
           PERFORM INIT-AMPLITUDES.

           PERFORM VALIDATE-AMPLITUDES
           IF TOTAL-PROB NOT = 1.0
               DISPLAY "Qudit outcome probabilities do not sum to 1: "
                       TOTAL-PROB
               STOP RUN
           END-IF.

           MOVE 0.6 TO OBSERVATION
           PERFORM MEASURE
           DISPLAY "Measurement result: " RESULT
           STOP RUN.

       INIT-AMPLITUDES.
           MOVE 0.5 TO AMPLITUDE(1).
           MOVE 0.5 TO AMPLITUDE(2).
           MOVE 0.0 TO AMPLITUDE(3).
           MOVE 0 TO I.

       VALIDATE-AMPLITUDES.
           MOVE 0 TO TOTAL-PROB.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NUM-AMPLITUDES
               COMPUTE TOTAL-PROB = TOTAL-PROB + AMPLITUDE(I) * AMPLITUDE(I)
           END-PERFORM.

       MEASURE.
           MOVE 0 TO CUMULATIVE-PROB.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NUM-AMPLITUDES
               COMPUTE CUMULATIVE-PROB = CUMULATIVE-PROB + AMPLITUDE(I) * AMPLITUDE(I)
               IF OBSERVATION < CUMULATIVE-PROB
                   MOVE I TO RESULT
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       END PROGRAM QUDIT.
