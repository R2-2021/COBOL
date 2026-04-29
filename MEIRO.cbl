       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAZE-DISPLAY.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 MAZE-LINES.
          05 MAZE-LINE OCCURS 10 TIMES PIC X(40)
             VALUE SPACES.

       01 I PIC 9(02).

       PROCEDURE DIVISION.
           MOVE "########################################" TO MAZE-LINE (1)
           MOVE "#     #        #        #              #" TO MAZE-LINE (2)
           MOVE "# ### # ###### ####### ####### ###### #" TO MAZE-LINE (3)
           MOVE "# #   #      #       #        #       #" TO MAZE-LINE (4)
           MOVE "# # ######## ######## ######## ###### #" TO MAZE-LINE (5)
           MOVE "# #        #        #        #        #" TO MAZE-LINE (6)
           MOVE "# ######## ######## ######## ######## #" TO MAZE-LINE (7)
           MOVE "#        #        #        #        # #" TO MAZE-LINE (8)
           MOVE "# ###### ######## ######## ######## # #" TO MAZE-LINE (9)
           MOVE "########################################" TO MAZE-LINE (10)

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
               DISPLAY MAZE-LINE(I)
           END-PERFORM

           STOP RUN.