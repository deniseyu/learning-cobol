IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.
DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME PIC A(30).
       01 YEAR PIC X(4) VALUE '2018'.
       01 USERNAME PIC X(30).

PROCEDURE DIVISION.
       A000-FIRST-PARA.
           DISPLAY 'Hello PLIBMTTBHGATY!!'.
           MOVE 'Denise' TO WS-NAME.
           DISPLAY "My name is "WS-NAME.
           DISPLAY "The year is "YEAR.
           DISPLAY "And I am writing COBOL.".
           DISPLAY "Who are you?".
           ACCEPT USERNAME.
           DISPLAY "Hello, "USERNAME.
    STOP RUN.
