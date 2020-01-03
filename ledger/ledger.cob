      * The MIT License (MIT)
      *
      * Copyright (c) 2019 Andrew Young
      *
      * Permission is hereby granted, free of charge, to any person
      * obtaining a copy of this software and associated documentation
      * files (the "Software"), to deal in the Software without
      * restriction, including without limitation the rights to use,
      * copy, modify, merge, publish, distribute, sublicense, and/or
      * sell copies of the Software, and to permit persons to whom the
      * Software is furnished to do so, subject to the following
      * conditions:
      *
      * The above copyright notice and this permission notice shall be
      * included in all copies or substantial portions of the Software.
      *
      * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
      * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
      * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
      * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
      * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
      * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
      * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
      * OTHER DEALINGS IN THE SOFTWARE.

      ***************************************
      * This is a simple accounting ledger. *
      ***************************************
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEDGER.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT OPTIONAL ACCOUNT-FILE
               ASSIGN TO "account.dat"
               ORGANIZATION IS INDEXED
               RECORD KEY IS ACCOUNT-ID
               ACCESS MODE IS DYNAMIC.

           SELECT OPTIONAL LEDGER-FILE
               ASSIGN TO "ledger.dat"
               ORGANIZATION IS INDEXED
               RECORD KEY IS LEDGER-ID
               ALTERNATE RECORD KEY IS LEDGER-ACCOUNT-ID WITH DUPLICATES
               ACCESS MODE IS DYNAMIC.

           SELECT OPTIONAL CONTROL-FILE
               ASSIGN TO "control.dat"
               ORGANIZATION IS SEQUENTIAL.
           
       DATA DIVISION.
       FILE SECTION.
       
       FD  ACCOUNT-FILE.
       01  ACCOUNT-RECORD.
           05  ACCOUNT-ID                 PIC 9(10)      VALUE ZEROS.
           05  ACCOUNT-COMPANY-NAME       PIC X(10)      VALUE SPACES.
           05  ACCOUNT-NUMBER             PIC X(20)      VALUE SPACES.
           05  ACCOUNT-TYPE               PIC X(20)      VALUE SPACES.
           05  ACCOUNT-DESCRIPTION        PIC X(50)      VALUE SPACES.
           05  ACCOUNT-STATUS             PIC X(1)       VALUE SPACE.

       FD  LEDGER-FILE.
       01  LEDGER-RECORD.
           05  LEDGER-ID                  PIC 9(10)      VALUE ZEROS.
           05  LEDGER-ACCOUNT-ID          PIC 9(10)      VALUE ZEROS.
           05  LEDGER-DATE.
               10  LEDGER-DATE-YEAR       PIC 9(4)       VALUE ZEROS.
               10  LEDGER-DATE-MONTH      PIC 9(2)       VALUE ZEROS.
               10  LEDGER-DATE-DAY        PIC 9(2)       VALUE ZEROS.
           05  LEDGER-DESCRIPTION         PIC X(50)      VALUE SPACES.
           05  LEDGER-AMOUNT              PIC S9(9)V9(2) VALUE ZEROS.

       FD  CONTROL-FILE.
       01  CONTROL-RECORD.
           05  NEXT-ACCOUNT-ID            PIC 9(10)      VALUE ZEROS.
           
       WORKING-STORAGE SECTION.

       77  MENU-OPTION                    PIC 9(1)       VALUE ZERO.
       77  END-OF-FILE                    PIC X(1)       VALUE SPACE.
       77  CURRENT-LINE                   PIC 9(3)       VALUE ZEROS.
       77  CONTINUE-KEY                   PIC X(1)       VALUE SPACE.

       SCREEN SECTION.

       01  MAIN-MENU-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COL 1 VALUE "Main Menu:".
           05 LINE 3 COL 3 VALUE "1) List Accounts".
           05 LINE 4 COL 3 VALUE "2) Add Account".
           05 LINE 5 COL 3 VALUE "3) Reports".
           05 LINE 7 COL 3 VALUE "9) Exit".
           05 LINE 9 COL 1 VALUE "Selecton => ".
           05 LINE 9 COL 13 PIC Z USING MENU-OPTION AUTO.

       01  REPORT-MENU-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COL 1 VALUE "Report Menu:".
           05 LINE 3 COL 3 VALUE "9) Exit".
           05 LINE 5 COL 1 VALUE "Selecton => ".
           05 LINE 5 COL 13 PIC Z USING MENU-OPTION AUTO.

       01  ADD-ACCOUNT-SCREEN.
           05  BLANK SCREEN.
           05  LINE 1 COL 1 VALUE "Add Account:"
           05  LINE 3 COL 1 VALUE "ID: ".
           05  LINE 3 COL 5 PIC 9(10) FROM ACCOUNT-ID.
           05  LINE 3 COL 20 VALUE "Company: ".
           05  LINE 3 COL 30 PIC X(10) USING ACCOUNT-COMPANY-NAME.
           05  LINE 3 COL 40 VALUE "Number: ".
           05  LINE 3 COL 49 PIC X(20) USING ACCOUNT-NUMBER.
           05  LINE 4 COL 1 VALUE "Type: ".
           05  LINE 4 COL 7 PIC X(20) USING ACCOUNT-TYPE.
           05  LINE 4 COL 20 VALUE "Status: ".
           05  LINE 4 COL 29 PIC X(1) USING ACCOUNT-STATUS.
           05  LINE 5 COL 1 VALUE "Description: ".
           05  LINE 5 COL 14 PIC X(50) USING ACCOUNT-DESCRIPTION.

       01  ADD-ACCOUNT-PROMPT.
           05  LINE 7 COL 1 VALUE "Add Account? (Y/N)".
           05  LINE 7 COL 20 PIC Z USING CONTINUE-KEY AUTO.
           
       01  LIST-ACCOUNT-SCREEN.
           05  BLANK SCREEN.
           05  LINE 1 COL 1  VALUE "ID".
           05  LINE 1 COL 12 VALUE "Company".
           05  LINE 1 COL 24 VALUE "Number".
           05  LINE 1 COL 46 VALUE "Type".
           05  LINE 1 COL 68 VALUE "S".
           05  LINE 2 COL 1  VALUE "----------".
           05  LINE 2 COL 12 VALUE "----------".
           05  LINE 2 COL 24 VALUE "--------------------".
           05  LINE 2 COL 46 VALUE "--------------------".
           05  LINE 2 COL 68 VALUE "-".

       01  ACCOUNT-LIST-ROW.
           05  LINE CURRENT-LINE COL 1  PIC 9(10)
                                        FROM ACCOUNT-ID.
           05  LINE CURRENT-LINE COL 12 PIC X(10)
                                        FROM ACCOUNT-COMPANY-NAME.
           05  LINE CURRENT-LINE COL 24 PIC X(20)
                                        FROM ACCOUNT-NUMBER.
           05  LINE CURRENT-LINE COL 46 PIC X(20)
                                        FROM ACCOUNT-TYPE.
           05  LINE CURRENT-LINE COL 68 PIC X(1)
                                        FROM ACCOUNT-STATUS.

       01  CONTINUE-PROMPT.
           05  LINE CURRENT-LINE COL 1
                                 VALUE "PRESS ANY KEY TO CONTINUE.".
           05  LINE CURRENT-LINE COL 28 PIC Z USING CONTINUE-KEY AUTO.
           
       PROCEDURE DIVISION.
      
       PROGRAM-BEGIN.
           PERFORM LOAD-CONTROL-FILE.
           OPEN I-O ACCOUNT-FILE.
           OPEN I-O LEDGER-FILE.
           PERFORM MAIN-PROCESS.
      
       PROGRAM-DONE.
           CLOSE LEDGER-FILE.
           CLOSE ACCOUNT-FILE.
           PERFORM WRITE-CONTROL-FILE.
           STOP RUN.

       MAIN-PROCESS.
           MOVE "N" TO END-OF-FILE.
           PERFORM INIT-ACCOUNT-RECORD.
           PERFORM INIT-LEDGER-RECORD.
           PERFORM MAIN-MENU.

       INIT-ACCOUNT-RECORD.
           MOVE SPACES TO ACCOUNT-RECORD.
           MOVE ZEROS TO ACCOUNT-ID.

       INIT-LEDGER-RECORD.
           MOVE ZEROS TO LEDGER-RECORD.
           MOVE SPACES TO LEDGER-DESCRIPTION.

       INIT-CONTROL-RECORD.
           MOVE ZEROS TO NEXT-ACCOUNT-ID.
           
       LOAD-CONTROL-FILE.
           PERFORM INIT-CONTROL-RECORD.
           OPEN INPUT CONTROL-FILE.
           READ CONTROL-FILE NEXT RECORD
               AT END MOVE "Y" TO END-OF-FILE.
           CLOSE CONTROL-FILE.

       WRITE-CONTROL-FILE.
           OPEN OUTPUT CONTROL-FILE.
           WRITE CONTROL-RECORD.
           CLOSE CONTROL-FILE.
           
       MAIN-MENU.
           PERFORM MAIN-MENU-LOOP
               UNTIL MENU-OPTION IS EQUAL TO 9.

       MAIN-MENU-LOOP.
           MOVE ZERO TO MENU-OPTION.
           DISPLAY MAIN-MENU-SCREEN.
           ACCEPT MAIN-MENU-SCREEN.
           MOVE FUNCTION UPPER-CASE(MENU-OPTION) TO MENU-OPTION.
           IF MENU-OPTION IS EQUAL TO 1
               PERFORM LIST-ACCOUNTS
           ELSE IF MENU-OPTION IS EQUAL TO 2
               PERFORM ADD-ACCOUNTS
           ELSE IF MENU-OPTION IS EQUAL TO 3
               PERFORM REPORT-MENU.

       LIST-ACCOUNTS.
           DISPLAY LIST-ACCOUNT-SCREEN.
           MOVE 3 TO CURRENT-LINE.
           MOVE "N" TO END-OF-FILE.
           PERFORM RESET-ACCOUNT-FILE-POSITION.
           IF END-OF-FILE IS NOT EQUAL TO "Y"
               PERFORM DISPLAY-NEXT-ACCOUNT-LIST-ROW
                   UNTIL END-OF-FILE IS EQUAL TO "Y"
               DISPLAY CONTINUE-PROMPT
               ACCEPT CONTINUE-PROMPT
           END-IF.
           DISPLAY " ".

       RESET-ACCOUNT-FILE-POSITION.
           MOVE ZEROS TO ACCOUNT-ID.
           START ACCOUNT-FILE KEY IS GREATER THAN OR EQUAL TO ACCOUNT-ID
               INVALID KEY MOVE "Y" TO END-OF-FILE.

       DISPLAY-NEXT-ACCOUNT-LIST-ROW.
           PERFORM READ-NEXT-ACCOUNT-RECORD.
           IF END-OF-FILE IS NOT EQUAL TO "Y"
               IF CURRENT-LINE IS GREATER THAN 23
                   DISPLAY CONTINUE-PROMPT
                   ACCEPT CONTINUE-PROMPT
                   DISPLAY LIST-ACCOUNT-SCREEN
                   MOVE 3 TO CURRENT-LINE
               END-IF
               PERFORM DISPLAY-ACCOUNT-LIST-RECORD
           END-IF.
           
       READ-NEXT-ACCOUNT-RECORD.
           READ ACCOUNT-FILE NEXT RECORD
               AT END MOVE "Y" TO END-OF-FILE.
           
       DISPLAY-ACCOUNT-LIST-RECORD.
           DISPLAY ACCOUNT-LIST-ROW.
           ADD 1 TO CURRENT-LINE.

       SHOW-ADD-ACCOUNT-PROMPT.        
           DISPLAY ADD-ACCOUNT-PROMPT.
           ACCEPT ADD-ACCOUNT-PROMPT.
           
       ADD-ACCOUNTS.
           PERFORM INIT-ACCOUNT-RECORD.
           MOVE NEXT-ACCOUNT-ID TO ACCOUNT-ID.
           DISPLAY ADD-ACCOUNT-SCREEN.
           ACCEPT ADD-ACCOUNT-SCREEN.
           MOVE SPACES TO CONTINUE-KEY.
           PERFORM SHOW-ADD-ACCOUNT-PROMPT
               UNTIL CONTINUE-KEY EQUALS "Y" OR "y"
               OR "N" OR "n".
           IF CONTINUE-KEY EQUALS "Y" OR "y"
               WRITE ACCOUNT-RECORD
               ADD 1 TO NEXT-ACCOUNT-ID
           END-IF.

       REMOVE-ACCOUNT.
                          
       REPORT-MENU.
           PERFORM REPORT-MENU-LOOP
               UNTIL MENU-OPTION IS EQUAL TO 9.
           MOVE ZERO TO MENU-OPTION.

       REPORT-MENU-LOOP.
           MOVE ZERO TO MENU-OPTION.
           DISPLAY REPORT-MENU-SCREEN.
           ACCEPT REPORT-MENU-SCREEN.
