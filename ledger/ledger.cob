      * The MIT License (MIT)
      *
      * Copyright (c) 2019 Andrew C. Young
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
               ORGANIZATION IS RELATIVE
               RELATIVE KEY IS ACCOUNT-ID
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS FILE-STATUS.

           SELECT OPTIONAL LEDGER-FILE
               ASSIGN TO LEDGER-FILE-NAME
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FILE-STATUS.

           SELECT OPTIONAL CONTROL-FILE
               ASSIGN TO "control.dat"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FILE-STATUS.
           
       DATA DIVISION.
       FILE SECTION.
       
       FD  ACCOUNT-FILE.
       01  ACCOUNT-RECORD.
           05  ACCOUNT-COMPANY            PIC X(5)       VALUE SPACES.
           05  ACCOUNT-NUMBER             PIC X(20)      VALUE SPACES.
           05  ACCOUNT-TYPE               PIC X(10)      VALUE SPACES.
           05  ACCOUNT-DESCRIPTION        PIC X(50)      VALUE SPACES.
           05  ACCOUNT-STATUS             PIC X(1)       VALUE SPACE.
           05  ACCOUNT-VALUE              PIC S9(9)V9(2) VALUE ZEROS.

       FD  LEDGER-FILE.
       01  LEDGER-RECORD.
           05  LEDGER-DATE-TIME.
               10  LEDGER-DATE.
                   15  LEDGER-DATE-YEAR   PIC 9(4)       VALUE ZEROS.
                   15  LEDGER-DATE-MONTH  PIC 9(2)       VALUE ZEROS.
                   15  LEDGER-DATE-DAY    PIC 9(2)       VALUE ZEROS.
               10  LEDGER-TIME.
                   15  LEDGER-TIME-HOUR   PIC 9(2)       VALUE ZEROS.
                   15  LEDGER-TIME-MIN    PIC 9(2)       VALUE ZEROS.
                   15  LEDGER-TIME-SEC    PIC 9(2)       VALUE ZEROS.
           05  LEDGER-DESCRIPTION         PIC X(50)      VALUE SPACES.
           05  LEDGER-AMOUNT              PIC S9(9)V9(2) VALUE ZEROS.
           05  LEDGER-STATUS              PIC X(1)       VALUE SPACE.

       FD  CONTROL-FILE.
       01  CONTROL-RECORD.
           05  NEXT-ACCOUNT-ID            PIC 9(10)      VALUE ZEROS.
           
       WORKING-STORAGE SECTION.

       77  MENU-OPTION                    PIC 9(1)       VALUE ZERO.
       77  END-OF-FILE                    PIC X(1)       VALUE SPACE.
       77  CURRENT-LINE                   PIC 9(3)       VALUE ZEROS.
       77  CONTINUE-KEY                   PIC X(1)       VALUE SPACE.
       01  LEDGER-FILE-NAME.
           05  ACCOUNT-ID                 PIC 9(8)       VALUE ZEROS.
           05  LEDGER-FN-EXT              PIC X(4)       VALUE ".dat".
       77  FILE-STATUS                    PIC 9(2)       VALUE ZEROS.
       77  FILE-NOT-FOUND                 PIC 9(2)       VALUE 05.
       77  ACCOUNT-LOADED                 PIC X(1)       VALUE "N".
       01  NOW.
           05  NOW-DATE-TIME.
               10  NOW-DATE.
                   15  NOW-YEAR           PIC 9(4)       VALUE ZEROS.
                   15  NOW-MONTH          PIC 9(2)       VALUE ZEROS.
                   15  NOW-DAY            PIC 9(2)       VALUE ZEROS.
               10  NOW-TIME.
                   15  NOW-HOUR           PIC 9(2)       VALUE ZEROS.
                   15  NOW-MINUTE         PIC 9(2)       VALUE ZEROS.
                   15  NOW-SECOND         PIC 9(2)       VALUE ZEROS.
           05  NOW-MS                     PIC 9(2)       VALUE ZEROS.

       SCREEN SECTION.

       01  MAIN-MENU-SCREEN.
           05  BLANK SCREEN.
           05  LINE 1  COLUMN 1  VALUE "Main Menu:".
           05  LINE 3  COLUMN 3  VALUE "1) List Accounts".
           05  LINE 4  COLUMN 3  VALUE "2) Account Ledger".
           05  LINE 5  COLUMN 3  VALUE "3) Update Accounts".
           05  LINE 6  COLUMN 3  VALUE "4) Add Account".
           05  LINE 7  COLUMN 3  VALUE "5) Reports".
           05  LINE 8  COLUMN 3  VALUE "9) Exit".
           05  LINE 10 COLUMN 1  VALUE "Selecton => ".
           05  LINE 10 COLUMN 13 PIC Z USING MENU-OPTION AUTO.

       01  LOAD-ACCOUNT-SCREEN.
           05  BLANK SCREEN.
           05  LINE 1  COLUMN 1  VALUE "Account: ".
           05  LINE 1  COLUMN 10 PIC Z(8) USING ACCOUNT-ID.

       01  LEDGER-MENU-SCREEN.
           05  LINE 5  COLUMN 1  VALUE "Ledger Menu:".
           05  LINE 7  COLUMN 3  VALUE "1) Show Ledger".
           05  LINE 8  COLUMN 3  VALUE "2) Add Ledger Entry".
           05  LINE 9  COLUMN 3  VALUE "3) Update Account".
           05  LINE 10 COLUMN 3  VALUE "9) Exit".
           05  LINE 12 COLUMN 1  VALUE "Selecton => ".
           05  LINE 12 COLUMN 13 PIC Z USING MENU-OPTION AUTO.

       01  REPORT-MENU-SCREEN.
           05  BLANK SCREEN.
           05  LINE 1  COLUMN 1  VALUE "Report Menu:".
           05  LINE 3  COLUMN 3  VALUE "9) Exit".
           05  LINE 5  COLUMN 1  VALUE "Selecton => ".
           05  LINE 5  COLUMN 13 PIC Z USING MENU-OPTION AUTO.

       01  ADD-ACCOUNT-SCREEN.
           05  BLANK SCREEN.
           05  LINE 1  COLUMN 1  VALUE "Add Account:"
           05  LINE 3  COLUMN 1  VALUE "ID: ".
           05  LINE 3  COLUMN 5  PIC Z(7)9 FROM ACCOUNT-ID.
           05  LINE 3  COLUMN 16 VALUE "Company: ".
           05  LINE 3  COLUMN 26 PIC X(5) USING ACCOUNT-COMPANY.
           05  LINE 3  COLUMN 32 VALUE "Number: ".
           05  LINE 3  COLUMN 41 PIC X(20) USING ACCOUNT-NUMBER.
           05  LINE 3  COLUMN 62 VALUE "Value: ".
           05  LINE 3  COLUMN 70 PIC -$$$$$$$$$9.99 USING ACCOUNT-VALUE.
           05  LINE 4  COLUMN 1  VALUE "Type: ".
           05  LINE 4  COLUMN 7  PIC X(10) USING ACCOUNT-TYPE.
           05  LINE 4  COLUMN 19 VALUE "Status: ".
           05  LINE 4  COLUMN 28 PIC X(1) USING ACCOUNT-STATUS.
           05  LINE 5  COLUMN 1  VALUE "Description: ".
           05  LINE 5  COLUMN 14 PIC X(50) USING ACCOUNT-DESCRIPTION.

       01  ADD-ACCOUNT-PROMPT.
           05  LINE 7 COLUMN 1  VALUE "Add Account? (Y/N)".
           05  LINE 7 COLUMN 20 PIC Z USING CONTINUE-KEY AUTO.
           
       01  LIST-ACCOUNT-SCREEN.
           05  BLANK SCREEN.
           05  LINE 1 COLUMN 1  VALUE "ID".
           05  LINE 1 COLUMN 10 VALUE "Comp".
           05  LINE 1 COLUMN 16 VALUE "Number".
           05  LINE 1 COLUMN 37 VALUE "Type".
           05  LINE 1 COLUMN 48 VALUE "S".
           05  LINE 1 COLUMN 50 VALUE "Value".
           05  LINE 2 COLUMN 1  VALUE "--------".
           05  LINE 2 COLUMN 10 VALUE "-----".
           05  LINE 2 COLUMN 16 VALUE "--------------------".
           05  LINE 2 COLUMN 37 VALUE "----------".
           05  LINE 2 COLUMN 48 VALUE "-".
           05  LINE 2 COLUMN 50 VALUE "----------------".

       01  ACCOUNT-LIST-ROW.
           05  LINE CURRENT-LINE COLUMN 1  PIC Z(7)9
                                        FROM ACCOUNT-ID.
           05  LINE CURRENT-LINE COLUMN 10 PIC X(5)
                                        FROM ACCOUNT-COMPANY.
           05  LINE CURRENT-LINE COLUMN 16 PIC X(20)
                                        FROM ACCOUNT-NUMBER.
           05  LINE CURRENT-LINE COLUMN 37 PIC X(10)
                                        FROM ACCOUNT-TYPE.
           05  LINE CURRENT-LINE COLUMN 48 PIC X(1)
                                        FROM ACCOUNT-STATUS.
           05  LINE CURRENT-LINE COLUMN 50 PIC -$$$$,$$$,$$9.99
                                        FROM ACCOUNT-VALUE.

       01  CONTINUE-PROMPT.
           05  LINE CURRENT-LINE COLUMN 1
                                 VALUE "PRESS ANY KEY TO CONTINUE.".
           05  LINE CURRENT-LINE COLUMN 28 PIC Z
                                        USING CONTINUE-KEY AUTO.
           
       PROCEDURE DIVISION.
      
       PROGRAM-BEGIN.
           PERFORM LOAD-CONTROL-FILE.
           PERFORM MAIN-PROCESS.
      
       PROGRAM-DONE.
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
           MOVE ZEROS TO ACCOUNT-VALUE.

       INIT-LEDGER-RECORD.
           MOVE ZEROS TO LEDGER-RECORD.
           MOVE SPACES TO LEDGER-DESCRIPTION.
           MOVE SPACE TO LEDGER-STATUS.

       INIT-CONTROL-RECORD.
           MOVE 1 TO NEXT-ACCOUNT-ID.

       GET-CURRENT-TIME.
           MOVE FUNCTION CURRENT-DATE TO NOW.
           
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
           IF MENU-OPTION IS EQUAL TO 1
               PERFORM LIST-ACCOUNTS
           ELSE IF MENU-OPTION IS EQUAL TO 2
               PERFORM LEDGER-MENU
           ELSE IF MENU-OPTION IS EQUAL TO 3
               PERFORM UPDATE-ACCOUNTS
           ELSE IF MENU-OPTION IS EQUAL TO 4
               PERFORM ADD-ACCOUNT
           ELSE IF MENU-OPTION IS EQUAL TO 5
               PERFORM REPORT-MENU.

       LIST-ACCOUNTS.
           OPEN INPUT ACCOUNT-FILE.
           DISPLAY LIST-ACCOUNT-SCREEN.
           MOVE 3 TO CURRENT-LINE.
           MOVE "N" TO END-OF-FILE.
           PERFORM RESET-ACCOUNT-FILE-POSITION.
           IF END-OF-FILE IS NOT EQUAL TO "Y"
               PERFORM DISPLAY-NEXT-ACCOUNT-LIST-ROW
                   UNTIL END-OF-FILE IS EQUAL TO "Y"
               ADD 1 TO CURRENT-LINE
               DISPLAY CONTINUE-PROMPT
               ACCEPT CONTINUE-PROMPT
           END-IF.
           CLOSE ACCOUNT-FILE.

       RESET-ACCOUNT-FILE-POSITION.
           MOVE 1 TO ACCOUNT-ID.
           MOVE "N" TO END-OF-FILE.
           START ACCOUNT-FILE
               KEY IS GREATER THAN OR EQUAL TO ACCOUNT-ID
               INVALID KEY MOVE "Y" TO END-OF-FILE.

       DISPLAY-NEXT-ACCOUNT-LIST-ROW.
           PERFORM READ-NEXT-ACCOUNT-RECORD.
           IF END-OF-FILE IS NOT EQUAL TO "Y"
               IF CURRENT-LINE IS GREATER THAN 23
                   ADD 1 TO CURRENT-LINE
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
           
       ADD-ACCOUNT.
           PERFORM INIT-ACCOUNT-RECORD.
           MOVE NEXT-ACCOUNT-ID TO ACCOUNT-ID.
           DISPLAY ADD-ACCOUNT-SCREEN.
           ACCEPT ADD-ACCOUNT-SCREEN.
           DISPLAY ADD-ACCOUNT-SCREEN.
           MOVE SPACES TO CONTINUE-KEY.
           PERFORM SHOW-ADD-ACCOUNT-PROMPT
               UNTIL CONTINUE-KEY EQUALS "Y" OR "y"
               OR "N" OR "n".
           IF CONTINUE-KEY EQUALS "Y" OR "y"
               PERFORM WRITE-ACCOUNT
               PERFORM CREATE-LEDGER-FILE
               ADD 1 TO NEXT-ACCOUNT-ID
               PERFORM WRITE-CONTROL-FILE
           END-IF.

       WRITE-ACCOUNT.
           OPEN I-O ACCOUNT-FILE.
           IF FILE-STATUS IS EQUAL TO FILE-NOT-FOUND
               CLOSE ACCOUNT-FILE
               OPEN OUTPUT ACCOUNT-FILE
           END-IF.
           WRITE ACCOUNT-RECORD
               INVALID KEY REWRITE ACCOUNT-RECORD.
           CLOSE ACCOUNT-FILE.
               
       UPDATE-ACCOUNTS.
           OPEN I-O ACCOUNT-FILE.
           PERFORM RESET-ACCOUNT-FILE-POSITION.
           PERFORM UPDATE-NEXT-ACCOUNT
               UNTIL END-OF-FILE IS EQUAL TO "Y".

       UPDATE-NEXT-ACCOUNT.
           PERFORM READ-NEXT-ACCOUNT-RECORD.
           IF END-OF-FILE IS NOT EQUAL TO "Y"
               PERFORM UPDATE-CURRENT-ACCOUNT
               REWRITE ACCOUNT-RECORD
           END-IF.

       UPDATE-CURRENT-ACCOUNT.
           MOVE ZEROS TO ACCOUNT-VALUE.
           MOVE SPACE TO ACCOUNT-STATUS.
           MOVE "N" TO END-OF-FILE.
           OPEN INPUT LEDGER-FILE.
           PERFORM UPDATE-CURRENT-ACCOUNT-LOOP
               UNTIL END-OF-FILE IS EQUAL TO "Y".
           CLOSE LEDGER-FILE.
           MOVE "N" TO END-OF-FILE.

       UPDATE-CURRENT-ACCOUNT-LOOP.
           PERFORM READ-NEXT-LEDGER-RECORD.
           IF END-OF-FILE IS NOT EQUAL TO "Y"
               ADD LEDGER-AMOUNT TO ACCOUNT-VALUE
               MOVE LEDGER-STATUS TO ACCOUNT-STATUS
           END-IF.
           
       LOAD-ACCOUNT.
           PERFORM INIT-ACCOUNT-RECORD.
           MOVE "N" TO ACCOUNT-LOADED.
           MOVE 0 to ACCOUNT-ID.
           DISPLAY LOAD-ACCOUNT-SCREEN.
           ACCEPT LOAD-ACCOUNT-SCREEN.
           DISPLAY LOAD-ACCOUNT-SCREEN.
           IF ACCOUNT-ID IS NOT EQUAL TO 0
               OPEN INPUT ACCOUNT-FILE
               IF FILE-STATUS IS NOT EQUAL TO FILE-NOT-FOUND
                   MOVE "Y" TO ACCOUNT-LOADED
                   READ ACCOUNT-FILE
                       INVALID KEY MOVE "N" TO ACCOUNT-LOADED
                   END-READ
               END-IF
               CLOSE ACCOUNT-FILE
           END-IF.

       LEDGER-MENU.
           PERFORM LOAD-ACCOUNT.
           IF ACCOUNT-LOADED IS EQUAL TO "Y"
               MOVE ZERO TO MENU-OPTION
               PERFORM LEDGER-MENU-LOOP
                   UNTIL MENU-OPTION IS EQUAL TO 9
           END-IF.
           MOVE ZERO TO MENU-OPTION.

       LEDGER-MENU-LOOP.
           DISPLAY LIST-ACCOUNT-SCREEN.
           MOVE 3 TO CURRENT-LINE.
           DISPLAY ACCOUNT-LIST-ROW.
           DISPLAY LEDGER-MENU-SCREEN.
           ACCEPT LEDGER-MENU-SCREEN.
           IF MENU-OPTION IS EQUAL TO 1
               PERFORM LIST-LEDGER
           ELSE IF MENU-OPTION IS EQUAL TO 2
               PERFORM ADD-LEDGER
           ELSE IF MENU-OPTION IS EQUAL TO 3
               PERFORM UPDATE-CURRENT-ACCOUNT
               PERFORM WRITE-ACCOUNT.

       CREATE-LEDGER-FILE.
           OPEN OUTPUT LEDGER-FILE.
           PERFORM INIT-LEDGER-RECORD.
           PERFORM GET-CURRENT-TIME.
           MOVE NOW-DATE-TIME TO LEDGER-DATE-TIME.
           MOVE "Initial Balance" TO LEDGER-DESCRIPTION.
           MOVE ACCOUNT-VALUE TO LEDGER-AMOUNT.
           MOVE ACCOUNT-STATUS TO LEDGER-STATUS.
           WRITE LEDGER-RECORD.
           CLOSE LEDGER-FILE.

       READ-NEXT-LEDGER-RECORD.
           READ LEDGER-FILE NEXT RECORD
               AT END MOVE "Y" TO END-OF-FILE.
           
       LIST-LEDGER.

       ADD-LEDGER.
               
       REPORT-MENU.
           MOVE ZERO TO MENU-OPTION.
           PERFORM REPORT-MENU-LOOP
               UNTIL MENU-OPTION IS EQUAL TO 9.
           MOVE ZERO TO MENU-OPTION.

       REPORT-MENU-LOOP.
           MOVE ZERO TO MENU-OPTION.
           DISPLAY REPORT-MENU-SCREEN.
           ACCEPT REPORT-MENU-SCREEN.
