      * The MIT License (MIT)
      *
      * Copyright (c) 2015 Andrew Young
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
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PHONE.
       AUTHOR. Andrew Young.

      ***********************************************************
      * This program maintains a simple phone number data file. *
      ***********************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL PHONE-FILE
                   ASSIGN TO "phone.dat"
                   ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       
       FD  PHONE-FILE
                LABEL RECORDS ARE STANDARD.
       01  PHONE-RECORD.
           05 PHONE-LAST-NAME           PIC X(20) VALUE SPACES.
           05 PHONE-FIRST-NAME          PIC X(15) VALUE SPACES.
           05 PHONE-NUMBER              PIC X(15) VALUE SPACES.

           
       WORKING-STORAGE SECTION.
       01  DISPLAY-LINE.
           05  RECORD-NUMBER            PIC 9(4) VALUE ZEROS.
           05  RECORD-NUMBER-SPACES     PIC X(1) VALUE SPACES.
           05  PROMPT-LAST-NAME         PIC X(11) VALUE "Last Name: ".
           05  DISPLAY-LAST-NAME        PIC X(20) VALUE SPACES.
           05  PROMPT-FIRST-NAME        PIC X(12) VALUE "First Name: ".
           05  DISPLAY-FIRST-NAME       PIC X(15) VALUE SPACES.
           05  PROMPT-NUMBER            PIC X(8) VALUE "Number: ".
           05  DISPLAY-NUMBER           PIC X(15) VALUE SPACES.

       01  PAGER                        PIC X(13) VALUE "[Press Enter]".

       01  COMMAND-PROMPT               PIC X(54) VALUE "Command ([A]dd,
      -         " [R]emove, [U]pdate, [L]ist, [Q]uit): ".
       01  CMD                          PIC X VALUE SPACES.
       01  ENTRY-OK                     PIC X VALUE SPACES.
       01  END-OF-FILE                  PIC X VALUE SPACES.
       01  MAX-SCREEN-LINES             PIC 999 VALUE 24.
       01  SCREEN-LINES                 PIC 999 VALUE ZEROS.
       01  ANY-KEY                      PIC X VALUE SPACES.

       01  MSG-COMMAND-NOT-FOUND        PIC X(17) VALUE "Command Not
      -         " Found".
       01  MSG-NOT-IMPLEMENTED          PIC X(15) VALUE "Not
      -         " Implemented".
      
       PROCEDURE DIVISION.
       MAIN-LOGIC SECTION.
       PROGRAM-BEGIN.

           PERFORM LIST-RECORDS.
           MOVE SPACES TO CMD
           PERFORM COMMAND-LOOP
                UNTIL CMD = "Q".
      
       PROGRAM-DONE.
           STOP RUN.

       COMMAND-LOOP.
           DISPLAY COMMAND-PROMPT.
           ACCEPT CMD.
           IF CMD = "A" OR CMD = "a"
                PERFORM ADD-RECORDS
           ELSE IF CMD = "R" OR CMD = "r"
                PERFORM REMOVE-RECORD
           ELSE IF CMD = "U" or CMD = "u"
                PERFORM UPDATE-RECORD
           ELSE IF CMD = "L" or CMD = "l"
                PERFORM LIST-RECORDS
           ELSE IF CMD = "Q" OR CMD = "q"
                MOVE "Q" TO CMD
           ELSE
                DISPLAY MSG-COMMAND-NOT-FOUND.

       LIST-RECORDS.
           OPEN INPUT PHONE-FILE.
           MOVE ZEROS TO SCREEN-LINES.
           MOVE "N" TO END-OF-FILE.
           PERFORM RESET-FILE-POSITION.
           PERFORM READ-NEXT-RECORD.
           PERFORM DISPLAY-RECORDS
                UNTIL END-OF-FILE = "Y".
           CLOSE PHONE-FILE.

       DISPLAY-RECORDS.
           IF SCREEN-LINES = MAX-SCREEN-LINES
                PERFORM PRESS-ENTER.
           PERFORM DISPLAY-CURRENT-RECORD.
           ADD 1 TO SCREEN-LINES.
           PERFORM READ-NEXT-RECORD.

       DISPLAY-CURRENT-RECORD.
           MOVE PHONE-LAST-NAME to DISPLAY-LAST-NAME.
           MOVE PHONE-FIRST-NAME TO DISPLAY-FIRST-NAME.
           MOVE PHONE-NUMBER TO DISPLAY-NUMBER.
           DISPLAY DISPLAY-LINE.

       RESET-FILE-POSITION.
           MOVE ZEROS TO RECORD-NUMBER.

       READ-NEXT-RECORD.
           READ PHONE-FILE NEXT RECORD
                AT END MOVE "Y" TO END-OF-FILE.
           ADD 1 TO RECORD-NUMBER.

       PRESS-ENTER.
           DISPLAY PAGER.
           ACCEPT ANY-KEY.
           MOVE ZEROES TO SCREEN-LINES.

       ADD-RECORDS.
           OPEN EXTEND PHONE-FILE.
           MOVE "N" TO ENTRY-OK.
           PERFORM GET-FIELDS.
           IF ENTRY-OK = "Y"
                PERFORM ADD-THIS-RECORD.
           CLOSE PHONE-FILE.

       GET-FIELDS.
           MOVE SPACE TO PHONE-RECORD.
           DISPLAY PROMPT-LAST-NAME.
           ACCEPT PHONE-LAST-NAME.
           DISPLAY PROMPT-FIRST-NAME.
           ACCEPT PHONE-FIRST-NAME.
           DISPLAY PROMPT-NUMBER.
           ACCEPT PHONE-NUMBER.
           PERFORM VALIDATE-FIELDS.

       VALIDATE-FIELDS.
           MOVE "Y" TO ENTRY-OK.
           IF PHONE-LAST-NAME = SPACE
                   DISPLAY "LAST NAME MUST BE ENTERED"
                   MOVE "N" TO ENTRY-OK.

       ADD-THIS-RECORD.
           WRITE PHONE-RECORD.

       REMOVE-RECORD.
           DISPLAY MSG-NOT-IMPLEMENTED.

       UPDATE-RECORD.
           DISPLAY MSG-NOT-IMPLEMENTED.
