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

           SELECT OPTIONAL COMPANY-FILE
               ASSIGN TO "company.dat"
               ORGANIZATION IS INDEXED
               RECORD KEY IS COMPANY-ID
               ACCESS MODE IS DYNAMIC.
           
           SELECT OPTIONAL ACCOUNT-FILE
               ASSIGN TO "account.dat"
               ORGANIZATION IS INDEXED
               RECORD KEY IS ACCOUNT-ID
               ALTERNATE RECORD KEY IS ACCOUNT-COMPANY-ID
               ACCESS MODE IS DYNAMIC.

           SELECT OPTIONAL LEDGER-FILE
               ASSIGN TO "ledger.dat"
               ORGANIZATION IS INDEXED
               RECORD KEY IS LEDGER-ID
               ALTERNATE RECORD KEY IS LEDGER-ACCOUNT-ID
               ALTERNATE RECORD KEY IS LEDGER-COMPANY-ID
               ACCESS MODE IS DYNAMIC.
           
       DATA DIVISION.
       FILE SECTION.

       FD  COMPANY-FILE.
       01  COMPANY-RECORD.
           05  COMPANY-ID                 PIC X(10).
           05  COMPANY-NAME               PIC X(20).
       
       FD  ACCOUNT-FILE.
       01  ACCOUNT-RECORD.
           05  ACCOUNT-ID.
               10  ACCOUNT-COMPANY-ID     PIC X(10).
               10  ACCOUNT-NUMBER         PIC X(20).
           05  ACCOUNT-TYPE               PIC X(20).
           05  ACCOUNT-DESCRIPTION        PIC X(50).

       FD  LEDGER-FILE.
       01  LEDGER-RECORD.
           05  LEDGER-ID                  PIC X(20).
           05  LEDGER-ACCOUNT-ID.
               10  LEDGER-COMPANY-ID      PIC X(10).
               10  LEDGER-ACCOUNT-NUMBER  PIC X(20).
           05  LEDGER-DESCRIPTION         PIC X(50).
           05  LEDGER-AMOUNT              PIC S9(9)V9(2).
       
       WORKING-STORAGE SECTION.
      
       PROCEDURE DIVISION.
      
       PROGRAM-BEGIN.
           OPEN I-O COMPANY-FILE.
           OPEN I-O ACCOUNT-FILE.
           OPEN I-O LEDGER-FILE.
           PERFORM MAIN-PROCESS.
           CLOSE COMPANY-FILE.
           CLOSE ACCOUNT-FILE.
           CLOSE LEDGER-FILE.
      
       PROGRAM-DONE.
            STOP RUN.

       MAIN-PROCESS.
           PERFORM INIT-RECORDS.

       INIT-RECORDS.
           MOVE SPACE TO COMPANY-RECORD.
           MOVE SPACE TO ACCOUNT-RECORD.
           MOVE SPACE TO LEDGER-RECORD.
           MOVE ZEROS TO LEDGER-AMOUNT.
            
