000100* The MIT License (MIT)                                                   
000200*                                                                         
000300* Copyright (c) 2015 Andrew Young                                         
000400*                                                                         
000500* Permission is hereby granted, free of charge, to any person             
000600* obtaining a copy of this software and associated documentation          
000700* files (the "Software"), to deal in the Software without                 
000800* restriction, including without limitation the rights to use,            
000900* copy, modify, merge, publish, distribute, sublicense, and/or            
001000* sell copies of the Software, and to permit persons to whom the          
001100* Software is furnished to do so, subject to the following                
001200* conditions:                                                             
001300*                                                                         
001400* The above copyright notice and this permission notice shall be          
001500* included in all copies or substantial portions of the Software.         
001600*                                                                         
001700* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         
001800* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES         
001900* OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND                
002000* NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT             
002100* HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,            
002200* WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING            
002300* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR           
002400* OTHER DEALINGS IN THE SOFTWARE.                                         
002500                                                                          
002600 IDENTIFICATION DIVISION.                                                 
002700 PROGRAM-ID. GUESS.                                                       
002800 AUTHOR. Andrew Young.                                                    
002900 ENVIRONMENT DIVISION.                                                    
003000 DATA DIVISION.                                                           
003100                                                                          
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400 01  ANSWER         PIC  9(2) VALUE IS ZEROES.                            
003500 01  GUESS          PIC S9(2) VALUE IS ZEROES.                            
003600 01  DELTA          PIC S9(2) VALUE IS ZEROES.                            
003700 01  TRIES          PIC  9(2) VALUE IS ZEROES.                            
003800 01  YES            PIC X VALUE IS "Y".                                   
003900 01  DONE           PIC X VALUE IS SPACES.                                
004000                                                                          
004100 01  SEED-TIME.                                                           
004200    05 SEED         PIC 9(4) VALUE IS ZEROES.                             
004300                                                                          
004400 PROCEDURE DIVISION.                                                      
004500                                                                          
004600 PROGRAM-BEGIN.                                                           
004700   DISPLAY "Welcome! Let's play a game.".                                 
004800   PERFORM SEED-RANDOM.                                                   
004900   PERFORM SELECT-NUMBER.                                                 
005000   PERFORM MAIN-LOOP                                                      
005100     UNTIL DONE = YES.                                                    
005200                                                                          
005300 PROGRAM-DONE.                                                            
005400   STOP RUN.                                                              
005500                                                                          
005600 MAIN-LOOP.                                                               
005700   PERFORM PROMPT-USER.                                                   
005800   PERFORM CHECK-GUESS.                                                   
005900                                                                          
006000 SEED-RANDOM.                                                             
006100   MOVE FUNCTION CURRENT-DATE(12:16) TO SEED-TIME.                        
006200   COMPUTE ANSWER = FUNCTION RANDOM(SEED).                                
006300                                                                          
006400 SELECT-NUMBER.                                                           
006500   MOVE 0 TO TRIES.                                                       
006600   COMPUTE ANSWER = (FUNCTION RANDOM() * 99) + 1.                         
006700                                                                          
006800 PROMPT-USER.                                                             
006900   DISPLAY "Guess what number I'm thinking of between 1 and 99."          
007000-    " (Enter -1 to give up.)".                                           
007100   ACCEPT GUESS.                                                          
007200                                                                          
007300 CHECK-GUESS.                                                             
007400   IF GUESS = -1                                                          
007500     PERFORM GIVE-UP                                                      
007600   ELSE                                                                   
007700     PERFORM SHOW-HINT.                                                   
007800                                                                          
007900 GIVE-UP.                                                                 
008000     DISPLAY "It was " ANSWER "!"                                         
008100     MOVE YES TO DONE.                                                    
008200                                                                          
008300 SHOW-HINT.                                                               
008400   ADD 1 TO TRIES.                                                        
008500   COMPUTE DELTA = GUESS - ANSWER.                                        
008600                                                                          
008700   IF DELTA = 0                                                           
008800     DISPLAY "Correct! You guessed it in " TRIES " tries!"                
008900     MOVE YES TO DONE.                                                    
009000                                                                          
009100   IF DELTA < 0                                                           
009200     DISPLAY "Too low, guess again!".                                     
009300                                                                          
009400   IF DELTA > 0                                                           
009500     DISPLAY "Too high, guess again!".                                    
009600                                                                          
009700   IF TRIES = 99                                                          
009800     DISPLAY "You've guessed too many times!"                             
009900     DISPLAY "The answer was " ANSWER "."                                 
010000     MOVE YES TO DONE                                                     
010100   ELSE                                                                   
010200     DISPLAY "You've guessed " TRIES " times.".                           
010300                                                                          
