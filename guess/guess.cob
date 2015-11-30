000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID. GUESS.                                                       
000300 AUTHOR. Andrew Young.                                                    
000400 ENVIRONMENT DIVISION.                                                    
000500 DATA DIVISION.                                                           
000600                                                                          
000700 WORKING-STORAGE SECTION.                                                 
000800                                                                          
000900 01  ANSWER         PIC  9(2).                                            
001000 01  GUESS          PIC S9(2).                                            
001100 01  DELTA          PIC S9(2).                                            
001200 01  TRIES          PIC  9(2).                                            
001300 01  YES            PIC X VALUE IS "Y".                                   
001400 01  DONE           PIC X.                                                
001500                                                                          
001600 PROCEDURE DIVISION.                                                      
001700                                                                          
001800 PROGRAM-BEGIN.                                                           
001900   DISPLAY "Welcome! Let's play a game.".                                 
002000   PERFORM SELECT-NUMBER.                                                 
002100   PERFORM MAIN-LOOP                                                      
002200     UNTIL DONE = YES.                                                    
002300                                                                          
002400 PROGRAM-DONE.                                                            
002500   STOP RUN.                                                              
002600                                                                          
002700 MAIN-LOOP.                                                               
002800   PERFORM PROMPT-USER.                                                   
002900   PERFORM CHECK-GUESS.                                                   
003000                                                                          
003100 SELECT-NUMBER.                                                           
003200   MOVE 0 TO TRIES.                                                       
003300* TODO: This needs to pick a random number.                               
003400   MOVE 50 TO ANSWER.                                                     
003500                                                                          
003600 PROMPT-USER.                                                             
003700   DISPLAY "Guess what number I'm thinking of between 1 and 100."         
003800-    " (Enter -1 to give up.)".                                           
003900   ACCEPT GUESS.                                                          
004000                                                                          
004100 CHECK-GUESS.                                                             
004200   IF GUESS = -1                                                          
004300     PERFORM GIVE-UP                                                      
004400   ELSE                                                                   
004500     PERFORM SHOW-HINT.                                                   
004600                                                                          
004700 GIVE-UP.                                                                 
004800     DISPLAY "It was " ANSWER "!"                                         
004900     MOVE YES TO DONE.                                                    
005000                                                                          
005100 SHOW-HINT.                                                               
005200   ADD 1 TO TRIES.                                                        
005300   COMPUTE DELTA = GUESS - ANSWER.                                        
005400                                                                          
005500   IF DELTA = 0                                                           
005600     DISPLAY "Correct! You guessed it in " TRIES " tries!"                
005700     MOVE YES TO DONE.                                                    
005800                                                                          
005900   IF DELTA < 0                                                           
006000     DISPLAY "Too low, guess again!".                                     
006100                                                                          
006200   IF DELTA > 0                                                           
006300     DISPLAY "Too high, guess again!".                                    
006400                                                                          
006500   IF TRIES = 99                                                          
006600     DISPLAY "You've guessed too many times!"                             
006700     DISPLAY "The answer was " ANSWER "."                                 
006800     MOVE YES TO DONE                                                     
006900   ELSE                                                                   
007000     DISPLAY "You've guessed " TRIES " times.".                           
007100                                                                          
