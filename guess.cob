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
001600 01  SEED-TIME.                                                           
001700    05 SEED         PIC 9(4).                                             
001800                                                                          
001900 PROCEDURE DIVISION.                                                      
002000                                                                          
002100 PROGRAM-BEGIN.                                                           
002200   DISPLAY "Welcome! Let's play a game.".                                 
002300   PERFORM SEED-RANDOM.                                                   
002400   PERFORM SELECT-NUMBER.                                                 
002500   PERFORM MAIN-LOOP                                                      
002600     UNTIL DONE = YES.                                                    
002700                                                                          
002800 PROGRAM-DONE.                                                            
002900   STOP RUN.                                                              
003000                                                                          
003100 MAIN-LOOP.                                                               
003200   PERFORM PROMPT-USER.                                                   
003300   PERFORM CHECK-GUESS.                                                   
003400                                                                          
003500 SEED-RANDOM.                                                             
003600   MOVE FUNCTION CURRENT-DATE(12:16) TO SEED-TIME.                        
003700   COMPUTE ANSWER = FUNCTION RANDOM(SEED).                                
003800                                                                          
003900 SELECT-NUMBER.                                                           
004000   MOVE 0 TO TRIES.                                                       
004100   COMPUTE ANSWER = (FUNCTION RANDOM() * 99) + 1.                         
004200                                                                          
004300 PROMPT-USER.                                                             
004400   DISPLAY "Guess what number I'm thinking of between 1 and 99."          
004500-    " (Enter -1 to give up.)".                                           
004600   ACCEPT GUESS.                                                          
004700                                                                          
004800 CHECK-GUESS.                                                             
004900   IF GUESS = -1                                                          
005000     PERFORM GIVE-UP                                                      
005100   ELSE                                                                   
005200     PERFORM SHOW-HINT.                                                   
005300                                                                          
005400 GIVE-UP.                                                                 
005500     DISPLAY "It was " ANSWER "!"                                         
005600     MOVE YES TO DONE.                                                    
005700                                                                          
005800 SHOW-HINT.                                                               
005900   ADD 1 TO TRIES.                                                        
006000   COMPUTE DELTA = GUESS - ANSWER.                                        
006100                                                                          
006200   IF DELTA = 0                                                           
006300     DISPLAY "Correct! You guessed it in " TRIES " tries!"                
006400     MOVE YES TO DONE.                                                    
006500                                                                          
006600   IF DELTA < 0                                                           
006700     DISPLAY "Too low, guess again!".                                     
006800                                                                          
006900   IF DELTA > 0                                                           
007000     DISPLAY "Too high, guess again!".                                    
007100                                                                          
007200   IF TRIES = 99                                                          
007300     DISPLAY "You've guessed too many times!"                             
007400     DISPLAY "The answer was " ANSWER "."                                 
007500     MOVE YES TO DONE                                                     
007600   ELSE                                                                   
007700     DISPLAY "You've guessed " TRIES " times.".                           
007800                                                                          
