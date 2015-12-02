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
003700   DISPLAY SEED.                                                          
003800   COMPUTE ANSWER = FUNCTION RANDOM(SEED).                                
003900                                                                          
004000 SELECT-NUMBER.                                                           
004100   MOVE 0 TO TRIES.                                                       
004200   COMPUTE ANSWER = (FUNCTION RANDOM() * 99) + 1.                         
004300                                                                          
004400 PROMPT-USER.                                                             
004500   DISPLAY "Guess what number I'm thinking of between 1 and 99."          
004600-    " (Enter -1 to give up.)".                                           
004700   ACCEPT GUESS.                                                          
004800                                                                          
004900 CHECK-GUESS.                                                             
005000   IF GUESS = -1                                                          
005100     PERFORM GIVE-UP                                                      
005200   ELSE                                                                   
005300     PERFORM SHOW-HINT.                                                   
005400                                                                          
005500 GIVE-UP.                                                                 
005600     DISPLAY "It was " ANSWER "!"                                         
005700     MOVE YES TO DONE.                                                    
005800                                                                          
005900 SHOW-HINT.                                                               
006000   ADD 1 TO TRIES.                                                        
006100   COMPUTE DELTA = GUESS - ANSWER.                                        
006200                                                                          
006300   IF DELTA = 0                                                           
006400     DISPLAY "Correct! You guessed it in " TRIES " tries!"                
006500     MOVE YES TO DONE.                                                    
006600                                                                          
006700   IF DELTA < 0                                                           
006800     DISPLAY "Too low, guess again!".                                     
006900                                                                          
007000   IF DELTA > 0                                                           
007100     DISPLAY "Too high, guess again!".                                    
007200                                                                          
007300   IF TRIES = 99                                                          
007400     DISPLAY "You've guessed too many times!"                             
007500     DISPLAY "The answer was " ANSWER "."                                 
007600     MOVE YES TO DONE                                                     
007700   ELSE                                                                   
007800     DISPLAY "You've guessed " TRIES " times.".                           
007900                                                                          
