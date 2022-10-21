;Name        Ryan DeSellems
;Date:       2/23/2022
;Course:     COMP 2201
;Instructor: Larue
;---------------------------------------
progStack SEGMENT STACK

    DW 256 DUP(?)

progStack ENDS
;---------------------------------------
progData SEGMENT

    sent0 DB " Self-destruct sequence initiated, re-type this sentence to confirm launch.",0
    sent1 DB "Good luck.",0
    sent2 DB "There's a contamination in the lab.",0
    sent3 DB "Out of the frying pan and into the fire.",0
    sent4 DB "The following statement is false and the previous statement is true.",0
    sent5 DB "Seven sly serpents spent several seconds summing series.",0
    sent6 DB "I needed a sentence for you to type so here it is.",0
    sent7 DB "What are the chances you'd get this sentence?",0
    sent8 DB "There's a bad moon on the rise.",0
    sent9 DB "Rev up those fryers, cause I sure am hungry for one-",0

    intro DB "Welcome to the TypeTest program!", 0
    guide DB "Type the sentence below. (When you begin typing, the test will start.)", 0
    timerText DB "Time Elapsed:    0.0", 0

    initialTicks  DW  (0)        ;value of when timing starts
    elapsedTenths DW  (0)        ;value computed when timer is to be updated
    cursorColumn DB (0)          ;visual   cursor position
    cursorOffset DW (0)          ;logical  cursor position


    SENTENCEPOS  EQU 160*5       ;sentence to type will be printed 6th row
    CURSORROW    EQU 160*6       ;typing will begin on 7th row
    TIMERPOS     EQU 160*3 + 38  ;the position of the tenths place in the timer
    MAXMINUTES   EQU 1*60*10     ;x * 60s * 10 tenths of a second = x minutes have passed (Max time for program)

progData ENDS
;----------------------------------------
progCode SEGMENT
            ASSUME CS:progCode, DS:progData

main PROC

    MOV AX, progData     ;set up Data Segment
    MOV DS, AX
    MOV AX, 0B800h       ;set up Screen Segment
    MOV ES, AX
    MOV cursorOffset, 160*6
    MOV SI, cursorOffset ;user typing will begin on 7th row
    MOV cursorColumn, 0  ;cursor will be positioned at the location (6,0)

    MOV AH, 02h
    MOV BH, 0
    MOV DX, 0600h        ;row 7, col 0
    INT 10h              ;move cursor here

    CALL clearScreen
    CALL prepareTypingTest

    MOV AH, 10h          ;block until key press
    INT 16h              ;
    CALL updateTimer     ;timing will start and main loop will be entered
    CALL processKey
waitForKey:
    CALL checkShiftKeys
    MOV AH, 11h          ;Check buffer to see if key is ready to be pressed
    INT 16h              ;
    JZ  noKeyReady       ;if no key pressed, block program
    MOV AH, 10h          ;if a key is pressed get it
    INT 16h              ;
    CALL processKey      ;AX will have key and dispatch it
 noKeyReady:
    CALL updateTimer     ;Timer will be adjusted if necessary
    CALL colorUserType
    CALL isDone
    CALL checkShiftKeys
    JMP waitForKey

    CALL handleEscape

main ENDP
;----------------------------------------
processKey PROC ;on entry
                ;ah will be scan code
                ;al will be ascii
                ;PROC will get key and handle it
                ;then move cursor

    PUSH AX BX DX SI

    CMP AH, 01h                   ;escapeKey
    JE processEscape
    CMP AH, 3Bh                   ;F1
    JE processF1
    CMP AH, 53h                   ;delete
    JE processDelete
    CMP AH, 0Eh                   ;backspace
    JE processBackspace
    CMP AH, 4Bh                   ;left arrow
    JE processLeftArrow
    CMP AH, 4Dh                   ;right arrow
    JE processRightArrow
    CMP AH, 52h

    CMP cursorColumn, 79
    JGE endProcessKey
    CALL handleRegularKey
    JMP endProcessKey

processEscape:
    CALL handleEscape
    JMP  endProcessKey
processF1:
    CALL handleF1
    JMP  endProcessKey
processDelete:
    CALL handleDelete
    JMP  endProcessKey
processBackspace:
    CALL handleBackspace
    JMP  endProcessKey
processLeftArrow:
    CALL handleLeftArrowKey
    JMP  endProcessKey
processRightArrow:
    CALL handleRightArrowKey
    JMP  endProcessKey

endProcessKey:
    CALL setCursor                 ;move cursor

    POP SI DX BX AX
    RET
processKey ENDP
;----------------------------------------
handleRegularKey PROC
    ;PROC will determine if key is alphanumeric or punctuation,
    ;     then ignore or display key accordingly.
    ;On entry, AH is scan code, AL is ascii code

    PUSH AX BX SI

    CMP  AL, 32                    ;is char punct/alphanumeric?
    JL   doneDisplaying            ;if not, dont display
    CMP  AL, 126                   ;is char punct/alphanumeric?
    JG   doneDisplaying            ;if not, dont display

    MOV  BX, AX                     ;temp preserve AX
    MOV  AH, 12h                    ;get keyboard status
    INT  16h
    AND  AX, 0080h                  ;Is the insert key on?
    CMP  AX, 0080h                  ;AX will have 128 in it if so
    JNE  insertOff                  ;handle insert pressed

doneMovingLine:
    MOV  AX, BX                     ;replace ASCII value in AX
    MOV  SI, cursorOffset           ;put it in si
    MOV  AH, 07h                    ;color char white
    MOV  ES:[SI], AL                ;display char on screen
    INC  cursorColumn               ;adjust cursor pos accordingly
    ADD  cursorOffset, 2            ;
    JMP  doneDisplaying
insertOff:
    MOV  AX,BX                      ;replace ASCII value in AX
    CALL shiftLineRight             ;line will be moved to the right so char can be placed
    JMP  doneMovingLine

doneDisplaying:
    POP SI BX AX
    RET

handleRegularKey ENDP
;----------------------------------------
shiftLineRight PROC
    ;PROC will move sentence from cursor position
    ;     over one position to the right
    ;     On entry, cursorOffset should point to
    ;               where the sentence should be shifte

    PUSH AX BX CX SI


    MOV SI, CURSORROW + 158            ;Put SI at end of typing row
rightShiftLoop:
    CMP SI, cursorOffset               ;Is SI at the cursor?
    JLE quitRightShiftLoop             ;Stop shifting
    MOV BX, ES:[SI-2]                  ;Put char at left of SI in BX
    MOV ES:[SI], BX                    ;Put that char at SI
    SUB SI, 2                          ;SI will point to the character at the left
    JMP rightShiftLoop
quitRightShiftLoop:

    POP SI CX BX AX
    RET

shiftLineRight ENDP
;----------------------------------------
handleBackspace PROC
    ;PROC will move left side of row over one position
    ;starting right one of current cursor position

    CMP cursorColumn, 0
    JE  dontBackspace
    DEC cursorColumn
    SUB cursorOffset, 2     ;move cursor back a position
    CALL shiftSentenceTail  ;bring sentence to it
dontBackspace:
    RET

handleBackspace ENDP
;----------------------------------------
handleDelete PROC
    ;PROC will move left side of row over one position
    ;starting at current cursor position

    CALL shiftSentenceTail
    RET

handleDelete ENDP
;----------------------------------------
handleF1 PROC
    ;PROC will get new target sentence, reset timer,
    ;     and clear user input

    PUSH AX CX DI SI

    MOV cursorColumn, 0             ;reset
    MOV cursorOffset, 160*6         ;
    CALL setCursor

    MOV CX, cursorOffset
    ADD CX, 158
 clearLine:
    CMP cursorOffset, CX            ;Is CX at the end of the second row?
    JG stopClear
    MOV SI, CX                      ;SI will point to char of row
    MOV ES:[SI], 0720h              ;put a space at that position
    MOV ES:[SI-160],0720h           ;put a space above the character
    DEC CX                          ;CX represents next char
    DEC CX
    JMP clearLine
stopClear:

    CALL getRandomSentence          ;get new sentence
    MOV  initialTicks, 0            ;reset initialticks
    MOV  elapsedTenths, 0           ;reset elapsed tenths
    CALL displayTimerBanner         ;clear timer display
    CALL updateTimer                ;refresh timer

    POP SI DI CX AX
    RET

handleF1 ENDP
;----------------------------------------
handleLeftArrowKey PROC
    ;Proc moves cursor over one pos to left

    DEC  cursorColumn
    SUB  cursorOffset, 2
    CALL setCursor
    RET

handleLeftArrowKey ENDP
;----------------------------------------
handleRightArrowKey PROC
    ;PROC moves cursor over one pos to right

    INC  cursorColumn
    ADD  cursorOffset, 2
    CALL setCursor
    RET

handleRightArrowKey ENDP
;----------------------------------------
handleEscape PROC

    MOV AH, 4Ch     ; kernal routine
    INT 21h         ; to return control to os

    RET

handleEscape ENDP
;----------------------------------------
checkShiftKeys PROC
    ;PROC will reset user input and timer
    ;     but keep same target sentence
    PUSH AX BX CX SI

    MOV AH, 12h                     ;get keyboard status
    INT 16h

    AND AX, 0003h                   ;Are the left and right shift down?
    CMP AX, 0003h                   ;AX will have 3 in it if so
    JE  shiftDepressed              ;handle shifts pressed

    JMP shiftNotPressed             ;shift isn't pressed, skip functionality
shiftDepressed:
    MOV SI, CURSORROW+158           ;SI will point to end of typing row
resetLine:
    CMP SI, CURSORROW               ;Is SI at the start of the row?
    JL  stopReset                   ;If so, line has been reset
    MOV ES:[SI], 0720h              ;put a space at that position
    SUB SI, 2                       ;go to previous char position
    JMP resetLine
stopReset:
    MOV cursorColumn, 0             ;reset cursor
    MOV cursorOffset, 160*6         ;
    CALL setCursor
    MOV  initialTicks, 0            ;reset initialticks
    MOV  elapsedTenths, 0           ;reset elapsed tenths
    CALL displayTimerBanner         ;clear timer display
    CALL updateTimer                ;refresh timer
shiftNotPressed:

    POP SI CX BX AX
    RET

checkShiftKeys ENDP
;---------------------------------------
shiftSentenceTail PROC
    ;PROC will move sentence from cursor position
    ;     over one position to the left
    ;     On entry, cursorOffset should point to
    ;               where the sentence should be shifted

    PUSH AX CX SI

    MOV SI, cursorOffset  ;current screen mem of cursor
    MOV CX, 158           ;one row of screen
    ADD CX, cursorOffset  ;Cx has end of cursor screen row
shiftSentence:
    CMP SI, CX            ;if equal, line is shifted
    JGE lineShifted
    MOV AX, ES:[SI+2]     ;get character to right of cursor
    MOV ES:[SI], AX       ;mov it to cursor
    ADD SI, 2             ;mov si over
    JMP shiftSentence
lineShifted:

    POP SI CX AX
    RET

shiftSentenceTail ENDP
;----------------------------------------
colorUserType PROC
    ;PROC will compared user input and target sentence
    ;     then color the user input red if it is incorrect

    PUSH AX BX CX DI SI

    MOV DI, SENTENCEPOS         ;DI points to where displayed sentence is (160*6
    MOV SI, CURSORROW           ;SI points to where user types (160*7
    MOV CL, 79

    MOV BX, 0

scanType:
    CMP CL, 0                   ;see if the row has finished running
    JE endScan                  ;if so stop comparing
    MOV BL, BYTE PTR ES:[SI]    ;swap si into a register so it can be compared
    CMP BYTE PTR ES:[DI], BL    ;is the character the user typed and the character the sentence contains the same?
    JE  dontColorText           ;if so, dont color
colorText:
    CMP SI, CURSORROW + 160     ;is it the end of the typing row
    JGE endScan                 ;if so, loop ends
    CMP ES:[SI], BYTE PTR ' '   ;is it a space on user typing row?
    JE  dontColorSpace          ;spaces shouldn't be colored, skip
    INC SI                      ;go to color byte
    MOV ES:[SI], BYTE PTR 04h   ;put red in
    INC SI                      ;return to char byte
    ADD DI, 2                   ;move byte to compare over
    DEC CX
    JMP colorText               ;color rest of line
dontColorText:
    INC SI                      ;go to color byte
    MOV ES:[SI], BYTE PTR 07h   ;letter is correct, make it white
    INC SI                      ;go to letter byte
    ADD DI, 2                   ;next char to compare
    DEC CX
    JMP scanType
dontColorSpace:
    ADD SI, 2                   ;next letter
    ADD DI, 2                   ;move byte to compare over
    DEC CX                      ;
    JMP colorText               ;continue to color line
endScan:

    POP SI DI CX BX AX
    RET

colorUserType ENDP
;----------------------------------------
updateTimer PROC
    ;PROC will get a start time if needed,
    ;     then update and display time accordingly

    PUSH AX CX DX

    CMP initialTicks, 0        ;see if an intial time has been stored
    JNE updateTimerLabel       ;if so, dont get a new inital time
    MOV AH, 00h                ;get the initial ticks in CX:DX
    INT 1Ah
    MOV initialTicks, DX       ;put the number of ticks into intialTicks
updateTimerLabel:
    CALL computeElapsedTenths
    CALL displayElapsedTenths

    POP DX CX AX
    RET

updateTimer ENDP
;----------------------------------------
computeElapsedTenths PROC
    ;PROC will get current tick amount, subtract it from initial ticks,
    ;          then conver that tick amount into tenths of a second
    ;          If that tick amount exceeds allotted time, the program quits
    ;On entry, initialTicks should contain the number of ticks
    ;          when the timer was started

    PUSH AX BX CX DX

    MOV AH, 00h           ;get the current ticks in CX:DX
    INT 1Ah

    MOV BX, initialTicks  ;BX has tick value when timer started
    SUB DX, BX            ;DX will be the difference in ticks

    MOV AX, DX            ;prep AX for mul with elapsed ticks
    MOV BX, 55            ;BX has conversion factor of ticks to ms
    MUL BX                ;DX:AX = AX (elapsed ticks) * BX(55 ticks per ms)
    MOV BX, 100           ;BX will divide the ms by 100 to get tenths of a second
    DIV BX                ;AX = DX:AX/BX ... DX = DX:AX % BX (DX isn't needed)

    MOV elapsedTenths, AX ;AX is elapsed tenths

    CMP elapsedTenths, MAXMINUTES
    JL  timeNotExceeded
    CALL handleEscape
timeNotExceeded:

    POP DX CX BX AX
    RET

computeElapsedTenths ENDP
;----------------------------------------
displayElapsedTenths PROC
    ;PROC will take elapsed tenths of a second, convert to ascii,
    ;     then display it on screen
    ;     On entry, elapsedTenths should have the number in
    ;               tenths of a second to display

    PUSH AX BX CX DX SI

    MOV SI, TIMERPOS            ;SI points to the location of the tenths place on the timer label

    MOV BL, 10                  ;10 will be used to convert from int to ascii
    MOV AX, elapsedTenths       ;elapsed tenths will be broken up into positions

    DIV BL                      ;AX divided by ten AH has remainder, AL has answer
    ADD AH, '0'                 ;append ascii value to tenth

    MOV ES:[SI], AH             ;put it on screen
    SUB SI, 4                   ;skip SI over decimal place

continueCalculating:
    CMP AL, 0                   ;is the timer position at the last place?
    JE  noSuppression           ;is so the calc is done
    MOV AH, 0                   ;clear remainder out of ah
    DIV BL                      ;divide elapsed tenths by 10
    ADD AH, '0'                 ;convert it to ascii
    ;CMP CX, 4                   ;is is the first place of the timer? (not suppressed)
    MOV ES:[SI], AH             ;put it on screen
    SUB SI, 2                   ;mov SI to previous position
    JMP continueCalculating     ;go until remainder is 0
noSuppression:
    POP SI DX CX BX AX
    RET

displayElapsedTenths ENDP
;----------------------------------------
isDone PROC
    ;PROC will compared user input and target sentence,
    ;     exiting the program if they are identitical

    PUSH AX BX CX DI SI

    MOV DI, SENTENCEPOS         ;DI points to where displayed sentence is (160*6
    MOV SI, CURSORROW           ;SI points to where user types (160*7
    MOV CL, 79                  ;loop will check whole row

checkInput:
    CMP CL, 0                   ;if the loop has run the whole way, the user is correct
    JE  correctlyTyped          ;quit program
    MOV BX, ES:[SI]             ;mov si into bx so it can compare
    CMP ES:[DI], BX             ;compare the user input with letter to type
    JNE doneChecking            ;not equal no further checking required
    ADD SI,2                    ;next letter
    ADD DI,2
    DEC CX
    JMP checkInput
correctlyTyped:
    CALL handleEscape           ;quit

doneChecking:
    POP SI DI CX BX AX
    RET

isDone ENDP
;----------------------------------------
clearScreen PROC
    ;PROC will clear screen

    PUSH SI CX

    MOV CX, 0
clrScreenLoop:
    CMP CX, 4000        ;2000 char pos on screen
    JGE endDebug        ;jmp when all have been set
    MOV SI,CX           ;move si to the char of CX's count
    MOV ES:[SI], 0720h  ;make it a white space
    INC CX              ;CX goes to next char pos
    INC CX
    JMP clrScreenLoop   ;reset loop

endDebug:
    POP CX SI
    RET

clearScreen ENDP
;----------------------------------------
prepareTypingTest PROC
    ;PROC will introduce user to program
    ;     and then set up the timer display
    ;     as well as choose a random sentence

    CALL printIntroMessage
    CALL displayTimerBanner
    CALL getRandomSentence

    RET
prepareTypingTest ENDP
;----------------------------------------
printIntroMessage PROC
    ;PROC will print a welcome message to the user in blue
    ;     on the first row of the screen

    PUSH SI AX DI

    MOV DI, 0             ; DI will be used to display on first row
    MOV SI, OFFSET intro  ; SI will be used to read introduction msg from DS
    MOV AH, 0Bh           ; Intro Message will be Cyan
    Call printLine        ; display message

    MOV DI, 2*160         ; DI will be used to display on 3rd row
    MOV SI, OFFSET guide  ; SI will be used to read guide msg from DS
    MOV AH, 0Bh           ; Guide Message will be Cyan
    CALL printLine        ; display message on screen

    POP DI AX SI
    RET

printIntroMessage ENDP
;----------------------------------------
displayTimerBanner PROC
    ;PROC will print Timer label to be used
    ;     during test along with dummy zeros

    PUSH SI AX DI

    MOV DI, 160*3             ; DI will be used to display on 4th row
    MOV SI, OFFSET timerText  ; SI will be used to read timer msg from DS
    MOV AH, 07h               ; Timer Text will be white
    CALL printLine            ; print that text

    POP DI AX SI
    RET

displayTimerBanner ENDP
;----------------------------------------
getRandomSentence PROC
    ;PROC will get ticks and divide by ten to get random sentence in DS
    ;then count number of terminators to choose and display it

    PUSH AX BX CX DX DI SI

    MOV AH, 00h    ;get elapsed timer ticks through interrupt
    INT 1Ah        ;CX:DX

    MOV AX, DX     ;Need DX:AX for divide
    MOV DX, 0      ;Avoid overflow by removing DX from div
    MOV BX, 10     ;BX will divide ticks by 10, AX = DX:AX / BX
    DIV BX         ;DX will have remainder

    LEA AX, sent0  ;AX will eventually have memory location of sentence to read, sent0 by default
    LEA SI, sent0  ;SI will have beginning of first random sentence
    MOV CX, 0      ;CX needs reset after divide

chooseSentence:
    CMP CX, DX             ;compare number of terminators with random in 0-9
    JE  printSentence      ;if equal print sentence
    CMP BYTE PTR [SI], 0   ;see if there is a terminator in SI
    JNE continueReading    ;if not continue reading
    MOV AX, SI             ;if so, save location
    INC CX                 ;CX will count number of terminators
continueReading:
    INC SI                 ;check next char in sentence
    JMP chooseSentence

printSentence:
    MOV DI, SENTENCEPOS       ; DI will be used to display on 6th row
    MOV SI, AX                ; AX will have memory address of terminator before sentence to pick
    MOV AH, 07h               ; Sentence Text will be white
    INC SI                    ; Mov terminator up so loop wont quit instantly
    CALL PrintLine            ; Print that sentence

    POP SI DI DX CX BX AX
    RET

getRandomSentence ENDP
;----------------------------------------
setCursor PROC
    ;PROC will mov cursor to correct column

    PUSH AX BX DX

    CMP cursorColumn, 79            ;cursor pos is at the end of row
    JG  holdCursorRight             ;dont update it, reset to right bound
    CMP cursorColumn, 0             ;cursor is at left side
    JL  holdCursorLeft              ;dont update it, reset to left bound
    JMP moveCursor
holdCursorRight:
    MOV cursorColumn, 79            ;make sure cursor is trapped at right border
    MOV cursorOffset, CURSORROW +158;make sure cursor is trapped at left border
    JMP moveCursor
holdCursorLeft:
    MOV cursorColumn, 0             ;make sure cursor is trapped at left border
    MOV cursorOffset, CURSORROW     ;make sure cursor is trapped at left border
moveCursor:
    MOV AH, 02h                     ;updateCursor after processing key
    MOV BH, 0
    MOV DH, 06h
    MOV DL, cursorColumn
    INT 10h

   POP DX BX AX
   RET

setCursor ENDP
;----------------------------------------
printLine PROC
   ;On entry, DI points to row to print to
   ;          SI had memory to read from
   ;          AH has color
   ;PROC will then print from memory to screen

    PUSH AX DI SI

printSentenceLoop:
    MOV AL, [SI]              ; read value from sentence msg
    CMP AL, 0                 ; check for terminator
    JE  endSentencePrinting   ; if so stop printing sentence to screen
    MOV ES:[DI], AX           ; put color and char on screen
    INC DI                    ; DI moves to next screen pos
    INC DI
    INC SI                    ; SI moves to next value in timer msg
    JMP printSentenceLoop     ; refresh loop
endSentencePrinting:

    POP SI DI AX
    RET

printLine ENDP
;----------------------------------------
progCode ENDS
;----------------------------------------
END main