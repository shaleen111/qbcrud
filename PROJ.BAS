DECLARE FUNCTION contA! ()
DECLARE SUB contAMenu ()
DECLARE SUB deleteRecord (recordNum!)
DECLARE SUB delete ()
DECLARE SUB edit ()
DECLARE SUB takeRecord (recordArray$(), recordNum!)
DECLARE FUNCTION reads! (st$)
DECLARE SUB readSetup (mRCurrPg!, mRNPg!, mCurrRec!, mROffset!, mRLEntry!, mTRecords$())
DECLARE FUNCTION numOfRecords! (dab$)
DECLARE SUB takeLineFromFile (dArray$(), num!)
DECLARE SUB writeRecords (dArray$(), mNRec!, mFNum!)
DECLARE SUB remove (base$, st$)
DECLARE FUNCTION areRecordsEmpty! (rArray$(), mNRec!)
DECLARE FUNCTION arrayToWritable$ (mArray$())
DECLARE SUB split (base$, st$, dest$)
DECLARE SUB removeQuotes (mBase$)
DECLARE SUB printRecordContent (desiredArray$(), numOfFieldsAtPage!, mCurrRec!, mOffset!, xA!)
DECLARE SUB createSetup (mRCurrPg!, mRNPg!, mCurrRec!, mNRec!, mROffset!, mRLEntry!, mTRecords$())
DECLARE SUB pause ()
DECLARE SUB create ()
DECLARE SUB exitFFMenu ()
DECLARE FUNCTION exitFF! ()
DECLARE SUB crudLogic ()
DECLARE SUB fieldFillSetup (mCurrPg!, mNPg!, mOffset!, mLEntry!)
DECLARE SUB main ()
TYPE cursor
        x AS INTEGER
        y AS INTEGER
        fg AS INTEGER
        bg AS INTEGER
END TYPE

DECLARE SUB rect (x, w, y, h, fg, bg)
DECLARE SUB xline (x, y, l, bg)
DECLARE SUB yline (x, y, l, bg)
DECLARE SUB drawCursor (tCursor AS cursor)
DECLARE SUB clearCursor (tCursor AS cursor)
DECLARE SUB border ()
DECLARE SUB menu ()
DECLARE SUB mmLogic ()
DECLARE SUB credits ()
DECLARE SUB start ()
DECLARE SUB displayError (errorMsg$, x!, y!)
DECLARE SUB displayMessage (msg$, x!, y!)
DECLARE SUB displayText (text$, x!, y!, fg!, bg!)
DECLARE SUB takeMetadata (num!)
DECLARE SUB writeMetadata (num!)
DECLARE SUB opn ()
DECLARE FUNCTION areFieldsEmpty! ()
DECLARE SUB arrayFill (num!)
DECLARE FUNCTION arrayCheck! ()
DECLARE FUNCTION fieldFill! ()
DECLARE FUNCTION fileExist! (file$)
DECLARE FUNCTION dabExist! (name$)
DECLARE SUB op ()
DECLARE FUNCTION takeInput$ (word$, leng, acc$, esc$)
DECLARE SUB new ()
DECLARE SUB moveCursorUp (tCursor AS cursor, stepToMove!, limit!)
DECLARE SUB moveCursorDown (tCursor AS cursor, stepToMove!, limit!)
DECLARE SUB CRUD ()
DECLARE SUB crudMenu ()
DECLARE SUB mainMenu ()
DECLARE SUB prevPage (mCurrPg!, mNPg!, mOffset!, mLEntry!, byHowMuch!, mXA!, mXB!)
DECLARE SUB nextPage (mCurrPg!, mNPg!, mOffset!, mLEntry!, byHowMuch!, mXA!, mXB!)
DECLARE SUB printPage (mOffset!, mNFPrint!, xA!, xB!)

DIM SHARED mmCursor AS cursor
DIM SHARED crudCursor AS cursor
DIM SHARED alphabet AS STRING
DIM SHARED numbers AS STRING

DIM SHARED metadata(3) AS STRING
DIM SHARED fields AS INTEGER
DIM SHARED records AS INTEGER
REDIM SHARED fieldTitles(1) AS STRING

alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
numbers = "1234567890"

CALL main

FUNCTION areFieldsEmpty
        FOR i = 1 TO UBOUND(fieldTitles)
            IF fieldTitles(i) = "" THEN
                areFieldsEmpty = 1
                EXIT FOR
            END IF
        NEXT i
END FUNCTION

FUNCTION areRecordsEmpty (rArray$(), mNRec)
        FOR i = 1 TO mNRec
            FOR j = 1 TO fields
            IF rArray$(i, j) = "" THEN
                areRecordsEmpty = 1
                EXIT FUNCTION
            END IF
            NEXT j
        NEXT i
END FUNCTION

FUNCTION arrayToWritable$ (mArray$())
        FOR i = 1 TO UBOUND(mArray$)
               full$ = full$ + CHR$(34) + mArray$(i) + CHR$(34) + ","
        NEXT i
        full$ = LEFT$(full$, LEN(full$) - 2)
        full$ = RIGHT$(full$, LEN(full$) - 1)
        arrayToWritable$ = full$
END FUNCTION

SUB border
        CALL xline(1, 1, 80, 4)
        CALL yline(1, 1, 23, 4)
        CALL xline(1, 23, 80, 4)
        CALL yline(80, 1, 23, 4)
      
        CALL xline(2, 2, 78, 2)
        CALL yline(2, 2, 21, 2)
        CALL xline(2, 22, 78, 2)
        CALL yline(79, 2, 21, 2)

END SUB

SUB clearCursor (tCursor AS cursor)
        COLOR tCursor.fg, tCursor.bg
        LOCATE tCursor.y, tCursor.x
        PRINT " "
        COLOR 7, 0
END SUB

FUNCTION contA
        DIM contCursor AS cursor

        contCursor.x = 38
        contCursor.y = 11
        contCursor.fg = 15
        contCursor.bg = 3

        DO
           CALL drawCursor(contCursor)
           press$ = INKEY$
           IF press$ = CHR$(13) THEN
              contA = contCursor.y
              EXIT FUNCTION
           END IF
           IF press$ = CHR$(0) + CHR$(72) THEN
              CALL moveCursorUp(contCursor, 1, 11)
           END IF
           IF press$ = CHR$(0) + CHR$(80) THEN
              CALL moveCursorDown(contCursor, 1, 13)
           END IF
        LOOP UNTIL 0
END FUNCTION

SUB contAMenu
        CALL xline(19, 8, 46, 4)
        CALL xline(19, 15, 46, 4)
        CALL yline(19, 8, 8, 4)
        CALL yline(65, 8, 8, 4)
        CALL rect(20, 45, 10, 5, 15, 3)
        CALL displayMessage("Do you want to continue with this action?", 20, 9)
        CALL displayMessage("Yes", 38, 11)
        CALL displayMessage("No", 38, 12)
        CALL displayMessage("CRUD Menu", 38, 13)
END SUB

SUB create
        nRec$ = ""
        CALL displayMessage("Press Ctrl+B to Cancel and Go Back to CRUD Menu!", 16, 4)
        LOCATE 10, 22
        PRINT "Number of Records (02 digit) :"
        CALL displayMessage(STRING$(4, " ") + "Fill all entries to continue!" + STRING$(4, " "), 20, 16)
captureInputCreate:
        LOCATE 10, 53
        IF takeInput(nRec$, 2, numbers, "b") = "b" THEN
                EXIT SUB
        END IF
        IF VAL(nRec$) < 1 THEN
           LOCATE 10, 53
           PRINT STRING$(2, " ")
           nRec$ = ""
           CALL displayError("Zero isn't valid input for this !", 19, 21)
           GOTO captureInputCreate
        END IF

        nRecords = VAL(nRec$)

        CALL displayMessage("Press Any Key to Continue", 26, 21)
        CALL pause
        CALL rect(3, 76, 3, 19, 7, 0)
       
        DIM tRecords(nRecords, fields) AS STRING
        DIM cCursor AS cursor
        currRec = 1
newPage:
        exitCode$ = ""

        cCursor.x = 10
        cCursor.y = 6
        cCursor.fg = 7
        cCursor.bg = 0

        rOffset = 0
        rLEntry = fields MOD 15
        rNPg = fields \ 15
        rCurrPg = 1

        IF rLEntry <> 0 THEN
           rNPg = rNPg + 1
        ELSE
           rLEntry = 15
        END IF

        CALL createSetup(rCurrPg, rNPg, currRec, nRecords, rOffset, rLEntry, tRecords())
      
        DO
                CALL drawCursor(cCursor)

                press$ = INKEY$
              
                IF press$ = CHR$(27) THEN
                        CALL exitFFMenu
                        CALL displayMessage("CRUD Menu", 38, 13)
                        opt = exitFF
                        IF opt = 11 THEN
                           CLS
                           END
                        ELSEIF opt = 12 THEN
                           CALL rect(25, 36, 8, 8, 7, 0)
                           CALL clearCursor(cCursor)
                           CALL createSetup(rCurrPg, rNPg, currRec, nRecords, rOffset, rLEntry, tRecords())
                        ELSEIF opt = 13 THEN
                           EXIT SUB
                        END IF
                END IF

                IF press$ = CHR$(13) THEN
                        IF tRecords(currRec, (cCursor.y - 5) + rOffset) <> "" THEN
                                LOCATE cCursor.y, 42
                                PRINT STRING$(LEN(tRecords(currRec, (cCursor.y - 5) + rOffset)), " ")
                                tRecords(currRec, (cCursor.y - 5) + rOffset) = ""
                        END IF
                        LOCATE cCursor.y, 42
                        exitCode$ = takeInput(tRecords(currRec, (cCursor.y - 5) + rOffset), 20, " _-()&" + numbers + alphabet, "xd")
                        IF exitCode$ = "" THEN
                                IF rCurrPg <> rNPg THEN
                                          CALL moveCursorDown(cCursor, 1, 20)
                                ELSE
                                          CALL moveCursorDown(cCursor, 1, rLEntry + 5)
                                END IF
                        END IF
                END IF

                IF press$ = CHR$(24) OR exitCode$ = "d" THEN
                   IF areRecordsEmpty(tRecords(), nRecords) <> 1 THEN
                           OPEN metadata(1) + ".dab" FOR APPEND AS #6
                           CALL writeRecords(tRecords(), nRecords, 6)
                           CLOSE #6
                           records = records + nRecords
                           CALL displayMessage("Data Written to File on Disk!", 26, 21)
                           CALL displayMessage("Press Anything (Any Key) to Continue on!", 20, 4)
                           CALL pause
                           EXIT DO
                   ELSE
                           CALL displayError("All Fields Must be Filled!", 22, 21)
                   END IF
                   exitCode$ = ""
                END IF

                IF press$ = CHR$(0) + CHR$(75) THEN
                        IF currRec > 1 THEN
                           currRec = currRec - 1
                           CALL rect(11, 51, 6, 15, 7, 0)
                           CALL clearCursor(cCursor)
                           GOTO newPage
                        END IF
                END IF
              
                IF press$ = CHR$(0) + CHR$(77) THEN
                        IF currRec < nRecords THEN
                           currRec = currRec + 1
                           CALL rect(11, 51, 6, 15, 7, 0)
                           CALL clearCursor(cCursor)
                           GOTO newPage
                        END IF
                END IF

                IF press$ = CHR$(0) + CHR$(72) THEN
                        IF rCurrPg > 1 AND cCursor.y = 6 THEN
                           CALL rect(11, 51, 6, 15, 7, 0)
                           CALL clearCursor(cCursor)
                           cCursor.x = 10
                           cCursor.y = 20
                           CALL prevPage(rCurrPg, rNPg, rOffset, rLEntry, 1, 11, 17)
                           IF rCurrPg = rNPg THEN
                              fPP = rLEntry
                           ELSE
                              fPP = 15
                           END IF
                           CALL printRecordContent(tRecords(), fPP, currRec, rOffset, 42)
                           LOCATE 21, 62
                           PRINT "Page "; rCurrPg; " of "; rNPg
                        ELSE
                           CALL moveCursorUp(cCursor, 1, 6)
                        END IF
                END IF

                IF press$ = CHR$(0) + CHR$(80) THEN
                        IF rCurrPg < rNPg AND cCursor.y = 20 THEN
                           CALL rect(11, 51, 6, 15, 7, 0)
                           CALL clearCursor(cCursor)
                           cCursor.x = 10
                           cCursor.y = 6
                           CALL nextPage(rCurrPg, rNPg, rOffset, rLEntry, 1, 11, 17)
                           IF rCurrPg = rNPg THEN
                              fPP = rLEntry
                           ELSE
                              fPP = 15
                           END IF
                           CALL printRecordContent(tRecords(), fPP, currRec, rOffset, 42)
                           LOCATE 21, 62
                           PRINT "Page "; rCurrPg; " of "; rNPg
                        ELSE
                           IF rCurrPg <> rNPg THEN
                              CALL moveCursorDown(cCursor, 1, 20)
                           ELSE
                              CALL moveCursorDown(cCursor, 1, rLEntry + 5)
                           END IF
                        END IF
                END IF
        LOOP UNTIL 0
END SUB


SUB createSetup (mRCurrPg, mRNPg, mCurrRec, mNRec, mROffset, mRLEntry, mTRecords$())
        LOCATE 3, 34
        PRINT "Records Creation"
       
        LOCATE 5, 12
        PRINT "N."

        LOCATE 5, 17
        PRINT "Field Title"

        LOCATE 5, 42
        PRINT "Content"

        LOCATE 21, 4
        PRINT "Rec. "; mCurrRec; " of "; mNRec
        LOCATE 21, 62
        PRINT "Page "; mRCurrPg; " of "; mRNPg
        CALL displayMessage("Press Ctrl+X Once Everything is Filled !", 20, 4)
    
        'HAX
        mRCurrPg = mRCurrPg - 1
        mROffset = mROffset - 15
       
        CALL nextPage(mRCurrPg, mRNPg, mROffset, mRLEntry, 1, 11, 17)
       
        IF mRCurrPg = mRNPg THEN
           mFPP = mRLEntry
        ELSE
           mFPP = 15
        END IF
       
        CALL printRecordContent(mTRecords$(), mFPP, mCurrRec, mROffset, 42)
END SUB

SUB credits
        CALL displayMessage("All Work is Done by Shaleen Baral", 25, 12)
        CALL displayMessage("< Press Any Key To Go Back to Menu >", 23, 13)
       
        DO

        LOOP WHILE INKEY$ = ""
END SUB

SUB CRUD
        DIM keys AS STRING

        records = numOfRecords(metadata(1) + ".dab")
       
        crudCursor.x = 33
        crudCursor.y = 11
        crudCursor.fg = 15
        crudCursor.bg = 3

        CALL crudMenu
       
        DO
                CALL drawCursor(crudCursor)
                keys = UCASE$(INKEY$)
                LOCATE 3, 3
                PRINT keys
                IF keys = CHR$(27) THEN
                        CALL exitFFMenu
                        opt = exitFF
                        IF opt = 11 THEN
                           CLS
                           END
                        ELSEIF opt = 12 THEN
                           CALL rect(3, 76, 3, 19, 7, 0)
                           CALL crudMenu
                        ELSEIF opt = 13 THEN
                           EXIT SUB
                        END IF
                ELSEIF keys = CHR$(13) THEN
                        CALL crudLogic
                        CALL crudMenu
                ELSEIF keys = CHR$(0) + CHR$(72) THEN
                        CALL moveCursorUp(crudCursor, 2, 11)
                ELSEIF keys = CHR$(0) + CHR$(80) THEN
                        CALL moveCursorDown(crudCursor, 2, 17)
                END IF
        LOOP UNTIL 0
END SUB

SUB crudLogic
        IF crudCursor.y = 11 THEN
                CALL rect(3, 76, 3, 19, 7, 0)
                CALL create
        ELSEIF crudCursor.y = 13 THEN
                CALL rect(3, 76, 3, 19, 7, 0)
                LOCATE 3, 35
                PRINT "Records Reader"
                CALL displayMessage("Press Ctrl+B To Go Back To the CRUD Menu !", 19, 4)
                exitCode = reads("b")
        ELSEIF crudCursor.y = 15 THEN
                CALL rect(3, 76, 3, 19, 7, 0)
                CALL edit
        ELSEIF crudCursor.y = 17 THEN
                CALL rect(3, 76, 3, 19, 7, 0)
                CALL delete
        END IF
        CALL rect(3, 76, 3, 19, 7, 0)
        EXIT SUB
END SUB

SUB crudMenu
        CALL rect(31, 20, 6, 3, 15, 3)
        CALL rect(31, 20, 10, 9, 15, 3)
        COLOR 15, 3
        LOCATE 7, 35
        PRINT "CRUD Actions"
        LOCATE 11, 35
        PRINT "Create"
        LOCATE 13, 35
        PRINT "Read"
        LOCATE 15, 35
        PRINT "Update"
        LOCATE 17, 35
        PRINT "Delete"
        COLOR 7, 0
END SUB

FUNCTION dabExist (name$)
        OPEN name$ + ".DAB" FOR INPUT AS #3
        INPUT #3, tMetaFirst$
        CLOSE #3
        IF UCASE$(tMetaFirst$) = UCASE$(name$) THEN
           dabExist = 1
        ELSE
           dabExist = 0
        END IF
END FUNCTION

SUB delete
                LOCATE 3, 34
                PRINT "Records Deletion"
                CALL displayMessage("Press Ctrl+X To Delete Selected Record !", 20, 4)
                exitCode = reads("d")
                CALL rect(3, 76, 3, 19, 7, 0)
                IF exitCode = 0 THEN
                        EXIT SUB
                ELSE
                        CALL deleteRecord(exitCode)
                        records = records - 1
                END IF
END SUB

SUB deleteRecord (recordNum)
        DIM tRecord(fields) AS STRING
        DIM tRecords(records - 1, fields) AS STRING
        a = FREEFILE
        OPEN metadata(1) + ".tdab" FOR OUTPUT AS #a
        CALL writeMetadata(a)
        i = 0
        k = 0
        DO
               i = i + 1
               k = k + 1
               IF i <> recordNum OR afterDeleted = 1 THEN
                  CALL takeRecord(tRecord(), i)
                  FOR j = 1 TO fields
                      tRecords(k, j) = tRecord(j)
                  NEXT j
                  afterDeleted = 0
               ELSE
                k = k - 1
               END IF
        LOOP UNTIL i = records
        CALL writeRecords(tRecords(), records - 1, a)
        CLOSE #a
        KILL metadata(1) + ".dab"
        NAME metadata(1) + ".tdab" AS metadata(1) + ".dab"
END SUB

SUB displayError (errorMsg$, x, y)
        CALL displayText("Error: " + errorMsg$, x, y, 15, 4)
        SLEEP 1
        CALL xline(x, y, 11 + LEN(errorMsg$), 0)
END SUB

SUB displayMessage (msg$, x, y)
        CALL displayText(msg$, x, y, 15, 3)
END SUB

SUB displayText (text$, x, y, fg, bg)
        LOCATE y, x
        COLOR fg, bg
        PRINT STRING$(2, " ") + text$ + STRING$(2, " ")
        COLOR 7, 0
END SUB

SUB drawCursor (tCursor AS cursor)
        COLOR tCursor.fg, tCursor.bg
        LOCATE tCursor.y, tCursor.x
        PRINT ">"
        COLOR 7, 0
END SUB

SUB edit
                LOCATE 3, 35
                PRINT "Records Editor"
                CALL displayMessage("Press Ctrl+X To Edit the Selected Record !", 19, 4)
                extCode = reads("d")
                CALL rect(3, 76, 3, 19, 7, 0)
                IF extCode = 0 THEN
                        EXIT SUB
                ELSE
                      
                        currRec = extCode
                        nRecords = 1
                       
                        DIM tRecords(records, fields) AS STRING
                        DIM tRecord(fields)  AS STRING
                        DIM exCursor AS cursor
                       
                        exitCode$ = ""

                        exCursor.x = 10
                        exCursor.y = 6
                        exCursor.fg = 7
                        exCursor.bg = 0

                        rOffset = 0
                        rLEntry = fields MOD 15
                        rNPg = fields \ 15
                        rCurrPg = 1

                        IF rLEntry <> 0 THEN
                           rNPg = rNPg + 1
                        ELSE
                           rLEntry = 15
                        END IF

                        FOR i = 1 TO records
                        CALL takeRecord(tRecord(), i)
                            FOR j = 1 TO fields
                                tRecords(i, j) = tRecord(j)
                            NEXT j
                        NEXT i

                        CALL createSetup(rCurrPg, rNPg, currRec, nRecords, rOffset, rLEntry, tRecords())
                     
                        LOCATE 21, 4
                        PRINT "Record Being Edited : "; currRec

                        DO
                                CALL drawCursor(exCursor)

                                press$ = INKEY$
                             
                                IF press$ = CHR$(27) THEN
                                        CALL exitFFMenu
                                        CALL displayMessage("CRUD Menu", 38, 13)
                                        opt = exitFF
                                        IF opt = 11 THEN
                                           CLS
                                           END
                                        ELSEIF opt = 12 THEN
                                           CALL rect(25, 36, 8, 8, 7, 0)
                                           CALL clearCursor(exCursor)
                                           CALL createSetup(rCurrPg, rNPg, currRec, nRecords, rOffset, rLEntry, tRecords())
                                           LOCATE 21, 4
                                           PRINT "Rec. "; 1; " of "; nRecords
                                        ELSEIF opt = 13 THEN
                                           EXIT SUB
                                        END IF
                                END IF

                                IF press$ = CHR$(13) THEN
                                        IF tRecords(currRec, (exCursor.y - 5) + rOffset) <> "" THEN
                                                LOCATE exCursor.y, 42
                                                PRINT STRING$(LEN(tRecords(currRec, (exCursor.y - 5) + rOffset)), " ")
                                                tRecords(currRec, (exCursor.y - 5) + rOffset) = ""
                                        END IF
                                        LOCATE exCursor.y, 42
                                        exitCode$ = takeInput(tRecords(currRec, (exCursor.y - 5) + rOffset), 20, " _-()&" + numbers + alphabet, "xd")
                                        IF exitCode$ = "" THEN
                                                IF rCurrPg <> rNPg THEN
                                                          CALL moveCursorDown(exCursor, 1, 20)
                                                ELSE
                                                          CALL moveCursorDown(exCursor, 1, rLEntry + 5)
                                                END IF
                                        END IF
                                END IF

                                IF press$ = CHR$(24) OR exitCode$ = "d" THEN
                                           IF areRecordsEmpty(tRecords(), nRecords) <> 1 THEN
                                                   OPEN metadata(1) + ".dab" FOR OUTPUT AS #7
                                                   CALL writeMetadata(7)
                                                   nRecords = records
                                                   CALL writeRecords(tRecords(), nRecords, 7)
                                                   CLOSE #7
                                                   CALL displayMessage("Data Written to File on Disk!", 26, 21)
                                                   CALL displayMessage("Press Anything (Any Key) to Continue on!", 20, 4)
                                                   CALL pause
                                                   EXIT DO
                                           ELSE
                                                   CALL displayError("All Fields Must be Filled!", 22, 21)
                                           END IF
                                           exitCode$ = ""
                                END IF

                                IF press$ = CHR$(0) + CHR$(72) THEN
                                        IF rCurrPg > 1 AND exCursor.y = 6 THEN
                                           CALL rect(11, 51, 6, 15, 7, 0)
                                           CALL clearCursor(exCursor)
                                           exCursor.x = 10
                                           exCursor.y = 20
                                           CALL prevPage(rCurrPg, rNPg, rOffset, rLEntry, 1, 11, 17)
                                           IF rCurrPg = rNPg THEN
                                              fPP = rLEntry
                                           ELSE
                                              fPP = 15
                                           END IF
                                           CALL printRecordContent(tRecords(), fPP, currRec, rOffset, 42)
                                           LOCATE 21, 62
                                           PRINT "Page "; rCurrPg; " of "; rNPg
                                        ELSE
                                           CALL moveCursorUp(exCursor, 1, 6)
                                        END IF
                                END IF

                                IF press$ = CHR$(0) + CHR$(80) THEN
                                        IF rCurrPg < rNPg AND exCursor.y = 20 THEN
                                           CALL rect(11, 51, 6, 15, 7, 0)
                                           CALL clearCursor(exCursor)
                                           exCursor.x = 10
                                           exCursor.y = 6
                                           CALL nextPage(rCurrPg, rNPg, rOffset, rLEntry, 1, 11, 17)
                                           IF rCurrPg = rNPg THEN
                                              fPP = rLEntry
                                           ELSE
                                              fPP = 15
                                           END IF
                                           CALL printRecordContent(tRecords(), fPP, currRec, rOffset, 42)
                                           LOCATE 21, 62
                                           PRINT "Page "; rCurrPg; " of "; rNPg
                                        ELSE
                                           IF rCurrPg <> rNPg THEN
                                              CALL moveCursorDown(exCursor, 1, 20)
                                           ELSE
                                              CALL moveCursorDown(exCursor, 1, rLEntry + 5)
                                           END IF
                                        END IF
                                END IF
                        LOOP UNTIL 0
                END IF
END SUB

FUNCTION exitFF
        DIM eCursor AS cursor

        eCursor.x = 38
        eCursor.y = 11
        eCursor.fg = 15
        eCursor.bg = 3

        DO
           CALL drawCursor(eCursor)
           press$ = INKEY$
           IF press$ = CHR$(13) THEN
              exitFF = eCursor.y
              EXIT FUNCTION
           END IF
           IF press$ = CHR$(0) + CHR$(72) THEN
              CALL moveCursorUp(eCursor, 1, 11)
           END IF
           IF press$ = CHR$(0) + CHR$(80) THEN
              CALL moveCursorDown(eCursor, 1, 13)
           END IF
        LOOP UNTIL 0
END FUNCTION

SUB exitFFMenu
        CALL xline(26, 8, 34, 4)
        CALL xline(26, 15, 34, 4)
        CALL yline(25, 8, 8, 4)
        CALL yline(60, 8, 8, 4)
        CALL rect(26, 34, 9, 6, 15, 3)
        CALL displayMessage("Do you want to quit?", 31, 9)
        CALL displayMessage("Yes", 38, 11)
        CALL displayMessage("No", 38, 12)
        CALL displayMessage("Main Menu", 38, 13)
END SUB

FUNCTION fieldFill
        REDIM fieldTitles(fields) AS STRING
        DIM fCursor AS cursor
        
        exitCode$ = ""

        fCursor.x = 29
        fCursor.y = 6
        fCursor.fg = 7
        fCursor.bg = 0

        offset = 0
        lEntry = fields MOD 15
        nPg = fields \ 15
        currPg = 1
       
        IF lEntry <> 0 THEN
           nPg = nPg + 1
        ELSE
           lEntry = 15
        END IF
       
        CALL fieldFillSetup(currPg, nPg, offset, lEntry)
       
        DO
                CALL drawCursor(fCursor)

                press$ = INKEY$
               
                IF press$ = CHR$(27) THEN
                        CALL exitFFMenu
                        opt = exitFF
                        IF opt = 11 THEN
                           CLS
                           END
                        ELSEIF opt = 12 THEN
                           CALL rect(25, 36, 8, 8, 7, 0)
                           CALL fieldFillSetup(currPg, nPg, offset, lEntry)
                        ELSEIF opt = 13 THEN
                           fieldFill = 1
                           EXIT FUNCTION
                        END IF
                END IF

                IF press$ = CHR$(13) THEN
                        IF fieldTitles((fCursor.y - 5) + offset) <> "" THEN
                                LOCATE fCursor.y, 36
                                PRINT STRING$(LEN(fieldTitles((fCursor.y - 5) + offset)), " ")
                                fieldTitles((fCursor.y - 5) + offset) = ""
                        END IF
                        LOCATE fCursor.y, 36
                        exitCode$ = takeInput(fieldTitles((fCursor.y - 5) + offset), 20, " _-()&" + numbers + alphabet, "xd")
                        IF exitCode$ = "" THEN
                                IF currPg <> nPg THEN
                                          CALL moveCursorDown(fCursor, 1, 20)
                                ELSE
                                          CALL moveCursorDown(fCursor, 1, lEntry + 5)
                                END IF
                        END IF
                END IF

                IF press$ = CHR$(24) OR exitCode$ = "d" THEN
                        IF areFieldsEmpty <> 1 THEN
                           OPEN metadata(1) + ".dab" FOR OUTPUT AS #5
                           CALL writeMetadata(5)
                           CLOSE #5
                           CALL displayMessage("Data Written to File on Disk!", 26, 21)
                           CALL displayMessage("Press Anything (Any Key) to Continue on!", 20, 4)
                           CALL pause
                           EXIT DO
                        ELSE
                           CALL displayError("All Fields Must be Filled!", 22, 21)
                        END IF
                        exitCode$ = ""
                END IF

                IF press$ = CHR$(0) + CHR$(75) THEN
                       IF currPg > 1 THEN
                           CALL rect(30, 26, 6, 15, 7, 0)
                           CALL clearCursor(fCursor)
                           fCursor.x = 29
                           fCursor.y = 6
                           CALL prevPage(currPg, nPg, offset, lEntry, 1, 30, 36)
                           LOCATE 21, 64
                           PRINT "Page "; currPg; " of "; nPg
                       END IF
                END IF
               
                IF press$ = CHR$(0) + CHR$(77) THEN
                       IF currPg < nPg THEN
                           CALL rect(30, 26, 6, 15, 7, 0)
                           CALL clearCursor(fCursor)
                           fCursor.x = 29
                           fCursor.y = 6
                           CALL nextPage(currPg, nPg, offset, lEntry, 1, 30, 36)
                           LOCATE 21, 64
                           PRINT "Page "; currPg; " of "; nPg
                        END IF
                END IF

                IF press$ = CHR$(0) + CHR$(72) THEN
                        CALL moveCursorUp(fCursor, 1, 6)
                END IF

                IF press$ = CHR$(0) + CHR$(80) THEN
                        IF currPg <> nPg THEN
                                CALL moveCursorDown(fCursor, 1, 20)
                        ELSE
                                CALL moveCursorDown(fCursor, 1, lEntry + 5)
                        END IF
                END IF
        LOOP UNTIL 0
END FUNCTION

SUB fieldFillSetup (mCurrPg, mNPg, mOffset, mLEntry)
        LOCATE 3, 34
        PRINT "Field Name Setup"
  
        LOCATE 5, 31
        PRINT "N."

        LOCATE 5, 36
        PRINT "Field Title"

        LOCATE 21, 64
        PRINT "Page "; mCurrPg; " of "; mNPg
        CALL displayMessage("Press Ctrl+X Once All Fields are Filled!", 20, 4)
     
        CALL nextPage(0, mNPg, -15, mLEntry, 1, 30, 36)
END SUB

FUNCTION fileExist (file$)
        OPEN file$ FOR BINARY AS #2
        num = LOF(2)
        CLOSE #2
        IF num > 0 THEN
                fileExist = 1
        ELSE
                KILL file$
                fileExist = 0
        END IF
END FUNCTION

SUB main
        CLS

        mmCursor.x = 33
        mmCursor.y = 11
        mmCursor.fg = 15
        mmCursor.bg = 3

        CALL mainMenu
        DO
             CALL drawCursor(mmCursor)
             keys$ = UCASE$(INKEY$)
             LOCATE 3, 3
             PRINT keys$
             IF keys$ = CHR$(13) THEN
                    CALL mmLogic
                    CALL mainMenu
             ELSEIF keys$ = CHR$(0) + CHR$(72) THEN
                    CALL moveCursorUp(mmCursor, 2, 11)
             ELSEIF keys$ = CHR$(0) + CHR$(80) THEN
                    CALL moveCursorDown(mmCursor, 2, 17)
             END IF
         LOOP UNTIL 0
END SUB

SUB mainMenu
        CALL border

        CALL rect(31, 20, 6, 3, 15, 3)
        CALL rect(31, 20, 10, 9, 15, 3)
        COLOR 15, 3
        LOCATE 7, 35
        PRINT "Database App"
        LOCATE 11, 35
        PRINT "New Database"
        LOCATE 13, 35
        PRINT "Load Database"
        LOCATE 15, 35
        PRINT "Credits"
        LOCATE 17, 35
        PRINT "Exit"
        COLOR 7, 0
END SUB

SUB mmLogic
        IF mmCursor.y = 11 THEN
               CALL rect(3, 76, 3, 19, 7, 0)
               CALL new
        ELSEIF mmCursor.y = 13 THEN
               CALL rect(3, 76, 3, 19, 7, 0)
               CALL opn
        ELSEIF mmCursor.y = 15 THEN
               CALL rect(3, 76, 3, 19, 7, 0)
               CALL credits
        ELSEIF mmCursor.y = 17 THEN
               CLS
               END
        END IF
        CALL rect(3, 76, 3, 19, 7, 0)
        EXIT SUB
END SUB

SUB moveCursorDown (tCursor AS cursor, stepToMove, limit)
    IF tCursor.y < limit THEN
       CALL clearCursor(tCursor)
       tCursor.y = tCursor.y + stepToMove
    END IF
END SUB

SUB moveCursorUp (tCursor AS cursor, stepToMove, limit)
    IF tCursor.y > limit THEN
       CALL clearCursor(tCursor)
       tCursor.y = tCursor.y - stepToMove
    END IF
END SUB

SUB new
        DIM tMetadata(4) AS STRING
        DIM inpArg(4) AS STRING
        DIM inpLen(4)
        DIM nCursor AS cursor

        exitCode$ = ""

        inpArg(1) = "" + alphabet
        inpArg(2) = " ." + alphabet
        inpArg(3) = " .!?()" + alphabet
        inpArg(4) = "" + numbers

        inpLen(1) = 8
        inpLen(2) = 10
        inpLen(3) = 23
        inpLen(4) = 2
       
        nCursor.x = 24
        nCursor.y = 10
        nCursor.fg = 7
        nCursor.bg = 0

        CALL displayMessage("Press Ctrl+B to Cancel and Go Back to Main Menu!", 16, 4)
        CALL displayMessage("Press Ctrl+X to Submit the Given Form", 22, 5)
       
        LOCATE 10, 26
        PRINT "Database Name (08 chars) :"
        LOCATE 11, 26
        PRINT "Author Name   (10 chars) :"
        LOCATE 12, 26
        PRINT "Description   (23 chars) :"
        LOCATE 13, 26
        PRINT "N. of Fields  (2 digits) :"
       
        CALL displayMessage("NOTE: Metadata can be edited afterwards", 21, 15)
        CALL displayMessage("NOTE: Existing File will be overwritten", 21, 16)
        CALL displayMessage(STRING$(5, " ") + "Fill all entries to continue!" + STRING$(5, " "), 21, 17)
       
        DO
          CALL drawCursor(nCursor)
          press$ = INKEY$
          IF press$ = CHR$(13) THEN
              count = nCursor.y - 9
              IF tMetadata(count) <> "" THEN
                  LOCATE nCursor.y, 53
                  PRINT STRING$(LEN(tMetadata(count)), " ")
                  tMetadata(count) = ""
              END IF
nf:
              LOCATE nCursor.y, 53
              exitCode$ = takeInput(tMetadata(count), inpLen(count), inpArg(count), "bdx")
              IF exitCode$ = "" THEN
                 CALL moveCursorDown(nCursor, 1, 13)
              END IF
              IF count = 4 AND VAL(tMetadata(4)) < 1 THEN
                 LOCATE 13, 53
                 PRINT STRING$(2, " ")
                 tMetadata(4) = ""
                 CALL displayError("Zero isn't valid input for this !", 19, 21)
                 GOTO nf
               END IF
          END IF
          IF press$ = CHR$(24) OR exitCode$ = "d" THEN
             filledMD = 0
             FOR i = 1 TO 4
                IF tMetadata(i) <> "" THEN
                        filledMD = filledMD + 1
                END IF
             NEXT i
             IF filledMD <> 4 THEN
                CALL displayError("All Fields Must be Filled!", 23, 21)
             ELSE
                EXIT DO
             END IF
             exitCode$ = ""
          END IF
          IF press$ = CHR$(2) OR exitCode$ = "b" THEN
              EXIT SUB
          END IF
          IF press$ = CHR$(0) + CHR$(72) THEN
             CALL moveCursorUp(nCursor, 1, 10)
          END IF
          IF press$ = CHR$(0) + CHR$(80) THEN
             CALL moveCursorDown(nCursor, 1, 13)
          END IF
        LOOP UNTIL 0
       
        FOR i = 1 TO 3
        metadata(i) = tMetadata(i)
        NEXT i
        fields = VAL(tMetadata(4))

        CALL displayMessage("Press Any Key to Continue", 27, 21)
        CALL pause

        CALL rect(3, 76, 3, 19, 7, 0)
        exitCode = fieldFill
        IF exitCode = 0 THEN
                CALL rect(3, 76, 3, 19, 7, 0)
                CALL CRUD
        ELSEIF exitCode = 1 THEN
        END IF
END SUB

SUB nextPage (mCurrPg, mNPg, mOffset, mLEntry, byHowMuch, mXA, mXB)
    mCurrPg = mCurrPg + (1 * byHowMuch)
    mOffset = mOffset + (15 * byHowMuch)
    IF mNPg = mCurrPg THEN
             CALL printPage(mOffset, mLEntry, mXA, mXB)
    ELSE
             CALL printPage(mOffset, 15, mXA, mXB)
    END IF
END SUB

FUNCTION numOfRecords (dab$)
        a = FREEFILE
        OPEN dab$ FOR INPUT AS #a
        CALL takeMetadata(a)
        count = 0
        DO UNTIL EOF(1)
                LINE INPUT #a, st$
                IF LTRIM$(RTRIM$(st$)) <> "" THEN
                        count = count + 1
                END IF
        LOOP
        CLOSE #a
        numOfRecords = count
END FUNCTION

SUB opn
        CALL displayMessage("Press Ctrl+B to Cancel and Go Back to Main Menu!", 16, 4)
        LOCATE 10, 23
        PRINT "Database Name (08 chars) :"
        CALL displayMessage("NOTE: This Can Only Open Files Complicit of the .DAB  File Spec.", 7, 14)
        CALL displayMessage(STRING$(4, " ") + "Fill all entries to continue!" + STRING$(4, " "), 20, 16)
opnS:
        LOCATE 10, 50
        IF takeInput(name$, 8, alphabet, "b") = "b" THEN
            EXIT SUB
        END IF
        IF fileExist(name$ + ".dab") = 1 THEN
                IF dabExist(name$) = 1 THEN
                        OPEN name$ + ".DAB" FOR INPUT AS #4
                        CALL takeMetadata(4)
                        CLOSE #4
                        CALL displayMessage("Data Read from File on Disk!", 24, 21)
                        CALL displayMessage("  Press Anything ( Any Key ) to Continue on !   ", 16, 4)
                        CALL pause
                        CALL rect(3, 76, 3, 19, 7, 0)
                        CALL CRUD
                ELSE
                        LOCATE 10, 53
                        PRINT STRING$(8, " ")
                        name$ = ""
                        CALL displayError("File is Not DAB Spec Compliant!", 24, 21)
                        GOTO opnS
                END IF
        ELSE
                LOCATE 10, 50
                PRINT STRING$(8, " ")
                name$ = ""
                CALL displayError("File Does not Exist!", 26, 21)
                GOTO opnS
        END IF
END SUB

SUB pause
        DO
                
        LOOP UNTIL INKEY$ <> ""
END SUB

SUB prevPage (mCurrPg, mNPg, mOffset, mLEntry, byHowMuch, mXA, mXB)
    mCurrPg = mCurrPg - (1 * byHowMuch)
    mOffset = mOffset - (15 * byHowMuch)
    IF mNPg = mCurrPg THEN
             CALL printPage(mOffset, mLEntry, mXA, mXB)
    ELSE
             CALL printPage(mOffset, 15, mXA, mXB)
    END IF
END SUB

SUB printPage (mOffset, mNFPrint, xA, xB)
    FOR i = 1 TO mNFPrint
        LOCATE i + 5, xA
        a = i + mOffset
        PRINT a
        tData$ = fieldTitles(a)
        IF tData$ <> "" THEN
           LOCATE i + 5, xB
           PRINT tData$
        END IF
     NEXT i
END SUB

SUB printRecordContent (desiredArray$(), numOfFieldsAtPage, mCurrRec, mOffset, xA)
    FOR i = 1 TO numOfFieldsAtPage
        tData$ = desiredArray$(mCurrRec, i + mOffset)
        IF tData$ <> "" THEN
            LOCATE i + 5, xA
            PRINT tData$
        END IF
     NEXT i
END SUB

FUNCTION reads (st$)
IF records < 1 THEN
        CALL xline(26, 8, 34, 4)
        CALL xline(26, 15, 34, 4)
        CALL yline(25, 8, 8, 4)
        CALL yline(60, 8, 8, 4)
        CALL rect(26, 34, 9, 6, 15, 3)
        CALL displayMessage("Error!", 38, 10)
        CALL displayMessage("No Records Found", 33, 11)
        CALL displayMessage("<Press Any Key To Go Back>", 28, 13)
        CALL pause
        reads = 0
        EXIT FUNCTION
ELSE
        DIM tRecords(records, fields) AS STRING
        DIM tRecord(fields) AS STRING
       
        IF INSTR(st$, "b") > 0 THEN
            b = 1
        END IF
       
        IF INSTR(st$, "d") > 0 THEN
            d = 1
        END IF

        currRec = 1
newRPage:
        exitCode$ = ""

        rOffset = 0
        rLEntry = fields MOD 15
        rNPg = fields \ 15
        rCurrPg = 1

        IF rLEntry <> 0 THEN
           rNPg = rNPg + 1
        ELSE
           rLEntry = 15
        END IF

        FOR i = 1 TO records
        CALL takeRecord(tRecord(), i)
            FOR j = 1 TO fields
                tRecords(i, j) = tRecord(j)
            NEXT j
        NEXT i

        CALL readSetup(rCurrPg, rNPg, currRec, rOffset, rLEntry, tRecords())
     
        DO
                press$ = INKEY$

                IF press$ = CHR$(27) THEN
                        CALL exitFFMenu
                        CALL displayMessage("CRUD Menu", 38, 13)
                        opt = exitFF
                        IF opt = 11 THEN
                           CLS
                           END
                        ELSEIF opt = 12 THEN
                           CALL rect(25, 36, 8, 8, 7, 0)
                           CALL readSetup(rCurrPg, rNPg, currRec, rOffset, rLEntry, tRecords())
                        ELSEIF opt = 13 THEN
                           reads = 0
                           EXIT FUNCTION
                        END IF
                END IF
               
                IF b = 1 THEN
                    IF press$ = CHR$(2) THEN
                       reads = 0
                       EXIT FUNCTION
                    END IF
                END IF
               
                IF d = 1 THEN
                   IF press$ = CHR$(24) THEN
                      CALL contAMenu
                      opt = contA
                      IF opt = 11 THEN
                         reads = currRec
                         EXIT FUNCTION
                      ELSEIF opt = 12 THEN
                           CALL rect(19, 47, 8, 8, 7, 0)
                           CALL readSetup(rCurrPg, rNPg, currRec, rOffset, rLEntry, tRecords())
                      ELSEIF opt = 13 THEN
                           reads = 0
                           EXIT FUNCTION
                      END IF
                   END IF
                END IF

                IF press$ = CHR$(0) + CHR$(75) THEN
                        IF currRec > 1 THEN
                           currRec = currRec - 1
                           IF rOffset = 0 THEN
                                CALL rect(42, 20, 6, 15, 7, 0)
                           ELSE
                                CALL rect(11, 51, 6, 15, 7, 0)
                           END IF
                           GOTO newRPage
                        END IF
                END IF
             
                IF press$ = CHR$(0) + CHR$(77) THEN
                        IF currRec < records THEN
                           currRec = currRec + 1
                           IF rOffset = 0 THEN
                                CALL rect(42, 20, 6, 15, 7, 0)
                           ELSE
                                CALL rect(11, 51, 6, 15, 7, 0)
                           END IF
                           GOTO newRPage
                        END IF
                END IF

                IF press$ = CHR$(0) + CHR$(72) THEN
                        IF rCurrPg > 1 THEN
                           CALL rect(11, 51, 6, 15, 7, 0)
                           CALL prevPage(rCurrPg, rNPg, rOffset, rLEntry, 1, 11, 17)
                           IF rCurrPg = rNPg THEN
                              fPP = rLEntry
                           ELSE
                              fPP = 15
                           END IF
                           CALL printRecordContent(tRecords(), fPP, currRec, rOffset, 42)
                           LOCATE 21, 62
                           PRINT "Page "; rCurrPg; " of "; rNPg
                        END IF
                END IF

                IF press$ = CHR$(0) + CHR$(80) THEN
                        IF rCurrPg < rNPg THEN
                           CALL rect(11, 51, 6, 15, 7, 0)
                           CALL nextPage(rCurrPg, rNPg, rOffset, rLEntry, 1, 11, 17)
                           IF rCurrPg = rNPg THEN
                              fPP = rLEntry
                           ELSE
                              fPP = 15
                           END IF
                           CALL printRecordContent(tRecords(), fPP, currRec, rOffset, 42)
                           LOCATE 21, 62
                           PRINT "Page "; rCurrPg; " of "; rNPg
                        END IF
                END IF
        LOOP UNTIL 0
END IF
END FUNCTION

SUB readSetup (mRCurrPg, mRNPg, mCurrRec, mROffset, mRLEntry, mTRecords$())

        LOCATE 5, 12
        PRINT "N."

        LOCATE 5, 17
        PRINT "Field Title"

        LOCATE 5, 42
        PRINT "Content"

        LOCATE 21, 4
        PRINT "Rec. "; mCurrRec; " of "; records
        LOCATE 21, 62
        PRINT "Page "; mRCurrPg; " of "; mRNPg
       
        mRCurrPg = mRCurrPg - 1
        mROffset = mROffset - 15
       
        CALL nextPage(mRCurrPg, mRNPg, mROffset, mRLEntry, 1, 11, 17)
      
        IF mRCurrPg = mRNPg THEN
           mFPP = mRLEntry
        ELSE
           mFPP = 15
        END IF
      
        CALL printRecordContent(mTRecords$(), mFPP, mCurrRec, mROffset, 42)
END SUB

SUB rect (x, w, y, h, fg, bg)
        mw = w - 1
        mh = h - 1
        FOR mx = x TO x + mw
                FOR my = y TO y + mh
                        LOCATE my, mx
                        COLOR fg, bg
                        PRINT " "
                NEXT my
        NEXT mx
        COLOR 7, 0
END SUB

SUB remove (base$, st$)
        DO
        a = INSTR(base$, st$)
        IF a > 0 THEN
                l$ = LEFT$(base$, a - 1)
                r$ = RIGHT$(base$, LEN(base$) - LEN(l$) - 1)
                base$ = l$ + r$
        ELSE
                EXIT DO
        END IF
        LOOP UNTIL 0
END SUB

SUB split (base$, st$, dest$)
        a = INSTR(base$, st$)
        IF a > 0 THEN
          dest$ = LEFT$(base$, a - 1)
          base$ = RIGHT$(base$, LEN(base$) - LEN(dest$) - 1)
        ELSE
          dest$ = base$
        END IF
END SUB

FUNCTION takeInput$ (word$, leng, acc$, esc$)
IF INSTR(esc$, "x") > 0 THEN
    xpr = 1
END IF
IF INSTR(esc$, "b") > 0 THEN
    b = 1
END IF
IF INSTR(esc$, "d") > 0 THEN
    d = 1
END IF
inpt:
count = 0
LOCATE CSRLIN, POS(1), 1, 8, 8
DO
        keys$ = INKEY$
        IF keys$ <> "" THEN
                IF keys$ = CHR$(13) THEN
                       LOCATE CSRLIN, POS(1), 0
                       y = CSRLIN
                       x = POS(1)
                       PRINT
                       EXIT DO
                END IF
                IF xpr = 1 THEN
                    IF keys$ = CHR$(27) THEN
                       LOCATE CSRLIN, POS(1), 0
                       PRINT
                       takeInput$ = "x"
                       EXIT FUNCTION
                    END IF
                END IF
                IF b = 1 THEN
                    IF keys$ = CHR$(2) THEN
                       LOCATE CSRLIN, POS(1), 0
                       PRINT
                       takeInput$ = "b"
                       EXIT FUNCTION
                    END IF
                END IF
                IF d = 1 THEN
                   IF keys$ = CHR$(24) THEN
                      LOCATE CSRLIN, POS(1), 0
                      PRINT
                      takeInput$ = "d"
                      EXIT FUNCTION
                   END IF
                END IF
                IF INSTR(acc$, UCASE$(keys$)) > 0 AND count < leng THEN
                               word$ = word$ + keys$
                               count = count + 1
                               PRINT keys$;
                END IF
                IF keys$ = CHR$(8) AND LEN(word$) > 0 THEN
                        word$ = LEFT$(word$, LEN(word$) - 1)
                        count = count - 1
                        LOCATE CSRLIN, POS(1) - 1
                        PRINT " ";
                        LOCATE CSRLIN, POS(1) - 1
                END IF
        END IF
LOOP UNTIL 0
IF count < 1 THEN
        CALL displayError("This Field Must be Filled", 23, 21)
        LOCATE y, x
        GOTO inpt
END IF
END FUNCTION

SUB takeLineFromFile (dArray$(), num)
    LINE INPUT #num, mTemp$
    FOR i = 1 TO UBOUND(dArray$)
        CALL split(mTemp$, ",", dArray$(i))
        CALL remove(dArray$(i), CHR$(34))
    NEXT i
END SUB

SUB takeMetadata (num)
        INPUT #num, metadata(1), metadata(2), metadata(3)
        INPUT #num, fields
        REDIM fieldTitles(fields) AS STRING
        FOR i = 1 TO fields
             INPUT #num, fieldTitles(i)
        NEXT i
END SUB

SUB takeRecord (recordArray$(), recordNum)
        a = FREEFILE
        OPEN metadata(1) + ".dab" FOR INPUT AS #a
        CALL takeMetadata(a)
        DIM tArray(fields) AS STRING
        FOR i = 1 TO recordNum
            CALL takeLineFromFile(tArray(), a)
        NEXT i
        FOR i = 1 TO fields
            recordArray$(i) = tArray(i)
        NEXT i
        CLOSE #a
END SUB

SUB writeMetadata (num)
        WRITE #num, metadata(1), metadata(2), metadata(3)
        WRITE #num, fields
        FOR i = 1 TO fields
             WRITE #num, fieldTitles(i)
        NEXT i
END SUB

SUB writeRecords (dArray$(), mNRec, mFNum)
    DIM tRec(fields) AS STRING
    FOR i = 1 TO mNRec
        FOR j = 1 TO fields
            tRec(j) = dArray$(i, j)
        NEXT j
        tempHolder$ = arrayToWritable(tRec())
        WRITE #mFNum, tempHolder$
    NEXT i
END SUB

SUB xline (x, y, l, bg)
        l = l - 1
        FOR mx = x TO x + l
               LOCATE y, mx
               COLOR 0, bg
               PRINT " "
        NEXT mx
        COLOR 7, 0
END SUB

SUB yline (x, y, l, bg)
        l = l - 1
        FOR my = y TO y + l
               LOCATE my, x
               COLOR 0, bg
               PRINT " "
        NEXT my
        COLOR 7, 0
END SUB

