Sample*>>SOURCE FORMAT IS FIXED
      * Author:    Brian Tiffin
      * Date:      25-July-2008
      * Modified:  2015-07-29 07:41 EDT, Wednesday
      * License:   Copyright 2008,2015 Brian Tiffin
      *            Public domain sample.  Zero warranty.
      * Purpose:   Demonstrate GnuCOBOL byte stream files
      *            and SCREEN SECTION features
      * Tectonics: cobc -x streams.cob
       identification division.
       program-id. streams2.

       environment division.
       configuration section.
       special-names.
           crt status is user-control.

       data division.
       working-storage section.
       78 READ-ONLY            value 1.
       78 WRITE-ONLY           value 2.
       78 READ-WRITE           value 3.

       01 filehandle           usage is pointer.
       01 filename             pic x(40).
       01 cfile                pic x(41).
       01 access-mode          usage binary-long.
       01 file-lock            pic x.
       01 device               pic x.
       01 result               usage binary-long.

       01 file-offset          pic 9(18) comp.
       01 read-length          pic 9(8)  comp.
       01 file-flags           binary-char.
       01 read-buffer          pic x(40).
       01 marquee              pic x(40).
       01 Zarquee              pic x(40) VALUE "ABC".
       01 N.
         03 Narquee            OCCURS 16 pic x(40).
       01 marquee-limit        pic 9(4).

       01 scr-result           pic 9(5).
       01 scr-file-offset      pic 9(5).
       01 scr-read-length      pic 9(5).
       01 scr-file-length      pic 9(6).
       01 scr-pass             pic x(5) value "Pre  ".

       01 user-control         pic 9(4).
       01 exit-message         pic x(10) value "CRT STATUS".
       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.

           COPY    CPDECODE07 REPLACING ==:##:== BY ==WDE07==.

       screen section.
       01 file-screen.
          05 blank screen.
          05 line 1 column 25 value "GnuCOBOL byte stream files"
              foreground-color 2.
          05 line 3 column 10 value "Enter filename and marquee count."
              foreground-color 3.
          05 line 3 column 44 value "Any function key to exit"
              foreground-color 4.
          05 line 4 column 10 value "File:".
          05 line 4 column 19 using filename pic x(40).
          05 line 5 column 10 value "limit:".
          05 line 5 column 19 using marquee-limit pic 9(4).
          05 line 5 column 30 value "<- limits marquee loop".
          05 line 7 column 19 from marquee reverse-video pic x(40).
          05 line 9 column 10 from scr-pass pic x(5).
          05 line 9 column 15 value "Result:".
          05 line 9 column 22 from scr-result pic 9(5).
          05 line 9 column 29 value "Length:".
          05 line 9 column 36 from scr-read-length pic 9(5).
          05 line 9 column 43 value "Offset:".
          05 line 9 column 50 from scr-file-offset pic 9(5).
          05 line 9 column 57 value "Total:".
          05 line 9 column 63 from scr-file-length pic 9(6).
          05 line 10 column 29 from exit-message pic x(10).
      *    05 line 10 column 40 from user-control pic 9(4).

          05 line 10 column 19 from Zarquee  pic x(40).
      *    05 line 10 column 19 from Narquee(01) reverse-video pic x(40).
          05 line 11 column 19 from Narquee(02) reverse-video pic x(40).
          05 line 12 column 19 from Narquee(03) reverse-video pic x(40).
          05 line 13 column 19 from Narquee(04) reverse-video pic x(40).
          05 line 14 column 19 from Narquee(05) reverse-video pic x(40).
          05 line 15 column 19 from Narquee(06) reverse-video pic x(40).
          05 line 16 column 19 from Narquee(07) reverse-video pic x(40).
          05 line 17 column 19 from Narquee(08) reverse-video pic x(40).
          05 line 18 column 19 from Narquee(09) reverse-video pic x(40).
          05 line 19 column 19 from Narquee(10) reverse-video pic x(40).
          05 line 20 column 19 from Narquee(11) reverse-video pic x(40).
          05 line 21 column 19 from Narquee(12) reverse-video pic x(40).
          05 line 22 column 19 from Narquee(13) reverse-video pic x(40).
          05 line 23 column 19 from Narquee(14) reverse-video pic x(40).
          05 line 24 column 19 from Narquee(15) reverse-video pic x(40).
          05 line 25 column 19 from Narquee(16) reverse-video pic x(40).

      ******************************************************************
       procedure division.

      * read screen with defaults
      * move "streams.cbl" to filename.
       move "TEST10ishihara.pot1" to filename.
       move 64 to marquee-limit.
       accept file-screen end-accept.

      * tapping a function key will bail
      * user-control にはキー内容が入る　ＰＦ１＝１００１
       if user-control not = 0
           move "Bailing..." to exit-message
           display file-screen
           call "C$SLEEP" using "2"
      *     goback
       end-if.

      * open the file, name needs terminating null byte
       move READ-ONLY to access-mode.
       string filename delimited by space
              low-value delimited by size
              into cfile
       end-string.
       call "CBL_OPEN_FILE" using cfile
                                  access-mode
                                  file-lock
                                  device
                                  filehandle
                            returning result
       end-call.
       move result to scr-result

       display file-screen.
       accept file-screen end-accept.

      * This section demonstrates the file-flags option
      * If 128 is in file-flags, CBL_READ_FILE will place
      * the actual file length into the file-offset field on
      * completion of the read.
       move result to scr-result
       move 0 to file-offset scr-file-offset.
       move 40 to read-length scr-read-length.
       move 128 to file-flags.

       call "CBL_READ_FILE" using filehandle
                                  file-offset
                                  read-length
                                  file-flags
                                  read-buffer
                            returning result.

       move "Post " to scr-pass
       move result to scr-result
       move file-offset to scr-file-offset
                           scr-file-length
       move read-length to scr-read-length

       display file-screen.
       accept file-screen end-accept.

      * display a sliding marquee, one character every
      * 170 million, one billionth's of a second; about 5.9cps
       move 0 to file-flags.
       move 40 to read-length.
           MOVE    "COBOL 2020" TO     WDE07-ASCII
           MOVE    "CHANGE"    TO      WDE07-ID
           CALL    "DECODE07"  USING   WDE07-DECODE07-AREA

      * perform varying file-offset from 0 by 1
       perform varying file-offset from 0 by 2
           until (result not = 0)
              or (file-offset > marquee-limit)
               call "CBL_READ_FILE" using filehandle
                                          file-offset
                                          read-length
                                          file-flags
                                          read-buffer
                                    returning result
               end-call

               move read-buffer to marquee
      *         COMPUTE I2 = file-offset + 1
               MOVE 1 TO I2
               PERFORM VARYING I FROM 1 BY 1
                       UNTIL I > 16
                       MOVE WDE07-LINE (I) (I2:40)
                            TO Narquee (I) 
               END-PERFORM
               inspect marquee replacing all x"0d0a" by "  "
               inspect marquee replacing all x"0a" by space

               move file-offset to scr-file-offset
               move result to scr-result
               display file-screen
      * accept file-screen end-accept

      *         call "CBL_OC_NANOSLEEP" using 170000000 end-call
               call "CBL_OC_NANOSLEEP" using 1000000000 end-call
       end-perform

       call "CBL_CLOSE_FILE" using filehandle
                             returning result.

       move "Leaving..." to exit-message.
       display file-screen.
       call "C$SLEEP" using "10" end-call.
       accept file-screen end-accept.

       goback.
       exit program.
