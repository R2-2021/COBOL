
      *GCobol >>SOURCE FORMAT IS FIXED 
      *5.3.1 UPDATE TO CBL_OC_DUMP
      *-----------------------------------------------------------------
      * Authors:   Brian Tiffin, Asger Kjelstrup, Simon Sobisch
      * Date:      19-Oct-2010
      * Purpose:   Hex Dump display
      * Tectonics: cobc -c CBL_OC_DUMP.cob
      *     Usage: export OC_DUMP_EXT=1 for explanatory text on dumps
      *            (memory address and dump length)
      *            export OC_DUMP_EXT=Y for extended explanatory text
      *            (architecture   and endian-order)
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBL_OC_DUMP.
      *
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  addr                             usage pointer.
       77  addr2addr                        usage pointer.
       77  counter               pic 999999 usage comp-5.
       77  byline                pic 999    usage comp-5.
       77  offset                pic 999999.
       01  some                  pic 999    usage comp-5.
           88 some-is-printable-iso88591
              values 32 thru 126, 160 thru 255.
           88 some-is-printable-ebcdic
              values 64, 65, 74 thru 80, 90 thru 97,
                     106 thru 111, 121 thru 127, 129 thru 137, 143,
                     145 thru 153, 159, 161 thru 169, 176,
                     186 thru 188, 192 thru 201, 208 thru 217, 224,
                     226 thru 233, 240 thru 249.
       77  high-var              pic 99     usage comp-5.
       77  low-var               pic 99     usage comp-5.
      *
       01  char-set              pic x(06).
           88 is-ascii           value 'ASCII'.
           88 is-ebdic           value 'EBCDIC'.
           88 is-unknown         value '?'.
       01  architecture          pic x(06).
           88 is-32-bit          value '32-bit'.
           88 is-64-bit          value '64-bit'.
       01  endian-order          pic x(10).
           88 is-big-endian-no   value 'Little-Big'.
           88 is-big-endian-yes  value 'Big-Little'.
      *
       77  hex-line              pic x(48).
       77  hex-line-pointer      pic 9(02) value 1.
      *
       77  show                  pic x(16).
       77  dots                  pic x value '.'.
       77  dump-dots             pic x.
      *
       77  hex-digit             pic x(16)  value '0123456789abcdef'.
       01  extended-infos        pic x.
           88 show-extended-infos      values '1', '2', 'Y', 'y'.
           88 show-very-extended-infos values '2', 'Y', 'y'.
      *
       77  len                   pic 999999 usage comp-5.
       77  len-display           pic 999999.
      *
       LINKAGE SECTION.
       01  buffer                pic x       any length.
       77  byte                  pic x.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION USING buffer.
      *
      *MAIN SECTION.
      *00.
           perform starting-address
      *
           perform varying counter from 0 by 16
                   until   counter  >=   len
              move counter to offset
              move spaces  to hex-line, show
              move '-'     to hex-line (24:01)
              move 1       to hex-line-pointer
              perform varying byline from 1 by 1
                      until   byline  >  16
                 if (counter + byline) > len
                    if byline < 9
                       move space to hex-line (24:01)
                    end-if
                    inspect show (byline:) replacing all spaces by dots
                    exit perform
                 else
                    move buffer (counter + byline : 1) to byte
                    perform calc-hex-value
                    if ((some-is-printable-iso88591 and is-ascii) or
                        (some-is-printable-ebcdic   and is-ebdic)   )
                       move byte to show (byline:1)
                    else
                       move dots to show (byline:1)
                    end-if
                 end-if
              end-perform
              display offset '  ' hex-line '  ' show
                      upon SYSERR
              end-display
           end-perform
           display ' '
                   upon SYSERR
           end-display
      *
           exit program.
      *-----------------------------------------------------------------
       CALC-HEX-VALUE SECTION.
      *00.
           subtract 1 from function ord(byte) giving some
           end-subtract
           divide   some by 16 giving high-var remainder low-var
           end-divide
           string hex-digit (high-var + 1:1)
                  hex-digit (low-var  + 1:1)
                  space
                  delimited by size
                  into hex-line
                  with pointer hex-line-pointer
           end-string
      *
           exit section.
      *-----------------------------------------------------------------
       STARTING-ADDRESS SECTION.
      *00.
      * Get the length of the transmitted buffer
           CALL 'C$PARAMSIZE' USING 1
              GIVING len
           END-CALL
      * If wanted, change the dots to something different than points
           accept dump-dots from environment 'OC_DUMP_DOTS'
             not on exception
                 move dump-dots to dots
           end-accept
      *
           perform TEST-ASCII
           perform TEST-ENDIAN
           set addr      to address of buffer
           set addr2addr to address of addr
      *
           if len > 0
      * To show hex-address, reverse if Big-Little Endian
              if is-big-endian-yes
                 set addr2addr up   by LENGTH OF addr
                 set addr2addr down by 1
              end-if
              move 1 to hex-line-pointer
              perform varying byline from 1 by 1
                      until byline > LENGTH OF addr
                 set address of byte to addr2addr
                 perform calc-hex-value
                 if is-big-endian-yes
                    set addr2addr down by 1
                 else
                    set addr2addr up   by 1
                 end-if
              end-perform
           end-if
      *
      * Get and display characteristics and headline
           accept extended-infos from environment 'OC_DUMP_EXT'
           end-accept
           if show-extended-infos
              display ' '
                      upon SYSERR
              end-display
              if len > 0
                 display 'Dump of memory beginning at Hex-address: '
                          hex-line (1 : 3 * (byline - 1) )
                          upon SYSERR
                 end-display
              end-if
              move len to len-display
              display 'Length of memory dump is: ' len-display
                       upon SYSERR
              end-display
              if show-very-extended-infos
                 perform TEST-64bit
                 display 'Program runs in '
                         architecture ' architecture. '
                         'Char-set is '
                         function trim (char-set) '.'
                         upon SYSERR
                 end-display
                 display 'Byte order is ' endian-order
                         ' endian.'
                         upon SYSERR
                 end-display
              end-if
           end-if
      *
      * Do we have anything to dump?
           if len > 0
      * Ensure that the passed size is not too big
              if len > 999998
                 move 999998 to len, len-display
                 display 'Warning, only the first '
                         len-display  ' Bytes are shown!'
                         upon SYSERR
                 end-display
              end-if
              display ' '
                      upon SYSERR
              end-display
              display 'Offset  '
                      'HEX-- -- -- -5 -- -- -- -- 10 '
                      '-- -- -- -- 15 -- '
                      '  '
                      'CHARS----1----5-'
                      upon SYSERR
              end-display
           else
              display ' '
                      upon SYSERR
              end-display
              display 'Nothing to dump.'
                      upon SYSERR
              end-display
           end-if
      *
           exit section.
      *-----------------------------------------------------------------
       TEST-ASCII SECTION.
      *Function: Discover if running Ascii or Ebcdic
      *00.
           evaluate space
              when x'20'
                 set  is-ascii   to true
              when x'40'
                 set  is-ebdic   to true
              when other
                 set  is-unknown to true
           end-evaluate
      *
           exit section.
      *-----------------------------------------------------------------
       TEST-64BIT SECTION.
      *Function: Discover if running 32/64 bit
      *00.
      *    Longer pointers in 64-bit architecture
           if function length (addr) <= 4
              set  is-32-bit to true
           else
              set  is-64-bit to true
           end-if
      *
           exit section.
      *-----------------------------------------------------------------
       TEST-ENDIAN SECTION.
      *00.
      *    Number-bytes are shuffled in Big-Little endian
           move 128 to byline
           set  address of byte to address of byline
           if function ord(byte) > 0
              set  is-big-endian-yes to true
           else
              set  is-big-endian-no  to true
           end-if
      *
           exit section.
      *----------------------------------------------------------------*
       end program CBL_OC_DUMP.
