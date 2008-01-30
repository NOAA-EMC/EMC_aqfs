      SUBROUTINE READ_SORT_GRIB (NFILE,MAXF,IBUFSIZE,LUGBIN,NFLDS,
     &  JPDS5,JPDS6,JPDS7,JPDS16,JPDS19,IBUF,IHAVE,NBITL,RCBYTE,
     &  ISTART,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C   SUBPROGRAM: READ_SORT_GRIB
C   PRGMMR: BALDWIN          ORG: NP22        DATE: 98-08-11  
C
C ABSTRACT: READ_SORT_GRIB PROCESSES THE INPUT GRIB FILE.
C
C PROGRAM HISTORY LOG:
C 1998-08-11  BALDWIN       ORIGINATOR
C 2002-10-11  GILBERT       FOR FROST: With 64-bit addressing the
C                           file size returned from the STAT function
C                           is now in element 11 of the array, instead of 8.
C 2007-06-26  VANDENBURGHE  Change IBUF from character to integer array; use
C                           vector syntax fro IBUF read (changes reduce runtime) 
C
C USAGE:  CALL READ_SORT_GRIB (NFILE,MAXF,IBUFSIZE,LUGBIN,NFLDS,
C    &  JPDS5,JPDS6,JPDS7,JPDS16,IBUF,IHAVE,NBITL,RCBYTE,ISTART,IRET)
C
C   INPUT:
C         NFILE             CHAR*90 - NAME OF INPUT GRIB FILE
C         MAXF              INTEGER - MAXIMUM NUMBER OF FIELDS
C         IBUFSIZE          INTEGER - MAXIMUM SIZE OF INPUT GRIB FILE
C         LUGBIN            INTEGER - UNIT NUMBER OF INPUT GRIB FILE
C                                     (WILL BE ASSIGNED WITHIN CODE)
C         NFLDS             INTEGER - NUMBER OF FIELDS READ FROM CTL FILE
C         JPDS5(MAXF)       INTEGER - PDS OCTET 9 OF EACH FIELD
C         JPDS6(MAXF)       INTEGER - PDS OCTET 10 OF EACH FIELD
C         JPDS7(MAXF)       INTEGER - PDS OCTET 11-12 OF EACH FIELD
C         JPDS16(MAXF)      INTEGER - PDS OCTET 21 OF EACH FIELD
C
C   OUTPUT:
C         IBUF(IBUFSIZE)    CHAR*1  - GRIB MESSAGES
C         IHAVE(MAXF)       LOGICAL - FLAG INDICATING WHETHER FIELD   
C                                     REQUESTED FROM CONTROL FILE WAS
C                                     FOUND IN GRIB FILE
C         NBITL(MAXF)       INTEGER - NUMBER OF PACKING BITS FOR EACH FIELD
C         RCBYTE(MAXF)      INTEGER - NUMBER OF BYTES IN GRIB RECORD FOR
C                                     EACH FIELD
C         ISTART(MAXF)      INTEGER - STARTING BYTE IN GRIB RECORD FOR
C                                     EACH FIELD
C         IRET              INTEGER - RETURN CODE
C
C   RETURN CODES:
C     IRET =   0 - NORMAL EXIT
C             20 - FILE OPEN ERROR
C             40 - GRIB FILE TOO LARGE, INCREASE IBUFSIZE
C             99 - ERROR GETTING FILE STAT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : CRAY J-916
C
C$$$

      INTEGER JPDS5(MAXF),JPDS6(MAXF),
     &        JPDS7(MAXF),JPDS16(MAXF),JPDS19(MAXF) 
      INTEGER RCBYTE(MAXF),ISTART(MAXF)
      INTEGER NBITL(MAXF)
      INTEGER(4) STAT,JSTAT(13)

      LOGICAL*1 RUSE,IHAVE(MAXF)

c     CHARACTER IBUF(IBUFSIZE)*1
      INTEGER IBUF(IBUFSIZE)*1

      CHARACTER NFILE*90

      IRET=0

C    
C     READ AND SORT OUT ENTIRE GRIB FILE.
C
C        CALL FUNCTION STAT TO FIND NUMBER OF BYTES IN FILE
C        WORD 8 OF ARRAY JSTAT HAS THE NUMBER OF BYTES IN FILE
C
         NFILE = TRIM(NFILE)//CHAR(0)
         IF (STAT(NFILE,JSTAT).NE.0) THEN
           PRINT *,'ERROR IN FUNCTION STAT GETTING FILE STATS'
           IRET=99
           RETURN
         ELSE
           KBYTES = JSTAT(11)
           PRINT *,'NUMBER OF BYTES IN GRIB FILE   = ',KBYTES
         END IF
C
C        TEST TO SEE IF INPUT GRIB FILE IS TO BIG
C
         IF (IBUFSIZE.LT.KBYTES) THEN
           PRINT *,'GRIB INPUT FILE IS TO BIG '
           PRINT *,'CHANGE PROGRAM SO PARAMETER IBUFSIZE IS LARGER'
           PRINT *,'THAN THE NUMBER OF BYTES IN THE FILE'
           IRET = 40
           RETURN
         END IF
C
         OPEN (UNIT=LUGBIN,FILE=NFILE,STATUS='OLD',ACCESS='DIRECT',
     &   FORM='UNFORMATTED',IOSTAT=MERR,RECL=KBYTES)

         IF (MERR.NE.0) THEN
           PRINT *,'OPEN INPUT FILE ERROR ON FILE = ', NFILE
           PRINT *,'ERROR = ',MERR
           IRET=20
           RETURN
         END IF

c        READ  (LUGBIN, REC=1, IOSTAT=JERR2) (IBUF(I),I=1,KBYTES)
         READ  (LUGBIN, REC=1, IOSTAT=JERR2) IBUF(1:kbytes)

         CLOSE (LUGBIN) 

C      
C  FIND ALL 'GRIB' IN FILE, SAVE ADDRESS,
C  LENGTH OF GRIB MESSAGE IN LOCATION THAT
C  MATCHES THE PDS OF THE CONTROL FILE RECORD.
C
      JSTART = 1
      I=JSTART

      DO WHILE (JSTART.LT.KBYTES)

        iflag=0
        DO WHILE (MOVA2I(IBUF(I  )).NE.71.or.MOVA2I(IBUF(I+1)).NE.82
     &      .or.MOVA2I(IBUF(I+2)).NE.73.or.MOVA2I(IBUF(I+3)).NE.66)
          I=I+1
          if (i.ge.kbytes) then
            iflag=1
            exit
          endif
        ENDDO

        if (iflag.eq.1) exit

        IF (MOVA2I(IBUF(I+7)).EQ.1) THEN

           I1=I+7
           ILPDS=MOVA2I(IBUF(I1+1))*65536
     &          +MOVA2I(IBUF(I1+2))*256
     &          +MOVA2I(IBUF(I1+3))
           ILGDS=0
           ILBDS=0
           ILBMS=0
           IPDS5=MOVA2I(IBUF(I1+9))
           IPDS6=MOVA2I(IBUF(I1+10))
           IPDS7=MOVA2I(IBUF(I1+11))*256
     &          +MOVA2I(IBUF(I1+12))
           IPDS16=MOVA2I(IBUF(I1+21))
           IPDS19=MOVA2I(IBUF(I1+4))

           IFL=MOVA2I(IBUF(I1+8))
           IGDS=IFL/128
           IBMS=MOD(IFL,128)/64

           I1=I1+ILPDS

           IF (IGDS.EQ.1) THEN
           ILGDS=MOVA2I(IBUF(I1+1))*65536
     &          +MOVA2I(IBUF(I1+2))*256
     &          +MOVA2I(IBUF(I1+3))
           I1=I1+ILGDS
           ENDIF
 
           IF (IBMS.EQ.1) THEN
           ILBMS=MOVA2I(IBUF(I1+1))*65536
     &          +MOVA2I(IBUF(I1+2))*256
     &          +MOVA2I(IBUF(I1+3))
           I1=I1+ILBMS
           ENDIF

           ILBDS=MOVA2I(IBUF(I1+1))*65536
     &          +MOVA2I(IBUF(I1+2))*256
     &          +MOVA2I(IBUF(I1+3))
           IBITL=MOVA2I(IBUF(I1+11))
           IRCBYTE = MOVA2I(IBUF(I+4))*65536 + MOVA2I(IBUF(I+5))*256
     &      + MOVA2I(IBUF(I+6)) 
           JSTART     = I + IRCBYTE
           RUSE=.FALSE.

         DO N=1,NFLDS
          IF (IPDS5.EQ.JPDS5(N).AND.IPDS6.EQ.JPDS6(N).AND.
     &        IPDS7.EQ.JPDS7(N).AND.IPDS16.EQ.JPDS16(N).AND.
     &        IPDS19.EQ.JPDS19(N)) THEN
           RCBYTE(N) = IRCBYTE
           ISTART(N) = I
           NBITL (N) = IBITL
           IHAVE(N)  = .TRUE.
           RUSE=.TRUE.
          ENDIF
         ENDDO

         I = JSTART
         IF (.NOT.RUSE) WRITE(6,3331) IPDS5,IPDS6,IPDS7,IPDS16,IPDS19
 3331    FORMAT(' PDS ',5I8,' READ IN BUT NOT USED BY CONTROL FILE')

        ENDIF

       ENDDO
C
         DO N=1,NFLDS
          IF (.NOT.IHAVE(N)) WRITE(6,3332) JPDS5(N),JPDS6(N),
     &            JPDS7(N),JPDS16(N),JPDS19(N)
 3332     FORMAT(' PDS ',5I8,' REQUESTED BY CONTROL FILE BUT',
     &           ' NOT FOUND IN GRIB FILE')
         ENDDO

      RETURN
      END
