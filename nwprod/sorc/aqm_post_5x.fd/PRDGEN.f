      PROGRAM PRDGEN
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: ETA_PRDGEN
C   PRGMMR: MANIKIN          ORG: NP22        DATE: 2002-07-02
C
C ABSTRACT: PRDGEN PRODUCES FILES THAT HAVE BEEN INTERPOLATED (USING
C   IPLIB) TO VARIOUS OUTPUT GRIDS WITH OPTIONAL WMO HEADERS.  PRDGEN
C   READS THROUGH A MASTER INPUT GRIB FILE, DETERMINES WHAT GRIDS TO
C   INTERPOLATE TO, PERFORMS PRE- AND POST-INTERPOLATION SMOOTHING,
C   PACKS THE DATA INTO GRIB, ADDS A WMO HEADER, THEN WRITES THE
C   PACKED DATA TO AN OUTPUT FILE, OR SEVERAL TILES.  THE CONTROL FILE
C   SPECIFIES THE OUTPUT GRID NUMBERS, WMO HEADER TYPES, OUTPUT FILE
C   NAMES, PACKING PRECISION, NUMBER OF PRE- AND POST-INTERPOLATION
C   SMOOTHING PASSES FOR EACH GRIB FIELD THAT IS DESIRED FOR POSTING,
C   AND THE NUMBER OF SUBSETS OR TILES TO POST THE GRID ON.  THE
C   MASTER INPUT GRID SHOULD BE LARGE ENOUGH TO ENCOMPASS ALL OF THE
C   REQUESTED OUTPUT GRIDS, AND ALSO SHOULD CONTAIN ALL OF THE
C   GRIB PARAMETERS REQUIRED, SINCE PRDGEN DOES NOT PROVIDE
C   ANY DIAGNOSTIC CALCULATIONS.
C
C PROGRAM HISTORY LOG:
C   97-12-01  BALDWIN,    ORIGINATOR
C             BRILL
C   98-08-24  BALDWIN - USE TYPE 201/203 DIRECTLY, 
C                        NO FILL OF STAGGERED E-GRID
C   99-03-25  BALDWIN - ADD TILING OPTION
C   00-04-29  MANIKIN - CONVERTED TILING OPTION CODE TO SP
C   03-03-18  MANIKIN - ADDED IN NEAREST NEIGHBOR OPTION AS WELL AS
C                         ABILITY TO DISCERN FIELDS IN DIFFERENT
C                         PARAMETER TABLES
C
C USAGE:  MAIN PROGRAM
C
C   INPUT FILES:
C           UNIT 5  - NAME OF MASTER GRIB INPUT FILE
C           UNIT 10 - CONTROL FILE (SEE CTL_RDR FOR FORMAT)
C           UNIT 11 - MASTER GRIB INPUT FILE (OPENED WITHIN CODE
C             NO NEED TO ASSIGN)
C           UNITS 21,22,23... - INTERPOLATION WEIGHTS FILES
C           UNIT 41 - KWBX TABLE FOR WMO HEADER INFO 
C           UNIT 42 - TIME TABLE FOR WMO HEADER INFO 
C           UNIT 43 - PARM TABLE FOR WMO HEADER INFO 
C           UNIT 44 - GRID TABLE FOR WMO HEADER INFO 
C           UNIT 45 - LEVL TABLE FOR WMO HEADER INFO 
C
C   ENVIRONMENT VARIABLES: (OPTIONAL)
C           COMSP   - PATH PREFIX OF OUTPUT FILE NAMES
C           fhr     - FORECAST HOUR APPENDED TO FILE NAME
C           tmmark  - TIME MARK (tm00) APPENDED TO FILE NAME
C
C   OUTPUT FILES:
C
C           UNIT   6 - STANDARD OUTPUT
C           UNITS 51,52,53... - OUTPUT FILES OPENED WITHIN PRDGEN
C           UNITS 61,...      - OUTPUT FILES OPENED WITHIN TILE_OUT
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:
C       READ_SORT_CTL - CONTROL FILE READER
C       READ_SORT_GRIB - READ IN ENTIRE INPUT GRIB FILE
C       FILTER_UV - FILTER WIND FIELD 
C       FILTER_SC - FILTER SCALAR FIELD
C       INTERP_UV - INTERPOLATE WIND FIELD
C       INTERP_SC - INTERPOLATE SCALAR FIELD
C       INTERP_PPT - INTERPOLATE PRECIP FIELD USING BUDGET METHOD
C       EXTEND - FILL IN MISSING REGIONS BY EXTENDING BOUNDARIES
C       GRIB_IN - UNPACK GRIB REPORT
C       GRIB_OUT - PACK AND WRITE OUT GRIB REPORT
C     LIBRARY:
C       W3LIB: W3FI63, W3FI72
C       IPLIB: MAKGDS, POLFIXS, POLFIXV
C       SPLIB: (FOR INTERPOLATION)
C       GEMLIB: (FOR WMO HEADER AND CONTROL READER)
C
C   EXIT STATES:
C     COND =   0 - NORMAL EXIT
C          =   10  - Error reading control file
C          =   20  - Error reading GRIB file
C          =   40  - INPUT GRIB FILE TOO LARGE (INCREASE IBUFSIZE)
C          =   99  - ERROR READING INPUT GRIB FILE
C          =   66  - INTERPOLATION WEIGHT READING ERROR
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C   MACHINE : IBM SP
C
C$$$
C
C   AWIPS PRODUCT GENERATOR
C
C
C   IMAXIN, JJMAXIN ARE MAX DIMENSIONS OF INPUT GRID
C   IMAXOT, JJMAXOT ARE MAX DIMENSIONS OF OUTPUT GRID
C   MAXF IS THE MAXIMUM NUMBER OF GRIB PARAMETERS IN CONTROL FILE
C   MAXG IS THE MAXIMUM NUMBER OF OUTPUT FILES
C   MAXO IS THE MAXIMUM NUMBER OF INTERPOLATION GRIDS  (WHICH CAN BE 
C        LESS THAN MAXG SINCE THE SAME GRID IS USED IN SEVERAL OUTPUT
C        FILES)
C
C    THE MEMORY REQUIREMENTS OF THE CODE DEPEND HEAVILY ON 
C    IMAXOT, JJMAXOT SINCE INTERPOLATION WEIGHTS ARE STORED
C    IN MEMORY AND ARE DIMENSIONED (IMAXOT*JJMAXOT)
C      SO, IF YOU ARE NOT MAKING OUTPUT FILES ON BIG GRIDS, SET THESE
C      TO LOWER VALUES AND THE EXECUTABLE WILL BE SMALLER
C    
C    
      PARAMETER(IMAXIN=675,JJMAXIN=1170,JMAXIN=IMAXIN*JJMAXIN)
      PARAMETER(IMAXOT=760,JJMAXOT=450,JMAXOT=IMAXOT*JJMAXOT)
      PARAMETER(MAXTILE=54,MAXF=2000,MAXG=10,IBUFSIZE=1800000000)

      INTEGER KGRIDA(MAXG,MAXF),KGRID(MAXG)
      INTEGER JPDS5(MAXF),JPDS6(MAXF),
     &        JPDS7(MAXF),JPDS16(MAXF),JPDS19(MAXF)
      INTEGER K5PDS(7,MAXF),NEARN(MAXF)
      INTEGER NGRDS(MAXF),NFILS(MAXF),MDLID(MAXG,MAXF)
      INTEGER KPDSIN(25),KGDSIN(22),KPDSU(25),KPDSV(25)
      INTEGER DUMGDS(200)
      INTEGER RCBYTE(MAXF),ISTART(MAXF),NBITL(MAXF)
      INTEGER KPDSOUT(25),KGDSOUT(22),IOUTUN(MAXG,MAXF)
      INTEGER N11(JMAXOT),N21(JMAXOT),KSMPO(MAXG,MAXF),
     &              N12(JMAXOT),N22(JMAXOT),KSMPR(MAXG,MAXF),
     &              NPP(JMAXOT,25)
      INTEGER NV11(JMAXOT),NV21(JMAXOT),NV12(JMAXOT),NV22(JMAXOT)
      INTEGER ITILS(MAXG,MAXF),JTILS(MAXG,MAXF), IOUTIL(MAXTILE)


      LOGICAL*1 LIN(JMAXIN),LOUT(JMAXOT),DOTILE(MAXTILE)
      LOGICAL*1 IHAVE(MAXF),INEED,IFIRST

      REAL(4) rtemp(JMAXOT)
      REAL RLAT(JMAXOT),RLON(JMAXOT)
      REAL RLAI(JMAXIN),RLOI(JMAXIN) 
      REAL CROI(JMAXIN),SROI(JMAXIN)
      REAL XPTI(JMAXIN),YPTI(JMAXIN)
      REAL FIN(JMAXIN),FOUT(JMAXOT),SCAL(MAXG,MAXF)
      REAL VIN(JMAXIN),VOUT(JMAXOT),SFCHGT(JMAXIN)
      REAL FIELD(JMAXOT)
      REAL UIN(JMAXIN),UOUT(JMAXOT)
      REAL CROT(JMAXOT),SROT(JMAXOT)
      REAL W11(JMAXOT),W21(JMAXOT),
     &     W12(JMAXOT),W22(JMAXOT)
      REAL C11(JMAXOT),C21(JMAXOT),
     &     C12(JMAXOT),C22(JMAXOT)
      REAL S11(JMAXOT),S21(JMAXOT),
     &     S12(JMAXOT),S22(JMAXOT)
      REAL WV11(JMAXOT),WV21(JMAXOT),
     &     WV12(JMAXOT),WV22(JMAXOT)
      REAL WW11(JMAXOT),WW21(JMAXOT),
     &     WW12(JMAXOT),WW22(JMAXOT)
      REAL CC11(JMAXOT),CC21(JMAXOT),
     &     CC12(JMAXOT),CC22(JMAXOT)
      REAL SS11(JMAXOT),SS21(JMAXOT),
     &     SS12(JMAXOT),SS22(JMAXOT)
      REAL CM11,CM12,CM21,CM22
      REAL SM11,SM12,SM21,SM22
      PARAMETER(FILL=-9999.)

      CHARACTER NAMES(MAXG,MAXF)*16
      CHARACTER TYPE(MAXG,MAXF)*4
      CHARACTER IBUF(IBUFSIZE)*1
      CHARACTER NFILE*80
C---- The following variables for Air Quality (PLee, April 2004)^^^^^
      INTEGER   n_durhr, envint, status
      CHARACTER fnamepre(MAXG)*80
      logical   HR_CENTERED,GFS_O3,notcdf,RESEARCH,INHOUSE,EXT_MIE
      character Dn2*3
      LOGICAL, EXTERNAL :: ENVYN
      data KPDSOUT/7,210,146,128,180,107,10000,4,6, 1,0,
     &             0, 1,  0,  0,  0,  0,     0,0,0,21,9,0,0,0/
     
      n_durhr = ENVINT ('n_durhr', ' ', n_durhr, status)
      HR_CENTERED = ENVYN('HR_CENTERED',' ',HR_CENTERED,status)
      GFS_O3 = ENVYN('GFS_O3',' ',GFS_O3,status)

      notcdf = ENVYN('notcdf',' ',
     &                      notcdf,status)
      RESEARCH = ENVYN('RESEARCH',' ',
     &             RESEARCH,status)
      INHOUSE = ENVYN('INHOUSE',' ',
     &             INHOUSE,status)
      EXT_MIE = ENVYN('EXT_MIE',' ',
     &             EXT_MIE,status) !! Air Quality (PLee, 8/2004)VVVV
C
      print *, 'into prdgen '
      CALL W3TAGB('ETA_PRDGEN',2002,0183,0060,'NP22')

!DIR$ INLINEALWAYS FILTER_SC,FILTER_UV
       LCNTRL=10
       LUGBIN=11
       LUNWGT=20

      mloop=1                      !!Air Quality (PLee, 4/2004) ^^^^
      if(HR_CENTERED.or.GFS_O3) mloop=2
      do 1000 iloop=1, mloop       !!                           VVVV
C
C     READ INPUT GRIB FILE NAME
C
      READ(5,80) NFILE
 80   FORMAT(A80)
C
C     READ AND SORT OUT CNTRL FILE.
C    
      if(iloop.eq.1) then
      CALL READ_SORT_CTL (LCNTRL,MAXG,MAXF,K5PDS,MDLID,KGRIDA,
     &  TYPE,SCAL,NAMES,KGRID,IG1,KSMPR,KSMPO,ITILS,JTILS, 
     &  NEARN,NFILS,NGRDS,NFLDS,JPDS5,JPDS6,JPDS7,JPDS16,!!PLee
     &  JPDS19,IOUTUN,notcdf,GFS_O3,fnamepre,IRET0)      !!get prefix

      IF (IRET0.NE.0) THEN
        WRITE(6,*) ' READ_SORT_CTL RETURN CODE = ',IRET0
        CALL W3TAGE('ETA_PRDGEN')
        STOP 10
      ENDIF
      endif
C
C     READ AND SORT OUT ENTIRE GRIB FILE.
C

       if(.not.notcdf) then !!PLee, notcdf equivalent getinfo later
          CALL READ_SORT_GRIB (NFILE,MAXF,IBUFSIZE,LUGBIN,NFLDS,
     &    JPDS5,JPDS6,JPDS7,JPDS16,JPDS19,IBUF,IHAVE,NBITL,RCBYTE,
     &    ISTART,IRET)

          IF (IRET.NE.0) THEN
             WRITE(6,*) ' READ_SORT_GRI RETURN CODE = ',IRET
             CALL W3TAGE('ETA_PRDGEN')
             STOP 20
          ENDIF
        else
          NFLDS_INT=NFLDS
          print*,"n_durhr=",n_durhr
          NFLDS = n_durhr   !  PLEE April 15 2004   REALLY 48
          ihave(1:48)=.true.    !!Currently, CMAQ runs 48 fcst h
        endif
C
C  BIG OUTER LOOP OVER NUMBER OF OUTPUT GRIDS
C
       DO 100 M=1,IG1
       if(iloop.eq.2) goto 2000
C
C  READ INTERPOLATION WEIGHTS
C
         LUNWGT=20+M
c        rewind LUNWGT

         if(.not.notcdf) then    !!PLee, no wgt file for notcdf

            READ(LUNWGT)KGRIDOT,NOUT
C
C  MAKE SURE THIS IS THE RIGHT SET OF WEIGHTS FOR THIS GRID
C
            print *,'KGRIDOT,KGRID(M) = ',KGRIDOT,KGRID(M)
            IF (KGRIDOT.NE.KGRID(M)) THEN
               CALL W3TAGE('ETA_PRDGEN')
               STOP 66
            ENDIF

             READ(LUNWGT) (KGDSOUT(I),I=1,22)
             READ(LUNWGT) (N11(I),I=1,NOUT)
             READ(LUNWGT) (N12(I),I=1,NOUT)
             READ(LUNWGT) (N21(I),I=1,NOUT)
             READ(LUNWGT) (N22(I),I=1,NOUT)
             READ(LUNWGT) (NV11(I),I=1,NOUT)
             READ(LUNWGT) (NV12(I),I=1,NOUT)
             READ(LUNWGT) (NV21(I),I=1,NOUT)
             READ(LUNWGT) (NV22(I),I=1,NOUT)
c            READ(LUNWGT) (C11(I),I=1,NOUT)
             READ(LUNWGT) (rtemp(I),I=1,NOUT)
             C11(1:NOUT)=rtemp(1:NOUT)
c            READ(LUNWGT) (C12(I),I=1,NOUT)
             READ(LUNWGT) (rtemp(I),I=1,NOUT)
             C12(1:NOUT)=rtemp(1:NOUT)
c            READ(LUNWGT) (C21(I),I=1,NOUT)
             READ(LUNWGT) (rtemp(I),I=1,NOUT)
             C21(1:NOUT)=rtemp(1:NOUT)
c            READ(LUNWGT) (C22(I),I=1,NOUT)
             READ(LUNWGT) (rtemp(I),I=1,NOUT)
             C22(1:NOUT)=rtemp(1:NOUT)
c            READ(LUNWGT) (S11(I),I=1,NOUT)
             READ(LUNWGT) (rtemp(I),I=1,NOUT)
             S11(1:NOUT)=rtemp(1:NOUT)
c            READ(LUNWGT) (S12(I),I=1,NOUT)
             READ(LUNWGT) (rtemp(I),I=1,NOUT)
             S12(1:NOUT)=rtemp(1:NOUT)
c            READ(LUNWGT) (S21(I),I=1,NOUT)
             READ(LUNWGT) (rtemp(I),I=1,NOUT)
             S21(1:NOUT)=rtemp(1:NOUT)
c            READ(LUNWGT) (S22(I),I=1,NOUT)
             READ(LUNWGT) (rtemp(I),I=1,NOUT)
             S22(1:NOUT)=rtemp(1:NOUT)
c            READ(LUNWGT) (W11(I),I=1,NOUT)
             READ(LUNWGT) (rtemp(I),I=1,NOUT)
             W11(1:NOUT)=rtemp(1:NOUT)
c            READ(LUNWGT) (W12(I),I=1,NOUT)
             READ(LUNWGT) (rtemp(I),I=1,NOUT)
             W12(1:NOUT)=rtemp(1:NOUT)
c            READ(LUNWGT) (W21(I),I=1,NOUT)
             READ(LUNWGT) (rtemp(I),I=1,NOUT)
             W21(1:NOUT)=rtemp(1:NOUT)
c            READ(LUNWGT) (W22(I),I=1,NOUT)
             READ(LUNWGT) (rtemp(I),I=1,NOUT)
             W22(1:NOUT)=rtemp(1:NOUT)
c            READ(LUNWGT) (WV11(I),I=1,NOUT)
             READ(LUNWGT) (rtemp(I),I=1,NOUT)
             WV11(1:NOUT)=rtemp(1:NOUT)
c            READ(LUNWGT) (WV12(I),I=1,NOUT)
             READ(LUNWGT) (rtemp(I),I=1,NOUT)
             WV12(1:NOUT)=rtemp(1:NOUT)
c            READ(LUNWGT) (WV21(I),I=1,NOUT)
             READ(LUNWGT) (rtemp(I),I=1,NOUT)
             WV21(1:NOUT)=rtemp(1:NOUT)
c            READ(LUNWGT) (WV22(I),I=1,NOUT)
             READ(LUNWGT) (rtemp(I),I=1,NOUT)
             WV22(1:NOUT)=rtemp(1:NOUT)
c            READ(LUNWGT) (RLAT(I),I=1,NOUT)
             READ(LUNWGT) (rtemp(I),I=1,NOUT)
             RLAT(1:NOUT)=rtemp(1:NOUT)
c            READ(LUNWGT) (RLON(I),I=1,NOUT)
             READ(LUNWGT) (rtemp(I),I=1,NOUT)
             RLON(1:NOUT)=rtemp(1:NOUT)
c            READ(LUNWGT) (SROT(I),I=1,NOUT)
             READ(LUNWGT) (rtemp(I),I=1,NOUT)
             SROT(1:NOUT)=rtemp(1:NOUT)
c            READ(LUNWGT) (CROT(I),I=1,NOUT)
             READ(LUNWGT) (rtemp(I),I=1,NOUT)
             CROT(1:NOUT)=rtemp(1:NOUT)
             READ(LUNWGT) ((NPP(I,J),I=1,NOUT),J=1,25)

          ENDIF

         IFIRST=.TRUE.

cmg         print *, 'weight checks'
cmg         print *, W12(888), W21(888), W11(888), W22(888)
cmg         print *, C12(888), C21(888), C11(888), C22(888)

C
C   BEGIN LOOP OVER NUMBER OF FIELDS
C

       print *, 'NUMBER OF FIELDS REQUESTED IS ', NFLDS

 2000  continue
        DO 30 N=1,NFLDS
        if(.not.notcdf) then   !!PLee, otherwise invoke notcdf reader
C
C    IF THIS FIELD IS V, WE'VE ALREADY TAKEN CARE OF IT
C
        IF (IHAVE(N).AND.JPDS5(N).NE.34.AND.JPDS5(N).NE.197) THEN
        if(HR_CENTERED.and.iloop.eq.1.and.jpds5(n).eq.246) goto 30
        if(HR_CENTERED.and.iloop.eq.2.and.jpds5(n).ne.246) goto 30
        if(GFS_O3.and.(jpds5(n).ne.154.and.jpds5(n).ne.1)) goto 30

        NFILS2=NFILS(N)

        INEED=.TRUE.

C       INEED=.T. MEANS WE STILL NEED TO UNPACK THIS FIELD

        DO MM=1,NFILS2
         IF (KGRID(M).EQ.KGRIDA(MM,N)) THEN
          IF (INEED) THEN
C<<>>PLee: Diagnostics
C       write(6,*)" N=",N,"KGRID_A=",KGRID(M),KGRIDA(MM,N),
C     &  "JPDS5=",JPDS5(N)

           IF (JPDS5(N).EQ.33.OR.JPDS5(N).EQ.196) THEN
C
C     UNPACK GRIB RECORD
C
            MBYTES = RCBYTE(N)
            KSTART = ISTART(N)
            CALL GRIB_IN (JMAXIN,IBUFSIZE,IBUF,KPDSU,KGDSIN,
     &      UIN,LIN,MBYTES,KSTART,IRET)
            IF(IRET.NE.0) THEN
               GO TO 30
            END IF
C
C     NOW FIND V THAT GOES WITH THIS U
C
            DO JK = 1,NFLDS
             IF(JPDS5(JK).EQ.JPDS5(N)+1 .AND.
     &          JPDS6(JK).EQ.JPDS6(N) .AND.
     &          JPDS7(JK).EQ.JPDS7(N) .AND.
     &          JPDS16(JK).EQ.JPDS16(N) .AND.
     &          NEARN(JK).EQ.NEARN(N) .AND.
     &          JPDS19(JK).EQ.JPDS19(N)) NN=JK
            ENDDO
            MBYTES = RCBYTE(NN)
            KSTART = ISTART(NN)
            CALL GRIB_IN (JMAXIN,IBUFSIZE,IBUF,KPDSV,KGDSIN,
     &      VIN,LIN,MBYTES,KSTART,IRET)
            IF(IRET.NE.0) THEN
               GO TO 30
            END IF
           ELSE
            MBYTES = RCBYTE(N)
            KSTART = ISTART(N)
            CALL GRIB_IN (JMAXIN,IBUFSIZE,IBUF,KPDSIN,KGDSIN,
     &      FIN,LIN,MBYTES,KSTART,IRET)
C<<>>PLee: Diagnostics
        if(JPDS5(N).eq.154) then
          write(6,*)"N=",N,FIN(1),FIN(2),(KPDSIN(1:25))
         endif
            IF(IRET.NE.0) THEN
               GO TO 30

            END IF
           ENDIF

       INEED=.FALSE.
       IBIN=(KPDSIN(4)-128)/64
       NOUT=KGDSOUT(2)*KGDSOUT(3)

          ENDIF
 
CGSM  new loop for nearest neighbor option
          IF (NEARN(N) .EQ. 2) THEN
cmg         print *, 'inside NN loop'
cGSM   at each point, find the largest weight of the 4 surrounding pts
c        first do the vector option
            IF (JPDS5(N).EQ.33.OR.JPDS5(N).EQ.196) THEN
             DO K = 1, NOUT
               WLARGE=AMAX1(WV11(K),WV12(K),WV21(K),WV22(K))
               IF(WLARGE.eq.0.) THEN
                  CC11(K)=0.0
                  SS11(K)=0.0
                  CC21(K)=0.0
                  SS21(K)=0.0
                  CC12(K)=0.0
                  SS12(K)=0.0
                  CC22(K)=0.0
                  SS22(K)=0.0
               ELSE
                  IF(WLARGE.eq.WV11(K)) THEN
                    WV11(K)=1.0
                    WV21(K)=0.0
                    WV12(K)=0.0
                    WV22(K)=0.0
                    CC11(K)=C11(K)
                    SS11(K)=S11(K)
                    CC21(K)=0.0
                    SS21(K)=0.0
                    CC12(K)=0.0
                    SS12(K)=0.0
                    CC22(K)=0.0
                    SS22(K)=0.0
                  ENDIF
                IF(WLARGE.eq.WV21(K)) THEN
                    WV11(K)=0.0
                    WV21(K)=1.0
                    WV12(K)=0.0
                    WV22(K)=0.0
                    CC21(K)=C21(K)
                    SS21(K)=S21(K)
                    CC11(K)=0.0
                    SS11(K)=0.0
                    CC12(K)=0.0
                    SS12(K)=0.0
                    CC22(K)=0.0
                    SS22(K)=0.0
                  ENDIF
                  IF(WLARGE.eq.WV12(K)) THEN
                    WV11(K)=0.0
                    WV21(K)=0.0
                    WV12(K)=1.0
                    WV22(K)=0.0
                    CC12(K)=C12(K)
                    SS12(K)=S12(K)
                    CC11(K)=0.0
                    SS11(K)=0.0
                    CC21(K)=0.0
                    SS21(K)=0.0
                    CC22(K)=0.0
                    SS22(K)=0.0
                  ENDIF
                  IF(WLARGE.eq.WV22(K)) THEN
                    WV11(K)=0.0
                    WV21(K)=0.0
                    WV12(K)=0.0
                    WV22(K)=1.0
                    CC22(K)=C22(K)
                    SS22(K)=S22(K)
                    CC11(K)=0.0
                    SS11(K)=0.0
                    CC21(K)=0.0
                    SS21(K)=0.0
                    CC12(K)=0.0
                    SS12(K)=0.0
                  ENDIF
               ENDIF
            ENDDO
           ELSE
c     else perform the scalar option
            DO K = 1, NOUT
             WLARGE=AMAX1(W11(K),W12(K),W21(K),W22(K))
             IF(WLARGE.eq.W11(K)) THEN
               WW11(K)=1.0
               WW21(K)=0.0
               WW12(K)=0.0
               WW22(K)=0.0
             ENDIF
             IF(WLARGE.eq.W21(K)) THEN
               WW11(K)=0.0
               WW21(K)=1.0
               WW12(K)=0.0
               WW22(K)=0.0
             ENDIF
             IF(WLARGE.eq.W12(K)) THEN
               WW11(K)=0.0
               WW21(K)=0.0
               WW12(K)=1.0
               WW22(K)=0.0
             ENDIF
             IF(WLARGE.eq.W22(K)) THEN
               WW11(K)=0.0
               WW21(K)=0.0
               WW12(K)=0.0
               WW22(K)=1.0
             ENDIF
           ENDDO
           ENDIF
          ELSE
              DO K = 1, NOUT
                WW11(K)=W11(K)
                WW21(K)=W21(K)
                WW12(K)=W12(K)
                WW22(K)=W22(K)
                CC22(K)=C22(K)
                SS22(K)=S22(K)
                CC11(K)=C11(K)
                SS11(K)=S11(K)
                CC21(K)=C21(K)
                SS21(K)=S21(K)
                CC12(K)=C12(K)
                SS12(K)=S12(K)
              ENDDO
cmg      print *, 'end of NN loop'

CGSM  end of loop for nearest neighbor option
        ENDIF
cmg      print *, 'end check ', CC12(888), CC21(888), CC11(888),
cmg  &         CC22(888)

C
           IF (JPDS5(N).EQ.33.OR.JPDS5(N).EQ.196) THEN
C
C   PRE-INTERPOLATION FILTER OF E-GRID FIELDS
C
           CALL FILTER_UV (IMAXIN,JJMAXIN,JMAXIN,KSMPR(MM,N),KGDSIN,
     &                      UIN,LIN,IRET)
     
           CALL FILTER_UV (IMAXIN,JJMAXIN,JMAXIN,KSMPR(MM,NN),KGDSIN,
     &                      VIN,LIN,IRET)

C  INTERPOLATE WITH OR WITHOUT BITMAPS

           CALL INTERP_UV (JMAXIN,UIN,VIN,LIN,IBIN,
     &                     JMAXOT,UOUT,VOUT,LOUT,IBOUT,NOUT,KGDSOUT,
     &                     NV11,NV12,NV21,NV22,WV11,WV12,WV21,WV22,
     &                     CC11,CC12,CC21,CC22,SS11,SS12,SS21,SS22,
     &                     CROT,SROT,IRET)


           ELSE

C   PRE-INTERPOLATION FILTER OF SCALARS ON E-GRID

            CALL FILTER_SC (IMAXIN,JJMAXIN,JMAXIN,KSMPR(MM,N),KGDSIN,
     &                      JPDS5(N),FIN,LIN,IRET)
C

C  PRECIP FIELDS USE BUDGET INTERP 
c    UNLESS NEAREST NEIGHBOR OPTION IS SELECTED 
C
        IF (KPDSIN(5).LE.66.AND.KPDSIN(5).GE.61.AND.
     &           K5PDS(7,N).NE.2) THEN

           CALL INTERP_PPT (JMAXIN,FIN,LIN,IBIN,
     &                      JMAXOT,FOUT,LOUT,IBOUT,NOUT,KGDSOUT,
     &                      NPP,IRET)

        ELSE
           IF (N .EQ. 10) THEN
             DO P=1,JMAXIN
                SFCHGT(P)=FIN(P)
             ENDDO
           ENDIF
           IF (N .EQ. 504) THEN
             DO P=1,JMAXIN
                 IF (SFCHGT(P).GT.FIN(P) .AND. FIN(P).GT.0.) THEN
cmg                 print *, 'check ',P, SFCHGT(P), FIN(P)
                 ENDIF
             ENDDO
           ENDIF

           CALL INTERP_SC (JMAXIN,FIN,LIN,IBIN,
     &                     JMAXOT,FOUT,LOUT,IBOUT,NOUT,KGDSOUT,
     &                     N11,N12,N21,N22,WW11,WW12,WW21,WW22,IRET)
        ENDIF

          ENDIF
C
C   DONE INTERPOLATING, NOW FILTER OUTPUT
C
           IF (JPDS5(N).EQ.33.OR.JPDS5(N).EQ.196) THEN

        CALL FILTER_UV (IMAXOT,JJMAXOT,JMAXOT,KSMPO(MM,N),KGDSOUT,
     &                      UOUT,LOUT,IRET)

        CALL FILTER_UV (IMAXOT,JJMAXOT,JMAXOT,KSMPO(MM,NN),KGDSOUT,
     &                      VOUT,LOUT,IRET)

C
C   COMPUTE MAX AND MIN FOR FINDING NUMBER OF BITS
C
         UMAX=-9.E40
         UMIN=9.E40
         VMAX=-9.E40
         VMIN=9.E40
         DO K=1,NOUT
             IF (LOUT(K).AND.UOUT(K).GT.UMAX) UMAX=UOUT(K)
             IF (LOUT(K).AND.UOUT(K).LT.UMIN) UMIN=UOUT(K)
             IF (LOUT(K).AND.VOUT(K).GT.VMAX) VMAX=VOUT(K)
             IF (LOUT(K).AND.VOUT(K).LT.VMIN) VMIN=VOUT(K)
         ENDDO
C
C  FILL IN MISSING REGIONS WITH BOUNDARY VALUES TO AVOID USING BITMAPS
C  ONLY FOR THOSE GRIDS WHOSE BMS IS CHANGING WITH THE NEW 32KM DOMAIN
C
       IF (IBOUT.EQ.1.AND.(KGRIDA(MM,N).EQ.6.OR.
     &      KGRIDA(MM,N).EQ.207)) THEN

           CALL EXTEND (IMAXOT,JJMAXOT,JMAXOT,KGDSOUT,
     &                      UOUT,LOUT,UMIN,IRET)

           CALL EXTEND (IMAXOT,JJMAXOT,JMAXOT,KGDSOUT,
     &                      VOUT,LOUT,VMIN,IRET)

           IBOUT=0
           LOUT=.TRUE.

       ENDIF

C
C  PACK INTO GRIB WRITE STUFF OUT
C
         CALL GRIB_OUT (JMAXOT,UMIN,UMAX,SCAL(MM,N),KPDSU,KGDSOUT,
     &      TYPE(MM,N),IOUTUN(MM,N),MDLID(MM,N),KGRID(M),
     &      IBOUT,NOUT,LOUT,UOUT,IRET)

         CALL GRIB_OUT (JMAXOT,VMIN,VMAX,SCAL(MM,NN),KPDSV,KGDSOUT,
     &      TYPE(MM,NN),IOUTUN(MM,NN),MDLID(MM,NN),KGRID(M),
     &      IBOUT,NOUT,LOUT,VOUT,IRET)

         IF (ITILS(MM,N).GT.0.AND.JTILS(MM,N).GT.0) THEN
         CALL TILE_OUT (MAXTILE,JMAXOT,ITILS(MM,N),JTILS(MM,N),
     &      IFIRST,SCAL(MM,N),KPDSU,KGDSOUT,IOUTIL,DOTILE,RLAT,RLON,
     &      TYPE(MM,N),MDLID(MM,N),
     &      NOUT,LOUT,UOUT,NAMES(MM,N),IRET)

         CALL TILE_OUT (MAXTILE,JMAXOT,ITILS(MM,NN),JTILS(MM,NN),
     &      IFIRST,SCAL(MM,NN),KPDSV,KGDSOUT,IOUTIL,DOTILE,RLAT,RLON,
     &      TYPE(MM,NN),MDLID(MM,NN),
     &      NOUT,LOUT,VOUT,NAMES(MM,NN),IRET)

         ENDIF

            ELSE

        CALL FILTER_SC (IMAXOT,JJMAXOT,JMAXOT,KSMPO(MM,N),KGDSOUT,
     &                      JPDS5(N),FOUT,LOUT,IRET)
C
      if(GFS_O3) then   !! Plee, Air Quality (April, 2004) ^^^^

         call gfs_derive(iloop,KPDSIN,K5PDS,JPDS5,FOUT,MAXG,
     &           fnamepre,MAXF,JMAXOT,NOUT,LOUT,MDLID,KGRIDA,
     &           N,NFLDS)
        endif           !!       Air Quality (April, 2004) VVVV

C
C   COMPUTE MAX AND MIN FOR FINDING NUMBER OF BITS
C
         FMAX=-9.E40
         FMIN=9.E40
         DO K=1,NOUT
             IF (LOUT(K).AND.FOUT(K).GT.FMAX) FMAX=FOUT(K)
             IF (LOUT(K).AND.FOUT(K).LT.FMIN) FMIN=FOUT(K)
         ENDDO


C  FILL IN MISSING REGIONS WITH BOUNDARY VALUES TO AVOID USING BITMAPS
C  ONLY FOR THOSE GRIDS WHOSE BMS IS CHANGING WITH THE NEW 32KM DOMAIN
C
       IF (IBOUT.EQ.1.AND.(KGRIDA(MM,N).EQ.6.OR.
     &      KGRIDA(MM,N).EQ.207)) THEN

           CALL EXTEND (IMAXOT,JJMAXOT,JMAXOT,KGDSOUT,
     &                      FOUT,LOUT,FMIN,IRET)

           IBOUT=0
           LOUT=.TRUE.

       ENDIF
C
C  PACK INTO GRIB WRITE STUFF OUT
C

         CALL GRIB_OUT (JMAXOT,FMIN,FMAX,SCAL(MM,N),KPDSIN,KGDSOUT,
     &      TYPE(MM,N),IOUTUN(MM,N),MDLID(MM,N),KGRID(M),
     &      IBOUT,NOUT,LOUT,FOUT,IRET)
         
         IF (ITILS(MM,N).GT.0.AND.JTILS(MM,N).GT.0) THEN
         CALL TILE_OUT (MAXTILE,JMAXOT,ITILS(MM,N),JTILS(MM,N),
     &      IFIRST,SCAL(MM,N),KPDSIN,KGDSOUT,IOUTIL,DOTILE,RLAT,RLON,
     &      TYPE(MM,N),MDLID(MM,N),
     &      NOUT,LOUT,FOUT,NAMES(MM,N),IRET)
         ENDIF

C
C  END OF LOOP OVER NUMBER OF FIELDS
C
        ENDIF
        ENDIF
       ENDDO
       ENDIF
       else  !! PLee, (April 2004) netcdf independent     ^^^^^

        if(RESEARCH) then
           call cmaq_inhouse(N,M,NFLDS_INT,KPDSIN,KPDSOUT,K5PDS,
     &        MDLID,KGRIDA,MAXG,MAXF,INHOUSE,EXT_MIE,fnamepre)
        else
           call cmaq_output(N,M,NFLDS_INT,KPDSIN,KPDSOUT,K5PDS,
     &           MDLID,KGRIDA,MAXG,MAXF,fnamepre)
        endif

       endif !! PLee, Air Quality                         VVVVV
 30    CONTINUE

C
C   END LOOP OVER NUMBER OF GRIDS
C

 100  CONTINUE
 1000 continue

 9999 CONTINUE

      CALL W3TAGE('ETA_PRDGEN')
      STOP
      END
