      SUBROUTINE INTERP_UV (JMAXIN,UIN,VIN,LIN,IBIN,
     &                      JMAXOT,UOUT,VOUT,LOUT,IBOUT,NOUT,KGDSOUT,
     &                      N11,N12,N21,N22,W11,W12,W21,W22,
     &                      C11,C12,C21,C22,S11,S12,S21,S22,
     &                      CROT,SROT,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C   SUBPROGRAM: INTERP_UV
C   PRGMMR: BALDWIN          ORG: NP22        DATE: 98-08-11  
C
C ABSTRACT: INTERP_UV INTERPOLATES A WIND FIELD USING GIVEN 
C           INTERPOLATION WEIGHTS AND ROTATION SINES/COSINES.
C
C PROGRAM HISTORY LOG:
C   98-08-11  BALDWIN     ORIGINATOR
C
C USAGE:  CALL INTERP_UV (JMAXIN,UIN,VIN,LIN,IBIN,
C    &                      JMAXOT,UOUT,VOUT,LOUT,IBOUT,NOUT,KGDSOUT,
C    &                      N11,N12,N21,N22,W11,W12,W21,W22,
C    &                      C11,C12,C21,C22,S11,S12,S21,S22,
C    &                      CROT,SROT,IRET)
C
C   INPUT:
C         JMAXIN            INTEGER - MAX DIMENSION OF UIN,VIN
C         JMAXOT            INTEGER - MAX DIMENSION OF UOUT,VOUT
C         NOUT              INTEGER - NUMBER OF PTS ON OUTPUT GRID
C         KGDSOUT(22)       INTEGER - KGDS OF OUTPUT GRID
C         UIN(JMAXIN)       REAL    - U-COMPONENT ON INPUT GRID
C         VIN(JMAXIN)       REAL    - V-COMPONENT ON INPUT GRID
C         LIN(JMAXIN)       LOGICAL*1 - BITMAP CORRESPONDING TO UIN,VIN
C         IBIN              INTEGER - BITMAP FLAG FOR INPUT GRIDS
C         N11,N12,N21,      INTEGER - INDEX OF SURROUNDING INPUT GRID 
C          N22(JMAXOT)                POINTS TO OUTPUT GRID
C         W11,W12,W21,      REAL    - INTERPOLATION WEIGHTS FOR INPUT
C          W22(JMAXOT)                GRID POINTS SURROUNDING OUTPUT GRID
C         C11,C12,C21,      REAL    - ROTATION COSINES FOR INPUT
C          C22(JMAXOT)                GRID POINTS SURROUNDING OUTPUT GRID
C         S11,S12,S21,      REAL    - ROTATION SINES FOR INPUT
C          S22(JMAXOT)                GRID POINTS SURROUNDING OUTPUT GRID
C                                     (ROTATE FROM INPUT GRID TO EARTH REL)
C         CROT(JMAXOT)      REAL    - ROTATION COSINES FOR OUTPUT GRID
C                                     (EARTH RELATIVE TO OUTPUT GRID)
C         SROT(JMAXOT)      REAL    - ROTATION SINES FOR OUTPUT GRID
C                                     (EARTH RELATIVE TO OUTPUT GRID)
C
C   OUTPUT:
C         UOUT(JMAXOT)      REAL    - U-COMPONENT OF OUTPUT GRID
C         VOUT(JMAXOT)      REAL    - V-COMPONENT OF OUTPUT GRID
C         LOUT(JMAXOT)      LOGICAL*1 - OUTPUT GRID BITMAP
C         IBOUT             INTEGER - BITMAP FLAG FOR OUTPUT GRID
C         IRET              INTEGER - RETURN CODE
C
C   RETURN CODES:
C     IRET =   0 - NORMAL EXIT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : CRAY J-916
C
C    
      INTEGER KGDSOUT(22),IBS(NOUT)
      INTEGER N11(JMAXOT),N21(JMAXOT),N12(JMAXOT),N22(JMAXOT)
      LOGICAL*1 LIN(JMAXIN),LOUT(JMAXOT)
      REAL WO(JMAXOT)
      REAL VIN(JMAXIN),VOUT(JMAXOT)
      REAL UIN(JMAXIN),UOUT(JMAXOT)
      REAL CROT(JMAXOT),SROT(JMAXOT)
      REAL C11(JMAXOT),C21(JMAXOT),
     &     C12(JMAXOT),C22(JMAXOT)
      REAL S11(JMAXOT),S21(JMAXOT),
     &     S12(JMAXOT),S22(JMAXOT)
      REAL W11(JMAXOT),W21(JMAXOT),
     &     W12(JMAXOT),W22(JMAXOT)

      IRET=0

C
C    LINEAR INTERP AND ROTATE TO OUTPUT GRID
C
C    INTERPOLATE WITH OR WITHOUT BITMAPS
C
c         !$OMP PARALLEL DO PRIVATE(U11,V11,U21,V21,U12,V12,U22,V22)
        DO I=1,NOUT
          UOUT(I)=0.
          VOUT(I)=0.
          WO(I)=0
          IF(N11(I).GT.0) THEN
            IF(IBIN.EQ.0) THEN
              U11=C11(I)*UIN(N11(I))-S11(I)*VIN(N11(I))
              V11=S11(I)*UIN(N11(I))+C11(I)*VIN(N11(I))
              U21=C21(I)*UIN(N21(I))-S21(I)*VIN(N21(I))
              V21=S21(I)*UIN(N21(I))+C21(I)*VIN(N21(I))
              U12=C12(I)*UIN(N12(I))-S12(I)*VIN(N12(I))
              V12=S12(I)*UIN(N12(I))+C12(I)*VIN(N12(I))
              U22=C22(I)*UIN(N22(I))-S22(I)*VIN(N22(I))
              V22=S22(I)*UIN(N22(I))+C22(I)*VIN(N22(I))
              UOUT(I)=W11(I)*U11+W21(I)*U21+
     &                W12(I)*U12+W22(I)*U22
              VOUT(I)=W11(I)*V11+W21(I)*V21+
     &                W12(I)*V12+W22(I)*V22
              WO(I)=1
            ELSE
              IF(LIN(N11(I))) THEN
                U11=C11(I)*UIN(N11(I))-S11(I)*VIN(N11(I))
                V11=S11(I)*UIN(N11(I))+C11(I)*VIN(N11(I))
                UOUT(I)=UOUT(I)+W11(I)*U11
                VOUT(I)=VOUT(I)+W11(I)*V11
                WO(I)=WO(I)+W11(I)
              ENDIF
              IF(LIN(N21(I))) THEN
                U21=C21(I)*UIN(N21(I))-S21(I)*VIN(N21(I))
                V21=S21(I)*UIN(N21(I))+C21(I)*VIN(N21(I))
                UOUT(I)=UOUT(I)+W21(I)*U21
                VOUT(I)=VOUT(I)+W21(I)*V21
                WO(I)=WO(I)+W21(I)
              ENDIF
              IF(LIN(N12(I))) THEN
                U12=C12(I)*UIN(N12(I))-S12(I)*VIN(N12(I))
                V12=S12(I)*UIN(N12(I))+C12(I)*VIN(N12(I))
                UOUT(I)=UOUT(I)+W12(I)*U12
                VOUT(I)=VOUT(I)+W12(I)*V12
                WO(I)=WO(I)+W12(I)
              ENDIF
              IF(LIN(N22(I))) THEN
                U22=C22(I)*UIN(N22(I))-S22(I)*VIN(N22(I))
                V22=S22(I)*UIN(N22(I))+C22(I)*VIN(N22(I))
                UOUT(I)=UOUT(I)+W22(I)*U22
                VOUT(I)=VOUT(I)+W22(I)*V22
                WO(I)=WO(I)+W22(I)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
        IBOUT=IBIN
        IBS(1:NOUT)=0
c          !$OMP PARALLEL DO PRIVATE(UROT,VROT)
        DO I=1,NOUT
          LOUT(I)=WO(I).GE.0.5
          IF(LOUT(I)) THEN
            UOUT(I)=UOUT(I)/WO(I)
            VOUT(I)=VOUT(I)/WO(I)
            UROT=CROT(I)*UOUT(I)-SROT(I)*VOUT(I)
            VROT=SROT(I)*UOUT(I)+CROT(I)*VOUT(I)
            UOUT(I)=UROT
            VOUT(I)=VROT
          ELSE
            IBS(I)=1
            UOUT(I)=0.
            VOUT(I)=0.
          ENDIF
        ENDDO
      IBOUT=MAXVAL(IBS)
c     IF(KGDSOUT(1).EQ.0) CALL POLFIXV(NOUT,NOUT,1,
c    &     RLAT,RLON,IBOUT,LOUT,UOUT,VOUT)
      RETURN
      END
