      SUBROUTINE INTERP_SC (JMAXIN,FIN,LIN,IBIN,
     &                      JMAXOT,FOUT,LOUT,IBOUT,NOUT,KGDSOUT,
     &                      N11,N12,N21,N22,W11,W12,W21,W22,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C   SUBPROGRAM: INTERP_SC
C   PRGMMR: BALDWIN          ORG: NP22        DATE: 98-08-11  
C
C ABSTRACT: INTERP_SC INTERPOLATES A SCALAR FIELD USING GIVEN 
C           INTERPOLATION WEIGHTS.
C
C PROGRAM HISTORY LOG:
C   98-08-11  BALDWIN     ORIGINATOR
C
C USAGE:  CALL INTERP_SC (JMAXIN,FIN,LIN,IBIN,
C    &                      JMAXOT,FOUT,LOUT,IBOUT,NOUT,KGDSOUT,
C    &                      N11,N12,N21,N22,W11,W12,W21,W22,IRET)
C
C   INPUT:
C         JMAXIN            INTEGER - MAX DIMENSION OF FIN
C         JMAXOT            INTEGER - MAX DIMENSION OF FOUT
C         NOUT              INTEGER - NUMBER OF PTS ON OUTPUT GRID
C         KGDSOUT(22)       INTEGER - KGDS OF OUTPUT GRID
C         FIN(JMAXIN)       REAL    - INPUT FIELD
C         LIN(JMAXIN)       LOGICAL*1 - BITMAP CORRESPONDING TO FIN
C         IBIN              INTEGER - BITMAP FLAG FOR INPUT GRIDS
C         N11,N12,N21,      INTEGER - INDEX OF SURROUNDING INPUT GRID 
C          N22(JMAXOT)                POINTS TO OUTPUT GRID
C         W11,W12,W21,      REAL    - INTERPOLATION WEIGHTS FOR INPUT
C          W22(JMAXOT)                GRID POINTS SURROUNDING OUTPUT GRID
C
C   OUTPUT:
C         FOUT(JMAXOT)      REAL    - OUTPUT GRID
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
      INTEGER KGDSOUT(22)
      INTEGER N11(JMAXOT),N21(JMAXOT),N12(JMAXOT),N22(JMAXOT)
      LOGICAL*1 LIN(JMAXIN),LOUT(JMAXOT)
      REAL WO(JMAXOT)
      REAL FIN(JMAXIN),FOUT(JMAXOT)
      REAL W11(JMAXOT),W21(JMAXOT),
     &     W12(JMAXOT),W22(JMAXOT)

      IRET=0

C$OMP PARALLEL DO PRIVATE(I)
        DO I=1,NOUT
          FOUT(I)=0.
          WO(I)=0.
          IF(N11(I).GT.0) THEN
            IF(IBIN.EQ.0) THEN
              FOUT(I)=W11(I)*FIN(N11(I))+W21(I)*FIN(N21(I))
     &               +W12(I)*FIN(N12(I))+W22(I)*FIN(N22(I))
              WO(I)=1.
            ELSE
              IF(LIN(N11(I))) THEN
                FOUT(I)=FOUT(I)+W11(I)*FIN(N11(I))
                WO(I)=WO(I)+W11(I)
              ENDIF
              IF(LIN(N21(I))) THEN
                FOUT(I)=FOUT(I)+W21(I)*FIN(N21(I))
                WO(I)=WO(I)+W21(I)
              ENDIF
              IF(LIN(N12(I))) THEN
                FOUT(I)=FOUT(I)+W12(I)*FIN(N12(I))
                WO(I)=WO(I)+W12(I)
              ENDIF
              IF(LIN(N22(I))) THEN
                FOUT(I)=FOUT(I)+W22(I)*FIN(N22(I))
                WO(I)=WO(I)+W22(I)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
        IBOUT=IBIN
        DO I=1,NOUT
          LOUT(I)=WO(I).GE.0.5
          IF(LOUT(I)) THEN
            FOUT(I)=FOUT(I)/WO(I)
          ELSE
            IBOUT=1
            FOUT(I)=0.
          ENDIF
        ENDDO
C
c     IF(KGDSOUT(1).EQ.0) CALL POLFIXS(NOUT,NOUT,1,
c    &      RLAT,RLON,IBOUT,LOUT,FOUT)

        RETURN
        END
