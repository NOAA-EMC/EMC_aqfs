      SUBROUTINE INTERP_PPT (JMAXIN,FIN,LIN,IBIN,
     &                      JMAXOT,FOUT,LOUT,IBOUT,NOUT,KGDSOUT,
     &                      NPP,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C   SUBPROGRAM: INTERP_PPT
C   PRGMMR: BALDWIN          ORG: NP22        DATE: 98-08-11  
C
C ABSTRACT: INTERP_PPT INTERPOLATES A PRECIPITATION FIELD USING GIVEN 
C           INTERPOLATION WEIGHTS INTENDED FOR BUDGET INTERPOLATION.
C
C PROGRAM HISTORY LOG:
C   98-08-11  BALDWIN     ORIGINATOR
C
C USAGE:  CALL INTERP_UV (JMAXIN,FIN,LIN,IBIN,
C    &                      JMAXOT,FOUT,LOUT,IBOUT,NOUT,KGDSOUT,
C    &                      NPP,IRET)
C
C   INPUT:
C         JMAXIN            INTEGER - MAX DIMENSION OF FIN
C         JMAXOT            INTEGER - MAX DIMENSION OF FOUT
C         NOUT              INTEGER - NUMBER OF PTS ON OUTPUT GRID
C         KGDSOUT(22)       INTEGER - KGDS OF OUTPUT GRID
C         FIN(JMAXIN)       REAL    - INPUT FIELD
C         LIN(JMAXIN)       LOGICAL*1 - BITMAP CORRESPONDING TO UIN,VIN
C         IBIN              INTEGER - BITMAP FLAG FOR INPUT GRIDS
C         NPP(JMAXOT,25)    INTEGER - INDEX OF NEAREST INPUT GRID 
C                                     POINTS TO A 5x5 SUBGRID CENTERED
C                                     OVER OUTPUT GRID POINT
C
C   OUTPUT:
C         FOUT(JMAXOT)      REAL    - OUTPUT FIELD
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
      INTEGER NPP(JMAXOT,25)
      LOGICAL*1 LIN(JMAXIN),LOUT(JMAXOT)
      REAL WO(JMAXOT)
      REAL FIN(JMAXIN),FOUT(JMAXOT)

      IRET=0

C  INTERPOLATE WITH OR WITHOUT BITMAPS

        DO I=1,NOUT
          FOUT(I)=0.
          WO(I)=0.
        ENDDO
        DO ISUB=1,25
            DO I=1,NOUT
              IF(NPP(I,ISUB).GT.0) THEN
                IF(IBIN.EQ.0.OR.LIN(NPP(I,ISUB))) THEN
                  FOUT(I)=FOUT(I)+FIN(NPP(I,ISUB))
                  WO(I)=WO(I)+1.
                ENDIF
              ENDIF
            ENDDO
        ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE OUTPUT BITMAPS AND FIELDS
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        IBOUT=IBIN
        DO I=1,NOUT
          LOUT(I)=WO(I).GE.12.5
          IF(LOUT(I)) THEN
            IF (FOUT(I).LT.0.0) FOUT(I)=0.0
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
