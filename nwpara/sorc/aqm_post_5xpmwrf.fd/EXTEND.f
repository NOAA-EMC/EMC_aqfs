      SUBROUTINE EXTEND (IMAXIN,JJMAXIN,JMAXIN,KGDSIN,
     &                      UIN,LIN,UMIN,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C   SUBPROGRAM: EXTEND   
C   PRGMMR: BALDWIN          ORG: NP22        DATE: 98-08-11  
C
C ABSTRACT: EXTEND FILLS IN MISSING VALUES WITH NEARBY VALID VALUES 
C           ON A GRID.  IT DOES NOT MODIFY THE BITMAP.
C
C PROGRAM HISTORY LOG:
C   98-08-11  BALDWIN     ORIGINATOR
C
C USAGE:  CALL EXTEND (IMAXIN,JJMAXIN,JMAXIN,KGDSIN,UIN,LIN,IRET)
C
C   INPUT:
C         IMAXIN            INTEGER - MAX X DIMENSION OF INPUT GRID
C         JJMAXIN           INTEGER - MAX Y DIMENSION OF INPUT GRID
C         JMAXIN            INTEGER - MAX DIMENSION OF UIN
C         KGDSIN(22)        INTEGER - KGDS FOR THIS GRID
C         UIN(JMAXIN)       REAL    - FIELD TO EXTEND
C         LIN(JMAXIN)       LOGICAL*1 - BITMAP CORRESPONDING TO UIN
C         UMIN              REAL    - MIN VAULE OF FIELD
C
C   OUTPUT:
C         UIN(JMAXIN)       REAL    - EXTENDED FIELD
C         IRET              INTEGER - RETURN CODE
C
C   RETURN CODES:
C     IRET =   0 - NORMAL EXIT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : CRAY J-916
C
C$$$
      INTEGER KGDSIN(22)
      LOGICAL*1 LIN(JMAXIN)
      LOGICAL*1 LFLT(IMAXIN,JJMAXIN)
      REAL UIN(JMAXIN),Z1(IMAXIN,JJMAXIN)

      IRET=0

      IMX=KGDSIN(2)
      JMX=KGDSIN(3)
C
      DO J=1,JMX
        DO I=1,IMX
          KK=(J-1)*IMX+I
          Z1(I,J)=UIN(KK)
          LFLT(I,J)=LIN(KK)
        ENDDO
      ENDDO
C
C     FILL MISSING VALUES WITH BOUNDARY VALUES. 
C     
C     EXTEND SOUTHERN AND NORTHERN BOUNDARIES.
      DO 2070 I = 1,IMX
C     
C        EXTEND SOUTHERN BOUNDARY.
         DO J = 1,JMX
            IF (LFLT(I,J)) THEN
               DO JJ = J,1,-1
                 IF(.NOT.LFLT(I,JJ)) THEN
                    Z1(I,JJ) = Z1(I,J)
                    LFLT(I,JJ) = .TRUE.
                 ENDIF
               ENDDO
               GOTO 2040
            ENDIF
         ENDDO
C     
C        EXTEND NORTHERN BOUNDARY
 2040    CONTINUE
         DO J = JMX,1,-1
            IF (LFLT(I,J)) THEN
               DO JJ = J,JMX
                 IF(.NOT.LFLT(I,JJ)) THEN
                    Z1(I,JJ) = Z1(I,J)
                    LFLT(I,JJ) = .TRUE.
                 ENDIF
               ENDDO
               GOTO 2070
            ENDIF
         ENDDO
C
C     REPEAT FOR NEXT COLUMN.
 2070 CONTINUE
C     
C     EXTEND WESTERN AND EASTERN BOUNDARIES
      DO 2170 J = 1,JMX
C     
C        EXTEND WESTERN BOUNDARY
         DO I = 1,IMX
            IF (LFLT(I,J)) THEN
               DO II = I,1,-1
                 IF(.NOT.LFLT(II,J)) THEN
                    Z1(II,J) = Z1(I,J)
                    LFLT(II,J) = .TRUE.
                 ENDIF
               ENDDO
               GOTO 2140
            ENDIF
         ENDDO
C     
C        EXTEND EASTERN BOUNDARY
 2140    CONTINUE
         DO I = IMX,1,-1
            IF (LFLT(I,J)) THEN
               DO II = I,IMX
                 IF(.NOT.LFLT(II,J)) THEN
                    Z1(II,J) = Z1(I,J)
                    LFLT(II,J) = .TRUE.
                 ENDIF
               ENDDO
               GOTO 2170
            ENDIF
         ENDDO
C     
C     REPEAT FOR NEXT ROW
 2170 CONTINUE
C     
C     IF WE STILL CAN'T FILL THE Z1 AFTER THIS,
C     FILL MISSING VALUES WITH MIN VALUE
C
         DO J = 1,JMX
         DO I = 1,IMX
            IF(.NOT.LFLT(I,J)) THEN
                 Z1(I,J) = UMIN
            ENDIF
         ENDDO
         ENDDO
         DO J=1,JMX
           DO I=1,IMX
             KK=(J-1)*IMX+I
             UIN(KK)=Z1(I,J)
           ENDDO
         ENDDO

      RETURN
      END
