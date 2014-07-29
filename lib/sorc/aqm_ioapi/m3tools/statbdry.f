
        SUBROUTINE  STATBDRY( SIZE, NCOLS, NROWS, NLAYS, NTHIK, NVARS,
     &                        JDATE, JTIME, NTHRES, THRES,
     &                        INNAME, VNAMES, VTYPES, LOGDEV )

C***********************************************************************
C Version "@(#)$Header$"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 Baron Advanced Meteorological Systems, LLC
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C  subroutine body starts at line  107
C
C  FUNCTION:
C       Statistics report to LOGDEV on variables VNAMES  from file
C       INNAME.
C
C  PRECONDITIONS REQUIRED:
C       Valid dates and times JDATE:JTIME
C       Stack-allocation operating environment (such as CRAY)
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       Models-3 I/O:  M3ERR(), READ3(), WRITE3()
C
C  REVISION  HISTORY:
C       Prototype 3/93 by CJC
C       Modified  9/99 by CJC for enhanced portability
C
C       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C
C       Version 02/2010 by CJC:  SIZE bug-fix
C***********************************************************************

      USE M3UTILIO
      IMPLICIT NONE


C...........   ARGUMENTS and their descriptions:

        INTEGER         SIZE    ! horiz bdy size,  from INNAME header
        INTEGER         NCOLS   ! grid dimensions, from INNAME header
        INTEGER         NROWS   ! grid dimensions, from INNAME header
        INTEGER         NLAYS   ! grid dimensions, from INNAME header
        INTEGER         NTHIK   ! bdry thickness,  from INNAME header
        INTEGER         NVARS   !  number of vbles to be totaled
        INTEGER         JDATE   ! current model date
        INTEGER         JTIME   ! current model time
        INTEGER         NTHRES( NVARS )		! number of tests per vble
        REAL		THRES ( 10,NVARS )	! thresholds for counting
        CHARACTER*16    INNAME                  !  input file logical name
        CHARACTER*16    VNAMES( NVARS )         !  list of vble names
        INTEGER         VTYPES( NVARS )		! number of tests per vble
        INTEGER         LOGDEV  ! unit number for output


C...........   LOCAL VARIABLES and their descriptions:

        REAL             BDRY( SIZE, NLAYS )
        INTEGER          IGRD( NCOLS, NROWS, NLAYS )
        DOUBLE PRECISION DGRD( NCOLS, NROWS, NLAYS )
        INTEGER          V

        CHARACTER*120   MESG


C***********************************************************************
C   begin body of subroutine  STATBDRY

        IF ( JDATE .NE. 0 .OR. JTIME .NE. 0 ) THEN	
            WRITE( LOGDEV,92010 )
     &          INNAME, JDATE, JTIME, DT2STR( JDATE, JTIME )
        ELSE
            WRITE( LOGDEV,92010 ) INNAME
        END IF
        
        SIZE = 2 * ( NCOLS + NROWS + 2*NTHIK ) * NLAYS

        DO  111  V = 1, NVARS

            IF ( VTYPES( V ) .EQ. M3REAL ) THEN

                IF ( .NOT. READ3( INNAME, VNAMES( V ), ALLAYS3,
     &                            JDATE, JTIME, BDRY ) ) THEN

                    MESG = 'Read failure:  file ' // INNAME //
     &                     ' variable ' // VNAMES( V )
                    CALL M3EXIT( 'M3STAT:STATBDRY', JDATE, JTIME,
     &                           MESG, 2 )
                    GO TO 111

                END IF              !  if read3() worked, or not

            ELSE IF ( VTYPES( V ) .EQ. M3INT ) THEN

                IF ( .NOT. READ3( INNAME, VNAMES( V ), ALLAYS3,
     &                            JDATE, JTIME, IGRD ) ) THEN

                    MESG = 'Read failure:  file ' // INNAME //
     &                     ' variable ' // VNAMES( V )
                    CALL M3EXIT( 'M3STAT:STATBDRY', JDATE, JTIME,
     &                           MESG, 2 )
                    GO TO 111

                END IF              !  if read3() worked, or not

                CALL INTG2REAL( SIZE, IGRD, BDRY )

            ELSE IF ( VTYPES( V ) .EQ. M3DBLE ) THEN

                IF ( .NOT. READ3( INNAME, VNAMES( V ), ALLAYS3,
     &                            JDATE, JTIME, DGRD ) ) THEN

                    MESG = 'Read failure:  file ' // INNAME //
     &                     ' variable ' // VNAMES( V )
                    CALL M3EXIT( 'M3STAT:STATBDRY', JDATE, JTIME,
     &                           MESG, 2 )
                    GO TO 111

                END IF              !  if read3() worked, or not

                CALL DBLE2REAL( SIZE, DGRD, BDRY )

            ELSE

                MESG = 'Unknown data type for' //
     &                 ' variable ' // VNAMES( V )
                CALL M3EXIT( 'M3STAT:STATBDRY', JDATE, JTIME, MESG, 2 )
                GO TO 111

            END IF

            CALL STATB( SIZE, NCOLS, NROWS, NLAYS, NTHIK, BDRY,
     &                  NTHRES( V ), THRES( 1,V ),
     &                  VNAMES( V ), LOGDEV )

111     CONTINUE        !  end loop on variables

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Informational (LOG) message formats... 92xxx

92010   FORMAT ( //5X, 'File:  ', A, :,
     &            /5X, 'Date and time:', I7.7, ':', I6.6, 2X, A )

        END

