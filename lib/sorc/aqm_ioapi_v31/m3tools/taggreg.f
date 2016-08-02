
        SUBROUTINE  TAGGREG

C***********************************************************************
C Version "$Id: taggreg.f 43 2014-09-12 14:06:19Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 Baron Advanced Meteorological Systems, LLC
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  entry INITTAG body starts at line   81
C  entry AGGREG  body starts at line  109
C  entry OUTAGG  body starts at line  160
C
C  FUNCTION:
C       Aggregate time steps as either sum, average, or max.
C       INITAGG: allocates memory and initializes array with first time
C                step of data
C       AGGREG:  Modifies gridded storage array based on current time step
C                of data
C       OUTAGG:  Completes gridded array values and writes to output file
C
C  PRECONDITIONS REQUIRED:
C       Valid dates and times JDATE:JTIME
C       f77 MALLOC()-allocation operating environment (such as Sun, SGI)
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       Models-3 I/O:  M3ERR(), READ3(), WRITE3()
C
C  REVISION  HISTORY:
C       Prototype 5/1997 by M Houyoux
C       Modified  9/1999 by CJC for enhanced portability
C
C       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C
C       Version 11/2013 by CJC:  OpenMP parallel, INTENT for arguments
C***********************************************************************

      USE M3UTILIO
      IMPLICIT NONE

C...........   PARAMETERS

        INTEGER, PARAMETER ::  M3SUM = 1
        INTEGER, PARAMETER ::  M3AVE = 2
        INTEGER, PARAMETER ::  M3MAX = 3

C...........   ARGUMENTS and their descriptions:

        INTEGER,      INTENT(IN) :: SIZE    ! total array dimensions for this variable
        INTEGER,      INTENT(IN) :: LAYER   ! layer number to read
        INTEGER,      INTENT(IN) :: JDATE   ! current model date
        INTEGER,      INTENT(IN) :: JTIME   ! current model time
        CHARACTER*16, INTENT(IN) :: INAME   !  logical name of the  input file
        CHARACTER*16, INTENT(IN) :: ONAME   !  logical name of the output file
        CHARACTER*16, INTENT(IN) :: VNAMEI  !  vble name (input)
        CHARACTER*16, INTENT(IN) :: VNAMEO  !  vble name (output)
        INTEGER,      INTENT(IN) :: TYPE    ! type of aggregation to perform
        INTEGER,      INTENT(IN) :: NSTEPS  ! number of time steps - used for average only
        INTEGER,      INTENT(IN) :: LOGDEV  ! unit number for output


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         I
        REAL            DIV
        CHARACTER*8     BUFFER

        REAL, ALLOCATABLE, SAVE::    GRID   ( : )
        REAL, ALLOCATABLE, SAVE::    SCRATCH( : )


C***********************************************************************
C   begin body of subroutine  TAGGREG

C***********************************************************************
C   begin body of entry point INITAGG

        ENTRY INITAGG( SIZE,   LAYER, JDATE, JTIME, INAME,
     &                 VNAMEI, LOGDEV )


        IF ( SIZE .GT. LSIZE ) THEN
            ALLOCATE( GRID( 2*SIZE ), SCRATCH( 2*SIZE ), STAT=I )
            IF ( I .NE. 0 ) THEN
                 CALL M3EXIT( 'M3TPROC:TAGGREG/INITAGG', JDATE, JTIME,
     &                        'Memory allocation error', 2 )
            END IF
            LSIZE = SIZE
        END IF
#
        IF ( .NOT. READ3( INAME, VNAMEI, LAYER,
     &                    JDATE, JTIME, GRID ) ) THEN

            CALL M3EXIT ( 'M3TPROC:TAGGREG', JDATE, JTIME,
     &                    'Read failure:  file "' // INAME //
     &                    '" variable "' // VNAMEI // '"', 2 )

        END IF              !  if read3() worked, or not

        RETURN


C***********************************************************************
C   begin body of entry point AGGREG

        ENTRY AGGREG( SIZE, LAYER , JDATE, JTIME, INAME,
     &                VNAMEI, TYPE , LOGDEV   )

        IF ( SIZE .EQ. LSIZE ) THEN

            IF ( .NOT. READ3( INAME, VNAMEI, LAYER,
     &                        JDATE, JTIME, SCRATCH ) ) THEN

                CALL M3EXIT ( 'M3TPROC:TAGGREG/AGGREG', JDATE, JTIME,
     &                        'Read failure:  file "' // INAME //
     &                        '" variable "' // VNAMEI // '"', 2 )

            ELSE IF( TYPE .EQ. M3SUM .OR. TYPE .EQ. M3AVE ) THEN

!$OMP           PARALLEL DO DEFAULT( NONE ),
!$OMP&                      SHARED( SIZE, GRID, SCRATCH ),
!$OMP&                      PRIVATE( I )
                DO 111 I = 1, SIZE
                    GRID( I ) = GRID( I ) + SCRATCH( I )
111             CONTINUE

            ELSE IF( TYPE .EQ. M3MAX ) THEN

!$OMP           PARALLEL DO DEFAULT( NONE ),
!$OMP&                      SHARED( SIZE, GRID, SCRATCH ),
!$OMP&                      PRIVATE( I )
                DO 122 I = 1, SIZE
                    GRID( I ) = MAX( GRID( I ), SCRATCH( I ) )
122             CONTINUE

            ELSE

                WRITE( BUFFER, '(I8)' ) TYPE
                CALL M3EXIT ( 'M3TPROC:TAGGREG/AGGREG', JDATE, JTIME,
     &                        'Aggregation type "' // BUFFER //
     &                        '" not supported' , 2 )

            END IF              !  if read3() worked, or not
        ELSE

            CALL M3EXIT ( 'M3TPROC:TAGGREG/AGGREG', JDATE, JTIME,
     &                    'Illegal attempt to change SIZE argument', 2 )

        END IF

        RETURN


C***********************************************************************
C   begin body of entry point OUTAGG

        ENTRY OUTAGG( SIZE, LAYER , JDATE, JTIME, ONAME,
     &                VNAMEO, TYPE , NSTEPS, LOGDEV )

        IF ( SIZE .EQ. LSIZE ) THEN

            IF( TYPE .EQ. M3AVE ) THEN

                DIV = 1.0 / REAL( NSTEPS )
!$OMP           PARALLEL DO DEFAULT( NONE ),
!$OMP&                      SHARED( SIZE, GRID, DIV ),
!$OMP&                      PRIVATE( I )
                DO 211 I = 1, SIZE
                    GRID( I ) = DIV * GRID( I )
211             CONTINUE

            END IF

            IF ( .NOT. WRITE3( ONAME, VNAMEO,
     &                         JDATE, JTIME, GRID ) ) THEN

                CALL M3EXIT ( 'M3TPROC:TAGGREG/OUTAGG, JDATE, JTIME,
     &                        'Write failure:  file "' // ONAME //
     &                        '" variable "' // VNAMEO // '"', 2 )

            END IF

        ELSE

            CALL M3EXIT ( 'M3TPROC:TAGGREG/OUTAGG', JDATE, JTIME,
     &                    'Illegal attempt to change SIZE argument', 2 )

        END IF

        RETURN

        END  SUBROUTINE  TAGGREG

