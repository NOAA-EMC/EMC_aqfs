
        SUBROUTINE  BCSTEP( NCOLS, NROWS, NLAYS, NVARS,
     &                      LOCOL, HICOL, LOROW, HIROW, NTHIK,
     &                      JDATE, JTIME, INAME, WNAME, LOGDEV )

C***********************************************************************
C Version "$Id bcstep.F 1703 2013-11-15 21:39:36Z coats@bdsl$"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
C and (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  70
C
C  FUNCTION:
C       Copy this time step of variable VNAME from INAME to WNAME.
C
C  PRECONDITIONS REQUIRED:
C       Valid dates and times JDATE:JTIME
C       f77 MALLOC()-allocation operating environment (such as Sun, SGI)
C	FDESC3 correctly set-up with VNAME3D
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       Models-3 I/O:  M3EXIT(), READ3(), WRITE3()
C
C  REVISION  HISTORY:
C       Prototype 1/1995 by CJC
C       Modified  9/1999 by CJC for enhanced portability
C       Modified  7/2002 by CJC:  standard-conforming DATA statement fix
C       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C***********************************************************************

      USE M3UTILIO

      IMPLICIT NONE


C...........   ARGUMENTS and their descriptions:

        INTEGER         NCOLS   ! input horiz grid dimension
        INTEGER         NROWS   ! input horiz grid dimension
        INTEGER         NLAYS   ! vertical dimension
        INTEGER         NVARS   ! vbles    dimension
        INTEGER         LOCOL   !  left   window boundary
        INTEGER         HICOL   !  right  window boundary
        INTEGER         LOROW   !  bottom window boundary
        INTEGER         HIROW   !  top    window boundary
        INTEGER         NTHIK   !  boundary thickness
        INTEGER         JDATE   ! current model date
        INTEGER         JTIME   ! current model time
        CHARACTER*16    INAME   !  logical name of the  input file
        CHARACTER*16    WNAME   !  logical name of the output file
        INTEGER         LOGDEV  ! unit number for output


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         I, C, R, L, V, T
        REAL            GRID( NCOLS, NROWS, NLAYS )
        REAL            BNDY( 2*NTHIK*( NCOLS3D + NROWS3D + 2*NTHIK ) ,
     &                        NLAYS )

        CHARACTER*256   MESG


C***********************************************************************
C   begin body of subroutine  BCSTEP

        DO  V = 1, NVARS

            IF ( .NOT. READ3( INAME, VNAME3D( V ), ALLAYS3,
     &                        JDATE, JTIME, GRID ) ) THEN
                MESG = 'Read failure:  file ' // INAME
                CALL M3EXIT( 'BCWNDW:BCSTEP', JDATE, JTIME, MESG, 2 )
            END IF              !  if read3() worked, or not


C.......   Extract the boundary from this subgrid:

            DO  L = 1, NLAYS

               I = 0				!  perimeter subscript for this layer

                DO  R = LOROW - NTHIK, LOROW - 1		!  south bdy component
                DO  C = LOCOL, HICOL + NTHIK
                    I = I + 1
                    BNDY( I,L ) = GRID( C,R,L )
                END DO
                END DO

                DO  R = LOROW, HIROW + NTHIK		!  east bdy
                DO  C = HICOL + 1, HICOL + NTHIK
                    I = I + 1
                    BNDY( I,L ) = GRID( C,R,L )
                END DO
                END DO

                DO  R = HIROW + 1, HIROW + NTHIK		!  north
                DO  C = LOCOL - NTHIK, HICOL
                    I = I + 1
                    BNDY( I,L ) = GRID( C,R,L )
                END DO
                END DO

                DO  R = LOROW - NTHIK, HIROW		!  west
                DO  C = LOCOL - NTHIK, LOCOL - 1
                    I = I + 1
                    BNDY( I,L ) = GRID( C,R,L )
                END DO
                END DO

            END DO	!  end loop on layers

            IF ( .NOT. WRITE3( WNAME, VNAME3D( V ),
     &                         JDATE, JTIME, BNDY ) ) THEN
                MESG = 'Write failure:  file ' // WNAME
                CALL M3EXIT( 'BCWNDW:BCSTEP', JDATE, JTIME, MESG, 2 )
            END IF              !  if read3() worked, or not

        END DO	!  end loop on variables

        RETURN

        END SUBROUTINE  BCSTEP

