
        SUBROUTINE RD3MASK( FNAME, JDATE, JTIME, NDIM, NDIM2, NVLIST, 
     &                      VNAMES, VINDX, OUTVAR )

C***********************************************************************
C  subroutine body starts at line
C
C  DESCRIPTION:
C      This subroutine reads in certain variables from a list of variables, 
C      depending on the index, which indicates if those variables are expected
C      to be present in a file.  The index is a pointer which says which
C      part of the output array to read the variable into. The variables are
C      assumed to be reals.
C
C  PRECONDITIONS REQUIRED:
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION  HISTORY:
C    01/24/05 David Wong
C      -- Removed repetitive species read
C
C****************************************************************************/
C
C Project Title: EDSS Tools Library
C File: @(#)$Id: rd3mask.f,v 1.7 2004/06/18 17:22:12 cseppan Exp $
C
C COPYRIGHT (C) 2004, Environmental Modeling for Policy Development
C All Rights Reserved
C
C Carolina Environmental Program
C University of North Carolina at Chapel Hill
C 137 E. Franklin St., CB# 6116
C Chapel Hill, NC 27599-6116
C
C smoke@unc.edu
C
C Pathname: $Source: /afs/isis/depts/cep/emc/apps/archive/edss_tools/edss_tools/src/lib/rd3mask.f,v $
C Last updated: $Date: 2004/06/18 17:22:12 $ 
C
C***************************************************************************

        IMPLICIT NONE

C...........   INCLUDES
        INCLUDE 'IOSTRG3.EXT'   !  I/O API string lengths
        INCLUDE 'IODECL3.EXT'   !  I/O API function declarations

C...........   EXTERNAL FUNCTIONS and their descriptions:
C       CHARACTER(2)    CRLF
C       INTEGER         INDEX1

C       EXTERNAL        CRLF, INDEX1

C.........  SUBROUTINE ARGUMENTS
        CHARACTER(*), INTENT (IN) :: FNAME     ! i/o api file name
        INTEGER     , INTENT (IN) :: JDATE     ! Julian date (YYYYDDD)
        INTEGER     , INTENT (IN) :: JTIME     ! time (HHMMSS)
        INTEGER     , INTENT (IN) :: NDIM      ! dimension for output array
        INTEGER     , INTENT (IN) :: NDIM2     ! 2nd dimension for output array
        INTEGER     , INTENT (IN) :: NVLIST    ! variable description to read
        CHARACTER(*), INTENT (IN) :: VNAMES( NVLIST ) ! variable names
        INTEGER     , INTENT (IN) :: VINDX ( NVLIST ) ! var index to OUTVAR
        REAL        , INTENT(OUT) :: OUTVAR( NDIM,NDIM2 ) ! coeffs for sources

C.........  Other local variables
        INTEGER         J, L, L1, V       !  counters and indices

        CHARACTER(IOVLEN3) VBUF    !  variable name buffer
        CHARACTER(300)     MESG    !  message buffer

        CHARACTER(16) :: PROGNAME = 'RD3MASK' ! program name

        CHARACTER( LEN = 16) :: LOC_VNAMES( NVLIST )
        INTEGER :: LOC_VINDX(NVLIST), FOUND(NVLIST), LOC

C***********************************************************************
C   begin body of subroutine RD3MASK

        FOUND = 0
        LOC = 0
        DO V = 1, NVLIST
           IF (FOUND(VINDX(V)) .EQ. 0) THEN
              LOC = LOC + 1
              LOC_VINDX(LOC) = VINDX(V)
              LOC_VNAMES(loc) = VNAMES( V )
              FOUND(VINDX(V)) = 1
           END IF
        END DO

C.........  Loop through variables that are possibilities for reading
C       DO V = 1, NVLIST
        DO V = 1, LOC

C.............  Check variable index to see if this variable is to be read
C           J = VINDX( V ) 
            J = LOC_VINDX( V )
            IF( J .EQ. 0 ) CYCLE  ! to go bottom of loop

C.............  Read variable and print nice error message if cannot
C           VBUF = VNAMES( V )
            VBUF = LOC_VNAMES( V )

C.............  Bounds check
            IF( J .GT. NDIM2 ) THEN

                MESG = 'INTERNAL ERROR: Prevented overflow for read '//
     &                 'of variable "' // TRIM( VBUF ) // '"'
                CALL M3MSG2( MESG )
                CALL M3EXIT( PROGNAME, 0, 0, ' ', 2 )

            END IF

            IF ( .NOT. READ3( 
     &           FNAME, VBUF, 1, JDATE, JTIME, OUTVAR( 1,J ) ) ) THEN

                L  = LEN_TRIM( FNAME )
                L1 = LEN_TRIM( VBUF )
                MESG = 'Could not read variable "' // VBUF( 1:L1 ) //
     &                 '" from file "' // FNAME( 1:L ) // '"'
                CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )

            END IF    !  if read3() failed for file

        END DO        !  End loop on possible variables

        RETURN

        END SUBROUTINE RD3MASK
