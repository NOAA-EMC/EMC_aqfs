
        SUBROUTINE  TSHIFT ( SIZE,  JDATE, JTIME, KDATE, KTIME,
     &                       INAME, ONAME, LOGDEV )

C***********************************************************************
C Version "@(#)$Header$"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 Baron Advanced Meteorological Systems, LLC
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  90
C
C  FUNCTION:
C       Copy this JDATE:JTIME from INAME to KDATE:KTIME in ONAME.
C
C  PRECONDITIONS REQUIRED:
C       Valid dates and times JDATE:JTIME
C       f77 MALLOC()-allocation operating environment (such as Sun, SGI)
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       Models-3 I/O:  M3ERR(), READ3(), WRITE3()
C
C  REVISION  HISTORY:
C       Prototype 1/95 by CJC
C       Modified  9/99 by CJC for enhanced portability
C
C       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C***********************************************************************

      USE M3UTILIO
      IMPLICIT NONE


C...........   ARGUMENTS and their descriptions:

        INTEGER         SIZE    ! total array dimensions for this timestep
        INTEGER         JDATE   ! source date
        INTEGER         JTIME   ! source time
        INTEGER         KDATE   ! target date
        INTEGER         KTIME   ! target time
        CHARACTER*16    INAME   ! logical name of the  input file
        CHARACTER*16    ONAME   ! logical name of the output file
        INTEGER         LOGDEV  ! unit number for output


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        REAL            GRID( SIZE )
        CHARACTER*256   MESG    ! buffer for M3EXIT()

C***********************************************************************
C   begin body of subroutine  TSHIFT
	
        IF ( READ3( INAME, ALLVAR3, ALLAYS3,
     &              JDATE, JTIME, GRID ) ) THEN
           
            IF ( .NOT. WRITE3( ONAME, ALLVAR3, 
     &                         KDATE, KTIME, GRID ) ) THEN
               
                MESG = 'Write failure:  file ' // ONAME
                CALL M3WARN( 'M3TSHIFT:TSHIFT', KDATE, KTIME, MESG )
               
            END IF              !  if read3() worked, or not
                   
        ELSE                !  read3() failed:
           
            MESG = 'Read failure:  file  ' // INAME
            CALL M3WARN( 'M3TSHIFT:TSHIFT', JDATE, JTIME, MESG )
           
        END IF              !  if read3() worked, or not

        RETURN

        END

