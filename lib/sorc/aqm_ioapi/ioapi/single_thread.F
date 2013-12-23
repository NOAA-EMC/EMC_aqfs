C================================================================
C @(#)$Header$
C
C     The next 3 functions: read3v, write3v, and shut3v
C     need a OpenMP critical section around them to ensure
C     thread safe behavior
C     Note: open3v and desc3v are currently not thread-safe
C     but open3 is used itself in critical sections, while
C     desc3 should not be used in parallel regions because of the 
C     variables in the FDESC common block
C
C================================================================

      LOGICAL FUNCTION READ3V(FNAME, VNAME, SKIP,
     $                        JDATE, JTIME, BUFFER, COUNT, TYPE)

      IMPLICIT NONE

C.....Arguments
      CHARACTER *(*) FNAME
      CHARACTER *(*) VNAME
      INTEGER        SKIP
      INTEGER        JDATE
      INTEGER        JTIME
      INTEGER        COUNT
      INTEGER        TYPE
      REAL           BUFFER(*)

C.....Function prototype
      LOGICAL        READ3V_ST
      EXTERNAL       READ3V_ST

!$OMP CRITICAL( S_PVM )
      READ3V = READ3V_ST(FNAME, VNAME, SKIP,
     $                   JDATE, JTIME, BUFFER, COUNT, TYPE)
!$OMP END CRITICAL( S_PVM )
      RETURN
      END
C================================================================

      LOGICAL FUNCTION WRITE3V(FNAME, VNAME,
     $                         JDATE, JTIME, BUFFER, COUNT, TYPE)

      IMPLICIT NONE

C.....Arguments
      CHARACTER *(*) FNAME
      CHARACTER *(*) VNAME
      INTEGER        JDATE
      INTEGER        JTIME
      INTEGER        COUNT
      INTEGER        TYPE
      REAL           BUFFER(*)

C.....Function prototype
      LOGICAL        WRITE3V_ST
      EXTERNAL       WRITE3V_ST

!$OMP CRITICAL( S_PVM )
      WRITE3V = WRITE3V_ST(FNAME, VNAME,
     $                     JDATE, JTIME, BUFFER, COUNT, TYPE)
!$OMP END CRITICAL( S_PVM )
      RETURN
      END
C================================================================

      LOGICAL FUNCTION SHUT3V()

      IMPLICIT NONE

C.....Arguments

C.....Function prototype
      LOGICAL        SHUT3V_ST
      EXTERNAL       SHUT3V_ST

!$OMP CRITICAL( S_PVM )
      SHUT3V = SHUT3V_ST()
!$OMP END CRITICAL( S_PVM )
      RETURN
      END
C================================================================




