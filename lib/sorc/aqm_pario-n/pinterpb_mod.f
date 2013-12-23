C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header$ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C....................................................................
C  CONTAINS:  Variables used by parallel file reading routines
C             PINTERPB, which are based on a domain 
C             decomposition over the horizontal grid coordinates. 
C             These I/O routines are part of the parallel Models-3 
C             I/O library.
              
C  REVISION HISTORY:
C       Original version 02/04 by David Wong
C....................................................................

      MODULE PINTERPB_MODULE

C         Parameters         Description
C         ----------         -----------

      INTEGER, PARAMETER :: MXNVARHD = 200    ! Max number of buffered file variables.

C  -----------------------------------------------------------------
C  The following (dynamic) variables are used to determine when
C  file I/O buffers need to be updated.  All are allocated to size
C  MXNVARHD. Times and dates are initialized to -9999 and buffer 
C  switches are initialized to 0 in function PINTERPB.
C
C  NOTE: These are the parallel versions of the I/O API state 
C        variables LDATE3 and NDATE3.  (see I/O API file STATE3.EXT )
C  -----------------------------------------------------------------

C          Variables                         Description
C          ---------                         -----------

      INTEGER, SAVE :: LDATHD(MXNVARHD)    ! Start date for current buffered 2D data.

      INTEGER, SAVE :: LTIMHD(MXNVARHD)    ! Start time for current buffered 2D data.

      INTEGER, SAVE :: NDATHD(MXNVARHD)    ! End date for current buffered 2D data.

      INTEGER, SAVE :: NTIMHD(MXNVARHD)    ! End time for current buffered 2D data.

      INTEGER, SAVE :: SWBUFHD(MXNVARHD)   ! Buffer switches (0 means normal, 1 means reversed).

C  -----------------------------------------------------------------
C  The following (dynamic) array holds offsets for positioning into
C  the file buffers.  Odd indices are used for offsets to the start 
C  buffers, and even indices are used for offsets to the end buffers.
C  buffers.  E.g., the start and end buffers for file variable N are 
C  held in BUFPOSHD(2*N - 1) and BUFPOSHD(2N), respectively.  This
C  array is allocated to size 2*MXNVARHD.
C  -----------------------------------------------------------------

c     INTEGER, SAVE :: BUFPOSHD(0:2,MXNVARHD)  ! Buffer positions for file variable buffers.
      INTEGER, SAVE :: BUFPOSHD(MXNVARHD)  ! Buffer positions for file variable buffers.

C  -----------------------------------------------------------------
C  The following (dynamic) array holds dimensions and boundary 
C  thicknesses for file variables.  This array is allocated to size
C  3*MXNVARHD.
C  -----------------------------------------------------------------

      INTEGER, SAVE :: SIZEHD(MXNVARHD)    ! Local processor variable size.

C  -----------------------------------------------------------------
C  The following (static) array is used to identify file variables 
C  uniquely.
C  -----------------------------------------------------------------

      CHARACTER (LEN = 33), SAVE :: VLISTHD( MXNVARHD )     ! Variable-name table.

C  -----------------------------------------------------------------
C  The following (dynamic) array is used for holding start and end 
C  buffers for file variables which need to be interpolated.  It is
C  allocated by extension, as each new variable is called for 
C  interpolation.
C  -----------------------------------------------------------------

      TYPE DATA_PTR_TYPE 
c       REAL, ALLOCATABLE :: DATA_PTR(:)
        REAL, POINTER :: DATA_PTR(:)
      END TYPE DATA_PTR_TYPE 

      TYPE MEM_TYPE 
        INTEGER :: SIZE
        TYPE(DATA_PTR_TYPE) :: MEM(0:1)
      END TYPE MEM_TYPE 

c     REAL, ALLOCATABLE, TARGET :: BUFFERHD(:)
      TYPE(MEM_TYPE) :: BUFFERHD(MXNVARHD)

      INTEGER, SAVE :: PTR_COUNT = 0
      INTEGER, SAVE :: BUFFERHD_SIZE = 0

C  ------------------------------------------------------------
C  The dynamic arrays below are used for reading and communi-
C  cating file variables.  They are allocated by function 
C  ALLORBUF, only in I/O processors, by PINTERPB or PREAD3.
C  ------------------------------------------------------------

      REAL, ALLOCATABLE, SAVE :: MSGBUFHD(:)           ! Message buffer.

      END MODULE PINTERPB_MODULE
