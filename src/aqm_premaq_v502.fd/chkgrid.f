
      SUBROUTINE CHKGRID( DATDESC, FTYPE, CHKLEVEL, EFLAG )

!***********************************************************************
!  subroutine body starts at line 91
!
!  DESCRIPTION:
!      This subroutine updates the grid information and compares to 
!      the existing information, if it has been previously set.
!
!  PRECONDITIONS REQUIRED:
!
!  SUBROUTINES AND FUNCTIONS CALLED:
!
!  REVISION  HISTORY:
!
!***********************************************************************
!
! Project Title: EDSS Tools Library
! File: @(#)$Id: chkgrid.f,v 1.12 2004/06/18 17:24:34 cseppan Exp $
!
! COPYRIGHT (C) 2004, Environmental Modeling for Policy Development
! All Rights Reserved
!
! Carolina Environmental Program
! University of North Carolina at Chapel Hill
! 137 E. Franklin St., CB# 6116
! Chapel Hill, NC 27599-6116
!
! smoke@unc.edu
!
! Pathname: $Source: /afs/isis/depts/cep/emc/apps/archive/edss_tools/edss_tools/src/lib/chkgrid.f,v $
! Last updated: $Date: 2004/06/18 17:24:34 $
!
!***********************************************************************

!-----------------------------------------------------------------------
!.... MODULES for public variables
!.... This module contains the global variables for the 3-d grid
!-----------------------------------------------------------------------

      USE MODGRID, ONLY:  XORIG, YORIG, XOFF, YOFF, GDTYP,
     &                    XCELL, YCELL, XCENT, YCENT,
     &                    P_ALP, P_BET, P_GAM, OFFLAG, GRDNM,
     &                    XOFF_A, YOFF_A, XDIFF, YDIFF, NGRID

      use premaqparm, only: ncols, nrows

      IMPLICIT NONE

!-----------------------------------------------------------------------
!.... INCLUDES:
!-----------------------------------------------------------------------

      INCLUDE 'IOCNST3.EXT'   !  emissions constant parameters
      INCLUDE 'PARMS3.EXT'    !  I/O API parameters
      INCLUDE 'FDESC3.EXT'    !  I/O API file desc. data structures

!-----------------------------------------------------------------------
!.... EXTERNAL FUNCTIONS and their descriptions:
!-----------------------------------------------------------------------

      CHARACTER(2) CRLF
      LOGICAL      DBLERR
      INTEGER      GETIFDSC  

      EXTERNAL     CRLF, DBLERR, GETIFDSC

!-----------------------------------------------------------------------
!.... SUBROUTINE ARGUMENTS
!-----------------------------------------------------------------------

      CHARACTER(*), INTENT(IN) :: DATDESC  ! data descriptions
      CHARACTER(*), INTENT(IN) :: FTYPE    ! GMAT|GRID file type of interest
      INTEGER     , INTENT(IN) :: CHKLEVEL ! strigency of check
      LOGICAL     , INTENT(OUT):: EFLAG    ! true: comparison failed

!-----------------------------------------------------------------------
!.... Local parameters
!-----------------------------------------------------------------------

      INTEGER, PARAMETER :: CHK_ALL = 0     ! check all of the grid settings
      INTEGER, PARAMETER :: CHK_SUBGRID = 1 ! check all but allow subgrids
      INTEGER, PARAMETER :: CHK_TMPSUBG = 2 ! check all, allow temporary subgrids

!-----------------------------------------------------------------------
!.... Local variables
!-----------------------------------------------------------------------

      INTEGER       L       ! length of file description
      INTEGER       NC      ! tmp number of columns
      INTEGER       NR      ! tmp number of rows
      INTEGER       XO      ! tmp x-offset  
      INTEGER       YO      ! tmp y-offset  

      REAL(8)       CHK_X   ! tmp val for checking subgrid even with grid
      REAL(8)       CHK_Y   ! tmp val for checking subgrid even with grid

      LOGICAL, SAVE :: GFLAG  = .FALSE. ! true: grid settings have been init
      LOGICAL       :: SFLAG  = .FALSE. ! true: local error

      CHARACTER(25)   FILDESC  ! description of input file
      CHARACTER(300)  MESG     ! message buffer

      CHARACTER(16) :: PROGNAME = 'CHKGRID' ! program name


!***********************************************************************
!   begin body of function CHKGRID
!***********************************************************************

!-----------------------------------------------------------------------
!.... Initialize local error flag
!-----------------------------------------------------------------------

      SFLAG = .FALSE.

!-----------------------------------------------------------------------
!.... Set tmp rows, columns, and total cells depending on file type
!-----------------------------------------------------------------------

      IF ( FTYPE .EQ. 'GMAT' ) THEN
         NC = GETIFDSC( FDESC3D, '/NCOLS3D/', .TRUE. )
         NR = GETIFDSC( FDESC3D, '/NROWS3D/', .TRUE. )
         FILDESC = 'gridding matrix'

      ELSEIF ( FTYPE .EQ. 'GROUPS' ) THEN
         NC = GETIFDSC( FDESC3D, '/NCOLS3D/', .TRUE. )
         NR = GETIFDSC( FDESC3D, '/NROWS3D/', .TRUE. )
         FILDESC = 'stack groups file'

      ELSEIF ( FTYPE .EQ. 'GRID' ) THEN
         NC = NCOLS3D
         NR = NROWS3D
         FILDESC = 'gridded file'

      ELSEIF ( FTYPE .EQ. 'GRIDDESC' ) THEN
         NC = NCOLS3D
         NR = NROWS3D
         FILDESC = 'grid description file'

      ELSEIF ( FTYPE .EQ. 'SURROGATES' ) THEN
         NC = NCOLS3D
         NR = NROWS3D
         FILDESC = 'surrogates file'

      ELSEIF ( FTYPE .EQ. 'LANDUSE' ) THEN
         NC = NCOLS3D
         NR = NROWS3D
         FILDESC = 'landuse file'

      ELSE
         MESG = 'INTERNAL ERROR: File type "' // FTYPE // 
     &          '" not known in call to ' // PROGNAME
         CALL M3MSG2( MESG )
         CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      ENDIF

      L = LEN_TRIM( FILDESC )

!-----------------------------------------------------------------------
!.... If grid information has already been initialized, then compare
!     existing to this file.
!-----------------------------------------------------------------------

      IF ( GFLAG ) THEN

        !---------------------------------------------------------------
        !... Check settings that must be consistent for exact grid match
        !---------------------------------------------------------------

         IF ( CHKLEVEL .EQ. CHK_ALL ) THEN

            IF ( NCOLS .NE. NC            .OR. 
     &           NROWS .NE. NR            .OR.
     &           DBLERR( XORIG, XORIG3D ) .OR.
     &           DBLERR( YORIG, YORIG3D )      ) THEN

               SFLAG = .TRUE.
               MESG = 'ERROR: Columns, rows, x-origin, or ' //
     &                'y-origin for ' // DATDESC // ' in ' //
     &                CRLF() // BLANK10 // FILDESC( 1:L ) // 
     &                ' are inconsistent with initialized values.'
               CALL M3MSG2( MESG ) 
            ENDIF

            XOFF = 0
            YOFF = 0
         ENDIF

        !---------------------------------------------------------------
        !... Check settings that must be consistent for grids and subgrids
        !---------------------------------------------------------------

         IF ( CHKLEVEL .LE. CHK_TMPSUBG ) THEN

            IF ( GDTYP .NE. GDTYP3D .OR.
     &           DBLERR( XCELL, XCELL3D  ) .OR.
     &           DBLERR( YCELL, YCELL3D  ) .OR.
     &           DBLERR( XCENT, XCENT3D  ) .OR.
     &           DBLERR( YCENT, YCENT3D  ) .OR.
     &           DBLERR( P_ALP, P_ALP3D  ) .OR.
     &           DBLERR( P_BET, P_BET3D  ) .OR.
     &           DBLERR( P_GAM, P_GAM3D  )      ) THEN

               SFLAG = .TRUE.
               MESG = 'ERROR: Grid type, cell sizes, or ' //
     &                'grid projection for ' // DATDESC // ' in '//
     &                CRLF() // BLANK10 // FILDESC( 1:L ) // 
     &                ' are inconsistent with initialized values.'
               CALL M3MSG2( MESG ) 
            ENDIF

           !-----------------------------------------------------------
           !... Ensure that origins are compatible with each other by
           !    making sure they line up based on the cell sizes
           !-----------------------------------------------------------

            CHK_X  = ( XORIG3D - XORIG ) / XCELL
            CHK_X  = CHK_X - INT( CHK_X )
            CHK_Y  = ( YORIG3D - YORIG ) / YCELL
            CHK_Y  = CHK_Y - INT( CHK_Y )

            IF ( DBLERR( CHK_X, 0.D0 ) .OR. DBLERR( CHK_Y, 0.D0 ) ) THEN
               SFLAG = .TRUE.
               MESG = 'ERROR: Grid origins not compatible ' //
     &                'between ' // DATDESC // ' in ' // 
     &                CRLF() // BLANK10 // FILDESC( 1:L ) // 
     &                ' and initialized values.'
               CALL M3MSG2( MESG ) 
            ENDIF

           !-----------------------------------------------------------
           !... If offset has been set, then check to ensure its the same
           !-----------------------------------------------------------

            IF ( OFFLAG ) THEN

              !--------------------------------------------------------
              !... If file has different origin from the subgrid...
              !--------------------------------------------------------

               IF ( XORIG3D .NE. XORIG  .OR.  YORIG3D .NE. YORIG ) THEN
                  XO = INT( ( XORIG3D - XORIG ) / XCELL )
                  YO = INT( ( YORIG3D - YORIG ) / YCELL )

                  IF ( XOFF .NE. XO  .OR.  YOFF .NE. YO ) THEN
                     SFLAG = .TRUE.
                     MESG = 'ERROR: Subgrid offset for ' //
     &                      DATDESC // ' in ' // CRLF() // BLANK10// 
     &                      FILDESC( 1:L ) // 'is ' //
     &                      'inconsistent with initialized values.'
                     CALL M3MSG2( MESG ) 
                  ENDIF

              !--------------------------------------------------------
              !... If file has same origin as subgrid
              !--------------------------------------------------------

               ELSE

                 !... Check current subgrid is the same as the previous

                  IF ( NCOLS .NE. NC            .OR.
     &                 NROWS .NE. NR            .OR.
     &                 DBLERR( XORIG, XORIG3D ) .OR.
     &                 DBLERR( YORIG, YORIG3D )      ) THEN

                     SFLAG = .TRUE.
                     MESG = 'ERROR: Columns, rows, x-origin, '//
     &                      'or y-origin for ' //DATDESC //' in ' 
     &                      //CRLF() //BLANK10 //FILDESC( 1:L ) // 
     &                      'are inconsistent with values from ' // 
     &                      GRDNM
                     CALL M3MSG2( MESG ) 
                  ENDIF
               ENDIF

           !-----------------------------------------------------------
           !... If offset for final subgrid hasn't been set yet...
           !-----------------------------------------------------------

            ELSE

              !--------------------------------------------------------
              !... Compute possible offset from upper right hand corner,
              !    and if there is one, set flag
              !--------------------------------------------------------

               XOFF_A = INT( ( XORIG  + NCOLS * XCELL   ) - 
     &                       ( XORIG3D+ NC    * XCELL3D ) ) / XCELL
               YOFF_A = INT( ( YORIG  + NROWS * YCELL   ) - 
     &                       ( YORIG3D+ NR    * YCELL3D ) ) / YCELL

              !--------------------------------------------------------
              !... Compute possible offset from origin, and if there is 
              !    one, set flag
              !--------------------------------------------------------

               XOFF_A = INT( ( XORIG3D - XORIG ) / XCELL )
               YOFF_A = INT( ( YORIG3D - YORIG ) / YCELL )

              !--------------------------------------------------------
              !...  Reset origin and number of cells to latest grid
              !--------------------------------------------------------

               GRDNM = GDNAM3D

              !--------------------------------------------------------
              !... Only store grid and offset parameters if the subgrid
              !    is not temporary
              !--------------------------------------------------------

               IF ( CHKLEVEL .LE. CHK_SUBGRID ) THEN
                  XOFF = XOFF_A
                  YOFF = YOFF_A

                  IF ( XOFF .NE. 0 .OR. YOFF .NE. 0 ) OFFLAG = .TRUE.

                  XDIFF = NCOLS - NC
                  YDIFF = NROWS - NR
                  XORIG = XORIG3D
                  YORIG = YORIG3D
                  NCOLS = NC
                  NROWS = NR
                  NGRID = NCOLS * NROWS
               ENDIF

            ENDIF    ! ############# END OF OFFLAG #############

         ENDIF    ! ############# END OF CHKLEVEL #############


!-----------------------------------------------------------------------
!.... Grid information has NOT been initialized => Initialized
!-----------------------------------------------------------------------

      ELSE

         GFLAG = .TRUE.
         GRDNM = GDNAM3D
         GDTYP = GDTYP3D
         P_ALP = P_ALP3D
         P_BET = P_BET3D
         P_GAM = P_GAM3D
         XCENT = XCENT3D
         YCENT = YCENT3D
         XORIG = XORIG3D
         YORIG = YORIG3D
         XCELL = XCELL3D
         YCELL = YCELL3D
         NCOLS = NC
         NROWS = NR
         NGRID = NCOLS * NROWS

         MESG = 'NOTE: Grid settings initialized using ' // 
     &          DATDESC // ' in ' // CRLF()// BLANK10 // 
     &          FILDESC( 1:L ) // '.'

         CALL M3MSG2( MESG )

      ENDIF

! ########################################
! ############# END OF GFLAG #############
! ########################################


      IF ( SFLAG ) THEN
         MESG = 'ERROR: Grid parameters for ' // DATDESC // ' in ' //
     &          CRLF() // BLANK10 // FILDESC( 1:L ) //
     &          ' are inconsistent with initialized values.'
         CALL M3MSG2( MESG )

         EFLAG = SFLAG
      ENDIF

      ! IF( SFLAG ) EFLAG = SFLAG

      RETURN


!***********************************************************************
!******************  FORMAT  STATEMENTS   ******************************
!***********************************************************************

!.... Formatted file I/O formats ..... 93xxx

93000 FORMAT( A )

!.... Internal buffering formats ..... 94xxx

94010 FORMAT( 10( A, :, I8, :, 1X ) )

      END

