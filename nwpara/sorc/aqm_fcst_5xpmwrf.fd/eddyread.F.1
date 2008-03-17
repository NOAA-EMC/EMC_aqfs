
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/CCTM/src/vdiff/eddy/edyintb.F,v 1.10 2002/04/05 18:23:43 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)edyintb.F 1.2 /project/mod3/CMAQ/src/vdiff/eddy/SCCS/s.edyintb.F 16 Jun 1997 15:47:48

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE EDDYREAD ( EDDYV, DT, JDATE, JTIME, TSTEP )

C-----------------------------------------------------------------------
C Function:
C   Reads vertical eddy diffusivity from MET_CRO_3D.
C   Calculates DT following edyintb algorithm.
 
C Preconditions:

C Subroutines and Functions Called:
C   INIT3, M3EXIT, SEC2TIME, TIME2SEC

C Revision history:
C  NO.   DATE     WHO    WHAT
C      15 Apr 02  TLO   Original version.
C      15 Jul 02  YOJ   dyn alloc - Use HGRD_DEFN; replace INTERP3 with INTERPX
C      29 Oct 05  YOJ   dyn. vert. layers
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications

      IMPLICIT NONE
 
C Includes:
 
!     INCLUDE SUBST_HGRD_ID     ! horizontal dimensioning parameters
!     INCLUDE SUBST_VGRD_ID     ! vertical dimensioning parameters
      INCLUDE SUBST_CONST       ! constants
      INCLUDE SUBST_IOPARMS     ! I/O parameters definitions
      INCLUDE SUBST_IOFDESC     ! file header data structuer
      INCLUDE SUBST_IODECL      ! I/O definitions and declarations
      INCLUDE SUBST_FILES_ID    ! file name parameters
!     INCLUDE SUBST_COORD_ID    ! coord. and domain definitions (req IOPARMS)

C Arguments:

      REAL         EDDYV ( NCOLS,NROWS,NLAYS ) ! eddy diffusivity (m**2/s)
      REAL         DT    ( NCOLS,NROWS )       ! computed diffusion time step
!     REAL      :: EDDYV ( :,:,: )     ! eddy diffusivity (m**2/s)
!     REAL      :: DT    ( :,: )       ! computed diffusion time step
      INTEGER      JDATE        ! current model date , coded YYYYDDD
      INTEGER      JTIME        ! current model time , coded HHMMSS
      INTEGER      TSTEP        ! sciproc sync. step (chem)

C External Functions not previously declared in IODECL3.EXT:

      INTEGER, EXTERNAL :: SEC2TIME, TIME2SEC, SETUP_LOGDEV

      CHARACTER*30 MSG1 
!                  123456789012345678901234567890
      DATA MSG1 / ' Error interpolating variable ' /

C File Variables:

      REAL         ZH    ( NCOLS,NROWS,NLAYS )    ! mid-layer elevation
      REAL         ZF    ( NCOLS,NROWS,0:NLAYS )  ! full layer elevation

C Local variables:

      LOGICAL,SAVE :: FIRSTIME = .TRUE.

      CHARACTER( 16 ) :: PNAME = 'EDDYREAD'
      CHARACTER( 16 ) :: VNAME
!     CHARACTER( 16 ) :: UNITSCK
      CHARACTER( 120 ) :: XMSG = ' '

      INTEGER      MDATE, MTIME, STEP
      INTEGER      C, R, L

      REAL         DTSEC

      INTEGER      GXOFF, GYOFF            ! global origin offset from file
C for INTERPX
      INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3

      INTEGER, SAVE :: LOGDEV

C-----------------------------------------------------------------------

      IF ( FIRSTIME )  THEN
         FIRSTIME = .FALSE.
!        LOGDEV = INIT3()
         LOGDEV = SETUP_LOGDEV ()

         IF ( .NOT. OPEN3( MET_CRO_3D, FSREAD3, PNAME ) ) THEN
            XMSG = 'Could not open '// MET_CRO_3D // ' file'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
                          
         IF ( .NOT. DESC3( MET_CRO_3D ) ) THEN
            XMSG = 'Could not get ' // MET_CRO_3D // ' file description'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF         !  error abort if if desc3() failed

         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

         END IF          !  if firstime

C Interpolate time dependent one-layer and layered input variables

      MDATE  = JDATE
      MTIME  = JTIME
      STEP   = TIME2SEC( TSTEP )
      CALL NEXTIME( MDATE, MTIME, SEC2TIME( STEP / 2 ) )

      DTSEC = FLOAT( STEP )

      VNAME = 'ZF'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MDATE, MTIME, ZF ) ) THEN
         XMSG = MSG1 // VNAME // ' from ' // MET_CRO_3D
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C Move 3rd dimension slabbed data from INTERP3 into proper order
C ( Using ZF as a read buffer and an argument variable.)
 
      DO L = NLAYS, 1, -1
         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS
               ZF( C,R,L ) = ZF( C,R,L-1 )
               END DO
            END DO
         END DO

      DO R = 1, MY_NROWS
         DO C = 1, MY_NCOLS
            ZF( C,R,0 ) = 0.0
            DT( C,R   ) = DTSEC
            END DO
         END DO

      VNAME = 'ZH'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MDATE, MTIME, ZH ) ) THEN
         XMSG = MSG1 // VNAME // ' from ' // MET_CRO_3D
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

      VNAME = 'EDDYV'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MDATE, MTIME, EDDYV ) ) THEN
         XMSG = MSG1 // VNAME // ' from ' // MET_CRO_3D
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

      DO L = 1, NLAYS-1
        DO R = 1, MY_NROWS
           DO C = 1, MY_NCOLS

             DT( C,R ) = MIN( DT( C,R ),
     &                        0.75 * ( ZF( C,R,L )   - ZF( C,R,L-1 ) )
     &                             * ( ZH( C,R,L+1 ) - ZH( C,R,L ) )
     &                             / EDDYV( C,R,L ) )

             END DO
           END DO
         END DO

!     do l = 1, nlays
!        do r = 1, my_nrows
!           do c = 1, my_ncols
!              if ( eddyv( c,r,l ) .lt. 0 .or.
!    &              eddyv( c,r,l ) .gt. 200.0 ) then
!                 print *, 'eddyread - c,r,l,eddyv: ', c,r,l, eddyv( c,r,l )
!                 end if
!              end do
!           end do
!        end do
 
      RETURN
      END
