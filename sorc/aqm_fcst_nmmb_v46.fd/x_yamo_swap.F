
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
C $Header: /project/work/rep/CCTM/src/hadv/yamo/x_yamo.F,v 1.1 2005/06/22 16:37:34 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE X_YAMO ( FDATE, FTIME, TSTEP, LVL, BCON, XTRHOJ )

C-----------------------------------------------------------------------
C Function:
C   Advection in the horizontal plane, Y-direction first
      
C Preconditions:
      
C Subroutines and functions called:
 
C Revision history:
C  19 Jan 2004: Jeff Young

C   1 Nov 06: Jeff Young - Following Glenn Hammond, moved all communication
C   out of HPPM to this level; using "swap_sandia" communication; update only
C   local values in the CGRID array within a time step, discarding previous
C   ghost values.
      
C-----------------------------------------------------------------------

      USE CGRID_DEFN            ! inherits GRID_CONF and CGRID_SPCS
      USE SUBST_MODULES         ! stenex
!     USE SUBST_COMM_MODULE     ! stenex
!     USE SUBST_UTIL_MODULE     ! stenex

      USE SWAP_SANDIA

#ifdef snl_timing
      use timing
#endif

      IMPLICIT NONE
      
C Includes:

!     INCLUDE SUBST_VGRD_ID     ! vertical dimensioning parameters
      INCLUDE SUBST_GC_SPC      ! gas chemistry species table
      INCLUDE SUBST_GC_ADV      ! gas chem advection species and map table
      INCLUDE SUBST_AE_ADV      ! aerosol advection species and map table
      INCLUDE SUBST_NR_ADV      ! non-react advection species and map table
      INCLUDE SUBST_TR_ADV      ! tracer advection species and map table
      INCLUDE SUBST_CONST       ! constants
      INCLUDE SUBST_IOPARMS     ! I/O parameters definitions
      INCLUDE SUBST_IODECL      ! I/O definitions and declarations
!     INCLUDE SUBST_FILES_ID    ! file name parameters
!     INCLUDE SUBST_COORD_ID    ! coordinate & domain definitions (req IOPARMS)
      INCLUDE SUBST_PE_COMM     ! PE communication displacement and direction

C Arguments:
      
      INTEGER     FDATE         ! current model date, coded YYYYDDD
      INTEGER     FTIME         ! current model time, coded HHMMSS
      INTEGER     TSTEP         ! time step (HHMMSS)
      INTEGER     LVL           ! layer
      REAL        BCON( NBNDY,* )       ! boundary concentrations
!     REAL        XTRHOJ( 0:NCOLS,NROWS )
      REAL        XTRHOJ( :,: ) ! subroutine argument entry as (1,NCOLS+1,NROWS)

C External Functions not declared in IODECL3.EXT:
      
      INTEGER, EXTERNAL :: SEC2TIME, TIME2SEC, setup_logdev
      REAL,    EXTERNAL :: ZFDBC
      
C Parameters:

C Advected species dimension

      INTEGER, PARAMETER :: N_SPC_ADV = N_GC_ADV
     &                                + N_AE_ADV
     &                                + N_NR_ADV
     &                                + N_TR_ADV
     &                                + 1       ! for advecting RHO*SqRDMT
 
C File Variables:
 
      REAL         UHAT( NCOLS+1,NROWS+1 )       ! x1-component CX-velocity

C Local Variables:

      REAL         TRRHOJ( 0:NCOLS )

      CHARACTER( 16 ) :: PNAME = 'X_YAMO'
      LOGICAL, SAVE :: FIRSTIME = .TRUE.

      integer, save :: logdev

      CHARACTER( 96 ) :: XMSG = ' '

      REAL          DX1                         ! dx1 (meters)
      INTEGER, SAVE :: ASPC                     ! RHOJ index in CGRID
      REAL          ADJFAC                      ! Yamo's adjustment - jp 11/02
      REAL          ADJFAC_L

      REAL, ALLOCATABLE, SAVE :: DSX( : ),      ! ds = DX1
     &                           VELX( : ),     ! Velocities along a column
     &                           CONX( :,: )    ! Conc's along a column

      REAL          DT                          ! TSTEP in sec
      INTEGER       ALLOCSTAT

      INTEGER, SAVE :: ADV_MAP( N_SPC_ADV )     ! global adv map to CGRID

      INTEGER      COL, ROW, SPC, VAR           ! loop counters
      INTEGER      A2C

      CHARACTER( 16 ) :: X1VEL = 'X1VEL'

      INTEGER MY_TEMP
!     INTEGER, SAVE :: STARTCOL, ENDCOL, STARTCOL2, ENDCOL2, ENDCOL2_LIM
      INTEGER, SAVE :: STARTCOL2, ENDCOL2, ENDCOL2_LIM
      LOGICAL, SAVE :: BNDY_PE_LOX, BNDY_PE_HIX

      integer, parameter :: swap_width = 3
      real bound_east( swap_width,nrows,n_spc_adv )
      real bound_west( swap_width,nrows,n_spc_adv )
      real buf_ew( swap_width*nrows*n_spc_adv )
      integer east_col, m
      
C Statement functions:

      INTEGER, SAVE :: EFX    ! fixed parameter for eastern boundary
      INTEGER, SAVE :: WFX    ! fixed parameter for western boundary

      REAL    BCCN            ! boundary concentrations stmt fn

      INTEGER BFX             ! dummy positional parameter
      INTEGER CR              ! row or column index
      INTEGER SS              ! species index

      BCCN ( BFX, CR, SS ) = BCON( BFX + CR, SS )

C Required interface for allocatable array dummy arguments

      INTERFACE
#ifdef snl_timing
         SUBROUTINE HCONTVEL ( FDATE, FTIME, TSTEP, LVL, UORV, UHAT, source )
#else
         SUBROUTINE HCONTVEL ( FDATE, FTIME, TSTEP, LVL, UORV, UHAT )
#endif
            IMPLICIT NONE
            INTEGER,         INTENT( IN )     :: FDATE, FTIME, TSTEP, LVL
            CHARACTER( 16 ), INTENT( IN )     :: UORV
            REAL,            INTENT( OUT )    :: UHAT( :,: )
#ifdef snl_timing
            INTEGER,         INTENT( IN )     :: source
#endif
         END SUBROUTINE HCONTVEL
         SUBROUTINE HPPM ( NI, CON, VEL, DT, DS, ORI )
            IMPLICIT NONE
            INTEGER,         INTENT( IN )     :: NI
            REAL,            INTENT( IN OUT ) :: CON( :,: )
            REAL,            INTENT( IN )     :: VEL( : )
            REAL,            INTENT( IN )     :: DT
            REAL,            INTENT( IN )     :: DS ( : )
            CHARACTER,       INTENT( IN )     :: ORI
         END SUBROUTINE HPPM
      END INTERFACE
C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
!        logdev = init3 ()
         logdev = setup_logdev()

         EFX = MY_NCOLS + 1
         WFX = 2 * MY_NCOLS + MY_NROWS + 4

C Get dx1 from HGRD_DEFN module

         IF ( GDTYP_GD .EQ. LATGRD3 ) THEN
            DX1 = DG2M * XCELL_GD
     &          * COS( PI180*( YORIG_GD + YCELL_GD*FLOAT( GL_NROWS/2 ))) ! in m.
            ELSE
            DX1 = XCELL_GD        ! in m.
            END IF

         ALLOCATE ( DSX(  -2:MY_NCOLS+3 ),
     &              CONX( -2:MY_NCOLS+3,N_SPC_ADV ), STAT = ALLOCSTAT )
         DO COL = -2, MY_NCOLS + 3
            DSX( COL ) = DX1
         END DO

#ifdef snl_timing
         call start_timing( hadv_int, comm_int, 2 )
#endif
         call swap2d( dsx( 1 ), dsx( my_ncols+1 ), swap_width, 1, 1, EAST )
         call swap2d( dsx( my_ncols-2 ), dsx( -2 ), swap_width, 1, 1, WEST )
#ifdef snl_timing
         call stop_timing( hadv_int, comm_int )
#endif

         ALLOCATE ( VELX( MY_NCOLS+1 ), STAT = ALLOCSTAT ) ! Vel along a row
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DSX, VELX, or CONX'
            CALL M3EXIT ( PNAME, FDATE, FTIME, XMSG, XSTAT1 )
         END IF

C Get CGRID offsets
 
         CALL CGRID_MAP( NSPCSD, GC_STRT, AE_STRT, NR_STRT, TR_STRT )

         ASPC = GC_STRT - 1 + N_GC_SPCD

C Create global map to CGRID
 
!        write( logdev,* ) ' advmap'
         SPC = 0
         DO VAR = 1, N_GC_ADV
            SPC = SPC + 1
            ADV_MAP( SPC ) = GC_STRT - 1 + GC_ADV_MAP( VAR )
!           write( logdev,* ) spc, adv_map( spc ), gc_spc( gc_adv_map( var ) )
            END DO
         DO VAR = 1, N_AE_ADV
            SPC = SPC + 1
            ADV_MAP( SPC ) = AE_STRT - 1 + AE_ADV_MAP( VAR )
!           write( logdev,* ) spc, adv_map( spc ), ae_spc( ae_adv_map( var ) )
            END DO
         DO VAR = 1, N_NR_ADV
            SPC = SPC + 1
            ADV_MAP( SPC ) = NR_STRT - 1 + NR_ADV_MAP( VAR )
!           write( logdev,* ) spc, adv_map( spc ), nr_spc( nr_adv_map( var ) )
            END DO
         DO VAR = 1, N_TR_ADV
            SPC = SPC + 1
            ADV_MAP( SPC ) = TR_STRT - 1 + TR_ADV_MAP( VAR )
            END DO
 
         ADV_MAP( N_SPC_ADV ) = ASPC
!        spc = n_spc_adv
!        write( logdev,* ) spc, adv_map( spc ), 'rhoj'

!        CALL SUBST_LOOP_INDEX ( 'C', 1, MY_NCOLS, 1, MY_TEMP,
!    &                           STARTCOL, ENDCOL )

         CALL SUBST_LOOP_INDEX ( 'C', 2, MY_NCOLS, 1, MY_TEMP,
     &                           STARTCOL2, ENDCOL2 )

         CALL SUBST_HI_LO_BND_PE ( 'C', BNDY_PE_LOX, BNDY_PE_HIX )

         IF ( BNDY_PE_HIX ) THEN
            ENDCOL2_LIM = ENDCOL2
            ELSE
            ENDCOL2_LIM = ENDCOL2 + 1
            END IF

!        write( logdev,2005 ) bndy_pe_lox, bndy_pe_hix,
!    &                        startcol, endcol, startcol2, endcol2, endcol2_lim
!        call flush_
2005  format(  '#x_yamo> :', 2l6, 2x, 5i6.3 )

         END IF                    ! if firstime

      DT = FLOAT ( TIME2SEC ( TSTEP ) )

C Do the computation for x advection

C Get the contravariant x1 velocity component

#ifdef snl_timing
      CALL HCONTVEL ( FDATE, FTIME, TSTEP, LVL, X1VEL, UHAT, hadv_int )
#else
      CALL HCONTVEL ( FDATE, FTIME, TSTEP, LVL, X1VEL, UHAT )
#endif

#ifdef snl_timing
      call start_timing( hadv_int, comm_int, 1 )
#endif
      call swap2d( uhat( 1,1 ), uhat( my_ncols+1,1 ), 1, my_nrows+1,
     &             my_ncols+1, EAST )
#ifdef snl_timing
      call stop_timing( hadv_int, comm_int )
#endif
      m = 0
      east_col = my_ncols - swap_width
      do spc = 1, n_spc_adv
         a2c = adv_map( spc )
         do row = 1, my_nrows
            do col = 1, swap_width
               bound_west( col,row,spc ) = cgrid( col,row,lvl,a2c )
               bound_east( col,row,spc ) = cgrid( east_col+col,row,lvl,a2c )
               m = m + 1
               buf_ew( m ) = bound_east( col,row,spc )
            end do
         end do
      end do

C swap3d interface (swap2d similar) ...
C     swap3d( 1st value of array to be sent,
C             1st value of array to be received,
C             number of values to send in 1st dimension of array,
C             number of values to send in 2nd dimension of array,
C             number of values to send in 3rd dimension of array,
C             size of 1st dimension in arrays,
C             size of 2nd dimension in arrays,
C             direction to receive from: NSEW)

#ifdef snl_timing
      call start_timing( hadv_int, comm_int, 2 )
#endif
      call swap3d( bound_west, bound_east, swap_width, my_nrows, n_spc_adv,
     &             swap_width, my_nrows, EAST )
      call swap3d( buf_ew,     bound_west, swap_width, my_nrows, n_spc_adv,
     &             swap_width, my_nrows, WEST )
#ifdef snl_timing
      call stop_timing( hadv_int, comm_int )
#endif

      DO 253 ROW = 1, MY_NROWS

!        DO COL = STARTCOL, ENDCOL      !     DO COL = 1, MY_NCOLS+1
         DO COL = 1, MY_NCOLS+1
            VELX( COL ) = UHAT( COL,ROW )
         END DO

         TRRHOJ( 1:NCOLS ) = CGRID( 1:NCOLS,ROW,LVL,ASPC )

         trrhoj( 0 ) = bound_west( swap_width,row,n_spc_adv )

         IF ( BNDY_PE_LOX ) THEN
            ADJFAC = XTRHOJ( 2,ROW ) / TRRHOJ( 1 )
            IF ( VELX( 1 ) .LT. 0.0 ) VELX( 1 ) = VELX( 1 ) * ADJFAC
         END IF

         DO COL = STARTCOL2, ENDCOL2

            IF ( COL .EQ. STARTCOL2 ) THEN
               ADJFAC_L = XTRHOJ( COL,ROW ) / TRRHOJ( COL-1 )
            ELSE
               ADJFAC_L = ADJFAC
            END IF

            IF ( COL .LT. ENDCOL2_LIM ) ADJFAC = XTRHOJ( COL+1,ROW )
     &                                         / TRRHOJ( COL )

            IF ( VELX( COL ) .LT. 0.0 ) THEN
               VELX( COL ) = VELX( COL ) * ADJFAC
            ELSE IF ( VELX( COL ) .GT. 0.0 ) THEN
               VELX( COL ) = VELX( COL ) * ADJFAC_L
            END IF

!           if ( velx( col ) .lt. 0.0 ) then
!              if ( adjfac .lt. 0.70 .or. adjfac .gt. 1.36 ) then
!                 write( logdev,2009 ) col, row, lvl, adjfac, velx(col)
!              end if   !
!           else
!              if ( adjfac_l .lt. 0.70 .or. adjfac_l .gt. 1.36 ) then
!                 write( logdev,2011 ) col, row, lvl, adjfac_l, velx(col)
!              end if   !
!           end if
2009        format(  '#x_yamo> col, row, lvl, adjfac,   velx: ',
     &                3i3, 1x, 2f7.2 )
2011        format(  '#x_yamo> col, row, lvl, adjfac_l, velx: ',
     &                3i3, 1x, 2f7.2 )

         END DO

#ifdef snl_timing
         call start_timing( hadv_int, comm_int, 1 )
#endif
         call swap1d( velx( 1 ), velx( my_ncols+1 ), 1, EAST )
#ifdef snl_timing
         call stop_timing( hadv_int, comm_int )
#endif

         DO SPC = 1, N_SPC_ADV

            A2C = ADV_MAP( SPC )
            DO COL = 1, MY_NCOLS
               CONX( COL,SPC ) = CGRID( COL,ROW,LVL,A2C )
            END DO

            do col = 1, swap_width
               conx( col-swap_width,spc ) = bound_west( col,row,spc )
               conx( my_ncols+col,spc )   = bound_east( col,row,spc )
            end do

C West boundary

            IF ( BNDY_PE_LOX ) THEN
               IF ( VELX( 1 ) .LT. 0.0 ) THEN          ! outflow
                  CONX( 1-swap_width:0,SPC ) =
     &                            ZFDBC ( CONX( 1,SPC ), CONX( 2,SPC ),
     &                                    VELX( 1 ),     VELX( 2 ) )
               ELSE    ! inflow
                  CONX( 1-swap_width:0,SPC ) = BCCN( WFX,ROW,SPC )
               END IF
            END IF

C East boundary

            IF ( BNDY_PE_HIX ) THEN
               IF ( VELX( MY_NCOLS+1 ) .GT. 0.0 ) THEN     ! outflow
                  CONX( MY_NCOLS+1:MY_NCOLS+swap_width,SPC ) =
     &                                     ZFDBC ( CONX( MY_NCOLS,SPC ),
     &                                             CONX( MY_NCOLS-1,SPC ),
     &                                             CONX( MY_NCOLS-1,SPC ),
     &                                             VELX( MY_NCOLS+1 ),
     &                                             VELX( MY_NCOLS ) )
               ELSE    ! inflow
                  CONX( MY_NCOLS+1:MY_NCOLS+swap_width,SPC ) =
     &                                     BCCN( EFX,ROW,SPC )
               END IF
            END IF

         END DO

!        if ( lvl .eq. 1 .and. row .eq. 14 ) then
!           write( logdev,* ) '@@ x_yamo# time, col, velx'
!           do col = 30, my_ncols+1
!              write( logdev,2017 ) ftime, col, velx( col )
!           end do
!        end if
2017  format( '@@', 5x, i7.6, i4, 1pe12.3 )

C PPM scheme

         CALL HPPM ( MY_NCOLS, CONX, VELX, DT, DSX, 'C' )

         DO SPC = 1, N_SPC_ADV
            A2C = ADV_MAP( SPC )
            DO COL = 1, MY_NCOLS
               CGRID( COL,ROW,LVL,A2C ) = CONX( COL,SPC )
!              if ( conx( col,spc ) .le. 0.0 )
!    &            write( logdev,2019 ) ftime, col, row, lvl, spc, conx(col,spc )
            END DO
         END DO

2019  format( 'x_yamo# time, c, r, l, s, conx: ',
     &         I7.6, 4I4, 1pe12.3 )

253   CONTINUE

      RETURN
      END
