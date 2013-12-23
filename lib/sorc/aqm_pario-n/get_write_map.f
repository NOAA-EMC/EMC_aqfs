
C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/air4/yoj/rel/models/PARIO/src/get_write_map.f,v 1.1.1.1 2003/05/05 17:18:09 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      SUBROUTINE GET_WRITE_MAP( NP, NPR, NPC, GNROWS, GNCOLS,
     &                          NROWS3D, NCOLS3D, NLAYS3D,
     &                          NROWS_PE, NCOLS_PE, ROWSX_PE, COLSX_PE,
     &                          WR_NROWS_PE, WR_NCOLS_PE,
     &                          WR_ROWSX_PE, WR_COLSX_PE )
C.....................................................................
C
C  PURPOSE:  Determine the processor-to-grid map for the grid
C            to be written.
C
C
C  REVISION HISTORY: 
C       Original version  1/1999 by Al Bourgeois, to allow pwrite3 to
C              write output on a subgrid.
C       Modified 08/06/1999 by Al Bourgeois to make this a subroutine
C              instead of a function.
C       Modified 10/08/01 by David Wong
C         -- added a missing variable IERR in the SUBDMAP calling arguments
C       Modified 12/31/02 by David Wong
C         -- extended to handle dot file
C
C
C  ARGUMENT LIST DESCRIPTION:
C  IN:
C     INTEGER  NP                    ! Number of processors.
C     INTEGER  NPR                   ! Number of processors across grid rows.
C     INTEGER  NPC                   ! Number of processors across grid cols.
C     INTEGER  GNROWS                ! Row dimension of global domain.
C     INTEGER  GNCOLS                ! Column dimension of global domain.
C     INTEGER  NROWS3D               ! Row dimension of file variables.
C     INTEGER  NCOLS3D               ! Column dimension of file variables.
C     INTEGER  NLAYS3D               ! Layer dimension of file variable.
C     INTEGER  NROWS_PE(NP)          ! Number rows in each processor.
C     INTEGER  NCOLS_PE(NP)          ! Number columns in each processor.
C     INTEGER  ROWSX_PE(2,NP)        ! Row range for each PE.
C     INTEGER  COLSX_PE(2,NP)        ! Column range for each PE.
C
C  OUT:
C     INTEGER  WR_NROWS_PE(NP)       ! No. rows each PE of subgrid to write.
C     INTEGER  WR_NCOLS_PE(NP)       ! No. cols each PE of subgrid to write.
C     INTEGER  WR_ROWSX_PE(2,NP)     ! Row range each PE of subgrid to write.
C     INTEGER  WR_COLSX_PE(2,NP)     ! Col range each PE of subgrid to write.
C
C
C  LOCAL VARIABLE DESCRIPTION:  see below
C
C  CALLS: SUBDMAP
C
C........................................................................
C
        IMPLICIT  NONE
C
C.......   ARGUMENTS:

      INTEGER  NP                    ! Number of processors.
      INTEGER  NPR                   ! Number of processors across grid rows.
      INTEGER  NPC                   ! Number of processors across grid cols.
      INTEGER  GNROWS                ! Row dimension of global domain.
      INTEGER  GNCOLS                ! Column dimension of global domain.
      INTEGER  NROWS3D               ! Row dimension of file variables.
      INTEGER  NCOLS3D               ! Column dimension of file variables.
      INTEGER  NLAYS3D               ! Layer dimension of file variable.
      INTEGER  NROWS_PE(NP)          ! Number rows in each processor.
      INTEGER  NCOLS_PE(NP)          ! Number columns in each processor.
      INTEGER  ROWSX_PE(2,NP)        ! Row range for each PE.
      INTEGER  COLSX_PE(2,NP)        ! Column range for each PE.
      INTEGER  WR_NROWS_PE(NP)       ! No. rows each PE of subgrid to write.
      INTEGER  WR_NCOLS_PE(NP)       ! No. cols each PE of subgrid to write.
      INTEGER  WR_ROWSX_PE(2,NP)     ! Row range each PE of subgrid to write.
      INTEGER  WR_COLSX_PE(2,NP)     ! Col range each PE of subgrid to write.

C
C.......   LOCAL VARIABLES: 

      INTEGER      I             ! Loop index.
      INTEGER      IDUMMY        ! Dummy argument to SUBDMAP, not used.]
      INTEGER      IERR          ! Return Error code

C........................................................................
C     begin subroutine GET_WRITE_MAP
C

C......   Determine the processor-to-subdomain mapping for the grid to
C......   be written. If the file variables to be written are defined on
C......   the entire (global) domain, load the previously defined
C......   decomposition map. Otherwise, get the new mapping on the subgrid.

c     IF( NROWS3D*NCOLS3D .EQ. GNROWS*GNCOLS ) THEN
      IF (( NROWS3D .EQ. GNROWS) .AND. ( NCOLS3D .EQ. GNCOLS )) THEN

C......   Load the full-grid processor-to-subdomain mapping.

         DO I = 1, NP
           WR_NROWS_PE(I) = NROWS_PE(I)
           WR_NCOLS_PE(I) = NCOLS_PE(I)
           WR_ROWSX_PE(1,I) = ROWSX_PE(1,I)
           WR_ROWSX_PE(2,I) = ROWSX_PE(2,I)
           WR_COLSX_PE(1,I) = COLSX_PE(1,I)
           WR_COLSX_PE(2,I) = COLSX_PE(2,I)
         END DO

      ELSE IF (( NROWS3D .EQ. GNROWS+1) .AND. ( NCOLS3D .EQ. GNCOLS+1 )) THEN

C......   Load the dot full-grid processor-to-subdomain mapping.

         WR_NCOLS_PE = NCOLS_PE
         WR_COLSX_PE = COLSX_PE
         WR_COLSX_PE = COLSX_PE
         WR_NROWS_PE = NROWS_PE
         WR_ROWSX_PE = ROWSX_PE
         WR_ROWSX_PE = ROWSX_PE

         DO I = NPC, NP, NPC
           WR_NCOLS_PE(I) = NCOLS_PE(I) + 1
           WR_COLSX_PE(2,I) = COLSX_PE(2,I) + 1
         END DO

         DO I = NP, NP-NPC+1, -1
           WR_NROWS_PE(I) = NROWS_PE(I) + 1
           WR_ROWSX_PE(2,I) = ROWSX_PE(2,I) + 1
         END DO

      ELSE

C......   Get the subgrid processor-to_subdomain mapping.

C.......   Calculate processor-to-subdomain maps.
        CALL SUBDMAP( NP, NROWS3D, NCOLS3D, NLAYS3D, NPC, NPR,
     &                WR_NROWS_PE, WR_NCOLS_PE,
     &                WR_ROWSX_PE, WR_COLSX_PE, IDUMMY, IERR)

      END IF

     
      RETURN
      END
