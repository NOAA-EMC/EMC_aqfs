C.........................................................................
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003 Baron Advanced Meteorological Systems
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

#ifdef IOAPICPL

      LOGICAL FUNCTION INTERP3V( FID, VID, JDATE, JTIME, BUFFER )

      IMPLICIT NONE

C...........   INCLUDES:

      INCLUDE 'PARMS3.EXT'
      INCLUDE 'STATE3.EXT'
      INCLUDE 'STATE3V.EXT'


C...........   ARGUMENTS and their descriptions:

      INTEGER       FID         !  file subscript for STATE3 arrays
      INTEGER       VID         !  variable subscript for STATE3 arrays
      INTEGER       JDATE       !  date, formatted YYYYDDD
      INTEGER       JTIME       !  time, formatted HHMMSS
      REAL          BUFFER(*)   !  interpolation-output buffer array


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

      REAL          P, Q        !  fractions used for interpolation.
      LOGICAL       RFLAG       !  BUFINT3() read-data flag
      INTEGER       DELTA       !  dummy variable
      INTEGER       DIMS( 5 )   !  dummy variable
      INTEGER       DELS( 5 )   !  dummy variable
      INTEGER       IP
      INTEGER       DT          !  seconds in TSTEP3
      INTEGER       DTJP        !  difference LDATE:LTIME to JDATE:JTIME
      integer       tstep

      integer pdate, ptime, fdate, ftime


C...........   EXTERNAL FUNCTIONS and their descriptions:

      INTEGER       BUFVGT3     !  alloc/interp from circ. bufs. (C)
      INTEGER       BUFVGT3D    !  alloc/interp from circ. bufs. (C)
      INTEGER       BUFINT3     !  alloc/interp from circ. bufs. (C)
      INTEGER       BUFINT3D    !  alloc/interp from circ. bufs. (C)
      LOGICAL       CURRSTEP    !  finds start of timestep
      INTEGER       SECSDIFF    !  computes date&time differences
      INTEGER       TIME2SEC    !  converts time to seconds

      EXTERNAL  BUFVGT3, BUFINT3, BUFVGT3D, BUFINT3D, CURRSTEP,
     $          SECSDIFF, TIME2SEC


      integer count
      logical pl, pn, fl, fn

      logical vir2buf
      external vir2buf

      tstep = TSTEP3( fid )
      count = BSIZE3(fid)*NLAYS3(fid)

C.....we use the next call only to allocate memory for circular buffer

      rflag = .FALSE.
      IF ( VTYPE3( VID,FID ) .EQ. M3REAL ) THEN
          IF (0 .EQ. BUFVGT3( FID, VID, RFLAG,
     &                        DIMS, DELS, COUNT, DELTA, TSTEP ) ) THEN
             CALL M3WARN('INTERP3V', JDATE, JTIME, 'BUFVGT3 ERROR')
             INTERP3V = .FALSE.
             RETURN
          ENDIF
      ELSE IF ( VTYPE3( VID,FID ) .EQ. M3DBLE ) THEN
          IF (0 .EQ. BUFVGT3D( FID, VID, RFLAG,
     &                         DIMS, DELS, COUNT, DELTA, TSTEP ) ) THEN
             CALL M3WARN('INTERP3V', JDATE, JTIME, 'BUFVGT3D ERROR')
             INTERP3V = .FALSE.
             RETURN
          ENDIF
      END IF
         
      IF ( .NOT. CURRSTEP( JDATE, JTIME, SDATE3( FID ), STIME3( FID ),
     &     TSTEP, PDATE, PTIME ) ) THEN
         CALL M3WARN('INTERP3V', JDATE, JTIME, 'CURRSTEP ERROR')
         INTERP3V = .FALSE.
         RETURN
      ENDIF
      
      IF ( TSTEP .EQ. 0 ) THEN
         P=1.0
         Q=0.0
         IP=0
         IF ( (LDATE3(VID,FID) .EQ. IMISS3) .OR.
     $        (LTIME3(VID,FID) .EQ. IMISS3) ) THEN
            INTERP3V = VIR2BUF( FID, VID, 0, 0, 0, BUFFER)
         ENDIF
      ELSE

         dt   = TIME2SEC( tstep )
         dtjp = SECSDIFF( pdate, ptime, JDATE, JTIME )

         q = FLOAT( dtjp ) / FLOAT( dt ) !  Interpolation coefficients
         p = 1.0 - q            

         fdate = pdate
         ftime = ptime
         call NEXTIME( fdate, ftime, ABS(tstep) )

         pl = (pdate.eq.LDATE3(vid,fid)).and.(ptime.eq.LTIME3(vid,fid))
         pn = (pdate.eq.NDATE3(vid,fid)).and.(ptime.eq.NTIME3(vid,fid))
         fl = (fdate.eq.LDATE3(vid,fid)).and.(ftime.eq.LTIME3(vid,fid))
         fn = (fdate.eq.NDATE3(vid,fid)).and.(ftime.eq.NTIME3(vid,fid))

         if ((pl .and. pn) .or. (fl .and. fn) .or.
     $        (pl .and. fl) .or. (pn .and. fn)) then
            call m3warn('INTERP3V', JDATE, JTIME,
     $           'Inconsistent LDATE3/LTIME3 or NDATE3/NTIME3 ERROR')
            interp3v = .FALSE.
            return
         endif

         interp3v = .TRUE.
         if(pl) then
            ip=0
            if (.not. fn) then
               if (q .ne. 0.0) then
                  interp3v = vir2buf( fid, vid, 1, fdate, ftime, buffer)
               endif
            endif
         else if(pn) then
            ip=1
            if (.not. fl) then
               if (q .ne. 0.0) then
                  interp3v = vir2buf( fid, vid, 0, fdate, ftime, buffer)
               endif
            endif
         else if(fl) then
            ip=1
            if (p .ne. 0.0) then
               interp3v = vir2buf( fid, vid, 1, pdate, ptime, buffer)
            endif
         else if(fn) then
            ip=0
            if (p .ne. 0.0) then
               interp3v = vir2buf( fid, vid, 0, pdate, ptime, buffer)
            endif
         else
            ip=0
            if (p .ne. 0.0) then
               interp3v = vir2buf( fid, vid, 0, pdate, ptime, buffer)
            endif
            if (q .ne. 0.0) then
               interp3v = interp3v .and .
     $              vir2buf( fid, vid, 1, fdate, ftime, buffer)
            endif
         endif
      endif

      if (.not. interp3v) then
         call m3warn('INTERP3V', JDATE, JTIME, 'vir2buf ERROR')
         return
      endif

      ILAST3(vid,fid) = ip

      if ( 0 .ne. BUFINT3( fid, vid,NLAYS3( FID ) * BSIZE3( FID ),
     $                     ip, tstep, p, q, buffer ) ) then
         interp3v = .TRUE.
      else
         call m3warn('INTERP3V', JDATE, JTIME, 'BUFINT3 ERROR')
         interp3v=.FALSE.
      endif

      return
      end

C
C*********************************************************************
C
      logical function vir2buf( fid, vid, ip, kdate, ktime, buffer )

      implicit none

C...........   INCLUDES:

      INCLUDE 'PARMS3.EXT'
      INCLUDE 'STATE3.EXT'
      INCLUDE 'STATE3V.EXT'


C...........   ARGUMENTS and their descriptions:

      integer vid, fid, ip, kdate, ktime
      real buffer(*)

      integer count, skip, type
      logical read3v
      external read3v
      character *16 vname
      integer ret

      INTEGER       BUFPUT3
      EXTERNAL BUFPUT3

      count = BSIZE3(fid)*NLAYS3(fid)
      skip = 0
      type = VTYPE3(vid,fid)
      vname = VLIST3(vid,fid)

      vir2buf = READ3V(plist3(fid),vname,skip,kdate,ktime,
     $                 buffer,count,type)
      ret = BUFPUT3( fid, vid, count, ip, buffer)
      if (vir2buf .and. ret .ne. 0) then
Calt         LDATE3(vid+2*ip*MXVARS3*MXFILE3,fid) = kdate
Calt         LTIME3(vid+2*ip*MXVARS3*MXFILE3,fid) = ktime
         if (ip .eq. 0) then
            LDATE3(vid,fid) = kdate
            LTIME3(vid,fid) = ktime
         else
            NDATE3(vid,fid) = kdate
            NTIME3(vid,fid) = ktime
         endif
         vir2buf = .TRUE.
      else
Calt         LDATE3(vid+2*ip*MXVARS3*MXFILE3,fid) = IMISS3
Calt         LTIME3(vid+2*ip*MXVARS3*MXFILE3,fid) = IMISS3
         if (ip .eq. 0) then
            LDATE3(vid,fid) = IMISS3
            LTIME3(vid,fid) = IMISS3
         else
            NDATE3(vid,fid) = IMISS3
            NTIME3(vid,fid) = IMISS3
         endif
         vir2buf = .FALSE.
      endif
      return
      end

#endif
