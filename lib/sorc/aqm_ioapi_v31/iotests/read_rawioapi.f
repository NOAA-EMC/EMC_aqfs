      PROGRAM READ_RAWIOAPI

C  This program reads obs upper air data that have been written to a 
C  NetCDF file.
C
C  Written by MCNC, copyright 2002.

      IMPLICIT NONE

      INCLUDE 'PARMS3.EXT'    !  I/O API parameters
      INCLUDE 'IODECL3.EXT'   !  I/O API function declarations
      INCLUDE 'FDESC3.EXT'    !  I/O API file description data structures.

C........  Enternal functions

      LOGICAL         DSCGRID
      INTEGER         LDEV      !  UNIT NUMBER FOR LOG FILE
      INTEGER         PROMPTFFILE
      CHARACTER*5     STRING
      INTEGER         JULIAN, JSTEP3
      CHARACTER*16    PROMPTMFILE
      CHARACTER*16    PROGNAME
      CHARACTER*16    RAWOBS
      DATA            PROGNAME /'READ_RAWIOAPI'/
      REAL            GETREAL
      LOGICAL         GETYN

      EXTERNAL        JULIAN, DSCGRID, PROMPTFFILE,
     &                PROMPTMFILE, GETREAL, GETYN

      CHARACTER*256   MESG      !  message buffer

      integer  inmxlvl,inmxstn,jdate,jtime,nprof,tstep,i
      parameter (inmxlvl=200,inmxstn=200)

      integer   inid(inmxstn),inlevs(inmxstn)
      real      intd(inmxlvl,inmxstn),inws(inmxlvl,inmxstn),
     &          intf(inmxlvl,inmxstn),inpr(inmxlvl,inmxstn),
     &          inht(inmxlvl,inmxstn),inwd(inmxlvl,inmxstn)
      REAL      inlat(inmxstn),inlon(inmxstn),inelev(inmxstn)

      COMMON /BAR/ NPROF,INID,INLEVS,INLON,INLAT,INELEV,
     &             INHT,INPR,INTF,INTD,INWD,INWS 

      LDEV = INIT3 ( )        !  Start up I/O API

      RAWOBS = PROMPTMFILE(
     &       'Enter logical name for RAWOBS input file',
     &        FSREAD3, 'RAWOBS', PROGNAME )

      IF ( .NOT. DESC3( RAWOBS ) ) THEN
          MESG = 'Could not get description of file  ' //
     &           RAWOBS
          CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      END IF

      jdate = SDATE3D
      jtime = STIME3D
      tstep = TSTEP3D

      print *, ' jdate: ', jdate
      print *, ' jtime: ', jtime
      IF ( .NOT.READ3( RAWOBS, 'ALL', ALLAYS3, JDATE,
     &                  JTIME, NPROF ) ) THEN
           MESG = 'Could not read from file'
           CALL M3EXIT( PROGNAME, JDATE, JTIME, MESG, 2 )
      END IF

      print *, ' Max sites (inmxstn): ', inmxstn
      print *, ' Max levels (inmxlvl): ', inmxlvl
      print *, ' nprof: ', nprof
      print *, ' inid(1): ', inid(1)
      print *, ' inid(6): ', inid(6)
      print *, ' inlevs(1): ', inlevs(1)
      print *, ' inlevs(6): ', inlevs(6)
      print *, ' inlon(1): ', inlon(1)
      print *, ' inlon(6): ', inlon(6)
      print *, ' inlat(1): ', inlat(1)
      print *, ' inlat(6): ', inlat(6)
      print *, ' inelev(1): ', inelev(1)
      print *, ' inelev(6): ', inelev(6)
      do i = 1, inlevs(1)
        write (*,222) inht(i,1), inpr(i,1), intf(i,1)
222     format ( ' Ht, pr, tf for stn 1: ', 3f9.2)
      end do
      do i = 1, inlevs(6)
        write (*,223) inht(i,6), inpr(i,6), intf(i,6)
223     format ( ' Ht, pr, tf for stn 6: ', 3f9.2)
      end do

      CALL NEXTIME(JDATE,JTIME,120000)

      print *, ' jdate: ', jdate
      print *, ' jtime: ', jtime
      IF ( .NOT.READ3( RAWOBS, 'ALL', ALLAYS3, JDATE,
     &                  JTIME, NPROF ) ) THEN
           MESG = 'Could not read from file'
           CALL M3EXIT( PROGNAME, JDATE, JTIME, MESG, 2 )
      END IF

      print *, ' Max sites (inmxstn): ', inmxstn
      print *, ' Max levels (inmxlvl): ', inmxlvl
      print *, ' nprof: ', nprof
      print *, ' inid(1): ', inid(1)
      print *, ' inid(6): ', inid(6)
      print *, ' inlevs(1): ', inlevs(1)
      print *, ' inlevs(6): ', inlevs(6)
      print *, ' inlon(1): ', inlon(1)
      print *, ' inlon(6): ', inlon(6)
      print *, ' inlat(1): ', inlat(1)
      print *, ' inlat(6): ', inlat(6)
      print *, ' inelev(1): ', inelev(1)
      print *, ' inelev(6): ', inelev(6)
      do i = 1, inlevs(1)
        write (*,222) inht(i,1), inpr(i,1), intf(i,1)
      end do
      do i = 1, inlevs(6)
        write (*,223) inht(i,6), inpr(i,6), intf(i,6)
      end do

      CALL M3EXIT(PROGNAME,0,0,'Normal completion',0)

      stop

      end

