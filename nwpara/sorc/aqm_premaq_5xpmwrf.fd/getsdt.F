SUBROUTINE getsdt (hdate, sdate, stime, gmt)

!-------------------------------------------------------------------------------
! Name:     Get SDATE, STIME, and GMT
! Purpose:  Compute SDATE, STIME, and GMT from MM5-type date.
! Revised:  10 Sep 2001  Original version.  (T. Otte)
!           07 Jan 2003  Modified for PREMAQ.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER                      :: dd
  REAL,          INTENT(OUT)   :: gmt
  CHARACTER*24,  INTENT(IN)    :: hdate    ! YYYY_MO_DD-HH:MI:SS.SSSS
  INTEGER                      :: hh
  INTEGER                      :: jjj
  INTEGER,       EXTERNAL      :: julian
  INTEGER                      :: mi
  INTEGER                      :: mo
  INTEGER,       INTENT(OUT)   :: sdate    ! YYYYJJJ
  INTEGER,       INTENT(OUT)   :: stime    ! HHMISS
  INTEGER                      :: yyyy

  READ ( hdate(1:4),   '(i4.4)' ) yyyy
  READ ( hdate(6:7),   '(i2.2)' ) mo
  READ ( hdate(9:10),  '(i2.2)' ) dd
  READ ( hdate(12:13), '(i2.2)' ) hh
  READ ( hdate(15:16), '(i2.2)' ) mi

  jjj   = julian (yyyy, mo, dd)

  sdate = ( yyyy * 1000 ) + jjj
  stime = ( hh * 10000 ) + ( mi * 100 )      ! assume seconds are 0

  gmt   = FLOAT(hh) + ( FLOAT(mi) / 60.0 )   ! assume HDATE is in UTC

END SUBROUTINE getsdt
