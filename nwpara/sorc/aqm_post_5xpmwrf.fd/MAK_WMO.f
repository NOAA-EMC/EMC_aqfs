C+++
	SUBROUTINE MAK_WMO ( ipds, hedr6, oct11, wmohdr, lnghdr, ier )
C************************************************************************
C* MAK_WMO								*
C*									*
C* This subroutine makes a WMO header using the PDS information and	*
C* the other pieces that are input.					*
C*									*
C*									*
C* MAK_WMO  ( IPDS, HEDR6, OCT11, WMOHDR, LNGHDR, IER )			*
C*									*
C* Input parameters:							*
C*	IPDS (28)	INTEGER		GRIB PDS information array	*
C*	HEDR6		CHAR*6		First 6 characters of WMO hdr	*
C*	OCT11		CHAR*1		11th character of WMO hdr	*
C*									*
C* Output parameters:							*
C*	WMOHDR 		CHAR*21		Complete WMO header		*
C*	LNGHDR		INTEGER		Number of characters in header	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/EMC		 6/97						*
C************************************************************************
	CHARACTER	hedr6*6, oct11*1, wmohdr*21
	INTEGER		ipds (*)
C*
	CHARACTER*1	cn
	CHARACTER*1	blnk
C------------------------------------------------------------------------
	iret = 0
	lnghdr = 21
	blnk = CHAR (32)
	wmohdr = blnk
	ic = 0
	DO i = 1, 6
	    ic = ic + 1
	    wmohdr (ic:ic) = hedr6 (i:i)
	END DO
C*
	wmohdr = wmohdr ( 1:ic ) // blnk // 'KWB' // oct11 // blnk
C*
	iday = ipds (15)
	id1 = iday / 10
	id2 = MOD ( iday, 10 )
	CALL ST_INCH ( id1, cn, ier )
	wmohdr (13:13) = cn
	CALL ST_INCH ( id2, cn, ier )
	wmohdr (14:14) = cn
C*
	ihr = ipds (16)
	ih1 = ihr / 10
	ih2 = MOD ( ihr, 10 )
	CALL ST_INCH ( ih1, cn, ier )
	wmohdr (15:15) = cn
	CALL ST_INCH ( ih2, cn, ier )
	wmohdr (16:16) = cn
C*
	imn = ipds (17)
	im1 = imn / 10
	im2 = MOD ( imn, 10 )
	CALL ST_INCH ( im1, cn, ier )
	wmohdr (17:17) = cn
	CALL ST_INCH ( im2, cn, ier )
	wmohdr (18:18) = cn
C*
	wmohdr (19:19) = CHAR (13)
	wmohdr (20:20) = CHAR (13)
	wmohdr (21:21) = CHAR (10)
C*
	RETURN
	END
