	SUBROUTINE ADD_WMO  ( gbmin, type, lunkwb, luntim,
     +			      lunprm, lungrd, lunlvl, gbmout, lenout,
     +			      iret )
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM: ADD_WMO        ADD WMO HEADER TO A GRIB MESSAGE
C   PRGMMR: BRILL            ORG: W/NP22    DATE: 97-07-01
C
C ABSTRACT: ADDS THE WMO HEADER FOR A GRIB MESSAGE AFTER MAKING IT
C   USING THE PDS INFORMATION.
C
C PROGRAM HISTORY LOG:
C
C USAGE:    CALL MAKWMO( GBMIN, TYPE, LUNKWB, LUNTIM, LUNPRM, LUNGRD,
C		         LUNLVL, GBMOUT, LENOUT, IRET )
C   INPUT ARGUMENT LIST:
C	See below.
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C	See below.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C   MACHINE:  PORTABLE
C
C$$$
C************************************************************************
C* ADD_WMO								*
C*									*
C* This subroutine adds a WMO header to a GRIB message.  The input and	*
C* output arrays may be the same.					*
C*									*
C* The Look Up Table (LUT) files are already assumed to be connected	*
C* to the input unit numbers.						*
C*									*
C* ADD_WMO  ( GBMIN, TYPE, LUNKWB, LUNTIM, LUNPRM, LUNGRD, LUNLVL,	*
C*	      GBMOUT, LENOUT, IRET )					*
C*									*
C* Input parameters:							*
C*	GBMIN (*)	CHAR*1		GRIB message			* 
C*	TYPE		CHAR*1		Message type:			*
C*						H = international	*
C*						A = AWIPS		*
C*	LUNKWB		INTEGER		Unit # of Model Designator LUT	*
C*	LUNTIM		INTEGER		Unit # of F hour Designator LUT *
C*	LUNPRM		INTEGER		Unit # of Parm Designator LUT	*
C*	LUNGRD		INTEGER		Unit # of Grid Designator LUT	*
C*	LUNLVL		INTEGER		Unit # of Level Designator LUT  *
C*									*
C* Output parameters:							*
C*	GBMOUT (LENOUT) CHAR*1		GRIB message with WMO header	*
C*	LENOUT		INTEGER		Number of bytes in output messg *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = GBMIN is not a GRIB messg *
C*					 -2 = cannot set octet 11	*
C*					 -3 = cannot make WMO header	*
C*					-21 = cannot make F hour label	*
C*					-22 = cannot make parm label	*
C*					-23 = cannot make grid label	*
C*					-24 = cannot make level label	*
C**									*
C* Log:									*
C* K. Brill/EMC		 6/97						*
C************************************************************************
	CHARACTER*1	gbmin (*), gbmout (*), type
C*
	INTEGER		ipds (28)
	CHARACTER*1	oct11
	CHARACTER*6	hedr6
	CHARACTER*132   wmohdr
C*
C------------------------------------------------------------------------
        iret = 0
C
C*	Compute the length of the GRIB message.
C
        is5 = mova2i ( gbmin (5) )
        is6 = mova2i ( gbmin (6) )
        is7 = mova2i ( gbmin (7) )
        lng = ( is5 * 256 + is6 ) * 256 + is7
C
C*	Get the GRIB PDS information.
C

	CALL GET_PDS ( gbmin, ipds, iret )
	IF ( iret .ne. 0 ) RETURN
C
C*	Set the byte that follows KWB.
C
	CALL SET_O11 ( lunkwb, ipds (6), oct11, ier )
	IF ( iret .ne. 0 ) THEN
	    iret = -2
	    RETURN
	END IF
C
C*	Construct the 6-character header.
C
	CALL SET_WMO ( ipds, luntim, lunprm, lungrd, lunlvl, type,
     +		       hedr6, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -20 + ier
	    RETURN
	END IF
C
C*	Build entire WMO header string.
C
	CALL MAK_WMO ( ipds, hedr6, oct11, wmohdr, lnghdr, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -3
	    RETURN
	END IF
C
C*	Add the header to the GRIB message.
C
	lenout = lng + lnghdr
	icin = lng
	DO i = lenout, ( lnghdr + 1 ), -1
	    gbmout (i) = gbmin (icin)
	    icin = icin - 1
	END DO
	DO i = 1, lnghdr
	    gbmout (i) = wmohdr (i:i)
	END DO
C*
	RETURN
	END
