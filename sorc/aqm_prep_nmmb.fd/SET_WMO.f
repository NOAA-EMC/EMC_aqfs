	SUBROUTINE SET_WMO  ( ipds, luntim, lunprm, lungrd, lunlvl,
     +			      type, wmohdr, iret )
C************************************************************************
C* SET_WMO								*
C*									*
C* This subroutine returns a WMO header corresponding to the input	*
C* values of GRIB PDS octets 1 through 20, given the information in	*
C* the time parameter, grid, and level look-up tables.			*
C*									*
C* Type is the kind of output---H for international, A for AWIPS.	*
C*									*
C* The lookup table for time has these three columns of information:	*
C*									*
C*   forecast hour    OCTET 1 characters   OCTET 4 character		*
C*									*
C* The lookup table for parms has these three columns of information:	*
C*									*
C*   parameter number   OCTET 1 characters   OCTET 2 character		*
C*									*
C* The lookup table for grids has these three columns of information:	*
C*									*
C*   grid number   OCTET 1 characters   OCTET 3 character		*
C*									*
C* The lookup table for levels has these six columns of information:	*
C*									*
C*  PDS (9)  PDS (7)  PDS (10)  PDS (11)  PDS (12)  OCTET 5 & 6		*
C*									*
C* In each table, the last row has the first column entry set to 9999,	*
C* to signal the end of the table.					*
C*									*
C* The level lookup table is searched first for the determination of	*
C* octets 5 and 6.  The contents of octets 5 and 6 are not always	*
C* determined by the level look-up table, but may be computed entirely	*
C* from the PDS information.						*
C*									*
C* Note that the calling program must have opened files corresponding	*
C* to the unit numbers in the calling sequence.  They must be opened 	*
C* for formatted read.  If any look-up table is not required, set	*
C* its corresponding unit number to zero.				*
C*									*
C*									*
C* SET_WMO  ( IPDS, LUNTIM, LUNPRM, LUNGRD, LUNLVL, TYPE, WMOHDR,	*
C*	      IRET )							* 
C*									*
C* Input parameters:							*
C*	IPDS (20) 	INTEGER		PDS octet values		*
C*	LUNTIM		INTEGER		Unit number of time LUT		*
C*	LUNPRM		INTEGER		Unit number of parameter LUT	*
C*	LUNGRD		INTEGER		Unit number of grid LUT		*
C*	LUNLVL		INTEGER		Unit number of level LUT	*
C*	TYPE		CHAR*1		Type of output (H or A)		*
C*									*
C* Output parameters:							*
C*	WMOHDR		CHAR*6		WMO header			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = could not make time lbl	*
C*					 -2 = could not make parm lbl	*
C*					 -3 = could not make grid lbl	*
C*					 -4 = could not make levl lbl	*
C**									*
C* Log:									*
C* K. Brill/EMC		 5/97						*
C************************************************************************
	PARAMETER	( MXT = 100, MXP = 512, MXG = 64, MXL = 128 )
C*
	INTEGER		ipds (*)
	CHARACTER*(*)	type, wmohdr*6
C*
	CHARACTER*4	timtbl (3,MXT), prmtbl (3,MXP), grdtbl (3,MXG),
     +			lvltbl (6,MXL)
	CHARACTER*1	octet (4)
	CHARACTER*2	oct56
	LOGICAL		first
	SAVE		first, timtbl, prmtbl, grdtbl, lvltbl
	DATA		first /.true./
C*
C------------------------------------------------------------------------
	iret  = 0
	wmohdr = ' '
	IF ( first ) THEN
	    CALL SET_TBL ( luntim, 3, MXT, timtbl, ier )
	    CALL SET_TBL ( lunprm, 3, MXP, prmtbl, ier )
	    CALL SET_TBL ( lungrd, 3, MXG, grdtbl, ier )
	    CALL SET_TBL ( lunlvl, 6, MXL, lvltbl, ier )
	    first = .false.
	END IF
	IF ( ipds (20) .ne. 0 ) THEN
	    ifhr = ipds (20)
	ELSE
	    ifhr = ipds (19)
	END IF
	CALL SET_O14  ( timtbl, type, ifhr, octet(1), octet(4), iret )
	IF ( iret .ne. 0 ) THEN
	    iret = -1
	    RETURN
	END IF
	ip = ipds (9)
	CALL SET_O23  ( prmtbl, ip, octet(1), octet(2), iret )
	IF ( iret .ne. 0 ) THEN
	    iret = -2
	    RETURN
	END IF
	ig = ipds (7)
	CALL SET_O23  ( grdtbl, ig, octet(1), octet(3), iret )
	IF ( iret .ne. 0 ) THEN
	    iret = -3
	    RETURN
	END IF
	CALL SET_O56  ( lvltbl, ipds, oct56, iret )
	IF ( iret .ne. 0 ) THEN
	    iret = -4
	    RETURN
	END IF
C*
	wmohdr = octet (1) // octet (2) // octet (3) // octet (4) //	
     +		 oct56
C*
	RETURN
	END
