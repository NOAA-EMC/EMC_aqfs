	SUBROUTINE SET_O23  ( ctable, ipg, octet1, octet, iret )
C************************************************************************
C* SET_O23								*
C*									*
C* This subroutine returns octet 2 OR 3 of a WMO header using the	*
C* information in the parameter or grid number conversion table,	*
C* respectively.  Octet 1 must be known before this routine is called.	*
C* The conversion table has three entries per row:			*
C*									*
C* The input table has these three columns:				*
C*									*
C*  Parm/Grid number   Octet1 characters     Octet2/3 character		*
C*									*
C* The last row has the parm/grid number set to 9999, to signal the end	*
C* of the table.							*
C*									*
C* SET_O14  ( CTABLE, IPG, OCTET1, OCTET, IRET )			* 
C*									*
C* Input parameters:							*
C*	CTABLE (3, *)	CHAR*4		Parm/grid conversion table	*
C*	IPG		INTEGER		Parm/grid number		*
C*	OCTET1		CHAR*1		Octet 1 character (H,Y,Z)	*
C*									*
C* Output parameters:							*
C*	OCTET		CHAR*1		Octet 2 or 3 character		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = no table entry found	*
C**									*
C* Log:									*
C* K. Brill/EMC		 5/97						*
C************************************************************************
	CHARACTER*(*)	ctable (3,*)*4, octet1, octet
C*
	CHARACTER	cipg*4
	LOGICAL		found
C*
C------------------------------------------------------------------------
	iret  = 0
	octet = ' '
	CALL ST_INCH ( ipg, cipg, ier )
	found = .false.
	indx = 1
	DO WHILE ( ctable (1,indx) .ne. '9999' .and. .not. found )
	    IF ( ctable (1,indx) .eq. cipg ) THEN
		iq = INDEX ( ctable (2,indx), octet1 )
		IF ( iq .ne. 0 ) THEN
		    found = .true.
		    octet = ctable (3,indx)
		END IF
	    END IF
	    indx = indx + 1
	END DO
	IF ( .not. found ) iret = -1
C*
	RETURN
	END
