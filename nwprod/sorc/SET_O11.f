	SUBROUTINE SET_O11  ( lunkwb, ipds6, oct11, iret )
C************************************************************************
C* SET_O11								*
C*									*
C* This subroutine returns octet 11 of a WMO header given the table	*
C* of model identifiers matched with the corresponding values of PDS	*
C* byte number 6.  The look-up table file has two entries per record:	*
C*									*
C*  Interger value of PDS byte 6       Octet 11 character		*
C*									*
C*									*
C* SET_O11  ( lunkwb, IPDS6, OCT11, IRET )				*
C*									*
C* Input parameters:							*
C*	LUNKWB 		INTEGER		Unit # of model ID table	*
C*	IPDS6		INTEGER		GRIB PDS byte 6 value		*
C*									*
C* Output parameters:							*
C*	OCT11		CHAR*1		Octet 11 character		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = no table entry found	*
C**									*
C* Log:									*
C* K. Brill/EMC		 6/97						*
C************************************************************************
	PARAMETER	( MXKWB = 32 )
C*
	CHARACTER*1	oct11
C*
	CHARACTER*4	kwbtbl (2, MXKWB)
C*
	CHARACTER	cpds6*4
	LOGICAL		found
C*
	LOGICAL		first
	SAVE		first, kwbtbl
	DATA		first /.true./
C*
C------------------------------------------------------------------------
	iret  = 0
	oct11 = ' '
	IF ( first ) THEN
	    CALL SET_TBL ( lunkwb, 2, MXKWB, kwbtbl, ier )
	    first = .false.
	END IF
C*
	CALL ST_INCH ( ipds6, cpds6, ier )
	found = .false.
	indx = 1
	DO WHILE ( kwbtbl (1,indx) .ne. '9999' .and. .not. found )
	    IF ( kwbtbl (1,indx) .eq. cpds6 ) THEN
		oct11 = kwbtbl (2,indx)
		found = .true.
	    END IF
	    indx = indx + 1
	END DO
	IF ( .not. found ) iret = -1
C*
	RETURN
	END
