	SUBROUTINE SET_O14  ( ctable, type, ifhr, octet1, octet4, iret )
C************************************************************************
C* SET_O14								*
C*									*
C* This subroutine returns octet1 and octet4 of a WMO header given	*
C* the time conversion table, the type of output, and the forecast	*
C* hour.  The type is either H for international GRIB data or A for	*
C* AWIPS GRIB data.  The conversion table has three entries per row:	*
C*									*
C*  Interger forecast hour   Octet1 characters   Octet4 character	*
C*									*
C* The last row has the forecast hour set to 9999, to signal the end	*
C* of the table.							*
C*									*
C* SET_O14  ( CTABLE, TYPE, IFHR, OCTET1, OCTET4, IRET )		* 
C*									*
C* Input parameters:							*
C*	CTABLE (3, *)	CHAR*4		Time conversion table		*
C*	TYPE		CHAR*1		Output GRIB data type		*
C*	IFHR		INTEGER		Forecast hour			*
C*									*
C* Output parameters:							*
C*	OCTET1		CHAR*1		Octet 1 character (H,Y,Z)	*
C*	OCTET4		CHAR*1		Octet 4 character		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = no table entry found	*
C**									*
C* Log:									*
C* K. Brill/EMC		 5/97						*
C************************************************************************
	CHARACTER*(*)	ctable (3,*)*4, type, octet1, octet4
C*
	CHARACTER	cfhr*4
	LOGICAL		found
C*
C------------------------------------------------------------------------
	iret  = 0
	octet1 = ' '
	octet4 = ' '
	CALL ST_INCH ( ifhr, cfhr, ier )
	found = .false.
	indx = 1
	DO WHILE ( ctable (1,indx) .ne. '9999' .and. .not. found )
	    IF ( ctable (1,indx) .eq. cfhr ) THEN
		iqh = INDEX ( ctable (2,indx), 'H' )
		iqy = INDEX ( ctable (2,indx), 'Y' )
		IF ( type .eq. 'H' .and. iqh .ne. 0 ) THEN
		    octet1 = 'H'
		ELSE IF ( type .eq. 'A' .and. iqy .ne. 0 ) THEN
		    octet1 = 'Y'
		ELSE IF ( type .eq. 'A' ) THEN
		    octet1 = ctable (2,indx)
		END IF
		IF ( octet1 .ne. ' ' ) THEN
		    found = .true.
		    octet4 = ctable (3,indx)
		END IF
	    END IF
	    indx = indx + 1
	END DO
	IF ( .not. found ) iret = -1
C*
	RETURN
	END
