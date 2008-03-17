	SUBROUTINE SET_O56  ( ctable, ipds, octet, iret )
C************************************************************************
C* SET_O56								*
C*									*
C* This subroutine returns octets 5 AND 6 of a WMO header using the	*
C* information in a level conversion table and the GRIB PDS.  If the	*
C* table does not contain an entry matching the PDS information,	*
C* this routine attempts to construct the values for octets 5 and 6.	*
C*									*
C* The input table has these six columns:				*
C*									*
C*   PDS (9)   PDS (7)   PDS (10)   PDS (11)   PDS (12)   Octet5_6	*
C*									*
C* Note that PDS (9) is the GRIB parameter number and PDS (7) is the	*
C* GRIB grid identification number.					*
C*									*
C* The entry in a column may be a *, indicating that whatever the	*
C* value of that PDS element, octet5_6 is used if the other PDS		*
C* elements match.							*
C*									*
C* The last row has the first column set to 9999, to signal the end of	*
C* the table.								*
C*									*
C* SET_O56  ( CTABLE, IPDS, OCTET, IRET )				* 
C*									*
C* Input parameters:							*
C*	CTABLE (6, *)	CHAR*4		Special conversion table	*
C*	IPDS (12)	INTEGER		PDS values 1-12			*
C*									*
C* Output parameters:							*
C*	OCTET 		CHAR*2		Octet 5 & 6 characters		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = no valid designator	*
C**									*
C* Log:									*
C* K. Brill/EMC		 5/97						*
C* K. Brill/EMC		 6/97	Make special table the level table;	*
C*				eliminate SET_S56			*
C************************************************************************
	CHARACTER*(*)	ctable (6,*)*4, octet
	INTEGER		ipds (*)
C*
	CHARACTER*4	cprm, cgrd, c10, c11, c12
	LOGICAL		found
C*
C------------------------------------------------------------------------
	iret  = 0
	octet = '98'
	CALL ST_INCH ( ipds (9), cprm, ier )
	CALL ST_INCH ( ipds (7), cgrd, ier )
	CALL ST_INCH ( ipds (10), c10, ier )
	CALL ST_INCH ( ipds (11), c11, ier )
	CALL ST_INCH ( ipds (12), c12, ier )
	found = .false.
	indx = 1
C
C*	Check the table first.
C
	DO WHILE ( ctable (1,indx) .ne. '9999' .and. .not. found )
	    IF ( ( ctable (1,indx) .eq. cprm .or.
     +			ctable (1,indx) .eq. '*' ) .and.
     +		 ( ctable (2,indx) .eq. cgrd .or.
     +			ctable (2,indx) .eq. '*' ) .and.
     +		 ( ctable (3,indx) .eq. c10 .or.
     +			ctable (3,indx) .eq. '*' ) .and.
     +		 ( ctable (4,indx) .eq. c11 .or.
     +			ctable (4,indx) .eq. '*' ) .and.
     +		 ( ctable (5,indx) .eq. c12 .or.
     +			ctable (5,indx) .eq. '*' ) ) THEN
		found = .true.
		octet = ctable (6,indx)
	    END IF
	    indx = indx + 1
	END DO
C*
	IF ( found ) RETURN
C
C*	Try to determine the designator from the PDS.
C
	i10 = ipds (10)
	i11 = ipds (11)
	i12 = ipds (12)
	IF ( i10 .eq. 1 ) THEN
	    octet = '98'
	ELSE IF ( i10 .eq. 2 ) THEN
	    octet = '01'
	ELSE IF ( i10 .eq. 3 ) THEN
	    octet = '74'
	ELSE IF ( i10 .eq. 4 ) THEN
	    octet = '94'
	ELSE IF ( i10 .eq. 5 ) THEN
	    octet = '01'
	ELSE IF ( i10 .eq. 6 ) THEN
	    octet = '96'
	ELSE IF ( i10 .eq. 7 ) THEN
	    octet = '97'
	ELSE IF ( i10 .eq. 8 ) THEN
	    octet = '01'
	ELSE IF ( i10 .eq. 9 ) THEN
	    octet = '01'
	ELSE IF ( i10 .eq. 100 ) THEN
	    iprs = i11 * 256 + i12
	    IF ( iprs .eq. 1000 ) THEN
		octet = '99'
	    ELSE IF ( iprs .eq. 975 ) THEN
		octet = '93'
	    ELSE IF ( iprs .eq. 925 ) THEN
		octet = '92'
	    ELSE IF ( iprs .eq. 875 ) THEN
		octet = '91'
	    ELSE
		iprs = iprs / 10
		CALL ST_INCH ( iprs, octet, iret )
		IF ( iret .ne. 0 ) THEN
		    iret = -2
		    RETURN
		END IF
		IF ( iprs .lt. 10 ) THEN
		    octet (2:2) = octet (1:1)
		    octet (1:1) = '0'
		END IF
	    END IF
	ELSE IF ( i10 .eq. 101 ) THEN
	    octet = '01'
	ELSE IF ( i10 .eq. 102 ) THEN
	    octet = '89'
	ELSE IF ( i10 .eq. 103 ) THEN
	    ihgt = i11 * 256 + i12
	    IF ( ihgt .gt. 1810 .and. ihgt .lt. 1840 ) THEN
		octet = '81'
	    ELSE IF ( ihgt .gt. 2730 .and. ihgt .lt. 2750 ) THEN
		octet = '73'
	    ELSE IF ( ihgt .gt. 3650 .and. ihgt .lt. 3670 ) THEN
		octet = '64'
	    ELSE IF ( ihgt .gt. 5476 .and. ihgt .lt. 5496 ) THEN
		octet = '51'
	    ELSE
		octet = '01'
	    END IF
	ELSE IF ( i10 .eq. 108 ) THEN
	    octet = '86'
	ELSE IF ( i10 .eq. 200 ) THEN
	    octet = '00'
	ELSE IF ( i10 .eq. 116 ) THEN
	    octet = '86'
	END IF
C*
	RETURN
	END
