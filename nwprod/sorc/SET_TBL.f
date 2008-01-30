	SUBROUTINE SET_TBL  ( lun, nc, maxrow, ctable, iret )
C************************************************************************
C* SET_TBL								*
C*									*
C* This subroutine loads a table from a file open on unit LUN.		*
C*									*
C* The last row has the parm number set to 9999, to signal the end of	*
C* the table.								*
C*									*
C* SET_TBL  ( LUN, NC, MAXROW, CTABLE, IRET )				*
C*									*
C* Input parameters:							*
C*	LUN		INTEGER		Unit number of table file	*
C*	NC		INTEGER		Number of columns in table	*
C*	MAXROW		INTEGER		Max number of rows in table	*
C*									*
C* Output parameters:							*
C*	CTABLE (NC, *)	CHAR*4		Level conversion table		*
C*	IRET		INTEGER		Return code			*
C*					 +2 = too many rows		*
C*					 +1 = too many columns		*
C*					  0 = normal return		*
C*					 -1 = too few columns		*
C*									*
C**									*
C* Log:									*
C* K. Brill/EMC		 5/97						*
C************************************************************************
	CHARACTER*(*)	ctable (nc,*)*4
C*
	CHARACTER*90	crow
	CHARACTER*4	carr (16)
	LOGICAL		prnt
C------------------------------------------------------------------------
	iret  = 0
	prnt = .false.
	ctable (1,1) = '9999'
	IF ( lun .le. 0 ) RETURN
	iostat = 0
	irow = 0
	REWIND lun
	DO WHILE ( iostat .eq. 0 .and. irow .lt. maxrow )
	    READ ( lun, 1000, IOSTAT=iostat ) crow
1000	    FORMAT (A)
	    ibang = INDEX ( crow(1:1), '!' )
	    IF ( iostat .eq. 0 .and. ibang .eq. 0 ) THEN
		CALL ST_LSTR ( crow, lng, ier )
		CALL ST_UTAB ( crow, lng, crow, ier )
		CALL ST_CLST ( crow, ' ', '9999', nc, carr, num, ier )
		IF ( num .lt. nc ) THEN
		    iret = -1
		    WRITE (6,*) ' Too few columns in table file = ', lun
		    RETURN
		ELSE IF ( num .gt. nc ) THEN
		    iret = 1
		    IF ( .not. prnt ) THEN
			WRITE (6,*)
     +			' Too many columns in table file = ', lun
			WRITE (6,*) ' Continuing anyway.'
			prnt = .true.
		    END IF
		END IF
		irow = irow + 1
		DO ic = 1, nc
		    ctable (ic, irow) = carr (ic)
		END DO
	    END IF
	END DO
	IF ( irow .ge. maxrow ) THEN
	    iret = 2
	    WRITE (6,*) ' Too many rows in table file = ', lun
	    irow = maxrow
	ELSE
	    irow = irow + 1
	END IF
	ctable (1,irow) = '9999'
C*
	RETURN
	END
