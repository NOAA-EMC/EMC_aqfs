C+++
	SUBROUTINE GET_PDS ( gbm, ipds, iret )
C************************************************************************
C* GET_PDS								*
C*									*
C* This subroutine extracts the PDS information from a GRIB message.	*
C*									*
C*									*
C* GET_PDS  ( GBM, IPDS, IRET )						*
C*									*
C* Input parameters:							*
C*	GBM 		CHAR*1		GRIB message			* 
C*									*
C* Output parameters:							*
C*	IPDS (28)	INTEGER		GRIB PDS information array	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = GBM is not a grib message *
C**									*
C* Log:									*
C* K. Brill/EMC		 6/97						*
C************************************************************************
	CHARACTER*1	gbm (*)
	INTEGER		ipds (*)
C------------------------------------------------------------------------
	iret = 0
	IF ( gbm (1) .ne. 'G' .and. gbm (2) .ne. 'R' ) THEN
	    iret = -1
	    RETURN
	END IF
C*
	DO i = 9, 36
            ipds (i-8) =  mova2i( gbm (i) )
	END DO
C*
	RETURN
	END
