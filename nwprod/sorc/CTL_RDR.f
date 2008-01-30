C-----------------------------------------------------------------------
	SUBROUTINE CTL_RDR  ( lunctl, mxg, kpds, kmdl, kgrd, type,
     +			      prcsn, name, ksmpr, ksmpo, kgt, nr, iret )
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM: CTL_RDR        Reads control file for Product Generator
C   PRGMMR: BRILL            ORG: W/NP22    DATE: 97-07-01
C
C ABSTRACT:  READS A CONTROL FILE AND RETURNS PDS (9-12,21) VALUES &
C            MODEL #, GRID #, WMO TYPE FOR EACH GRID TARGETED IN THE
C	     FILE CONNECTED TO LUNCTL.  ALSO RETURNS THE PRECISION FOR
C	     PACKING AND THE NUMBER OF SMOOTHING PASSES.
C
C PROGRAM HISTORY LOG:
C
C USAGE:    CALL CTL_RDR ( LUNCTL, MXG, KPDS, KMDL, KGRD, TYPE, PRCSN,
C			   NAME, KSMPR, KSMPO, KGT, NR, IRET )
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
C* CTL_RDR								*
C*									*
C* This subroutine reads a product generator control file and returns	*
C* GRIB PDS elements 9-12, and 21 along with the model numbers, grid	*
C* numbers, corresponding WMO header types, packing precision numbers	*
C* for FNDBTS, output file names, and smoothing numbers.		*
C*									*
C* Each control file entry consists of a header record followed by a	*
C* family of output grid specifications.  The header consists of PDS	*
C* elements 9-12, and 21, along with a character string describing the	*
C* grid.  The output grid specifications consist of the following	*
C* fields separated by slashes:						*
C*									*
C* 	PDS6 - the model ID number					*
C*	GRD# - the grid number						*
C*	WMO  - the WMO header type (blank, H, A, X, or combination)	*
C*	SCAL - the precision number for FNDBTS				*
C*	FNAM - the output file name					*
C*	PRSM - the pre-smooth number					*
C*	POSM - the post-smooth number					*
C*									*
C* The smoothing numbers may be omitted.  If there is only one smooth	*
C* number, it is assumed to be a pre-smooth number.  If there must be	*
C* a post-smooth number, a pre-smooth number must also be entered, or	*
C* it may be a single number preceded by two slashes.			*
C*									*
C* Here is an exmple of such a control file entry:			*
C*									*
C*   PDS9 PDS10 PDS11 PDS12 PDS21  description				*
C*   PDS6/###1/HAX/3.3/name1						*
C*   PDS6/###2/HAX/-3/name2/7/3						*
C*   PDS6/###3/HAX/2.9/name3//2						*
C*   PDS6/###4/HAX/-2/name4/2						*
C*									*
C* The control file file is already assumed to be connected to the	*
C* input unit numbers.							*
C*									*
C* CTL_RDR  ( LUNCTL, MXG, KPDS, KMDL, KGRD, TYPE, PRCSN, NAME,		*
C*	      KSMPR, KSMPO, KGT, NR,IRET )				*
C*									*
C* Input parameters:							*
C*	LUNCTL 		INTEGER		Unit number of control file	*
C*	MXG		INTEGER		Maximum # of grids per control	*
C*					record				*
C*									*
C* Output parameters:							*
C*	KPDS  (5,  NR)	INTEGER		PDS elements 9-12, 21		*
C*	KMDL  (MXG,NR)  INTEGER		Model number for this grid	*
C*	KGRD  (MXG,NR)	INTEGER		Grid numbers for output		*
C*	TYPE  (MXG,NR)	CHAR*4		Message type string for each	*
C*					grid # in KGRD--one or more of: *
C*						H = international	*
C*						A = AWIPS		*
C*						X = No header		*
C*	PRCSN (MXG,NR)  REAL		Precision for packing		*
C*	NAME  (MXG,NR)	CHAR*16		Output name ID			*
C*	KSMPR (MXG,NR)	INTEGER		Number of pre-smoothing passes	*
C*	KSMPO (MXG,NR)  INTEGER		Number of post-smoothing passes *
C*	KGT   (NR)	INTEGER		Number of grid/type records	*
C*					following each control record	*
C*	NR		INTEGER		Number of control records read	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = invalid file entry	*
C*					 -2 = misplaced grid/type info  *
C*					 -3 = invalid grid/type entry	*
C*					 -4 = invalid grid #		*
C*					 -5 = invalid precision #	*
C*					 -6 = invalid smoothing #	*
C**									*
C* Log:									*
C* K. Brill/EMC		 6/97						*
C* K. Brill/EMC		 8/97	Added KSMO, PRCSN			*
C* K. Brill/EMC		 9/97	Added NAME				*
C* K. Brill/EMC		10/97	Added KMDL, 2 smo #'s, K6PDS->KPDS	*
C* G. Manikin/EMC        3/03   Added 2 more elements to KPDS
C************************************************************************
	CHARACTER*(*)	type (mxg,*)
	CHARACTER*(*)	name (mxg,*)
	INTEGER		kpds (7,*), kmdl (mxg,*), kgrd (mxg,*),
     +			ksmpr (mxg,*), ksmpo (mxg,*), kgt (*)
	REAL		prcsn (mxg,*)
C*
	CHARACTER*90	buff
	CHARACTER*32	carr (10)
C------------------------------------------------------------------------
	iret = 0
	nr = 0
	REWIND lunctl
	iostat = 0
	DO WHILE ( iostat .eq. 0 )
	    READ ( lunctl, '(A)', IOSTAT=iostat ) buff
	    IF ( iostat .eq. 0 ) THEN
		islsh = INDEX ( buff, '/' )
		IF ( islsh .ne. 0 ) THEN
		    IF ( nr .eq. 0 ) THEN
			iret = -2
			RETURN
		    END IF
		    CALL ST_CLST ( buff, '/', '?', 7, carr, num, ier )
		    IF ( ier .lt. 0 ) THEN
			iret = -3
			RETURN
		    ELSE IF ( carr (1) .eq. '?' .or.
     +			      carr (2) .eq. '?' .or.
     +			      carr (3) .eq. '?' .or.
     +			      carr (4) .eq. '?' .or.
     +			      carr (5) .eq. '?' ) THEN
			iret = -3
			RETURN
		    ELSE
			CALL ST_NUMB ( carr (1), imdl, ier )
			CALL ST_NUMB ( carr (2), igrd, ier1 )
			ier = ier + ier1
			IF ( ier .ne. 0 ) THEN
			    iret = -4
			    RETURN
			END IF
			kgt (nr) = kgt (nr) + 1
			kmdl ( kgt(nr), nr ) = imdl
			kgrd ( kgt(nr), nr ) = igrd
			type ( kgt(nr), nr ) = carr (3)
			CALL ST_CRNM ( carr (4), prc, ier )
			IF ( ier .ne. 0 ) THEN
			    iret = -5
			    RETURN
			END IF
			prcsn ( kgt(nr), nr ) = prc
			name ( kgt(nr), nr ) = carr (5)
			IF ( carr (6) .ne. '?' ) THEN
			    CALL ST_NUMB ( carr (6), ismth, ier )
			    IF ( ier .ne. 0 ) THEN
				iret = -6
				RETURN
			    ELSE
			        ksmpr ( kgt(nr), nr ) = ismth
			    END IF
			ELSE
			    ksmpr ( kgt(nr), nr ) = 0
			END IF
			IF ( carr (7) .ne. '?' ) THEN
			    CALL ST_NUMB ( carr (7), ismth, ier )
			    IF ( ier .ne. 0 ) THEN
				iret = -6
				RETURN
			    ELSE
			        ksmpo ( kgt(nr), nr ) = ismth
			    END IF
			ELSE
			    ksmpo ( kgt(nr), nr ) = 0
			END IF
		    END IF
		ELSE
		    CALL ST_CLST ( buff, ' ', ' ', 7, carr, num, ier )
		    IF ( ier .lt. 0 .or. num .lt. 7 ) THEN
			iret = - 1
		    ELSE
			nr = nr + 1
			kgt (nr) = 0
			kmdl (1,nr) = 0
			kgrd (1,nr) = 0
			type (1,nr) = ' '
			DO i = 1, 7
			  CALL ST_NUMB ( carr (i), kpds (i,nr), ier )
			END DO
		    END IF
		END IF
	    END IF
	END DO
C*
	RETURN
	END
