
        MODULE MODREPBN

!***********************************************************************
!  Module body starts at line
!
!  DESCRIPTION:
!     This module contains the public data that are used for reporting. The
!     main purpose of this module is to contain the data structures needed
!     for grouping records into bins for QA and reporting.
!
!  PRECONDITIONS REQUIRED:
!
!  SUBROUTINES AND FUNCTIONS CALLED:
!
!  REVISION HISTORY:
!     Created 7/2000 by M. Houyoux
!
!***************************************************************************
!
! Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
!                System
! File: @(#)$Id: modrepbn.f,v 1.14 2005/08/16 16:14:30 cseppan Exp $
!
! COPYRIGHT (C) 2004, Environmental Modeling for Policy Development
! All Rights Reserved
! 
! Carolina Environmental Program
! University of North Carolina at Chapel Hill
! 137 E. Franklin St., CB# 6116
! Chapel Hill, NC 27599-6116
! 
! smoke@unc.edu
!
! Pathname: $Source: /afs/isis/depts/cep/emc/apps/archive/smoke/smoke/src/emmod/modrepbn.f,v $
! Last updated: $Date: 2005/08/16 16:14:30 $ 
!
!****************************************************************************

        IMPLICIT NONE

        INCLUDE 'EMPRVT3.EXT'

!.........  Module-specific parameters
        INTEGER, PARAMETER, PUBLIC :: LV1 = IOVLEN3 + 2     ! "S-" len = 2
        INTEGER, PARAMETER, PUBLIC :: LV2 = IOVLEN3 * 2 + 4 ! ETJOIN len = 2
        INTEGER, PARAMETER, PUBLIC :: LV3 = IOVLEN3 * 3 + 6 

!.........  Define types needed for module
        TYPE :: BYETYPE

            SEQUENCE

            INTEGER            :: ETP        ! output index for emission types
            INTEGER            :: DAT        ! output index for pol/act
            INTEGER            :: AGG        ! output for either

            LOGICAL            :: SPCYN      ! true: speciation applies
            LOGICAL            :: PRYN       ! true: projection applies
            LOGICAL            :: CUYN       ! true: mult control applies
            LOGICAL            :: CRYN       ! true: react control applies

        END TYPE

        TYPE :: BYSPVAR

            SEQUENCE

            INTEGER            :: SPC        ! output index for species
            INTEGER            :: ETPSPC     ! output index for emis type || spc
            INTEGER            :: PRCSPC     ! output index for process || spc
            INTEGER            :: SUMETP     ! output index for summed to etype
            INTEGER            :: SUMPOL     ! output index for summed to pol
            INTEGER            :: AGG        ! output for any

        END TYPE

!.........  Output records to be summed into bins
        INTEGER, PUBLIC :: NOUTREC  = 0   ! no. of output records to be summed
        INTEGER, PUBLIC :: NSRCDROP = 0   ! no. of sources being dropped

        INTEGER, ALLOCATABLE, PUBLIC :: OUTSRC ( : )  ! smoke src ID
        INTEGER, ALLOCATABLE, PUBLIC :: OUTBIN ( : )  ! bin number
        INTEGER, ALLOCATABLE, PUBLIC :: OUTCELL( : )  ! cell number or zero

        REAL   , ALLOCATABLE, PUBLIC :: OUTGFAC( : )  ! gridding factor or 1.

!.........  Scalar values for data bins
        INTEGER, PUBLIC :: NOUTBINS = 0  ! no. of output bins

!.........  Grouped output information and data values
        INTEGER, ALLOCATABLE, PUBLIC :: BINBAD   ( : )   ! code number if something wrong
        INTEGER, ALLOCATABLE, PUBLIC :: BINCOIDX ( : )   ! index to country name
        INTEGER, ALLOCATABLE, PUBLIC :: BINCYIDX ( : )   ! index to county name
        INTEGER, ALLOCATABLE, PUBLIC :: BINDIUID ( : )   ! index to diurnal prof
        INTEGER, ALLOCATABLE, PUBLIC :: BINMONID ( : )   ! index to monthly prof
        INTEGER, ALLOCATABLE, PUBLIC :: BINREGN  ( : )   ! region code
        INTEGER, ALLOCATABLE, PUBLIC :: BINRCL   ( : )   ! roadclass code
        INTEGER, ALLOCATABLE, PUBLIC :: BINSMKID ( : )   ! SMOKE source ID
        INTEGER, ALLOCATABLE, PUBLIC :: BINSNMIDX( : )   ! SCC name index
        INTEGER, ALLOCATABLE, PUBLIC :: BINSIC   ( : )   ! SIC 
        INTEGER, ALLOCATABLE, PUBLIC :: BINSICIDX( : )   ! SIC name index
        INTEGER, ALLOCATABLE, PUBLIC :: BINMACIDX( : )   ! MACT name index
        INTEGER, ALLOCATABLE, PUBLIC :: BINNAIIDX( : )   ! NAICS name index
        INTEGER, ALLOCATABLE, PUBLIC :: BINSRGID1( : )   ! primary surg ID
        INTEGER, ALLOCATABLE, PUBLIC :: BINSRGID2( : )   ! fallback surg ID
        INTEGER, ALLOCATABLE, PUBLIC :: BINSTIDX ( : )   ! index to state name
        INTEGER, ALLOCATABLE, PUBLIC :: BINWEKID ( : )   ! index to weekly prof
        INTEGER, ALLOCATABLE, PUBLIC :: BINX     ( : )   ! x cell
        INTEGER, ALLOCATABLE, PUBLIC :: BINY     ( : )   ! y cell

        REAL   , ALLOCATABLE, PUBLIC :: BINPOPDIV( : )   ! popltn normalize fac
        REAL   , ALLOCATABLE, PUBLIC :: BINDATA  ( :,: ) ! output data values

        CHARACTER, ALLOCATABLE, PUBLIC :: BINELEV( : )! elevated flag
        CHARACTER(PLTLEN3), ALLOCATABLE, PUBLIC :: BINPLANT ( : ) ! Plant ID
        CHARACTER(SCCLEN3), ALLOCATABLE, PUBLIC :: BINSCC   ( : ) ! SCC
        CHARACTER(MACLEN3), ALLOCATABLE, PUBLIC :: BINMACT  ( : ) ! MACT
        CHARACTER(NAILEN3), ALLOCATABLE, PUBLIC :: BINNAICS ( : ) ! NAICS
        CHARACTER(STPLEN3), ALLOCATABLE, PUBLIC :: BINSRCTYP( : ) ! source type
        CHARACTER(SPNLEN3), ALLOCATABLE, PUBLIC :: BINSPCID ( : ) ! spec prof

!.........  Arrays for determining output from emission types to report columns
!.........  Dimensioned ( NIPPA, NREPORT )
        TYPE( BYETYPE ), ALLOCATABLE, PUBLIC :: TODOUT( :,: )

        CHARACTER(LV2), ALLOCATABLE, PUBLIC :: ETPNAM( : ) ! emis type names
        CHARACTER(LV1), ALLOCATABLE, PUBLIC :: DATNAM( : ) ! pol/act names

!.........  Arrays for determining output from species variables to report cols
!.........  Dimensioned ( NSVARS, NREPORT )
        INTEGER, PUBLIC :: NSVARS = 0       ! no. speciation variables

        TYPE( BYSPVAR ), ALLOCATABLE, PUBLIC :: TOSOUT( :,: )          

        CHARACTER(LV1), ALLOCATABLE, PUBLIC :: SPCNAM   ( : )  ! valid species names
        CHARACTER(LV3), ALLOCATABLE, PUBLIC :: ETPSPCNAM( : )  ! valid emis type || species
        CHARACTER(LV2), ALLOCATABLE, PUBLIC :: PRCSPCNAM( : )  ! valid process || species
        CHARACTER(LV2), ALLOCATABLE, PUBLIC :: SUMETPNAM( : )  ! S- || emis type names
        CHARACTER(LV1), ALLOCATABLE, PUBLIC :: SUMPOLNAM( : )  ! S- || pollutant names

        CHARACTER(IODLEN3), ALLOCATABLE, PUBLIC :: SLUNIT( : ) ! spc var units
        CHARACTER(IODLEN3), ALLOCATABLE, PUBLIC :: SSUNIT( : ) ! spc var units

!.........  Arrays for referencing input data needed across whole program run
        INTEGER, PUBLIC :: NDATIN = 0 ! Actual number of data vars input
        INTEGER, PUBLIC :: NSPCIN = 0 ! Actual number of speciation vars input

        INTEGER, ALLOCATABLE, PUBLIC :: INVIDX( : )  ! index data to inven
        INTEGER, ALLOCATABLE, PUBLIC :: INVTOCMU( : )! index inv-to-mult cntl
        INTEGER, ALLOCATABLE, PUBLIC :: INVTOPRJ( : )! index inv-to-projectn
        INTEGER, ALLOCATABLE, PUBLIC :: TPRIDX( : )  ! index data to hourly
        INTEGER, ALLOCATABLE, PUBLIC :: SPCIDX( : )  ! index global-to-input
        INTEGER, ALLOCATABLE, PUBLIC :: SPCTOINV( : )! index spc-to-inven
        INTEGER, ALLOCATABLE, PUBLIC :: SPCTOTPR( : )! index spc-to-temporal
        INTEGER, ALLOCATABLE, PUBLIC :: TPACTIDX( : )! index etype-to-activity

        LOGICAL, ALLOCATABLE, PUBLIC :: DATOUT( : )  ! true: inven data needed
        LOGICAL, ALLOCATABLE, PUBLIC :: SPCOUT( : )  ! true: spc factors needed

!.........  Unique species list and count
        INTEGER, PUBLIC :: NMSPC = 0  ! no. species in whole run
        CHARACTER(LV1), ALLOCATABLE, PUBLIC :: EMNAM( : ) ! species names

        END MODULE MODREPBN