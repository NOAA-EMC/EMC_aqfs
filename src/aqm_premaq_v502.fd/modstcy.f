
        MODULE MODSTCY

!***********************************************************************
!
!  DESCRIPTION:
!     This module contains the public allocatable arrays for doing country, 
!     state and county summaries
!
!  PRECONDITIONS REQUIRED:
!
!  SUBROUTINES AND FUNCTIONS CALLED:
!
!  REVISION HISTORY:
!     
!
!***********************************************************************
!
! Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
!                System
! File: @(#)$Id: modstcy.f,v 1.9 2004/06/21 17:23:03 cseppan Exp $
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
! Pathname: $Source: /afs/isis/depts/cep/emc/apps/archive/smoke/smoke/src/emmod/modstcy.f,v $
! Last updated: $Date: 2004/06/21 17:23:03 $ 
!
!****************************************************************************

        IMPLICIT NONE

        INCLUDE 'EMPRVT3.EXT'   !  emissions private parameters

!.........  Indices from per-source inventory arrays to county array
        INTEGER, ALLOCATABLE, PUBLIC :: AICNY( : )  ! dim NASRC
        INTEGER, ALLOCATABLE, PUBLIC :: MICNY( : )  ! dim NMSRC
        INTEGER, ALLOCATABLE, PUBLIC :: PICNY( : )  ! dim NPSRC

!.........  Codes, names, and their dimensions
        INTEGER, PUBLIC :: NCOUNTRY    ! Number of counties
        INTEGER, PUBLIC :: NSTATE      ! Number of countries/states
        INTEGER, PUBLIC :: NCOUNTY     ! Number of countries/states/counties

        INTEGER, ALLOCATABLE, PUBLIC :: CTRYCOD( : ) ! country codes
        INTEGER, ALLOCATABLE, PUBLIC :: STATCOD( : ) ! country/state codes
        INTEGER, ALLOCATABLE, PUBLIC :: CNTYCOD( : ) ! country/state/county code

        CHARACTER(20), ALLOCATABLE, PUBLIC :: CTRYNAM( : ) ! Country names
        CHARACTER(20), ALLOCATABLE, PUBLIC :: STATNAM( : ) ! State names
        CHARACTER(20), ALLOCATABLE, PUBLIC :: CNTYNAM( : ) ! County names

!.........  Other arrays by county 
        INTEGER, ALLOCATABLE, PUBLIC :: CNTYTZON( : )  ! time zone by county
        REAL   , ALLOCATABLE, PUBLIC :: CNTYPOPL( : )  ! population by county
        LOGICAL, ALLOCATABLE, PUBLIC :: USEDAYLT( : )  ! true: use daylight time

!.........  Other arrays by state
        REAL   , ALLOCATABLE, PUBLIC :: STATPOPL( : )  ! population by state

!.........  Other arrays by country
        REAL   , ALLOCATABLE, PUBLIC :: CTRYPOPL( : )  ! population by country

!.........  ORIS list with state and county codes
        INTEGER, PUBLIC :: NORIS     ! Number of all ORIS codes

        INTEGER, ALLOCATABLE, PUBLIC :: ORISFIP( : ) ! country/state/county code
        CHARACTER(ORSLEN3), ALLOCATABLE, PUBLIC :: ORISLST( : ) ! ORIS codes
        CHARACTER(DSCLEN3), ALLOCATABLE, PUBLIC :: ORISDSC( : ) ! ORIS desc
        
!.........  Flags ans settings for optional file attributes
        INTEGER, PUBLIC :: STCYPOPYR = 0       ! year of population data

        LOGICAL, PUBLIC :: LSTCYPOP  = .FALSE. ! true: population is present

        END MODULE MODSTCY
