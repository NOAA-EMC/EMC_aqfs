       MODULE MOD_RW

! variable EMIS_MODEL is expanded to include species dimension

       REAL, ALLOCATABLE, TARGET :: EMIS_MODEL (:, :, :)
!      REAL, ALLOCATABLE :: EMIS_MODEL (:, :)

! variable ARRAY_PT is used to associate a particular species
! in EMIS_MODEL, EMISL

       REAL, POINTER :: ARRAY_PT (:, :)

! variable RW_M_INDEX and RW_HR_INDEX are used to store m_index(jdate)
! and hr_index(jdate,jtime), which are used to output the correct
! pm_emis data, in nlsqrm6.f so the corresponding pm_emis data can be
! determined in mrggrid.f

       INTEGER :: RW_M_INDEX, RW_HR_INDEX

       LOGICAL, SAVE :: STORE_FILE

       END MODULE MOD_RW
