SUBROUTINE null_x 

!-------------------------------------------------------------------------------
! Name:     Nullify X Pointers
! Purpose:  Nullify X pointers to transfer arrays.
! Notes:    Liberally adapted from EPA's MCIP2.
! Revised:  27 Feb 2002  Original version in MCIP2.  (EPA/ORD/NERL/AMD)
!           18 Mar 2003  Removed XJDRATE.  (T. Otte)
!           10 Apr 2003  Modified for PREMAQ.  (T. Otte)
!-------------------------------------------------------------------------------

  USE xvars

!-------------------------------------------------------------------------------
! Nullify pointers to variable names in the XDEPIDX array.
!-------------------------------------------------------------------------------

  NULLIFY ( lso2      )   ;   NULLIFY ( lso4      )   ;   NULLIFY ( lno2      )
  NULLIFY ( lno       )   ;   NULLIFY ( lo3       )   !   [01 - 05]

  NULLIFY ( lhno3     )   ;   NULLIFY ( lh2o2     )   ;   NULLIFY ( lald      )
  NULLIFY ( lhcho     )   ;   NULLIFY ( lop       )   !   [06 - 10]

  NULLIFY ( lpaa      )   ;   NULLIFY ( lora      )   ;   NULLIFY ( lnh3      )
  NULLIFY ( lpan      )   ;   NULLIFY ( lhono     )   !   [11 - 15]

  NULLIFY ( lco       )   ;   NULLIFY ( lmethanol )   ;   NULLIFY ( ln2o5     )
  NULLIFY ( lno3      )   ;   NULLIFY ( lgald     )   !   [16 - 20]

END SUBROUTINE null_x
