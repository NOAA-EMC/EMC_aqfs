! ----------------------------------------------------------------------------  
! to computer pm25 from conc file
!
! Author: David Wong, NOAA
!
! Protype: Oct, 2007
!
! outfname  -- output file
! vfname  -- variable list file
! vlist(:,:) -- each column, the first element is the name of the new variable,
!               and from second onward are the variables used to form the new
!               variable
! ----------------------------------------------------------------------------  
                                                                                
        program csum

        use get_env_module
        use data_module

        implicit none

        type(formula_type), allocatable :: formula(:)

        integer :: n_dif_file, n_file, n_formula, stat, nrows,
     &             ncols, nlays, sdate, stime, timeinc, n_output_spc
        logical :: colsum
        character (len = 16) :: outfname

        interface
          subroutine extract_vlist (formula, n_formula, n_output_spc)
            use data_module
            type(formula_type), intent(inout) :: formula(:)
            integer, intent(in)               :: n_formula
            integer, intent(inout)            :: n_output_spc
          end subroutine extract_vlist

          subroutine setup (outfname, formula, n_formula, n_output_spc, nrows,
     &                      ncols, nlays, sdate, stime, timeinc, colsum)
            use data_module
            character (len = 16), intent(in) :: outfname
            type(formula_type), intent(in)   :: formula(:)
            integer, intent(in)              :: n_formula, n_output_spc
            logical, intent(in)              :: colsum
            integer, intent(out)             :: nrows, ncols, nlays, 
     &                                          sdate, stime, timeinc
          end subroutine setup

          subroutine compute (outfname, formula, n_formula, nrows, ncols,
     &                      nlays, sdate, stime, timeinc, colsum)
            use data_module
            character (len=16), intent(in) :: outfname
            type(formula_type), intent(in) :: formula(:)
            integer, intent(in)            :: n_formula, nrows, ncols, nlays,
     &                                        sdate, stime, timeinc
            logical, intent(in) :: colsum
          end subroutine compute

        end interface

        outfname = 'outfile'

        call get_env (n_formula, 'n_formula', 1)
        call get_env (colsum, 'colsum', .false.)

        allocate (formula(n_formula), stat=stat)

        n_output_spc = 0

        call extract_vlist (formula, n_formula, n_output_spc)

        call setup (outfname, formula, n_formula, n_output_spc, nrows,
     &              ncols, nlays, sdate, stime, timeinc, colsum) 

        call  compute (outfname, formula, n_formula, nrows, ncols,
     &                 nlays, sdate, stime, timeinc, colsum)

        deallocate (formula)

        end program csum
