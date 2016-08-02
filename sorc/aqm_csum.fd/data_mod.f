       module data_module

         integer, parameter :: max_num_spc = 150

         type formula_type
           character (len = 16) :: formula_spc_name
           character (len = 20) :: formula_spc_unit
           integer              :: n_spc
           logical              :: output_spc
           integer              :: group(0:20)
           character (len = 16) :: spc_name(max_num_spc)      ! species name
           integer              :: file_number(max_num_spc)   ! file number where the species is from
           integer              :: spc_index(max_num_spc)
           real                 :: coef(max_num_spc)          ! coefficient associated with a species
           real                 :: constant(10)
           integer              :: n_constant
           character (len = 1)  :: operator(max_num_spc)
           character (len = 1)  :: sequence(max_num_spc)
           integer              :: n_seq
         end type formula_type

       end module data_module
