
C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/STENEX/src/se_snl/se_ori_ext.f,v 1.1 2004/03/26 16:16:47 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C --------------------------------------------------------------------------
C data_ori -- data structure orientation
C             The conversion between the logical grid point structure and the 
C             physical data structure is handled in the code by this flag. At 
C             this point, two values are available: "cr" and "rc", which means 
C             (COL, ROW), (ROW, COL) in the data structure, respectively.
C
C geo_ori  -- geometry orientation
C               0 -- cartessian coordination orientation
C               1 -- regular matrix orientation
C 
C for details, see readme file
C --------------------------------------------------------------------------

        module se_ori_ext

          implicit none

          character (len=2) :: se_data_ori
          integer :: se_geo_ori

        end module se_ori_ext
