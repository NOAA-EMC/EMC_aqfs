        !COMPILER-GENERATED INTERFACE MODULE: Tue Oct 10 20:03:56 2017
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE READ_LINE__genmod
          INTERFACE 
            SUBROUTINE READ_LINE(FILENUM,LINE,LINE_LEN,STATUS,LINE_NUM)
              INTEGER(KIND=4) :: FILENUM
              CHARACTER(*) :: LINE
              INTEGER(KIND=4) :: LINE_LEN
              INTEGER(KIND=4) :: STATUS
              INTEGER(KIND=4) :: LINE_NUM
            END SUBROUTINE READ_LINE
          END INTERFACE 
        END MODULE READ_LINE__genmod