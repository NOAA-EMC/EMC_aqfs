	subroutine check_mismatch

        implicit none

        include 'PARMS3.EXT'

        integer :: mxdlen, namlen, mxfile, mxvars, mxdesc, mxlays, mxatts

        call ioparms3 (mxdlen, namlen, mxfile, mxvars, mxdesc, mxlays, mxatts)

        if (( mxdlen .ne. mxdlen3 ) .or. ( namlen .ne. namlen3 ) .or.&
            ( mxfile .ne. mxfile3 ) .or. ( mxvars .ne. mxvars3 ) .or.&
            ( mxdesc .ne. mxdesc3 ) .or. ( mxlays .ne. mxlays3 ) .or.&
            ( mxatts .ne. mxatts3 )) then
           write (6, *) ' '
           write (6, '(a28)') ' Error: Mismatch variable(s)'
           write (6, *) ' '
           write (6, '(a33)') ' variable    library    fixed_src'
           write (6, '(a33)') ' --------    -------    ---------'
           write (6, '(a8, 5x, i7, 4x, i9)') ' mxfile ', mxfile, mxfile3
           write (6, '(a8, 5x, i7, 4x, i9)') ' mxvars ', mxvars, mxvars3
           write (6, *) ' '
           stop
        end if

	end subroutine check_mismatch
