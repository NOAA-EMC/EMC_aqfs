        PROGRAM TESTQUX

        IMPLICIT NONE

        INCLUDE  'CTEST.EXT'
        EXTERNAL  INITQUX
        CALL      INITQUX
        WRITE( *,* ) 'FOO = ', FOO
        WRITE( *,* ) 'BAR = ', BAR
        STOP
        END
