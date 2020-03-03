
PROGRAM ENVTEST

    USE M3UTILIO
    IMPLICIT NONE

    INTEGER, PARAMETER ::  MXLIST = 256
    
    INTEGER         ILIST( MXLIST )
    REAL            RLIST( MXLIST )
    CHARACTER(16)   CLIST( MXLIST )
    
    INTEGER         NLIST, N, LDEV
    LOGICAL         EFLAG, AFLAG
    CHARACTER(256)  MESG
    
    
    LDEV  = INIT3()
    EFLAG = .FALSE.

    WRITE( LDEV, '( 5X, A )' )  'Testing ENVLIST() etc.', ' '
    
    IF ( .NOT. STRLIST( 'FOO', 'List of STRING variables', MXLIST, NLIST, CLIST ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Bad environment variable "FOO"'
        CALL M3MESG( MESG )
    ELSE
        WRITE( *, '(5X, A )' ) 'STRLIST( "FOO", ... )'
        DO N = 1, NLIST
            WRITE( *, '( I6, A, 2X, A )' ) N, ':', CLIST( N )
        END DO
    END IF
    
    IF ( .NOT. ENVLIST( 'FOO', 'List of STRING variables', MXLIST, NLIST, CLIST ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Bad environment variable "FOO"'
        CALL M3MESG( MESG )
    ELSE
        WRITE( *, '(5X, A )' ) 'ENVLIST( "FOO", ... )'
        DO N = 1, NLIST
            WRITE( *, '( I6, A, 2X, A )' ) N, ':', CLIST( N )
        END DO
    END IF
    
    IF ( .NOT. INTLIST( 'BAR', 'List of INT variables', MXLIST, NLIST, ILIST ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Bad environment variable "BAR"'
        CALL M3MESG( MESG )
    ELSE
        WRITE( *, '(5X, A )' ) 'STRLIST( "BAR", ... )'
        DO N = 1, NLIST
            WRITE( *, '( I6, A, 2X, I10 )' ) N, ':', ILIST( N )
        END DO
    END IF
    
    IF ( .NOT. ENVLIST( 'BAR', 'List of INT variables', MXLIST, NLIST, ILIST ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Bad environment variable "BAR"'
        CALL M3MESG( MESG )
    ELSE
        WRITE( *, '(5X, A )' ) 'ENVLIST( "BAR", ... )'
        DO N = 1, NLIST
            WRITE( *, '( I6, A, 2X, I10 )' ) N, ':', ILIST( N )
        END DO
    END IF
    
    IF ( .NOT. REALIST( 'QUX', 'List of REAL variables', MXLIST, NLIST, RLIST ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Bad environment variable "QUX"'
        CALL M3MESG( MESG )
    ELSE
        WRITE( *, '(5X, A )' ) 'REALIST( "QUX", ... )'
        DO N = 1, NLIST
            WRITE( *, '( I6, A, 2X, G12.6 )' ) N, ':', RLIST( N )
        END DO
    END IF
    
    IF ( .NOT. ENVLIST( 'QUX', 'List of REAL variables', MXLIST, NLIST, RLIST ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Bad environment variable "QUX"'
        CALL M3MESG( MESG )
    ELSE
        WRITE( *, '(5X, A )' ) 'ENVLIST( "QUX", ... )'
        DO N = 1, NLIST
            WRITE( *, '( I6, A, 2X, G12.6 )' ) N, ':', RLIST( N )
        END DO
    END IF
    
    IF ( EFLAG ) THEN
        CALL M3EXIT( 'ENVTEST', 0, 0, 'Failure', 2 )
    ELSE
        CALL M3EXIT( 'ENVTEST', 0, 0, 'Success', 0 )
    END IF


END PROGRAM ENVTEST

