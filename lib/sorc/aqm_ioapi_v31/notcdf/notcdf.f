
C.........................................................................
C Version "@(#)$Header$"
C Models-3 *not*CDF interface.
C Copyright (C) 2003 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C
C  FUNCTION:
C       To be used with EDSS/Models-3 I/O API version 3 or later,
C       to satisfy linker references to netCDF in environments
C       for which calls to "libnetcdf.a" are forbidden.
C
C  CONTAINS
C       a complete rendering of the netCDF version 2 and version 3
C       Fortran interfaces, in optionally-fatal "dummy-wrapper" form.
C
C  PRECONDITIONS:
C       setenv  NOTCDF_FATAL    [Y|N], according to whether the first
C                               notCDF call should be fatal or not
C
C  REVISION  HISTORY:
C       prototype 12/2003 by Carlie J. Coats, Jr.,Baron Advanced
C       Meteorological Systems.
C
C.........................................................................

        LOGICAL FUNCTION NCF_STAT()
        IMPLICIT NONE
        LOGICAL  ENVYN
        INTEGER  INITLOG3
        EXTERNAL ENVYN, INITLOG3
        LOGICAL  FIRSTIME, STATUS
        SAVE     FIRSTIME, STATUS
        DATA     FIRSTIME, STATUS / 2 * .TRUE. /
        INTEGER  LOGDEV
        INTEGER  IOS
        IF ( FIRSTIME ) THEN
            LOGDEV = INITLOG3( 'notCDF' )
            STATUS = ENVYN( 'NOTCDF_FATAL',
     &          'Cause fatal error upon call to "notCDF"',
     &          .TRUE., IOS )
            IF ( IOS .GT. 0 ) THEN
                CALL M3EXIT( 'NOTCDF', 0, 0,
     &          'Badly formatted environment variable "NOTCDF_FATAL"',
     &          2 )
            END IF
            FIRSTIME = .FALSE.
        END IF
        NCF_STAT = STATUS
        RETURN
        END

        CHARACTER*80 FUNCTION  NF_INQ_LIBVERS()
        IMPLICIT NONE
        LOGICAL  NCF_STAT
        EXTERNAL NCF_STAT
        CALL M3ERR( 'NF_INQ_LIBVERS',0,0, 
     &              'notCDF call to NF_INQ_LIBVERS', .FALSE. )
        NF_INQ_LIBVERS = 'notCDF version 1.0'
        RETURN
        END

        CHARACTER*80 FUNCTION  NF_STRERROR  (NCERR)
        IMPLICIT NONE
        INTEGER       NCERR
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_STRERROR',0,0, 
     &              'notCDF call to NF_STRERROR', NCF_STAT() )
        NF_STRERROR = 'DO NOT CALL notCDF'
        NCERR       = 2
        RETURN
        END

        INTEGER FUNCTION  NF_CREATE (PATH, CMODE, NCID)
        IMPLICIT NONE
        CHARACTER*(*) PATH
        INTEGER       CMODE
        INTEGER       NCID
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_CREATE',0,0, 
     &              'notCDF call to NF_CREATE', NCF_STAT() )
        NF_CREATE = -1
        RETURN
        END

        INTEGER FUNCTION  NF_OPEN(PATH, MODE, NCID)
        IMPLICIT NONE
        CHARACTER*(*) PATH
        INTEGER       MODE
        INTEGER       NCID
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_OPEN',0,0, 
     &              'notCDF call to NF_OPEN', NCF_STAT() )
        NF_OPEN = -1
        RETURN
        END

        INTEGER FUNCTION  NF_SET_FILL(NCID, FILLMODE, OLD_MODE)
        IMPLICIT NONE
        INTEGER       NCID, FILLMODE, OLD_MODE
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_SET_FILL',0,0, 
     &              'notCDF call to NF_SET_FILL', NCF_STAT() )
        NF_SET_FILL = -1
        RETURN
        END

        INTEGER FUNCTION  NF_REDEF(NCID)
        IMPLICIT NONE
        INTEGER       NCID
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_REDEF',0,0, 
     &              'notCDF call to NF_REDEF', NCF_STAT() )
        NF_REDEF = -1
        RETURN
        END

        INTEGER FUNCTION  NF_ENDDEF(NCID)
        IMPLICIT NONE
        INTEGER       NCID
        LOGICAL  NCF_STAT
        EXTERNAL NCF_STAT
        CALL M3ERR( 'NF_ENDDEF',0,0, 
     &              'notCDF call to NF_ENDDEF', NCF_STAT() )
        NF_ENDDEF = -1
        RETURN
        END

        INTEGER FUNCTION  NF_SYNC(NCID)
        IMPLICIT NONE
        INTEGER       NCID
        LOGICAL  NCF_STAT
        EXTERNAL NCF_STAT
        CALL M3ERR( 'NF_SYNC',0,0, 
     &              'notCDF call to NF_SYNC', NCF_STAT() )
        NF_SYNC = -1
        RETURN
        END

        INTEGER FUNCTION  NF_ABORT(NCID)
        IMPLICIT NONE
        INTEGER       NCID
        LOGICAL  NCF_STAT
        EXTERNAL NCF_STAT
        CALL M3ERR( 'NF_ABORT',0,0, 
     &              'notCDF call to NF_ABORT', NCF_STAT() )
        NF_ABORT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_CLOSE(NCID)
        IMPLICIT NONE
        INTEGER       NCID
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_CLOSE',0,0, 
     &              'notCDF call to NF_CLOSE', NCF_STAT() )
        NF_CLOSE = -1
        RETURN
        END

        INTEGER FUNCTION  NF_INQ(NCID, NDIMS, NVARS, NGATTS, UNLIMDIMID)
        IMPLICIT NONE
        INTEGER       NCID, NDIMS, NVARS, NGATTS, UNLIMDIMID
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_INQ',0,0, 'notCDF call to NF_INQ', NCF_STAT() )
        NF_INQ = -1
        RETURN
        END

        INTEGER FUNCTION  NF_INQ_NDIMS(NCID, NDIMS)
        IMPLICIT NONE
        INTEGER       NCID, NDIMS
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_INQ_NDIMS',0,0, 
     &              'notCDF call to NF_INQ_NDIMS', NCF_STAT() )
        NF_INQ_NDIMS = -1
        RETURN
        END

        INTEGER FUNCTION  NF_INQ_NVARS(NCID, NVARS)
        IMPLICIT NONE
        INTEGER       NCID, NVARS
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_INQ_NVARS',0,0, 
     &              'notCDF call to NF_INQ_NVARS', NCF_STAT() )
        NF_INQ_NVARS = -1
        RETURN
        END

        INTEGER FUNCTION  NF_INQ_NATTS(NCID, NATTS)
        IMPLICIT NONE
        INTEGER       NCID, NATTS
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_INQ_NATTS',0,0, 
     &              'notCDF call to NF_INQ_NATTS', NCF_STAT() )
        NF_INQ_NATTS = -1
        RETURN
        END

        INTEGER FUNCTION  NF_INQ_UNLIMDIM(NCID, UNLIMDIMID)
        IMPLICIT NONE
        INTEGER       NCID, UNLIMDIMID
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_INQ_UNLIMDIM',0,0, 
     &              'notCDF call to NF_INQ_UNLIMDIM', NCF_STAT() )
        NF_INQ_UNLIMDIM = -1
        RETURN
        END

        INTEGER FUNCTION  NF_DEF_DIM(NCID, NAME, ILEN, DIMID)
        IMPLICIT NONE
        INTEGER       NCID, ILEN, DIMID
        CHARACTER*(*) NAME
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_DEF_DIM',0,0, 
     &              'notCDF call to NF_DEF_DIM', NCF_STAT() )
        NF_DEF_DIM = -1
        RETURN
        END

        INTEGER FUNCTION  NF_INQ_DIMID(NCID, NAME, DIMID)
        IMPLICIT NONE
        INTEGER       NCID
        CHARACTER*(*) NAME
        INTEGER       DIMID
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_DEF_DIM',0,0, 
     &              'notCDF call to NF_DEF_DIM', NCF_STAT() )
        NF_INQ_DIMID = -1
        RETURN
        END

        INTEGER FUNCTION  NF_INQ_DIM(NCID, DIMID, NAME, DLEN)
        IMPLICIT NONE
        INTEGER       NCID
        CHARACTER*(*) NAME
        INTEGER       DIMID
        INTEGER       DLEN
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_INQ_DIM',0,0, 
     &              'notCDF call to NF_INQ_DIM', NCF_STAT() )
        NF_INQ_DIM = -1
        RETURN
        END

        INTEGER FUNCTION  NF_INQ_DIMNAME(NCID, DIMID, NAME)
        IMPLICIT NONE
        INTEGER       NCID
        CHARACTER*(*) NAME
        INTEGER       DIMID
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_INQ_DIMNAME',0,0, 
     &              'notCDF call to NF_INQ_DIMNAME', NCF_STAT() )
        NF_INQ_DIMNAME = -1
        RETURN
        END

        INTEGER FUNCTION  NF_INQ_DIMLEN(NCID, DIMID, DLEN)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       DIMID
        INTEGER       DLEN
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_INQ_DIMLEN',0,0, 
     &              'notCDF call to NF_INQ_DIMLEN', NCF_STAT() )
        NF_INQ_DIMLEN = -1
        RETURN
        END

        INTEGER FUNCTION  NF_RENAME_DIM(NCID, DIMID, NAME)
        IMPLICIT NONE
        INTEGER       NCID
        CHARACTER*(*) NAME
        INTEGER       DIMID
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_RENAME_DIM',0,0, 
     &              'notCDF call to NF_RENAME_DIM', NCF_STAT() )
        NF_RENAME_DIM = -1
        RETURN
        END

        INTEGER FUNCTION  NF_DEF_VAR(NCID, NAME, XTYPE,
     &                               NDIMS, DIMID,  VARID)
        IMPLICIT NONE
        INTEGER       NCID
        CHARACTER*(*) NAME
        INTEGER       XTYPE
        INTEGER       NDIMS
        INTEGER       DIMID
        INTEGER       VARID
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_DEF_VAR',0,0, 
     &              'notCDF call to NF_DEF_VAR', NCF_STAT() )
        NF_DEF_VAR = -1
        RETURN
        END

        INTEGER FUNCTION  NF_INQ_VAR(NCID, VARID, NAME, XTYPE,
     &                               NDIMS,  DIMID, NATTS)
        IMPLICIT NONE
        INTEGER       NCID, VARID
        CHARACTER*(*) NAME
        INTEGER       XTYPE
        INTEGER       NDIMS
        INTEGER       DIMID
        INTEGER       NATTS
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_INQ_VAR',0,0, 
     &              'notCDF call to NF_INQ_VAR', NCF_STAT() )
        NF_INQ_VAR = -1
        RETURN
        END

        INTEGER FUNCTION  NF_INQ_VARID(NCID, NAME, VARID)
        IMPLICIT NONE
        INTEGER       NCID
        CHARACTER*(*) NAME
        INTEGER       VARID
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_INQ_VARID',0,0, 
     &              'notCDF call to NF_INQ_VARID', NCF_STAT() )
        NF_INQ_VARID = -1
        RETURN
        END

        INTEGER FUNCTION  NF_INQ_VARNAME(NCID, VARID, NAME)
        IMPLICIT NONE
        INTEGER       NCID
        CHARACTER*(*) NAME
        INTEGER       VARID
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_INQ_VARNAME',0,0, 
     &              'notCDF call to NF_INQ_VARNAME', NCF_STAT() )
        NF_INQ_VARNAME = -1
        RETURN
        END

        INTEGER FUNCTION  NF_INQ_VARTYPE(NCID, VARID, XTYPE)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       XTYPE
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_INQ_VARTYPE',0,0, 
     &              'notCDF call to NF_INQ_VARTYPE', NCF_STAT() )
        NF_INQ_VARTYPE = -1
        RETURN
        END

        INTEGER FUNCTION  NF_INQ_VARNDIMS(NCID, VARID, NDIMS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       NDIMS
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_INQ_VARNDIMS',0,0, 
     &              'notCDF call to NF_INQ_VARNDIMS', NCF_STAT() )
        NF_INQ_VARNDIMS = -1
        RETURN
        END

        INTEGER FUNCTION  NF_INQ_VARDIMID(NCID, VARID, DIMID)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       DIMID
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_INQ_VARDIMID',0,0, 
     &              'notCDF call to NF_INQ_VARDIMID', NCF_STAT() )
        NF_INQ_VARDIMID = -1
        RETURN
        END

        INTEGER FUNCTION  NF_INQ_VARNATTS (NCID, VARID, NATTS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       NATTS
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_INQ_VARNATTS',0,0, 
     &              'notCDF call to NF_INQ_VARNATTS', NCF_STAT() )
        NF_INQ_VARNATTS = -1
        RETURN
        END

        INTEGER FUNCTION  NF_RENAME_VAR(NCID, VARID, NAME)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        CHARACTER*(*) NAME
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_RENAME_VAR',0,0, 
     &              'notCDF call to NF_RENAME_VAR', NCF_STAT() )
        NF_RENAME_VAR = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VAR_TEXT(NCID, VARID, TEXT)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        CHARACTER*(*) TEXT
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VAR_TEXT',0,0, 
     &              'notCDF call to NF_PUT_VAR_TEXT', NCF_STAT() )
        NF_PUT_VAR_TEXT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VAR_TEXT (NCID, VARID, TEXT)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        CHARACTER*(*) TEXT
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VAR_TEXT',0,0, 
     &              'notCDF call to NF_GET_VAR_TEXT', NCF_STAT() )
        NF_GET_VAR_TEXT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VAR_INT1(NCID, VARID, I1VAL)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER*1     I1VAL
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VAR_INT1',0,0, 
     &              'notCDF call to NF_PUT_VAR_INT1', NCF_STAT() )
        NF_PUT_VAR_INT1 = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VAR_INT1(NCID, VARID, I1VAL)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER*1     I1VAL
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VAR_INT1',0,0, 
     &              'notCDF call to NF_GET_VAR_INT1', NCF_STAT() )
        NF_GET_VAR_INT1 = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VAR_INT2(NCID, VARID, I2VAL)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER*2     I2VAL
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VAR_INT2',0,0, 
     &              'notCDF call to NF_PUT_VAR_INT2', NCF_STAT() )
        NF_PUT_VAR_INT2 = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VAR_INT2(NCID, VARID, I2VAL)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER*2     I2VAL
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VAR_INT2',0,0, 
     &              'notCDF call to NF_GET_VAR_INT2', NCF_STAT() )
        NF_GET_VAR_INT2 = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VAR_INT (NCID, VARID, IVAL)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       IVAL
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VAR_INT',0,0, 
     &              'notCDF call to NF_PUT_VAR_INT', NCF_STAT() )
        NF_PUT_VAR_INT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VAR_INT (NCID, VARID, IVAL)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       IVAL
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VAR_INT',0,0, 
     &              'notCDF call to NF_GET_VAR_INT', NCF_STAT() )
        NF_GET_VAR_INT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VAR_REAL(NCID, VARID, RVAL)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        REAL          RVAL
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VAR_REAL',0,0, 
     &              'notCDF call to NF_PUT_VAR_REAL', NCF_STAT() )
        NF_PUT_VAR_REAL = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VAR_REAL(NCID, VARID, RVAL)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        REAL          RVAL
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VAR_REAL',0,0, 
     &              'notCDF call to NF_GET_VAR_REAL', NCF_STAT() )
        NF_GET_VAR_REAL = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VAR_DOUBLE (NCID, VARID, DVAL)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        DOUBLE PRECISION DVAL
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VAR_DOUBLE',0,0, 
     &              'notCDF call to NF_PUT_VAR_DOUBLE', NCF_STAT() )
        NF_PUT_VAR_DOUBLE = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VAR_DOUBLE (NCID, VARID, DVAL)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        DOUBLE PRECISION DVAL
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VAR_DOUBLE',0,0, 
     &              'notCDF call to NF_GET_VAR_DOUBLE', NCF_STAT() )
        NF_GET_VAR_DOUBLE = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VAR1_TEXT(NCID, VARID, INDEX, TEXT)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       INDEX
        CHARACTER*(*) TEXT
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VAR1_TEXT',0,0, 
     &              'notCDF call to NF_PUT_VAR1_TEXT', NCF_STAT() )
        NF_PUT_VAR1_TEXT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VAR1_TEXT(NCID, VARID, INDEX, TEXT)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       INDEX
        CHARACTER*(*) TEXT
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VAR1_TEXT',0,0, 
     &              'notCDF call to NF_GET_VAR1_TEXT', NCF_STAT() )
        NF_GET_VAR1_TEXT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VAR1_INT1(NCID, VARID, INDEX, I1VAL)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       INDEX
        INTEGER*1     I1VAL
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VAR1_INT1',0,0, 
     &              'notCDF call to NF_PUT_VAR1_INT1', NCF_STAT() )
        NF_PUT_VAR1_INT1 = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VAR1_INT1(NCID, VARID, INDEX, I1VAL)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       INDEX
        INTEGER*1     I1VAL
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VAR1_INT1',0,0, 
     &              'notCDF call to NF_GET_VAR1_INT1', NCF_STAT() )
        NF_GET_VAR1_INT1 = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VAR1_INT2(NCID, VARID, INDEX, I2VAL)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       INDEX
        INTEGER*2     I2VAL
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VAR1_INT2',0,0, 
     &              'notCDF call to NF_PUT_VAR1_INT2', NCF_STAT() )
        NF_PUT_VAR1_INT2 = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VAR1_INT2(NCID, VARID, INDEX, I2VAL)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       INDEX
        INTEGER*2     I2VAL
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VAR1_INT2',0,0, 
     &              'notCDF call to NF_GET_VAR1_INT2', NCF_STAT() )
        NF_GET_VAR1_INT2 = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VAR1_INT(NCID, VARID, INDEX, IVAL)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       INDEX
        INTEGER       IVAL
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VAR1_INT',0,0, 
     &              'notCDF call to NF_PUT_VAR1_INT', NCF_STAT() )
        NF_PUT_VAR1_INT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VAR1_INT(NCID, VARID, INDEX, IVAL)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       INDEX
        INTEGER       IVAL
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VAR1_INT',0,0, 
     &              'notCDF call to NF_GET_VAR1_INT', NCF_STAT() )
        NF_GET_VAR1_INT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VAR1_REAL(NCID, VARID, INDEX, RVAL)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       INDEX
        REAL          RVAL
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VAR1_REAL',0,0, 
     &              'notCDF call to NF_PUT_VAR1_REAL', NCF_STAT() )
        NF_PUT_VAR1_REAL = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VAR1_REAL(NCID, VARID, INDEX, RVAL)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       INDEX
        REAL          RVAL
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VAR1_REAL',0,0, 
     &              'notCDF call to NF_GET_VAR1_REAL', NCF_STAT() )
        NF_GET_VAR1_REAL = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VAR1_DOUBLE(NCID, VARID, INDEX, DVAL)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       INDEX
        DOUBLEPRECISION  DVAL
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VAR1_DOUBLE',0,0, 
     &              'notCDF call to NF_PUT_VAR1_DOUBLE', NCF_STAT() )
        NF_PUT_VAR1_DOUBLE = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VAR1_DOUBLE(NCID, VARID, INDEX, DVAL)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       INDEX
        DOUBLEPRECISION  DVAL
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VAR1_DOUBLE',0,0, 
     &              'notCDF call to NF_GET_VAR1_DOUBLE', NCF_STAT() )
        NF_GET_VAR1_DOUBLE = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VARA_TEXT(NCID, VARID,
     &                                     START, COUNT, TEXT)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START, COUNT
        CHARACTER*(*) TEXT(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VARA_TEXT',0,0, 
     &              'notCDF call to NF_PUT_VARA_TEXT', NCF_STAT() )
        NF_PUT_VARA_TEXT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VARA_TEXT(NCID, VARID,
     &                                     START, COUNT, TEXT)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START, COUNT
        CHARACTER*(*) TEXT(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VARA_TEXT',0,0, 
     &              'notCDF call to NF_GET_VARA_TEXT', NCF_STAT() )
        NF_GET_VARA_TEXT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VARA_INT1(NCID, VARID,
     &                                     START, COUNT, I1VALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START, COUNT
        INTEGER*1     I1VALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VARA_INT1',0,0, 
     &              'notCDF call to NF_PUT_VARA_INT1', NCF_STAT() )
        NF_PUT_VARA_INT1 = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VARA_INT1(NCID, VARID,
     &                                     START, COUNT, I1VALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START, COUNT
        INTEGER*1     I1VALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VARA_INT1',0,0, 
     &              'notCDF call to NF_GET_VARA_INT1', NCF_STAT() )
        NF_GET_VARA_INT1 = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VARA_INT2(NCID, VARID,
     &                                     START, COUNT, I2VALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START, COUNT
        INTEGER*2     I2VALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VARA_INT2',0,0, 
     &              'notCDF call to NF_PUT_VARA_INT2', NCF_STAT() )
        NF_PUT_VARA_INT2 = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VARA_INT2(NCID, VARID,
     &                                     START, COUNT, I2VALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START, COUNT
        INTEGER*2     I2VALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VARA_INT2',0,0, 
     &              'notCDF call to NF_GET_VARA_INT2', NCF_STAT() )
        NF_GET_VARA_INT2 = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VARA_INT(NCID, VARID,
     &                                    START, COUNT, IVALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START, COUNT
        INTEGER       IVALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VARA_INT',0,0, 
     &              'notCDF call to NF_PUT_VARA_INT', NCF_STAT() )
        NF_PUT_VARA_INT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VARA_INT (NCID, VARID,
     &                                     START, COUNT, IVALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START, COUNT
        INTEGER       IVALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VARA_INT',0,0, 
     &              'notCDF call to NF_GET_VARA_INT', NCF_STAT() )
        NF_GET_VARA_INT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VARA_REAL(NCID, VARID,
     &                                     START, COUNT, RVALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START, COUNT
        REAL          RVALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VARA_REAL',0,0, 
     &              'notCDF call to NF_PUT_VARA_REAL', NCF_STAT() )
        NF_PUT_VARA_REAL = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VARA_REAL(NCID, VARID,
     &                                     START, COUNT, RVALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START, COUNT
        REAL          RVALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VARA_REAL',0,0, 
     &              'notCDF call to NF_GET_VARA_REAL', NCF_STAT() )
        NF_GET_VARA_REAL = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VARA_DOUBLE(NCID, VARID,
     &                                     START, COUNT, DVALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START, COUNT
        DOUBLEPRECISION DVALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VARA_DOUBLE',0,0, 
     &              'notCDF call to NF_PUT_VARA_DOUBLE', NCF_STAT() )
        NF_PUT_VARA_DOUBLE = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VARA_DOUBLE(NCID, VARID,
     &                                     START, COUNT, DVALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START, COUNT
        DOUBLEPRECISION DVALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VARA_DOUBLE',0,0, 
     &              'notCDF call to NF_GET_VARA_DOUBLE', NCF_STAT() )
        NF_GET_VARA_DOUBLE = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VARS_TEXT(NCID, VARID,
     &                                     START, COUNT, STRIDE, TEXT)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START(*), COUNT(*), STRIDE
        CHARACTER*(*) TEXT(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VARS_TEXT',0,0, 
     &              'notCDF call to NF_PUT_VARS_TEXT', NCF_STAT() )
        NF_PUT_VARS_TEXT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VARS_TEXT(NCID, VARID,
     &                                     START, COUNT, STRIDE, TEXT)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START(*), COUNT(*), STRIDE
        CHARACTER*(*) TEXT(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VARS_TEXT',0,0, 
     &              'notCDF call to NF_GET_VARS_TEXT', NCF_STAT() )
        NF_GET_VARS_TEXT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VARS_INT1(NCID, VARID,
     &                                     START, COUNT, STRIDE, I1VALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START(*), COUNT(*), STRIDE
        INTEGER*1     I1VALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VARS_INT1',0,0, 
     &              'notCDF call to NF_PUT_VARS_INT1', NCF_STAT() )
        NF_PUT_VARS_INT1 = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VARS_INT1(NCID, VARID,
     &                                     START, COUNT, STRIDE, I1VALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START(*), COUNT(*), STRIDE
        INTEGER*1     I1VALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VARS_INT1',0,0, 
     &              'notCDF call to NF_GET_VARS_INT1', NCF_STAT() )
        NF_GET_VARS_INT1 = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VARS_INT2(NCID, VARID,
     &                                     START, COUNT, STRIDE, I2VALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START(*), COUNT(*), STRIDE
        INTEGER*2     I2VALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VARS_INT2',0,0, 
     &              'notCDF call to NF_PUT_VARS_INT2', NCF_STAT() )
        NF_PUT_VARS_INT2 = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VARS_INT2(NCID, VARID,
     &                                     START, COUNT, STRIDE, I2VALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START(*), COUNT(*), STRIDE
        INTEGER*2     I2VALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VARS_INT2',0,0, 
     &              'notCDF call to NF_GET_VARS_INT2', NCF_STAT() )
        NF_GET_VARS_INT2 = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VARS_INT (NCID, VARID,
     &                                     START, COUNT, STRIDE, IVALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START(*), COUNT(*), STRIDE
        INTEGER       IVALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VARS_INT',0,0, 
     &              'notCDF call to NF_PUT_VARS_INT', NCF_STAT() )
        NF_PUT_VARS_INT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VARS_INT (NCID, VARID,
     &                                     START, COUNT, STRIDE, IVALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START(*), COUNT(*), STRIDE
        INTEGER       IVALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VARS_INT',0,0, 
     &              'notCDF call to NF_GET_VARS_INT', NCF_STAT() )
        NF_GET_VARS_INT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VARS_REAL(NCID, VARID,
     &                                     START, COUNT, STRIDE, RVALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START(*), COUNT(*), STRIDE
        REAL          RVALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VARS_REAL',0,0, 
     &              'notCDF call to NF_PUT_VARS_REAL', NCF_STAT() )
        NF_PUT_VARS_REAL = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VARS_REAL(NCID, VARID,
     &                                     START, COUNT, STRIDE, RVALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START(*), COUNT(*), STRIDE
        REAL          RVALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VARS_REAL',0,0, 
     &              'notCDF call to NF_GET_VARS_REAL', NCF_STAT() )
        NF_GET_VARS_REAL = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VARS_DOUBLE(NCID, VARID,
     &                                   START, COUNT, STRIDE, DVALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START(*), COUNT(*), STRIDE
        DOUBLEPRECISION DVALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VARS_DOUBLE',0,0, 
     &              'notCDF call to NF_PUT_VARS_DOUBLE', NCF_STAT() )
        NF_PUT_VARS_DOUBLE = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VARS_DOUBLE(NCID, VARID,
     &                                   START, COUNT, STRIDE, DVALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START(*), COUNT(*), STRIDE
        DOUBLEPRECISION DVALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VARS_DOUBLE',0,0, 
     &              'notCDF call to NF_GET_VARS_DOUBLE', NCF_STAT() )
        NF_GET_VARS_DOUBLE = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VARM_TEXT(NCID, VARID, START, COUNT,
     &                                     STRIDE, IMAP, TEXT)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START(*), COUNT(*), STRIDE
        INTEGER       IMAP(*)
        CHARACTER*(*) TEXT(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VARM_TEXT',0,0, 
     &              'notCDF call to NF_PUT_VARM_TEXT', NCF_STAT() )
        NF_PUT_VARM_TEXT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VARM_TEXT(NCID, VARID, START, COUNT,
     &                                     STRIDE, IMAP, TEXT)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START(*), COUNT(*), STRIDE
        INTEGER       IMAP(*)
        CHARACTER*(*) TEXT(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VARM_TEXT',0,0, 
     &              'notCDF call to NF_GET_VARM_TEXT', NCF_STAT() )
        NF_GET_VARM_TEXT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VARM_INT1(NCID, VARID, START, COUNT,
     &                                     STRIDE, IMAP, I1VALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START(*), COUNT(*), STRIDE
        INTEGER       IMAP(*)
        INTEGER*1     I1VALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VARM_INT1',0,0, 
     &              'notCDF call to NF_PUT_VARM_INT1', NCF_STAT() )
        NF_PUT_VARM_INT1 = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VARM_INT1(NCID, VARID, START, COUNT,
     &                                     STRIDE, IMAP, I1VALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START(*), COUNT(*), STRIDE
        INTEGER       IMAP(*)
        INTEGER*1     I1VALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VARM_INT1',0,0, 
     &              'notCDF call to NF_GET_VARM_INT1', NCF_STAT() )
        NF_GET_VARM_INT1 = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VARM_INT2(NCID, VARID, START, COUNT,
     &                                     STRIDE, IMAP, I2VALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START(*), COUNT(*), STRIDE
        INTEGER       IMAP(*)
        INTEGER*2     I2VALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VARM_INT2',0,0, 
     &              'notCDF call to NF_PUT_VARM_INT2', NCF_STAT() )
        NF_PUT_VARM_INT2 = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VARM_INT2(NCID, VARID, START, COUNT,
     &                                     STRIDE, IMAP, I2VALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START(*), COUNT(*), STRIDE
        INTEGER       IMAP(*)
        INTEGER*2     I2VALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VARM_INT2',0,0, 
     &              'notCDF call to NF_GET_VARM_INT2', NCF_STAT() )
        NF_GET_VARM_INT2 = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VARM_INT (NCID, VARID, START, COUNT,
     &                                     STRIDE, IMAP, IVALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START(*), COUNT(*), STRIDE
        INTEGER       IMAP(*)
        INTEGER       IVALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VARM_INT',0,0, 
     &              'notCDF call to NF_PUT_VARM_INT', NCF_STAT() )
        NF_PUT_VARM_INT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VARM_INT (NCID, VARID, START, COUNT,
     &                                     STRIDE, IMAP, IVALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START(*), COUNT(*), STRIDE
        INTEGER       IMAP(*)
        INTEGER       IVALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VARM_INT',0,0, 
     &              'notCDF call to NF_GET_VARM_INT', NCF_STAT() )
        NF_GET_VARM_INT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VARM_REAL(NCID, VARID, START, COUNT,
     &                                     STRIDE, IMAP, RVALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START(*), COUNT(*), STRIDE
        INTEGER       IMAP(*)
        REAL          RVALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VARM_REAL',0,0, 
     &              'notCDF call to NF_PUT_VARM_REAL', NCF_STAT() )
        NF_PUT_VARM_REAL = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VARM_REAL(NCID, VARID, START, COUNT,
     &                                     STRIDE, IMAP, RVALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START(*), COUNT(*), STRIDE
        INTEGER       IMAP(*)
        REAL          RVALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VARM_REAL',0,0, 
     &              'notCDF call to NF_GET_VARM_REAL', NCF_STAT() )
        NF_GET_VARM_REAL = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_VARM_DOUBLE(NCID, VARID, START, COUNT,
     &                                       STRIDE,  IMAP, DVALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START(*), COUNT(*), STRIDE
        INTEGER       IMAP(*)
        DOUBLEPRECISION  DVALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_VARM_DOUBLE',0,0, 
     &              'notCDF call to NF_PUT_VARM_DOUBLE', NCF_STAT() )
        NF_PUT_VARM_DOUBLE = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_VARM_DOUBLE(NCID, VARID, START, COUNT,
     &                                       STRIDE, IMAP, DVALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       START(*), COUNT(*), STRIDE
        INTEGER       IMAP(*)
        DOUBLEPRECISION  DVALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_VARM_DOUBLE',0,0, 
     &              'notCDF call to NF_GET_VARM_DOUBLE', NCF_STAT() )
        NF_GET_VARM_DOUBLE = -1
        RETURN
        END

        INTEGER FUNCTION  NF_INQ_ATT(NCID, VARID, NAME, XTYPE,ILEN)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        CHARACTER*(*) NAME
        INTEGER       XTYPE
        INTEGER       ILEN
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_INQ_ATT',0,0, 
     &              'notCDF call to NF_INQ_ATT', NCF_STAT() )
        NF_INQ_ATT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_INQ_ATTID(NCID, VARID, NAME, ATTNUM)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        CHARACTER*(*) NAME
        INTEGER       ATTNUM
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_INQ_ATTID',0,0, 
     &              'notCDF call to NF_INQ_ATTID', NCF_STAT() )
        NF_INQ_ATTID = -1
        RETURN
        END

        INTEGER FUNCTION  NF_INQ_ATTTYPE    (NCID, VARID, NAME, XTYPE)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        CHARACTER*(*) NAME
        INTEGER       XTYPE
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_INQ_ATTTYPE',0,0, 
     &              'notCDF call to NF_INQ_ATTTYPE', NCF_STAT() )
        NF_INQ_ATTTYPE = -1
        RETURN
        END

        INTEGER FUNCTION  NF_INQ_ATTLEN     (NCID, VARID, NAME, ILEN)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        CHARACTER*(*) NAME
        INTEGER       ILEN
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_INQ_ATTLEN',0,0, 
     &              'notCDF call to NF_INQ_ATTLEN', NCF_STAT() )
        NF_INQ_ATTLEN = -1
        RETURN
        END

        INTEGER FUNCTION  NF_INQ_ATTNAME    (NCID, VARID, ATTNUM, NAME)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        INTEGER       ATTNUM
        CHARACTER*(*) NAME
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_INQ_ATTNAME',0,0, 
     &              'notCDF call to NF_INQ_ATTNAME', NCF_STAT() )
        NF_INQ_ATTNAME = -1
        RETURN
        END

        INTEGER FUNCTION  NF_COPY_ATT(NCID_IN, VARID_IN, NAME,
     &                                NCID_OUT, VARID_OUT)
        IMPLICIT NONE
        INTEGER       NCID_IN,  NCID_OUT
        INTEGER       VARID_IN, VARID_OUT
        CHARACTER*(*) NAME
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_COPY_ATT',0,0, 
     &              'notCDF call to NF_COPY_ATT', NCF_STAT() )
        NF_COPY_ATT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_RENAME_ATT(NCID, VARID, CURNAME, NEWNAME)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        CHARACTER*(*) CURNAME, NEWNAME
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_RENAME_ATT',0,0, 
     &              'notCDF call to NF_RENAME_ATT', NCF_STAT() )
        NF_RENAME_ATT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_DEL_ATT(NCID, VARID, NAME)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        CHARACTER*(*) NAME
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_DEL_ATT',0,0, 
     &              'notCDF call to NF_DEL_ATT', NCF_STAT() )
        NF_DEL_ATT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_ATT_TEXT(NCID, VARID, NAME, ILEN, TEXT)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        CHARACTER*(*) NAME
        INTEGER       ILEN
        CHARACTER*(*) TEXT(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_ATT_TEXT',0,0, 
     &              'notCDF call to NF_PUT_ATT_TEXT', NCF_STAT() )
        NF_PUT_ATT_TEXT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_ATT_TEXT (NCID, VARID, NAME, TEXT)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        CHARACTER*(*) NAME
        CHARACTER*(*) TEXT(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_ATT_TEXT',0,0, 
     &              'notCDF call to NF_GET_ATT_TEXT', NCF_STAT() )
        NF_GET_ATT_TEXT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_ATT_INT1 (NCID, VARID, NAME,
     &                                     XTYPE, ILEN, I1VALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        CHARACTER*(*) NAME
        INTEGER       XTYPE, ILEN
        INTEGER*1     I1VALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_ATT_INT1',0,0, 
     &              'notCDF call to NF_PUT_ATT_INT1', NCF_STAT() )
        NF_PUT_ATT_INT1 = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_ATT_INT1 (NCID, VARID, NAME, I1VALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        CHARACTER*(*) NAME
        INTEGER*1     I1VALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_ATT_INT1',0,0, 
     &              'notCDF call to NF_GET_ATT_INT1', NCF_STAT() )
        NF_GET_ATT_INT1 = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_ATT_INT2 (NCID, VARID, NAME,
     &                                     XTYPE, ILEN, I2VALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        CHARACTER*(*) NAME
        INTEGER       XTYPE, ILEN
        INTEGER*2     I2VALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_ATT_INT2',0,0, 
     &              'notCDF call to NF_PUT_ATT_INT2', NCF_STAT() )
        NF_PUT_ATT_INT2 = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_ATT_INT2 (NCID, VARID, NAME, I2VALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        CHARACTER*(*) NAME
        INTEGER*2     I2VALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_ATT_INT2',0,0, 
     &              'notCDF call to NF_GET_ATT_INT2', NCF_STAT() )
        NF_GET_ATT_INT2 = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_ATT_INT(NCID, VARID, NAME,
     &                                   XTYPE, ILEN, IVALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        CHARACTER*(*) NAME
        INTEGER       XTYPE, ILEN
        INTEGER       IVALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_ATT_INT',0,0, 
     &              'notCDF call to NF_PUT_ATT_INT', NCF_STAT() )
        NF_PUT_ATT_INT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_ATT_INT(NCID, VARID, NAME, IVALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        CHARACTER*(*) NAME
        INTEGER       IVALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_ATT_INT',0,0, 
     &              'notCDF call to NF_GET_ATT_INT', NCF_STAT() )
        NF_GET_ATT_INT = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_ATT_REAL (NCID, VARID, NAME, XTYPE,
     &                                     ILEN, RVALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        CHARACTER*(*) NAME
        INTEGER       XTYPE, ILEN
        REAL          RVALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_ATT_REAL',0,0, 
     &              'notCDF call to NF_PUT_ATT_REAL', NCF_STAT() )
        NF_PUT_ATT_REAL = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_ATT_REAL (NCID, VARID, NAME, RVALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        CHARACTER*(*) NAME
        REAL          RVALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_ATT_REAL',0,0, 
     &              'notCDF call to NF_GET_ATT_REAL', NCF_STAT() )
        NF_GET_ATT_REAL = -1
        RETURN
        END

        INTEGER FUNCTION  NF_PUT_ATT_DOUBLE(NCID, VARID, NAME, XTYPE,
     &                                      ILEN, DVALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        CHARACTER*(*) NAME
        INTEGER       XTYPE, ILEN
        DOUBLEPRECISION DVALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_PUT_ATT_DOUBLE',0,0, 
     &              'notCDF call to NF_PUT_ATT_DOUBLE', NCF_STAT() )
        NF_PUT_ATT_DOUBLE = -1
        RETURN
        END

        INTEGER FUNCTION  NF_GET_ATT_DOUBLE(NCID, VARID, NAME, DVALS)
        IMPLICIT NONE
        INTEGER       NCID
        INTEGER       VARID
        CHARACTER*(*) NAME
        DOUBLEPRECISION DVALS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NF_GET_ATT_DOUBLE',0,0, 
     &              'notCDF call to NF_GET_ATT_DOUBLE', NCF_STAT() )
        NF_GET_ATT_DOUBLE = -1
        RETURN
        END

        INTEGER FUNCTION NCCRE(FNAME, CMODE, RCODE)
        IMPLICIT NONE
        CHARACTER*(*) FNAME
        INTEGER       CMODE, RCODE
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCCRE',0,0,'notCDF call to NCCRE', NCF_STAT() )
        NCCRE = -1
        RCODE = -1
        RETURN
        END

        INTEGER FUNCTION NCOPN(FNAME, RWMODE, RCODE)
        IMPLICIT NONE
        CHARACTER*(*) FNAME
        INTEGER       RWMODE, RCODE
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCOPN',0,0,'notCDF call to NCOPN', NCF_STAT() )
        NCOPN = -1
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCREDF(NCID, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, RCODE
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCREDF',0,0,'notCDF call to NCREDF', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCENDF(NCID, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, RCODE
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCENDF',0,0,'notCDF call to NCENDF', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCCLOS(NCID, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, RCODE
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCCLOS',0,0,'notCDF call to NCCLOS', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCINQ(NCID, NDIMS, NVARS, NATTS, RECDIM, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, NDIMS, NVARS, NATTS, RECDIM, RCODE
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCINQ',0,0,'notCDF call to NCINQ', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCSNC(NCID, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, RCODE
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCSNC',0,0,'notCDF call to NCSNC', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCABOR(NCID, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, RCODE
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCABOR',0,0,'notCDF call to NCABOR', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCDDEF(NCID, NAME, SIZE, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, SIZE, RCODE
        CHARACTER*(*) NAME
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCDDEF',0,0,'notCDF call to NCDDEF', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        INTEGER FUNCTION NCDID(NCID, NAME, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, RCODE
        CHARACTER*(*) NAME
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCDID',0,0,'notCDF call to NCDID', NCF_STAT() )
        RCODE = -1
        NCDID = -1
        RETURN
        END

        SUBROUTINE NCDINQ(NCID, DIMID, NAME, SIZE, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, DIMID, SIZE, RCODE
        CHARACTER*(*) NAME
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCDINQ',0,0,'notCDF call to NCDINQ', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCDREN(NCID, DIMID, NAME, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, DIMID, RCODE
        CHARACTER*(*) NAME
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCDREN',0,0,'notCDF call to NCDREN', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        INTEGER FUNCTION NCVDEF(NCID, NAME, DATATYPE, NVDIMS, VDIMS,
     &                          RCODE)
        IMPLICIT NONE
        INTEGER       NCID, DATATYPE, NVDIMS, RCODE
        CHARACTER*(*) NAME
        INTEGER       VDIMS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCVDEF',0,0,'notCDF call to NCVDEF', NCF_STAT() )
        RCODE  = -1
        NCVDEF = -1
        RETURN
        END

        INTEGER FUNCTION NCVID(NCID, NAME, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, RCODE
        CHARACTER*(*) NAME
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCVID',0,0,'notCDF call to NCVID', NCF_STAT() )
        RCODE = -1
        NCVID = -1
        RETURN
        END

        SUBROUTINE NCVINQ(NCID, VARID, NAME, DATATYPE, NVDIMS,
     &                    VDIMS, NATTS, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, VARID, DATATYPE, NVDIMS, NATTS, RCODE
        CHARACTER*(*) NAME
        INTEGER       VDIMS(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCVINQ',0,0,'notCDF call to NCVINQ', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCVPT1(NCID, VARID, INDICES, VALUE, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, VARID, RCODE
        INTEGER       INDICES(*)
        REAL          VALUE
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCVPT1',0,0,'notCDF call to NCVPT1', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCVP1C(NCID, VARID, INDICES, CHVAL, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, VARID, RCODE
        INTEGER       INDICES(*)
        CHARACTER*(*) CHVAL
        REAL          VALUE
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCVP1C',0,0,'notCDF call to NCVP1C', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCVGT1(NCID, VARID, INDICES, VALUE, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, VARID, RCODE
        INTEGER       INDICES(*)
        REAL          VALUE
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCVGT1',0,0,'notCDF call to NCVGT1', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCVG1C(NCID, VARID, INDICES, CHVAL, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, VARID, RCODE
        INTEGER       INDICES(*)
        CHARACTER*(*) CHVAL
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCVG1C',0,0,'notCDF call to NCVG1C', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCVPT(NCID, VARID, START, COUNTS, VALUE, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, VARID, RCODE
        INTEGER       START(*), COUNTS(*)
        REAL          VALUE(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCVPT',0,0,'notCDF call to NCVPT', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCVPTC(NCID, VARID, START, COUNTS, STRING,
     &                    ILENSTR, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, VARID, ILENSTR, RCODE
        INTEGER       START(*), COUNTS(*)
        CHARACTER*(*) STRING(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCVPTC',0,0,'notCDF call to NCVPTC', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCVGT(NCID, VARID, START, COUNTS, VALUE, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, VARID, RCODE
        INTEGER       START(*), COUNTS(*)
        REAL          VALUE(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCVGT',0,0,'notCDF call to NCVGT', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCVGTC(NCID, VARID, START, COUNTS, STRING,
     &                    ILENSTR, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, VARID, ILENSTR, RCODE
        INTEGER       START(*), COUNTS(*)
        CHARACTER*(*) STRING(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCVGTC',0,0,'notCDF call to NCVGTC', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCVPTG(NCID, VARID, START, COUNTS, STRIDE, IMAP,
     &                    VALUE, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, VARID, STRIDE, RCODE
        INTEGER       START(*), COUNTS(*), IMAP(*)
        REAL          VALUE(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCVPTG',0,0,'notCDF call to NCVPTG', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCVPGC(NCID, VARID, START, COUNTS, STRIDE, IMAP,
     &                    STRING, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, VARID, ILENSTR, STRIDE, RCODE
        INTEGER       START(*), COUNTS(*), IMAP(*)
        CHARACTER*(*) STRING(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCVPGC',0,0,'notCDF call to NCVPGC', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCVGTG(NCID, VARID, START, COUNTS, STRIDE, IMAP,
     &                    VALUE, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, VARID, STRIDE, RCODE
        INTEGER       START(*), COUNTS(*), IMAP(*)
        REAL          VALUE(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCVGTG',0,0,'notCDF call to NCVGTG', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCVGGC(NCID, VARID, START, COUNTS, STRIDE, IMAP,
     &                    STRING, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, VARID, ILENSTR, STRIDE, RCODE
        INTEGER       START(*), COUNTS(*), IMAP(*)
        CHARACTER*(*) STRING(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCVGGC',0,0,'notCDF call to NCVGGC', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCVREN(NCID, VARID, NAME, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, VARID, RCODE
        CHARACTER*(*) NAME
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCVREN',0,0,'notCDF call to NCVREN', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCAPT(NCID, VARID, NAME, DATATYPE, ATTLEN, VALUE,
     &                   RCODE)
        IMPLICIT NONE
        INTEGER       NCID, VARID, DATATYPE, ATTLEN, RCODE
        CHARACTER*(*) NAME
        REAL          VALUE(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCAPT',0,0,'notCDF call to NCAPT', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCAPTC(NCID, VARID, NAME, DATATYPE, ILENSTR,
     &                    STRING, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, VARID, DATATYPE, ILENSTR, RCODE
        CHARACTER*(*) NAME, STRING
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCAPTC',0,0,'notCDF call to NCAPTC', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCAINQ(NCID, VARID, NAME, DATATYPE, ATTLEN,
     &                    RCODE)
        IMPLICIT NONE
        INTEGER       NCID, VARID, DATATYPE, ATTLEN, RCODE
        CHARACTER*(*) NAME
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCAPT',0,0,'notCDF call to NCAPT', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCAGT(NCID, VARID, NAME, VALUE, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, VARID, RCODE
        CHARACTER*(*) NAME
        REAL          VALUE(*)
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCAGT',0,0,'notCDF call to NCAGT', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCAGTC(NCID, VARID, NAME, STRING, ILENSTR, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, VARID, ILENSTR, RCODE
        CHARACTER*(*) NAME, STRING
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCAPTC',0,0,'notCDF call to NCAPTC', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCACPY(INNCID, INVARID, NAME, OUTNCID, OUTVARID,
     &                    RCODE)
        IMPLICIT NONE
        INTEGER       INNCID, INVARID, OUTNCID, OUTVARID, RCODE
        CHARACTER*(*) NAME
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCACPY',0,0,'notCDF call to NCACPY', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCANAM(NCID, VARID, ATTNUM, NAME, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, VARID, ATTNUM, RCODE
        CHARACTER*(*) NAME
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCANAM',0,0,'notCDF call to NCANAM', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCAREN(NCID, VARID, NAME, NEWNAME, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, VARID, RCODE
        CHARACTER*(*) NAME, NEWNAME
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCAREN',0,0,'notCDF call to NCAREN', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        SUBROUTINE NCADEL(NCID, VARID, NAME, RCODE)
        IMPLICIT NONE
        INTEGER       NCID, VARID, RCODE
        CHARACTER*(*) NAME
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCADEL',0,0,'notCDF call to NCADEL', NCF_STAT() )
        RCODE = -1
        RETURN
        END

        INTEGER FUNCTION NCTLEN(DATATYPE, RCODE)
        IMPLICIT NONE
        INTEGER       DATATYPE, RCODE
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCTLEN',0,0,'notCDF call to NCTLEN', NCF_STAT() )
        RCODE  = -1
        NCTLEN = -1
        RETURN
        END

        SUBROUTINE NCPOPT(NCOPTS)
        IMPLICIT NONE
        INTEGER       NCOPTS
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCPOPT',0,0,'notCDF call to NCPOPT', .FALSE. )
        RETURN
        END

        SUBROUTINE NCGOPT(NCOPTS)
        IMPLICIT NONE
        INTEGER       NCOPTS
        LOGICAL       NCF_STAT
        EXTERNAL      NCF_STAT
        CALL M3ERR( 'NCGOPT',0,0,'notCDF call to NCGOPT', NCF_STAT() )
        RETURN
        END


