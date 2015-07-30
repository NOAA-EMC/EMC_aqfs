
        SUBROUTINE FLTRNEG( STRING )

C***********************************************************************
C  subroutine body starts at line
C
C  DESCRIPTION:
C      This subroutine filters the valid "missing" terms that can be in a 
C      cross-reference file changes the argument value to blank when it is
C      one of the missing values.
C
C  PRECONDITIONS REQUIRED:
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C     
C
C  REVISION  HISTORY:
C      Started 6/99 by M. Houyoux
C
C****************************************************************************/
C
C Project Title: EDSS Tools Library
C File: @(#)$Id: fltrneg.f,v 1.2 2004/06/03 14:16:51 cseppan Exp $
C
C COPYRIGHT (C) 2004, Environmental Modeling for Policy Development
C All Rights Reserved
C
C Carolina Environmental Program
C University of North Carolina at Chapel Hill
C 137 E. Franklin St., CB# 6116
C Chapel Hill, NC 27599-6116
C
C smoke@unc.edu
C
C Pathname: $Source: /afs/isis/depts/cep/emc/apps/archive/edss_tools/edss_tools/src/lib/fltrneg.f,v $
C Last updated: $Date: 2004/06/03 14:16:51 $ 
C
C***************************************************************************

        IMPLICIT NONE

C.............  Subroutine arguments
            CHARACTER(*), INTENT (IN OUT) :: STRING   ! string for filter '-9'

C***********************************************************************
C   Begin body of subroutine FLTRNEG

        IF( INDEX( STRING, '-9' ) .GT.  0  .OR.
     &      STRING                .EQ. '0'      ) STRING = ' '

        RETURN

        END SUBROUTINE FLTRNEG
