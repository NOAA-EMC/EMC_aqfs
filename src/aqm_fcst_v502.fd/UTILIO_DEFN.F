
!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!


C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/util/util/UTILIO_DEFN.F,v 1.3 2011/10/21 16:11:36 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C....................................................................
C  CONTAINS:  This module provides the interface to the Models3 IOAPI
C     using the M3UTILIO module.  For parallel implementation of CMAQ, 
C     several IOAPI routines are replaced with PARIO versions.  For
C     example, the IOAPI function WRITE3 is renamed to an unused 
C     function name "EXCLUDE_WRITE3", and the PARIO function PWRITE3
C     is aliased to WRITE3.  So when WRITE3 is used in CMAQ, then the
C     the PARIO function PWRITE3 is accessed.  This approach allows
C     flexibility in naming within CMAQ and eliminated the usage of
C     "cpp" flags to achieve the function overloading.
C              
C  REVISION HISTORY:
C       Original version 03/2011 by Shawn Roselle
C                        08/2011 by David Wong
C                          -- extended to handle buffer file
C....................................................................

      MODULE UTILIO_DEFN

#ifdef parallel

         USE M3UTILIO, EXCLUDE_INTERPB => INTERP3,
     &                 EXCLUDE_M3EXIT  => M3EXIT,
     &                 EXCLUDE_M3WARN  => M3WARN,
     &                 EXCLUDE_SHUT3   => SHUT3,
     &                 EXCLUDE_WRITE3  => WRITE3,
     &                 BUF_WRITE3      => WRITE3

         USE PARUTILIO, INTERPB => PINTERPB,
     &                  M3ERR   => PM3ERR,
     &                  M3EXIT  => PM3EXIT,
     &                  M3WARN  => PM3WARN,
     &                  SHUT3   => PSHUT3,
     &                  WRITE3  => PWRITE3
#else
         USE M3UTILIO, INTERPB => INTERP3
#endif

      END MODULE UTILIO_DEFN
