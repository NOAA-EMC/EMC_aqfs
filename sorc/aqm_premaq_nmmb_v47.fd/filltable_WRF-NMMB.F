SUBROUTINE filltable

!-------------------------------------------------------------------------------
! Name:     Fill Table
! Purpose:  Fills out information that would have been input from "VTable"
!           in NCAR's REGRID code (part of MM5).
! Notes:    Code for "take all levels" is splatcode=-88.
!           Code for "this column left blank" is blankcode=-99.
! Revised:  15 Apr 2003  Original version.  (T. Otte)
!           -- Will probably update this later, but think that a user-definable
!              table (like VTable) is probably not the best way to go for an
!              operational code.  Would like to see this type of info read in
!              from a fixed file into an f90 "TYPE" that contains all info
!              about variables we expect here.
!           20 Oct 2003  Added Kh as variable #31.  (T. Otte)
!           25 Nov 2003  Added GFS Ozone as variable #32.  (T. Otte)
!           04 Dec 2003  Added tropopause height as variable #33.  (T. Otte)
!           20 Feb 2004  Added option for Kh to be in GRIB table 129, variable
!                        182.  (T. Otte)
!           30 Mar 2004  Added identification of GRIB parameter tables where
!                        the table is MOD(table,10) such that table 2 = "2",
!                        table 129 = "9", and table 130 = "0".  Removed
!                        "fil" descriptor for nearest-neighbor vs. bilinear
!                        interpolation since there is currently no way to
!                        verify the method of filling the fields.  (T. Otte)
!           30 Apr 2004  Added 10-m component winds.  (T. Otte)
!           19 Jan 2005  1. Added Clear sky radiation 
!                        2. criteria "tr" for ave=>3  and instaneous=>0
!                        3. change "l2" on surface (lcode=1) to be "0" to 
!                           consist with the ON388-PDS
!                           ** ksec1( 7) => KPDS( 5  ) => Octet  9    ... gcode
!                              ksec1( 8) => KPDS( 6  ) => Octet 10    ... lcode
!                              ksec1( 9) => KPDS( 7-1) => Octet 11    ... l1
!                              ksec1(10) => KPDS( 7-2) => Octet 12    ... l2
!                              ksec1(19) => KPDS(16  ) => Octet 21    ... tr
!                              ksec1( 2) => KPDS(19  ) => Octet  4    ... tbl
!                        Currently, ON-388-TABLE2 (PDS Octet 9) has 3 Parameter 
!                        Table version, 2, 129, 130.
!                        The desired varilable will not only on "gcode" (Octet9)
!                        , but also on "tbl" (Octet4)
!                                                                  (Hsin-mu Lin)
!           08 Feb 2005  Revised the code and Added convective cloud fields
!                        based on ON388-Table3a
!                        ** Remove "MOD(table,10)"  30 Mar 2004, by (T. Otte)
!                                                                  (Hsin-mu Lin)
!           14 Jly 2005  comment on ID 246                         (Hsin-mu Lin)
!-------------------------------------------------------------------------------

  USE table
  USE premaqparm, ONLY: nmlays, neta1

  IMPLICIT NONE

  INTEGER   :: i

!*** number of data needed

  maxvar = 53  ! in module table, and initialized to 0; needed in rdgrib loop

!
! "tr" is the criteria to make sure the gribed data is instaneous, in case
! averaged value of the same "iparm" (gcode) is stored in the data ahead.
!
! *** if the data that needed is ave, set tr=3
! *** if the data that needed is accumulated, set tr=4
!

  do i=1,maxvar
     tr(i)=0
     l2(i)=0      ! non "0" when extract value between  l1 & l2
  enddo

  tr(23)=4        ! accumulated Convective precip, iparm=62
  tr(24)=4        ! accumulated Grid scale precip, iparm=63
  tr(11)=10       ! O3 MR, iparm=154

 !*** Hybrid Level,  "take all levels" is splatcode=-88, rdgrib2.F

  gcode( 1) =   1 ; lcode( 1) = 109 ; l1( 1) = -88 ; tbl( 1) =   2
  gcode( 2) =   7 ; lcode( 2) = 109 ; l1( 2) = -88 ; tbl( 2) =   2
  gcode( 3) =  11 ; lcode( 3) = 109 ; l1( 3) = -88 ; tbl( 3) =   2
  gcode( 4) =  33 ; lcode( 4) = 109 ; l1( 4) = -88 ; tbl( 4) =   2
  gcode( 5) =  34 ; lcode( 5) = 109 ; l1( 5) = -88 ; tbl( 5) =   2
  gcode( 6) =  39 ; lcode( 6) = 109 ; l1( 6) = -88 ; tbl( 6) =   2
  gcode( 7) =  51 ; lcode( 7) = 109 ; l1( 7) = -88 ; tbl( 7) =   2
  gcode( 8) =  58 ; lcode( 8) = 109 ; l1( 8) = -88 ; tbl( 8) =   2
  gcode( 9) =  71 ; lcode( 9) = 109 ; l1( 9) = -88 ; tbl( 9) =   2
  gcode(10) = 153 ; lcode(10) = 109 ; l1(10) = -88 ; tbl(10) =   2
  gcode(11) = 154 ; lcode(11) = 109 ; l1(11) = -88 ; tbl(11) =   2
  gcode(12) = 158 ; lcode(12) = 109 ; l1(12) = -88 ; tbl(12) =   2
  gcode(13) = 170 ; lcode(13) = 109 ; l1(13) = -88 ; tbl(13) =   2
  gcode(14) = 171 ; lcode(14) = 109 ; l1(14) = -88 ; tbl(14) =   2
  gcode(15) = 182 ; lcode(15) = 109 ; l1(15) = -88 ; tbl(15) = 129

 !*** Specified height level above ground

  gcode(16) =  11 ; lcode(16) = 105 ; l1(16) =   2 ; tbl(16) =   2
  gcode(17) =  33 ; lcode(17) = 105 ; l1(17) =  10 ; tbl(17) =   2
  gcode(18) =  34 ; lcode(18) = 105 ; l1(18) =  10 ; tbl(18) =   2

 !*** Tropopause : l1=0, l2=0

  gcode(19) =   1 ; lcode(19) =   7 ; l1(19) =   0 ; tbl(19) =   2

 !*** Surface : l1=0, l2=0

  gcode(20) =   1 ; lcode(20) =   1 ; l1(20) =   0 ; tbl(20) =   2
  gcode(21) =   7 ; lcode(21) =   1 ; l1(21) =   0 ; tbl(21) =   2 
  gcode(22) =  11 ; lcode(22) =   1 ; l1(22) =   0 ; tbl(22) =   2
  gcode(23) =  62 ; lcode(23) =   1 ; l1(23) =   0 ; tbl(23) =   2
  gcode(24) =  63 ; lcode(24) =   1 ; l1(24) =   0 ; tbl(24) =   2
  gcode(25) =  87 ; lcode(25) =   1 ; l1(25) =   0 ; tbl(25) =   2
  gcode(26) = 121 ; lcode(26) =   1 ; l1(26) =   0 ; tbl(26) =   2
  gcode(27) = 122 ; lcode(27) =   1 ; l1(27) =   0 ; tbl(27) =   2
  gcode(28) = 160 ; lcode(28) =   1 ; l1(28) =   0 ; tbl(28) =   2
  gcode(29) = 161 ; lcode(29) =   1 ; l1(29) =   0 ; tbl(29) =   2
  gcode(30) = 181 ; lcode(30) =   1 ; l1(30) =   0 ; tbl(30) = 130
  gcode(31) = 204 ; lcode(31) =   1 ; l1(31) =   0 ; tbl(31) =   2
  gcode(32) = 208 ; lcode(32) =   1 ; l1(32) =   0 ; tbl(32) =   2
  gcode(33) = 211 ; lcode(33) =   1 ; l1(33) =   0 ; tbl(33) =   2
  gcode(34) = 212 ; lcode(34) =   1 ; l1(34) =   0 ; tbl(34) =   2
  gcode(35) = 221 ; lcode(35) =   1 ; l1(35) =   0 ; tbl(35) =   2

 !*** this is a temporary field for PBL by Richardson number ***
  gcode(48) =   7 ; lcode(48) = 220 ; l1(48) =   0 ; tbl(48) =   2
 !***

  gcode(49) =  67 ; lcode(49) =   1 ; l1(49) =   0 ; tbl(49) =   2

  gcode(36) = 223 ; lcode(36) =   1 ; l1(36) =   0 ; tbl(36) =   2
  gcode(37) = 225 ; lcode(37) =   1 ; l1(37) =   0 ; tbl(37) =   2
  gcode(38) = 253 ; lcode(38) =   1 ; l1(38) =   0 ; tbl(38) =   2
  gcode(39) =  84 ; lcode(39) =   1 ; l1(39) =   0 ; tbl(39) =   2

 !*** Convective cloud level : l1=0, l2=0

  gcode(40) =   1 ; lcode(40) = 242 ; l1(40) =   0 ; tbl(40) =   2
  gcode(41) =   1 ; lcode(41) = 243 ; l1(41) =   0 ; tbl(41) =   2
  gcode(42) =   1 ; lcode(42) = 248 ; l1(42) =   0 ; tbl(42) =   2
  gcode(43) =   1 ; lcode(43) = 249 ; l1(43) =   0 ; tbl(43) =   2
  gcode(44) =   1 ; lcode(44) = 251 ; l1(44) =   0 ; tbl(44) =   2
  gcode(45) =   1 ; lcode(45) = 252 ; l1(45) =   0 ; tbl(45) =   2

 !*** PDTOP & PD

  gcode(46) =   1 ; lcode(46) = 110 ; l1(46) =       1 ; tbl(46) =   2
                                    ; l2(46) = neta1
  gcode(47) =   1 ; lcode(47) = 110 ; l1(47) =       1 ; tbl(47) =   2
                                    ; l2(47) = nmlays+1

 !*** SOTYP & SOILW

  gcode(50) = 224 ; lcode(50) =   1 ; l1(50) =   0 ; tbl(50) =   2
  gcode(51) = 144 ; lcode(51) = 112 ; l1(51) =   0 ; tbl(51) =   2
                                    ; l2(51) =  10

 !*** LAI & SNOWC 

  gcode(52) = 182 ; lcode(52) =   1 ; l1(52) =   0 ; tbl(52) = 130
  gcode(53) = 238 ; lcode(53) =   1 ; l1(53) =   0 ; tbl(53) =   2

END SUBROUTINE filltable
