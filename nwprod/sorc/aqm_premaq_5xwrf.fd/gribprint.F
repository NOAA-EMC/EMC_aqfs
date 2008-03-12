subroutine gribprint(isec)
  use module_grib
  implicit none
  integer :: isec
  integer :: ou = 6
  character(len=12) :: string = ',t45,":",i8)'
  character(len=15) :: rstring = ',t45,":",f12.5)'

  if (isec.eq.0) then
     write(*,'(/,"GRIB SECTION 0:")')
     write(ou,'(5x,"Grib Length"'//string) sec0(1)
     write(ou,'(5x,"Grib Edition"'//string) sec0(2)
  else if (isec.eq.1) then
     write(*,'(/,"GRIB SECTION 1:")')
     write(ou,'(5x,"Length of PDS"'//string) sec1(1)
     write(ou,'(5x,"Parameter Table Version"'//string) sec1(2)
     write(ou,'(5x,"Center ID"'//string) sec1(3)
     write(ou,'(5x,"Process ID"'//string) sec1(4)
     write(ou,'(5x,"Grid ID"'//string) sec1(5)
     if (sec1(25) == 1) then
        write(ou,'(5x,"Is there a Grid Desc. Section (GDS)?",t45,":     Yes")')
     else if (sec1(25) == 0) then
        write(ou,'(5x,"Is there a Grid Desc. Section (GDS)?",t45,":      No")')
     else
        print*, 'Unrecognized sec1(25): ', sec1(25)
     endif
     if (sec1(26) == 1) then
        write(ou,'(5x,"Is there a Bit Map Section (BMS)?",t45,":     Yes")')
     else if (sec1(26) == 0) then
        write(ou,'(5x,"Is there a Bit Map Section (BMS)?",t45,":      No")')
     else
        print*, 'Unrecognized sec1(26): ', sec1(26)
     endif
     write(ou,'(5x,"Parameter"'//string) sec1(7)
     write(ou,'(5x,"Level type"'//string) sec1(8)
     if ( (sec1(8) == 101) .or. (sec1(8) == 104) .or. (sec1(8) == 106) .or. &
          (sec1(8) == 108) .or. (sec1(8) == 110) .or. (sec1(8) == 112) .or. &
          (sec1(8) == 114) .or. (sec1(8) == 116) .or. (sec1(8) == 120) .or. &
          (sec1(8) == 121) .or. (sec1(8) == 128) .or. (sec1(8) == 141) ) then
        write(ou,'(5x,"Hgt, pres, etc. of layer top "'//string) sec1(9)
        write(ou,'(5x,"Hgt, pres, etc. of layer bottom "'//string) sec1(10)
     else
        write(ou,'(5x,"Height, pressure, etc "'//string) sec1(9)
     endif
     write(ou,'(5x,"Year"'//string) sec1(11)
     write(ou,'(5x,"Month"'//string) sec1(12)
     write(ou,'(5x,"Day"'//string) sec1(13)
     write(ou,'(5x,"Hour"'//string) sec1(14)
     write(ou,'(5x,"Minute"'//string) sec1(15)
     write(ou,'(5x,"Forecast time unit"'//string) sec1(16)
     write(ou,'(5x,"P1"'//string) sec1(17)
     write(ou,'(5x,"P2"'//string) sec1(18)
     write(ou,'(5x,"Time Range Indicator"'//string) sec1(19)
     write(ou,'(5x,"Number in Ave?"'//string) sec1(20)
     write(ou,'(5x,"Number missing from ave?"'//string) sec1(21)
     write(ou,'(5x,"Century"'//string) sec1(22)
     write(ou,'(5x,"Sub-center"'//string) sec1(23)
     write(ou,'(5x,"Decimal scale factor"'//string) sec1(24)
  elseif ((isec.eq.2) .and. (sec1(25).eq.1)) then
     write(*,'(/,"GRIB SECTION 2:")')
     write(ou,'(5x,"Length of GRID Desc. Section"'//string) sec2(1)
     if ((sec2(2) /= 0).or.(sec2(3) /= 0) .or. (sec2(4) /= 0)) then
        write(ou,'(5x,"Number of V. Coordinate Parms"'//string) sec2(2)
        write(ou,'(5x,"List Starting point"'//string) sec2(3)
        write(ou,'(5x,"Data Representation type"'//string) sec2(4)
     endif

     if (sec2(4).eq.0) then
        write(ou,'(5x,"Cylindrical Equidistant Grid")')
        write(ou,'(10x,"NI"'//string) infogrid(1)
        write(ou,'(10x,"NJ"'//string) infogrid(2)
        write(ou,'(10x,"Lat 1"'//rstring) gridinfo(3)
        write(ou,'(10x,"Lon 1"'//rstring) gridinfo(4)
        write(ou,'(10x,"Resolution and Component:", t45,":",B8.8)') infogrid(5)
        write(ou,'(10x,"Lat NI"'//string) infogrid(6)
        write(ou,'(10x,"Lon NJ"'//string) infogrid(7)
        write(ou,'(10x,"Delta-Lon"'//string) infogrid(8)
        write(ou,'(10x,"Delta-Lat"'//string) infogrid(9)
        write(ou,'(10x,"Scanning mode"'//string) infogrid(10)
        write(ou,'(10x,"I-Scanning increment"'//string) infogrid(21)
        write(ou,'(10x,"J-Scanning increment"'//string) infogrid(22)

     else if (sec2(4).eq.1) then
        write(ou,'(5x,"Mercator Grid")')
        write(ou,'(10x,"NI"'//string) infogrid(1)
        write(ou,'(10x,"NJ"'//string) infogrid(2)
        write(ou,'(10x,"Lat 1"'//rstring) gridinfo(3)
        write(ou,'(10x,"Lon 1"'//rstring) gridinfo(4)
        write(ou,'(10x,"Resolution and Component",t45,":", B8.8)') infogrid(5)
        write(ou,'(10x,"Lat NI"'//rstring) gridinfo(6)
        write(ou,'(10x,"Lon NJ"'//rstring) gridinfo(7)
        write(ou,'(10x,"Dx"'//rstring) gridinfo(8)
        write(ou,'(10x,"Dy"'//rstring) gridinfo(9)
        write(ou,'(10x,"Scanning mode"'//string) infogrid(10)
        write(ou,'(10x,"Latin"'//rstring) gridinfo(11)
        write(ou,'(10x,"I-Scanning increment"'//string) infogrid(21)
        write(ou,'(10x,"J-Scanning increment"'//string) infogrid(22)

     else if (sec2(4).eq.4) then
        write(ou,'(5x,"Gaussian Grid")')
        write(ou,'(10x,"NI"'//string) infogrid(1)
        write(ou,'(10x,"NJ"'//string) infogrid(2)
        write(ou,'(10x,"Original (stored) Lat 1"'//rstring) gridinfo(18)
        write(ou,'(10x,"Lat 1"'//rstring) gridinfo(3)
        write(ou,'(10x,"Lon 1"'//rstring) gridinfo(4)
        write(ou,'(10x,"Resolution and Component",t45,":", B8.8)') infogrid(5)
        write(ou,'(10x,"Original (stored) Lat NI"'//rstring) gridinfo(17)
        write(ou,'(10x,"Lat NI"'//rstring) gridinfo(6)
        write(ou,'(10x,"Lon NJ"'//rstring) gridinfo(7)
        write(ou,'(10x,"Delta-Lon"'//rstring) gridinfo(8)
        write(ou,'(10x,"Delta-Lat"'//rstring) gridinfo(19)
        write(ou,'(10x,"Number of lats (pole - eq)"'//string) infogrid(9)
        write(ou,'(10x,"Scanning mode"'//string) infogrid(10)
        write(ou,'(10x,"I-Scanning increment"'//string) infogrid(21)
        write(ou,'(10x,"J-Scanning increment"'//string) infogrid(22)

     elseif (sec2(4).eq.3) then
        write(ou,'(5x,"Lambert Conformal Grid")')
        write(ou,'(10x,"NI"'//string) infogrid(1)
        write(ou,'(10x,"NJ"'//string) infogrid(2)
        write(ou,'(10x,"Lat 1"'//string) infogrid(3)
        write(ou,'(10x,"Lon 1"'//string) infogrid(4)
        write(ou,'(10x,"Resolution and Component",t45,":", B8.8)') infogrid(5)
        write(ou,'(10x,"Lov"'//string) infogrid(6)
        write(ou,'(10x,"Dx"'//string) infogrid(7)
        write(ou,'(10x,"Dy"'//string) infogrid(8)
        write(ou,'(10x,"Projection center"'//string) infogrid(9)
        write(ou,'(10x,"Scanning mode"'//string) infogrid(10)
        write(ou,'(10x,"I-Scanning increment"'//string) infogrid(21)
        write(ou,'(10x,"J-Scanning increment"'//string) infogrid(22)
        write(ou,'(10x,"Latin 1"'//string) infogrid(11)
        write(ou,'(10x,"Latin 2"'//string) infogrid(12)
        write(ou,'(10x,"Lat of southern pole"'//string) infogrid(13)
        write(ou,'(10x,"Lon of southern pole"'//string) infogrid(14)
     elseif (sec2(4).eq.5) then
        write(ou,'(5x,"Polar Stereographic Grid")')
        write(ou,'(10x,"NI"'//string) infogrid(1)
        write(ou,'(10x,"NJ"'//string) infogrid(2)
        write(ou,'(10x,"Lat 1"'//string) infogrid(3)
        write(ou,'(10x,"Lon 1"'//string) infogrid(4)
        write(ou,'(10x,"Resolution and Component", t45,":",B8.8)') infogrid(5)
        write(ou,'(10x,"Lov"'//string) infogrid(6)
        write(ou,'(10x,"Dx"'//string) infogrid(7)
        write(ou,'(10x,"Dy"'//string) infogrid(8)
        write(ou,'(10x,"Projection center"'//string) infogrid(9)
        write(ou,'(10x,"Scanning mode"'//string) infogrid(10)
        write(ou,'(10x,"I-Scanning increment"'//string) infogrid(21)
        write(ou,'(10x,"J-Scanning increment"'//string) infogrid(22)
     elseif (sec2(4).eq.50) then
        write(ou,'(5x,"Spherical harmonic components")')
        write(ou,'(10x,"J-Pentagonal resolution parm:"'//string) infogrid(1)
        write(ou,'(10x,"K-Pentagonal resolution parm:"'//string) infogrid(2)
        write(ou,'(10x,"M-Pentagonal resolution parm:"'//string) infogrid(3)
        write(ou,'(10x,"Representation type"'//string) infogrid(4)
        write(ou,'(10x,"Coefficient storage mode"'//string) infogrid(5)
     endif
  elseif ((isec.eq.3) .and. (sec1(26).eq.1)) then
     write(*,'(/,"GRIB SECTION 3:")')
     write(ou,'(5x,"Length of bit map section"'//string) sec3(1)
     write(ou,'(5x,"Number of unused bits"'//string) sec3(2)
     write(ou,'(5x,"Numeric"'//string) sec3(3)

  elseif (isec.eq.4) then
     write(*,'(/,"GRIB SECTION 4:")')
     write(ou,'(5x,"Length of BDS"'//string) sec4(1)
     write(ou,'(5x,"0/1: grid-point or sph. harm. data"'//string) sec4(2)
     write(ou,'(5x,"0/1: simple or complex packing"'//string) sec4(3)
     write(ou,'(5x,"0/1: floating or integer"'//string) sec4(4)
     write(ou,'(5x,"0/1: No addl flags or addl flags"'//string) sec4(5)
     write(ou,'(5x,"Unused bits"'//string) sec4(6)
     write(ou,'(5x,"Binary Scale Factor"'//string) sec4(7)
     write(ou,'(5x,"Reference Value", t45, ":", F18.8)') xec4(1)
     write(ou,'(5x,"Number of bits for packing"'//string) sec4(8)
  endif

end subroutine gribprint
