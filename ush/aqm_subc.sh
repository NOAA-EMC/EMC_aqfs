#!/bin/csh -f

# type dif -h for help
set exist_h = ` echo $argv | grep -e "-h" | wc -w `

if ($exist_h != 0) then
   goto usage
else
   goto checkarg
endif

cont:

set system = `uname -a`

#set Base = /sss/emc/naqfc/shared/Pius.Lee/mytools/subc
#set Base = $USHaqm

if (`strings $source | head -n 1 | grep TSTEP | wc -l`) then
   set filetype = ncf
else
   if (`file $source | grep -i symbolic | wc -l`) then
      set linfile = `ls -al $source`
      set count = `echo $linfile | wc -w`
      set tinfile = $source
      set infile = $linfile[11]
      set symbolic = 1
   else
     set symbolic = 0
     set infile = $source
   endif

   if (`file $infile | grep -i data | wc -l`) then
      set filetype = bin
   else
      echo ' '
      echo ' Wrong file type'
      echo ' '
      exit
   endif

   if ($symbolic) then
      set source = $tinfile
   endif
endif

if ($ttarget == '') then
#  set t1 = `echo $source | sed 's/\// /g'`
#  set c1 = `echo $t1 | wc -w`
#  set target = $t1[$c1].s
   set target = $source.s
else
   set t1 = `echo $ttarget | sed 's/\./ /g'`
   set c1 = `echo $t1 | wc -w`
   if ($t1[$c1] == ncf) then
#     set target = `echo $ttarget | sed 's/\.ncf//' `
      set target = $ttarget.s
   else
      set target = $ttarget
   endif
endif

if ($filetype == ncf) then
   setenv INFILE1 "$source"
   setenv OUTFILE "$target -v"
   setenv ncf Y
else
   setenv INFILE1 "BIN:$source"
   setenv OUTFILE "BIN:$target -v"
   setenv NOTCDF_FATAL F
   setenv ncf N
endif
set Exec = $EXECaqm/aqm_combine
$EXECaqm/aqm_setup_date_timesp >> $pgmout 2>errfile1

set HeadDefn = ./header_defn
set SpecDefn = ./species_defn
if ($default_template == Y) then
   set Template = $PARMaqm/template
else
   set Template = $template
endif
cat $HeadDefn $Template > $SpecDefn

setenv SPECIES_DEF $SpecDefn

#> turn off excessive WRITE3 logging
setenv IOAPI_LOG_WRITE N

$Exec

#if ($filetype == bin) then
#   /sss/emc/naqfc/shared/Pius.Lee/mytools/convert_prog/convert $target
#   rm $target
#endif

# chmod 644 $source

rm $HeadDefn $SpecDefn

exit

#---------------------------------------------------------------------
checkarg:

set count = $#argv

#set default values
setenv mysdate -99
setenv mystime -99
setenv mynsteps -99
setenv layer -1

set ttarget =

if ($count > 0) then
   @ stop = 1
   @ lc = 0

   while ($stop)
     @ lc++

     if ("$argv[$lc]" == '-t') then
        @ lc++
        set argument = `echo $argv[$lc]`
        setenv mysdate $argument[1]
        setenv mystime $argument[2]
        setenv mynsteps $argument[3]
     else if ("$argv[$lc]" == '-l') then
        @ lc++
        set argument = `echo $argv[$lc]`
        setenv layer $argument[1]
     else
        @ remaining = 1 + $count - $lc
        if ($remaining == 3) then
           set default_template=N
           set template = $argv[$lc]
           if ((! `file $argv[$lc] | grep -i ascii | wc -l`) && \
               (! `file $argv[$lc] | grep -i text | wc -l`)) then
              echo ' Wrong template file'
              exit
           endif
           @ lc++
           set source=$argv[$lc]
           @ lc++
           set ttarget=$argv[$lc]
        else
           if ($lc == $count) then
              set default_template=Y
              set source=$argv[$lc]
           else
              if ((`file $argv[$lc] | grep -i ascii | wc -l`) || \
                  (`file $argv[$lc] | grep -i text | wc -l`)) then
                 set default_template=N
                 set template = $argv[$lc]
                 @ lc++
                 set source=$argv[$lc]
              else
                 set default_template=Y
                 set source=$argv[$lc]
                 @ lc++
                 set ttarget=$argv[$lc]
              endif
           endif
        endif
     endif

     if ($count == $lc) then
        @ stop = 0
     endif

   end

endif

goto cont

# -------------------------------------------------------------------------
usage:
echo ' '
echo ' subc [ -h ] [ -t "start_date start_time nsteps " ] '
echo '      [ -l layer_number ] [ template ] infile [ outfile ] '
echo ' '
echo '    where -h -- display usage information'
echo '          -t -- indicate a specific startint date and time and number '
echo '                of time steps '
echo '          -l -- indicate a specific layer number '
echo ' '
echo '    Note: 1. If no template file is supplied, the default one, which'
echo '             is in the same directory of this script, will be used'
echo '          2. If no outfile name is supplied, the default naming '
echo '             convention with .s extension will be used '
echo '          3. infile can be netCDF or binary file '
echo ' '
echo '    e.g. subc temp infile '
echo ' '
echo '         This will subset the data in file infile base on the'
echo '         species list in file temp and convert to netCDF format'
echo '         if necessary.'
echo ' '

exit

