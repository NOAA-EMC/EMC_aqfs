#! /bin/csh -f

# type fmm -h for help
set exist_h = ` echo $argv | grep -e "-h" | wc -w `

if ($exist_h != 0) then
   goto usage
else
   goto checkarg
endif

cont:

if (! -r $infile) then
   echo ' '
   echo ' File' $infile 'does not exist'
   echo ' '
   exit
endif

if (-r $outfile) then
   echo ' '
   echo ' Output file' $outfile 'exists. Please remove it.'
   echo ' '
   exit
endif

if (`head $infile | strings | head -n 50 | grep TFLAG | wc -l` > 0 ) then
   setenv ncf Y
else
   if (`file $infile | grep -i symbolic | wc -l`) then
      set linfile = `ls -al $infile`
      set count = `echo $linfile | wc -w`
      set tinfile = $infile
      set infile = $linfile[11]
      set symbolic = 1
   else
      set symbolic = 0
      set infile = $infile
   endif

   if (`file $infile | grep -i data | wc -l`) then
      setenv ncf N
   else
      echo ' '
      echo ' Wrong file type '
      echo ' '
      exit
   endif

   if ($symbolic) then
      set infile = $tinfile
   endif
endif

if ($ncf == Y) then
   setenv infile $infile
   setenv outfile $outfile
else
   setenv infile "BIN:$infile"
   setenv outfile "BIN:$outfile"
endif

echo ' '

#/naqfc/noscrub/Youhua.Tang/CMAQ/dwtools/chg/chg.x
#/u/Jianping.Huang/noscrub/nwdev/cmaq.v5.0.2/exec/aqm_chg
${HOMEaqm}/exec/aqm_chg

exit

#---------------------------------------------------------------------
checkarg:

setenv chgtime    N
setenv chgvar     N
setenv chggrid    N
setenv chgvaluel  N
setenv chgcent    N
setenv chgorig    N
setenv chgtstep   N
setenv spc        N
setenv chvgtype   N

set count = $#argv

@ lc = 0

while ($lc < $count)
  @ lc++

  if ("$argv[$lc]" == '-t') then
     @ lc++
     set argument = `echo $argv[$lc]`
     setenv newdate $argument[1]
     setenv newtime $argument[2]
     setenv chgtime Y
  else if ("$argv[$lc]" == '-v') then
     @ lc++
     set argument = `echo $argv[$lc]`
     setenv var_list "$argument"
     setenv chgvar Y
  else if ("$argv[$lc]" == '-vl') then
     @ lc++
     set argument = `echo $argv[$lc]`
     setenv slvl $argument[1]
     setenv elvl $argument[2]
     setenv value $argument[3]
     setenv chgvaluel Y
  else if ("$argv[$lc]" == '-ts') then
     @ lc++
     setenv tstep $argv[$lc]
     setenv chgtstep Y
  else if ("$argv[$lc]" == '-c') then
     @ lc++
     set argument = `echo $argv[$lc]`
     setenv xcent $argument[1]
     setenv ycent $argument[2]
     setenv chgcent Y
  else if ("$argv[$lc]" == '-o') then
     @ lc++
     set argument = `echo $argv[$lc]`
     setenv xorig $argument[1]
     setenv yorig $argument[2]
     setenv chgorig Y
  else if ("$argv[$lc]" == '-g') then
     @ lc++
     set argument = `echo $argv[$lc]`
     setenv newgrid $argv[$lc]
     setenv chggrid Y
  else if ("$argv[$lc]" == '-s') then
     @ lc++
     set argument = `echo $argv[$lc]`
     setenv spc Y
     setenv spc_list "$argument"
  else if ("$argv[$lc]" == '-vg') then
     @ lc++
     setenv chvgtype Y
     setenv vgtype $argv[$lc]
  else
     set len = `echo $argv[$lc] | wc -c`
     set found = `echo $argv[$lc] | grep "-" | wc -l`
     if (($found) && ($len <= 3)) then
        echo ' '
        echo " Error: Invalid option $argv[$lc]"
        echo ' '
        exit
     else
        set infile  = $argv[$lc]
        @ lc++
        set outfile = $argv[$lc]
     endif
  endif
end

goto cont

# -------------------------------------------------------------------------
usage:

echo ' '
echo ' Change Time, variable name, or grid name in a file'
echo ' '
echo 'chg [ -t "newdate newtime" ] [ -v "oldvar1 newvar1 ... " ] '
echo '    [ -g new_grid_name ] [ -vl "start_layer end_layer new_value" ] '
echo '    [ -c "new_xcent new_ycent" ] [ -o "new_xorig new_yorig" ] '
echo '    [ -ts new_tstep ] [ -s "species_subset_list" ] [ -vg vgtype ] '
echo '    infile outfile '
echo ' '
echo '    where  -t  -- specify new date and new time'
echo '           -v  -- specify old and new variable name'
echo '           -g  -- specify new grid name'
echo '           -vl -- change value with a range of levels '
echo '           -c  -- change xcent and ycent values'
echo '           -o  -- change xorig and yorig values'
echo '           -ts -- change tstep'
echo '           -s  -- specify a subset of species, works witn -vl option only'
echo '           -vg -- specify a new vgtype'
echo ' '
echo ' Note: setenv IOAPI_OFFSET_64 T to enable 64 bit offset netcdf file'
echo ' '
echo ' If you have any comment/question/problem using this script/program, '
echo ' please contact David Wong at wong.david-c@epa.gov'
echo ' '

exit
