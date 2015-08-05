#! /bin/csh -f

# type fmm -h for help
set exist_h = ` echo $argv | grep -e "-h" | wc -w `

if ($exist_h != 0) then
   goto usage
else
   goto checkarg
endif

cont:

if (! -f $inputfile) then
   echo ' '
   echo ' File' $inputfile 'does not exist'
   echo ' '
   exit
endif

if (-f $outputfile) then
   if ($delete == Y) then
      rm -f $outputfile
   else
      echo ' '
      echo ' Output file' $outputfile 'exists. Please remove it or use -rm option.'
      echo ' '
      exit
   endif
endif

# if (`head $inputfile | strings | head -n 1 | grep TSTEP | wc -l` == 1 ) then
if (`head $inputfile | strings | head -n 50 | grep TFLAG | wc -l` > 0 ) then
   setenv ncf Y
else
   if (`file $inputfile | grep -i symbolic | wc -l`) then
      set linfile = `ls -al $inputfile`
      set count = `echo $linfile | wc -w`
      set tinfile = $inputfile
      set infile = $linfile[11]
      set symbolic = 1
   else
      set symbolic = 0
      set infile = $inputfile
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
      set inputfile = $tinfile
   endif
endif

if ($ncf == Y) then
   setenv file1 $inputfile
   if (($outputtype == '') || ($outputtype == ncf)) then
      setenv file2 $outputfile
   else
      setenv file2 "BIN:$outputfile"
   endif
else
   setenv file1 "BIN:$inputfile"
   if (($outputtype == '') || ($outputtype == bin)) then
      setenv file2 "BIN:$outputfile"
   else
      setenv file2 $outputfile
   endif
endif

ls -al $inputfile

# sleep 3

echo ' '

$EXECaqm/aqm_subset_x 

exit

#---------------------------------------------------------------------
checkarg:

set count = $#argv
@ countm1 = $#argv - 1

#set default values
setenv mysdate -99
setenv mystime -99
setenv mynsteps -99
setenv mytsteps -99
setenv mysrec -99
setenv myerec -99
setenv myslay -1
setenv myelay -1
setenv myscol -99
setenv myecol -99
setenv mysrow -99
setenv myerow -99
setenv spc N
setenv display N
setenv output N
setenv tol 0
setenv info N
setenv vp N
setenv window N
setenv delete N
setenv timeind N

set outputtype = '' 

set mt = 0
set mr = 0

# if (`echo $argv | grep "-" | wc -l `) then

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
           set mt = 1
        else if ("$argv[$lc]" == '-r') then
           @ lc++
           setenv mysrec $argv[$lc]
           y lc++
           setenv myerec $argv[$lc]
           set mr = 1
        else if ("$argv[$lc]" == '-s') then
           @ lc++
           set argument = `echo $argv[$lc]`
           setenv spc Y
           setenv spc_list "$argument"
        else if ("$argv[$lc]" == '-ot') then
           @ lc++
           set outputtype = $argv[$lc]
        else if ("$argv[$lc]" == '-l') then
           @ lc++
           set argument = `echo $argv[$lc]`
           setenv myslay $argument[1]
           setenv myelay $argument[2]
        else if ("$argv[$lc]" == '-w') then
           @ lc++
           set argument = `echo $argv[$lc]`
           setenv myscol $argument[1]
           setenv myecol $argument[2]
           setenv mysrow $argument[3]
           setenv myerow $argument[4]
           setenv window Y
        else if ("$argv[$lc]" == '-vp') then
           @ lc++
           set argument = `echo $argv[$lc]`
           setenv myscol $argument[1]
           setenv myecol $argument[2]
           setenv mysrow $argument[3]
           setenv myerow $argument[4]
           setenv vp Y
        else if ("$argv[$lc]" == '-ti') then
           setenv timeind Y
        else if ("$argv[$lc]" == '-rm') then
           setenv delete Y
        else if ("$argv[$lc]" == '-i') then
           setenv info Y
        else
           set len = `echo $argv[$lc] | wc -c`
           set found = `echo $argv[$lc] | grep "-" | wc -l`
           if (($found) && ($len <= 3)) then
              echo ' '
              echo " Error: Invalid option $argv[$lc]"
              echo ' '
              exit
           else
              @ remaining = 1 + $count - $lc
              if ($remaining == 2) then
                 set inputfile = $argv[$lc]
                 @ lc++
                 set outputfile = $argv[$lc]
              else
                 set inputfile = $argv[$lc]
                 set outputfile = $inputfile.s
              endif
           endif
        endif

        if ($count == $lc) then
           @ stop = 0
        endif

      end

   endif
# endif

if (($mt) && ($mr)) then
   echo ' '
   echo ' You cannot use -t and -r options at the same time'
   echo ' '
   exit
endif

if (($timeind == Y) && ( $mt == 0)) then
   echo ' '
   echo ' When you use -ti option, you need to select one time step data by using -t option'
   echo ' '
   exit
endif

if (($timeind == Y) && ( $mt == 1) && ($mynsteps != 1)) then
   echo ' '
   echo ' When you use -ti option, make sure you only select one time step of data'
   echo ' '
   exit
endif

if (($myslay != -1) && ($vp == Y)) then
   echo ' '
   echo ' You cannot use -l and -vp options at the same time'
   echo ' '
   exit
endif

goto cont

# -------------------------------------------------------------------------
usage:
echo ' '
echo 'subset [ -t "date time nsteps" ] [ -r starting_record_# ending_record_# ]'
echo '       [ -s "SPC_LIST" ] [ -w "start_col end_col start_row end_row" ] '
echo '       [ -vp "start_col end_col start_row end_row" ] [ -ot bin/ncf ] '
echo '       [ -l "start_layer end_layer" ] [ -i] [ -rm ] [ -ti ] [ -h] '
echo '       infile [ outfile ] '
echo ' '
echo '    where -t  -- subset at a specific data and time for nsteps'
echo '          -r  -- subset at a specifice record range'
echo '          -s  -- subset with a specific species list'
echo '          -w  -- subset with respect to a specific window defined by'
echo '                 starting and ending column and row, respectively'
echo '          -vp -- subset with respect to a vertical profile specification'
echo '                 and data will be output to a file, vp.dat'
echo '          -ot -- output_file type: bin or ncf'
echo '                 Note: it is recommended to specify an output file name'
echo '                       if you use this option'
echo '          -l  -- subset with a specific layer range'
echo '          -i  -- display information of the infile'
# echo '          -a -- output in ASCII format (default in netCDF format)'
echo '          -rm -- delete an existing output file '
echo '          -ti -- create an output file in time independent mode'
echo '          -h  -- display usage information'
echo ' '
echo '    Note: 1. If no outfile is supplied, the default naming '
echo '             convention with .s extension will be used '
echo '          2. With -i option, no outfile is needed'
echo '          3. options -t and -r should not be used at the same time'
echo '          4. setenv IOAPI_OFFSET_64 T to enable 64 bit offset netcdf file'
echo ' '
echo '    e.g. subset -t "1999185 120000 3" -s "NO NO2 O3" ICON1 ICON2"'
echo ' '
echo '         This will extract species NO, NO2 and O3 in 1999185 at '
echo '         120000 for 3 time steps from ICON1 and output to ICON2'
echo ' '

exit

