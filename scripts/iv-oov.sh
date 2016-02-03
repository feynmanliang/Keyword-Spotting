#!/bin/tcsh

set ALLARGS=($*)

set CHANGED
if ($#argv > 1) then
while ($?CHANGED)
  unset CHANGED
  if ( "$argv[1]" == "-THRESH" )  then
    set CHANGED
    shift argv
    set THRESH = $argv[1]
    shift argv
  endif  
end
endif

# Check Number of Args
if ( $#argv != 3 ) then
   echo "Usage: $0  [-THRESH threshold] KWS-resultsdir KWS-term-map select"
   echo " e.g.: $0  kws/word lib/terms/ivoov.map iv"
   exit 1
endif

set LGEID=202
set RESDIR=$1
set TERMMAP=$2
set IV_OOV_TAG=$3

if ( ! -d $RESDIR ) then
    echo "ERROR: KWD results dir not found: $RESDIR"
    exit 1
else
    set CSV = $RESDIR/Full-Occur-MITLLFA3-AppenWordSeg.alignment.csv
    if ( ! -f $CSV ) then
        echo "ERROR: KWS alignment file not found: $CSV"
        exit 1
    endif
    set SUMFILE = $RESDIR/Full-Occur-MITLLFA3-AppenWordSeg.sum.txt
    if ( ! -f $SUMFILE ) then
        echo "ERROR: summary file not found: $SUMFILE"
        exit 1
    endif
endif
if ( ! -f $TERMMAP ) then
    echo "ERROR: term map file not found: $TERMMAP"
    exit 1
endif

set TOTDUR=`awk '{if (NF==5 && $2=="TotDur") print $4;}' $SUMFILE`
if ( ! $?THRESH ) then
    # use MTWV threshold from summary file
    set THRESH=`grep Occurr $SUMFILE | awk '{if (NF==57) {print $34} else if (NF>15) {print $(NF-5)};}'`
endif

sed -e 's/OP2VLLP9o.//g' -e 's/tune-/-/g' $CSV | perl /usr/local/teach/MLSALT5/Practical/lib/perls/compute_ATWV.IV.OOV.pl $LGEID - $TERMMAP $THRESH $IV_OOV_TAG $TOTDUR


