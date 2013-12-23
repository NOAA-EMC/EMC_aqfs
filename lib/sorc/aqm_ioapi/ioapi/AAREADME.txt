
                PROCESSING 5KM "MMOUT" DATA
            TO GENERATE MET_CRO_2D, THEN RFS INPUT.


1.  On "apollo", cd /scratch/DESWAT/; ls MMOUT_DOMAIN3.*

2.  The input file should be of the form MMOUT_DOMAIN3.YYYYDDDHH
    where YYYYDDD is the Julian date, and HH is the forecast cycle
    (00 or 12)

3.  setenv DATE YYYYDDD

4.  setenv CYCLE HH

5.  setenv BIN IRIX6n32f90

6.  Run.mm5tom3 >& Log.mm5tom3.${DATE}${CYCLE}

7.  Run.raintorfs >& Log.raintorfs.${DATE}${CYCLE}

8.  The RFS file you want is named RAIN_3.elsa.${DATE}${CYCLE};
    the MET files are MET_*.elsal.${DATE}${CYCLE}

9.  Clean up antiquated files, if appropriate.

