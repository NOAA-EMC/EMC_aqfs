#!/bin/bash

#	mv grib2_aqm_HIpm25nmmb.t06z.196   grib2_aqm_hipm25nmmb.t06z.196 
#	mv grib2_aqm_AKpm25nmmb.t06z.196   grib2_aqm_akpm25nmmb.t06z.196 
#	mv grib2_aqm_HIpm25nmmb.t12z.198   grib2_aqm_hipm25nmmb.t12z.198 
#	mv grib2_aqm_AKpm25nmmb.t12z.198   grib2_aqm_akpm25nmmb.t12z.198
#	mv grib2_aqm_5xpm25nmmb.t06z.227 grib2_aqm_cspm25nmmb.t06z.227 
#	mv grib2_aqm_5xpm25nmmb.t12z.227 grib2_aqm_cspm25nmmb.t12z.227 

for id in 196 198 227
do
#mv grib2_cmaq_1hro3-awpozcon.t06z.$id  grib2_aqm_1hro3-awpozcon.t06z.$id
#mv grib2_cmaq_1hro3-awpozcon.t12z.$id  grib2_aqm_1hro3-awpozcon.t12z.$id
#mv grib2_cmaq-1hro3-maxi.t06z.$id  grib2_aqm-1hro3-maxi.t06z.$id
#mv grib2_cmaq-1hro3-maxi.t12z.$id  grib2_aqm-1hro3-maxi.t12z.$id
#mv grib2_cmaq_ave_1hr_o3-awpozcon.t06z.$id grib2_aqm_ave_1hr_o3-awpozcon.t06z.$id
#mv grib2_cmaq_ave_1hr_o3-awpozcon.t12z.$id grib2_aqm_ave_1hr_o3-awpozcon.t12z.$id
#mv grib2_cmaq_ave_8hr_o3-awpozcon.t06z.$id grib2_aqm_ave_8hr_o3-awpozcon.t06z.$id
#mv grib2_cmaq_ave_8hr_o3-awpozcon.t12z.$id grib2_aqm_ave_8hr_o3-awpozcon.t12z.$id
mv grib2_cmaq-8hro3-maxi.t06z.$id  grib2_aqm-8hro3-maxi.t06z.$id
mv grib2_cmaq-8hro3-maxi.t12z.$id  grib2_aqm-8hro3-maxi.t12z.$id

done

#mv grib2_cmaq-1hro3_bc-maxi.t06z.227  grib2_aqm-1hro3_bc-maxi.t06z.227
#mv grib2_cmaq-1hro3_bc-maxi.t12z.227  grib2_aqm-1hro3_bc-maxi.t12z.227
#mv grib2_cmaq_8hro3-awpozcon.t06z.227 grib2_aqm_8hro3-awpozcon.t06z.227
#mv grib2_cmaq_8hro3-awpozcon.t12z.227 grib2_aqm_8hro3-awpozcon.t12z.227
#mv grib2_cmaq_ave_1hr_o3_bc-awpozcon.t06z.227  grib2_aqm_ave_1hr_o3_bc-awpozcon.t06z.227
#mv grib2_cmaq_ave_1hr_o3_bc-awpozcon.t12z.227  grib2_aqm_ave_1hr_o3_bc-awpozcon.t12z.227
#mv grib2_cmaq-8hro3_bc-maxi.t06z.227     grib2_aqm-8hro3_bc-maxi.t06z.227
#mv grib2_cmaq-8hro3_bc-maxi.t12z.227     grib2_aqm-8hro3_bc-maxi.t12z.227
#mv grib2_cmaq_ave_8hr_o3_bc-awpozcon.t06z.227  grib2_aqm_ave_8hr_o3_bc-awpozcon.t06z.227
#mv grib2_cmaq_ave_8hr_o3_bc-awpozcon.t12z.227  grib2_aqm_ave_8hr_o3_bc-awpozcon.t12z.227

