#!/bin/csh
foreach file (laypoint.f postplm.f pre_point.f mod_laypoint.f )
#echo checking $file
#diff $file /gpfs/work/pou/aqf/premaq/premaq_2005_update_2/$file
end


foreach file (pre_mobile.f mod_mobile.f nlsqrm6.f )
#echo checking $file
#diff $file /gpfs/work/pou/aqf/premaq/premaq_2005_update_2/$file
end


foreach file ( mrggrid.f pre_mrggrid.f  mod_mrggrid.f )
#echo checking $file
#diff $file /gpfs/work/pou/aqf/premaq/premaq_2005_update_2/$file 
end

foreach file (emissions.F prep_emis.F getmet.F  modgrid.f wrtsup.f )
#echo checking $file
#diff $file /gpfs/work/pou/aqf/premaq/premaq_2005_update_2/$file
end
