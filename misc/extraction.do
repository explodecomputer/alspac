

// now extract variable lables
log using mylog, name(newlog) replace 

use `1'
foreach var of varlist _all{ 
 di _col(3) "`var'" _col(20) "`:var label `var''" 
} 
log close newlog

// now write variable labels in horrible format
translate mylog.smcl `2'.txt, replace

// write to csv
outsheet using `2'.csv , comma noquote replace
