/* Latest effort at MNL */
clear all
import delimited "C:\Users\mjbaker\OneDrive - CUNY\Documents\github\ShareFormNMNL\nhgis0051_csv\nhgis0051_csv\nhgis0051_ds195_20095_cbsa.csv"

drop rlzm*

rename rlze001 rlze1
rename rlze002 rlze2
rename rlze003 rlze3 
rename rlze004 rlze4 
rename rlze005 rlze5
rename rlze006 rlze6 
rename rlze007 rlze7
rename rlze008 rlze8
rename rlze009 rlze9 
rename rlze010 rlze10 
rename rlze011 rlze11
rename rlze012 rlze12
rename rlze013 rlze13 
rename rlze014 rlze14 
rename rlze015 rlze15
rename rlze016 rlze16 
rename rlze017 rlze17
rename rlze018 rlze18
rename rlze019 rlze19 
rename rlze020 rlze20 
rename rlze021 rlze21

reshape long rlze, j(j) i(gisjoin)




/* All of the basic transit variables appear in Table 1: "Means of transportation to work" */
/* Let's use Car, truck, van as the default */
/* Then make a nest with carpool */

/* First, what we would like to do is reshape all these variables so they are long */

reshape long rlze rlzm, i(gisjoin) j(j)

gen share_caralone = rlze002 / rlze001

gen share_carpool = rlze004 / rlze001

gen share_transit = rlze010 / rlze001

gen cond_share_bus  = rlze011 / rlze010
gen cond_share_scar =  rlze012 / rlze010
gen cond_share_sway = rlze013 / rlze010
gen cond_share_rail = rlze014 / rlze010
gen cond_share_ferry = rlze015 / rlze010

/* So, conditional on caralone being the excluded group, we might want to then run our nesting program to see */
/* What best fits */

