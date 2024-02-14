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

label variable rlze1  "total workers"
label variable rlze2  "Car, truck, or van"
label variable rlze3  "Car, truck or van: drove alone"
label variable rlze4  "Car, truck, van: carpooled"
label variable rlze5  "2-person carpool"
label variable rlze6  "3-person carpool"
label variable rlze7  "4-person carpool"
label variable rlze8  "5 or 6-person carpool"
label variable rlze9  "7 or more person carpool"
label variable rlze10 "Public transit"
label variable rlze11 "Pt: Bus"
label variable rlze12 "Pt: Streetcar"
label variable rlze13 "Pt: Subway or elevated"
label variable rlze14 "Pt: Railroad"
label variable rlze15 "Pt: Ferryboat"
label variable rlze16 "Taxi"
label variable rlze17 "Motorcycle"
label variable rlze18 "Bike"
label variable rlze19 "Walk"
label variable rlze20 "Other"
label variable rlze21 "Worked at home"

reshape long rlze, j(j) i(gisjoin)

rename j transtype

drop year

/* So what we want to do first is just drop some of the variables */ 
/* How about a nesting scheme where we lump all the "other" categories into the */
/* Excluded category and see what happens */

gen excluded = 0
replace excluded = 1 if transtype == 16 |  transtype == 17 |   transtype == 18 |   transtype == 19  | transtype == 20 | transtype == 21   

bysort gisjoin: egen totaltrans = total(rlze*(_n == 1))

bysort gisjoin: egen outside = total(excluded*rlze)

gen si = rlze / totaltrans


bysort gisjoin: egen transit = total(rlze*(transtype == 10))

gen swg = 1
replace swg = rlze/transit if transtype == 11
replace swg = rlze/transit if transtype == 12
replace swg = rlze/transit if transtype == 13
replace swg = rlze/transit if transtype == 14
replace swg = rlze/transit if transtype == 15

drop if transtype == 1 | transtype == 16 |  transtype == 17 |   transtype == 18 |   transtype == 19  | transtype == 20 | transtype == 21  | transtype == 10

drop if si == 0 

bysort gisjoin: egen transcount = total( (transtype==11) + (transtype==12) + (transtype == 13) + (transtype ==14) + (transtype==15) )

bysort gisjoin: gen last = _n == _N


tab transtype, gen(tdum)

gen lnsi = ln(si)
gen lnswg = ln(swg) 

reg lnsi lnswg tdum*

capture program drop mlfunbase
program mlfunbase
    version 14.1
    args lnf mu rho1 sigma
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho1'*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + $ML_y4*($ML_y3 - 1)*ln(1 - `rho1')
end

ml model lf mlfunbase (mu: lnsi = tdum1 tdum2 tdum3 tdum4 tdum5 tdum6 tdum7 tdum8 tdum9 tdum10 tdum11 tdum12) (rho1: lnswg = ) (sigma: transcount last = )
ml maximize 

