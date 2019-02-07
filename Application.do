global currentdir = "C:\Users\mjbaker\Documents\github\ShareFormNMNL\"
set more off

cd $currentdir
use ACSclean.dta, clear

/* The data is arranged so that each observation is a county/year/class */
/* The classes number 1 - 10 and are:

   1) Drive alone
   2) Carpool - One passenger
   3) Carpool - Two passengers
   4) Carpool - Three passengers
   5) Carpool - Four passengers
   6) Public transit
   7) Walk
   8) Bicycle
   9) Taxi or Other Means
   10) Worked at Home
   
   */
drop if share == . /* Some have missing share information - can't be used */

bysort geography year: egen totalshare = total(share)

/* The renormalization is basically as follows */

replace share = share / totalshare

/* These are neighboring values that I calculated - for instruments I guess */

forvalues i=1/15 {
	bysort geography year: egen total`i'share = total(nv`i')
	replace nv`i' = nv`i' / total`i'share
	drop total`i'share
}

/* Let's begin with a situation in which only one thing is nested */
/* The carpool next */

/* Group shares */
/* the real puzzle to understand is why people drive so much alone to their workplaces */
/* So, we will make this the null class */

*bysort geography year: gen group1 = class == 10 
*bysort geography year: gen group2 = class >= 2 & class <=5
*bysort geography year: gen group3 = class == 6 | class == 9
*bysort geography year: gen group4 = class == 7 | class == 8

bysort geography year: gen group1 = class == 10 | class == 2
bysort geography year: gen group2 = class >= 3 & class <=5
bysort geography year: gen group3 = class == 7 | class == 9
bysort geography year: gen group4 = class == 6 | class == 8






bysort geography year: gen gnull  = class == 1 

bysort geography year: gen g1s = group1*share
bysort geography year: gen g2s = group2*share
bysort geography year: gen g3s = group3*share
bysort geography year: gen g4s = group4*share
bysort geography year: gen g0s = gnull*share

bysort geography year: egen g1share = total(g1s)
bysort geography year: egen g2share = total(g2s)
bysort geography year: egen g3share = total(g3s)
bysort geography year: egen g4share = total(g4s)
bysort geography year: egen g0share = total(g0s)

/* We also want to do this for the nearest-neighbors data */

forvalues i=1/15 {
	bysort geography year: gen ng1`i's = group1*nv`i'
	bysort geography year: gen ng2`i's = group2*nv`i'
	bysort geography year: gen ng3`i's = group3*nv`i'
	bysort geography year: gen ng4`i's = group4*nv`i'
	bysort geography year: gen ng0`i's = gnull*share

	bysort geography year: egen ng1`i'share = total(ng1`i's)
	bysort geography year: egen ng2`i'share = total(ng2`i's)
	bysort geography year: egen ng3`i'share = total(ng3`i's)
	bysort geography year: egen ng4`i'share = total(ng4`i's)
	bysort geography year: egen ng0`i'share = total(ng0`i's)	
}

sort geography year class

/* We first need to get a sense of how the size of our nest varies */
/* In some areas, we can't use observations because they have zero */
/* shares. Let's suppose (for the sake of moving things along)     */
/* that this is because these things aren't options                */
/* This seems like a safe assumption!                              */

bysort geography year: egen Ng1 = total(class == 10)
bysort geography year: egen Ng2 = total(class >= 2 & class <= 5)
bysort geography year: egen Ng3 = total(class == 6 | class == 9)
bysort geography year: egen Ng4 = total(class == 7 | class == 8)
bysort geography year: egen Null = total(class == 1)

sum Null
assert r(mean) == 1

/* So, now we have group counts and group shares. We can now calculate shares within groups, 
   and create our dependent variables */

gen lns = ln(share) - ln(g0share)

gen lnsi =ln(share)
/* We shouldn't need group ten as a group now, so let's drop it. */

drop if gnull

/* Create the value for neighbors as well, along with the average value among neighbors */

forvalues i=1/15 {
	gen nlns`i' = ln(nv`i') - ln(ng0`i'share)
	drop ng0`i'share
}

/* generate shares within group */

gen swg = share/(group1*g1share + group2*g2share + group3*g3share + group4*g4share)

forvalues i=1/15 {
	gen nswg`i' = nv`i'/(group1*ng1`i'share+group2*ng2`i'share+group3*ng3`i'share)
}

/* note that there are a fair number of zeros for the above. This is because in
   some instances, the share number is the same as the group number */

gen lnswg = ln(swg)  

forvalues i=1/15 {
	gen lnnswg`i' = ln(nswg`i')
}

/* Make an average and lag it */

egen avenswg = rowmean(lnnswg1-lnnswg15)
replace avenswg = 0 if avenswg == .
sum lnnswg1-lnnswg15

tostring(year), gen(years)
tostring(class), gen(class_s)

gen xtvar = geography + "_" + class_s
encode xtvar, gen(xtvcode)

xtset xtvcode year

/* Let's make lags of all variables two-deep */

gen l_lnswg = l.lnswg
gen l_avenswg = l.avenswg
gen l2_lnswg = l2.lnswg
gen l2_avenswg = l2.avenswg
gen lnswg1 = group1*lnswg
gen lnswg2 = group2*lnswg
gen lnswg3 = group3*lnswg
gen l_lns = l.lns


replace l_lnswg = 0 if l_lnswg == .
replace l_avenswg = 0 if l_avenswg == .
replace l2_lnswg = 0 if l2_lnswg == .
replace l2_avenswg = 0 if l2_lnswg == .

/* Some dummies for each option */

gen carpool  = class == 2
gen carpool2 = class == 3
gen carpool3 = class == 4
gen carpool4 = class == 5

gen bike = class == 8
gen taxi = class == 9

/* Some state-level dummies */

tab state, gen(stdum)
tab year, gen(ydum)
tab geography, gen(gd)

/******************************************/
/******************************************/
/****** A base model and regression *******/

/* We put in group dummies, and drop one from each category as well */

reg lns lnswg group2 group3 carpool2 carpool3 carpool4 taxi bike

/* Include a time trend , including group-specific trends */

gen trend = year - 2005
gen g1trend = group1*trend
gen g2trend = group2*trend
gen g3trend = group3*trend
gen g4trend = group4*trend

reg lns lnswg group2 group3 group4 carpool carpool2 carpool3 carpool4 taxi bike g2trend g3trend g4trend trend
reg lns lnswg group2 group3 group4 carpool carpool2 carpool3 carpool4 taxi bike g2trend g3trend g4trend trend stdum* 

/* Very first thing - let's work with the "right" function here, and only consider a carpool dummy */

capture program drop mlfunbase
program mlfunbase
    version 14.1
    args lnf mu rho1 sigma
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho1'^2*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + $ML_y4 * ($ML_y3 - 1)*ln(1 - `rho1'^2)
end

gen Ng = 1
replace Ng = Ng1 if group1 == 1
replace Ng = Ng2 if group2 == 1
replace Ng = Ng3 if group3 == 1
replace Ng = Ng4 if group4 == 1

gen group = 1
replace group = 2 if group2
replace group = 3 if group3
replace group = 4 if group4

bysort xtvcode year group: gen last = _n == _N


ml model lf mlfunbase (mu: lns lnswg Ng last = ) (rho1:) (sigma:)
ml maximize

ml model lf mlfunbase (mu: lns lnswg Ng last group1 group2 group3 = ) (rho1: ) (sigma: ) if state=="New Jersey"
ml maximize, difficult

gen gy2 = group2*year
gen gy3 = group3*year
gen gy4 = group4*year

ml model lf mlfunbase (mu: lns lnswg Ng last = group2 group3 group4 incounty) (rho1: ) (sigma:)
ml maximize, difficult

/* Maybe there are just too many different places */
/* What if we just do California? */ 
/* Or large counties? */

