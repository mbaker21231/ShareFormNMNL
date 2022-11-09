/* revamping the project */

/* Using new data - we are going to work it as follows  */
/* We imagine that each individual has to make a choice */
/* Let's first look at the data we have and think about */
/* the best way to model transit choice.                */

import delimited "C:\Users\mjbaker\OneDrive - CUNY\Documents\github\ShareFormNMNL\Data\nhgis0029_csv\nhgis0029_ts_nominal_county.csv", clear
set more off

/* Let's clean up some of the data so we have a bit less */
/* Drop margins of error for the relevant variables */

drop b78aa125 b78aa125m b78aa195 b78aa195m b86aa125m b86aa195m b86ab125m b86ab195m 
drop b86ac125m b86ac195m b84aa125m b84aa195m b84ab125m b84ab195m b84ac125m b84ac195m
drop b84ad125m b84ad195m b84ae125m b84ae195m b84af125m b84af195m c53aa125m c53aa195m
drop c53ab125m c53ab195m
drop c53ac125m c53ac195m c53ad125m c53ad195m c53ae125m c53ae195m c53af125m c53af195m
drop c53ag125m c53ag195m c53ah125m c53ah195m c53ai125m c53ai195m c53aj125m c53aj195m
drop c53ak125m c53ak195m c53al125m c53al195m c53am125m c53am195m c53an125m c53an195m
drop c53ao125m c53ao195m c53ap125m c53ap195m c53aq125m c53aq195m c53ar125m c53ar195m 
drop c53as125m c53as195m c53at125m c53at195m cw0aa125m cw0aa195m

/* Drop 1970s variables as we don't need them */
drop b84aa1970 b84ab1970 b84ac1970 b84ad1970 b84ae1970 b84af1970

rename b86aa125 b86aa2010
rename b86ab125 b86ab2010

rename b86aa195 b86aa2020
rename b86ab195 b86ab2020

rename b86ac125 b86ac2010
rename c53ac125 c53ac2010 
rename c53ag125 c53ag2010 
rename c53ak125 c53ak2010 
rename c53ao125 c53ao2010 
rename c53as125 c53as2010 

rename b86ac195 b86ac2020
rename c53ac195 c53ac2020
rename c53ag195 c53ag2020
rename c53ak195 c53ak2020
rename c53ao195 c53ao2020
rename c53as195 c53as2020

rename b84aa125 b84aa2010
rename c53ad125 c53ad2010
rename c53ah125 c53ah2010
rename c53al125 c53al2010
rename c53ap125 c53ap2010 
rename c53at125 c53at2010 

rename b84aa195 b84aa2020
rename c53ad195 c53ad2020
rename c53ah195 c53ah2020
rename c53al195 c53al2020
rename c53ap195 c53ap2020 

rename b84ab125 b84ab2010
rename c53ae125 c53ae2010
rename c53ai125 c53ai2010
rename c53am125 c53am2010
rename c53aq125 c53aq2010 
rename cw0aa125 cw0aa2010

rename b84ab195 b84ab2020
rename c53ae195 c53ae2020
rename c53ai195 c53ai2020
rename c53am195 c53am2020
rename c53aq195 c53aq2020 
rename cw0aa195 cw0aa2020

rename b84ac125 b84ac2010
rename c53af125 c53af2010
rename c53aj125 c53aj2010
rename c53an125 c53an2010
rename c53ar125 c53ar2010 

rename b84ad125 b84ad2010
rename b84ae125 b84ae2010
rename b84af125 b84af2010

rename b84ad195 b84ad2020
rename b84ae195 b84ae2020
rename b84af195 b84af2020

rename c53aa125 c53aa2010
rename c53ab125 c53ab2010

rename c53aa195 c53aa2020
rename c53ab195 c53ab2020

rename b84ac195 b84ac2020
rename c53af195 c53af2020
rename c53aj195 c53aj2020
rename c53an195 c53an2020
rename c53ar195 c53ar2020 
rename c53at195 c53at2020

local misslist c53aa1990 c53ab1990 c53ac1990 c53ad1990 c53ae1990 c53af1990 c53ag1990 
local misslist `misslist' c53ah1990 c53ai1990 
local misslist `misslist' c53aj1990 c53ak1990 c53al1990 c53am1990 c53an1990 
local misslist `misslist' c53ao1990 c53ap1990 c53aq1990 c53ar1990 c53as1990 c53at1990

local misslist : subinstr local misslist "1990" "1980", all

foreach v of local misslist {
    gen `v' = .
}

unab  allvars: _all

local removed "gisjoin state statefp statenh county countyfp countynh"
local tsvars : list allvars - removed
local tsvars : subinstr local tsvars "1970" "", all
local tsvars : subinstr local tsvars "1980" "", all
local tsvars : subinstr local tsvars "1990" "", all
local tsvars : subinstr local tsvars "2000" "", all
local tsvars : subinstr local tsvars "2010" "", all
local tsvars : subinstr local tsvars "2020" "", all

local tsvars : list uniq tsvars

disp "`tsvars'"

reshape long "`tsvars'", i(gisjoin) j(year)

/* Add some variable labels */

label var b78aa "Total Population"
label var aa0aa "White Population"
label var aa0ab "Black Population"
label var aa0ac "Native American Population"
label var aa0ad "Asian Population"
label var aa0ae "Other Race Population"
label var aa0af "Two or more Races Population"

drop aa0af /* There are a lot of missings */

label var b86aa	"Less than 4 years college"	               
label var b86ab "4 years college"		               
label var b86ac "5 or more years college"		               
label var b84aa	"16 years older - in labor force"	                
label var b84ab	"16 years older - in lf, in armed forces"	               
label var b84ac "16 years older - in labor force, Civilian"		               
label var b84ad "In labor force, civilian, employed"		               
label var b84ae "In labor force, civilian, unemployed"		               
label var b84af "Persons not in labor force"	

label var c53aa "Car, Truck, Van"
label var c53ab "Car, Truck, Van - drove alone"
label var c53ac "Car, Truck, Van - Carpooled"
label var c53ad "Car, Truck, Van - 2 Person Carpool"
label var c53ae "Car, Truck, Van - 3 person carpool"
label var c53af "Car, Truck, Van - 4 person carpool"
label var c53ag "Car, Truck, Van - 5, 6 person carpool"
label var c53ah "Car, Truck, Van - 7 or more person carpool"

label var c53ai "Public transit"
label var c53aj "Public transit - Bus"
label var c53ak "Public transit - Streetcar, Trolley"
label var c53al "Public transit - Subway"
label var c53am "Public transit - Train or Rail"
label var c53an "Public transit - Ferry"
label var c53ao "Taxi"
label var c53ap "Motorcycle"
label var c53aq "Bicycle"
label var c53ar "Walked" 
label var c53as "Other means"
label var c53at "Walked"

label var cw0aa "Total commuters"

/* Keeping track of missing variables */
/* 1980 is missing transit variables, so */

drop if year == 1980

egen b78miss = rowmiss(b78*)
egen aamiss  = rowmiss(aa*)
egen b86miss = rowmiss(b86*)
egen b84miss = rowmiss(b84*)
egen c53miss = rowmiss(c53*)
egen cw0miss = rowmiss(cw0*)

/* Dropping panels with any of these missing */

bysort gisjoin: egen b78count = max(b78miss)
bysort gisjoin: egen aacount  = max(aamiss)
bysort gisjoin: egen b86count = max(b86miss)
bysort gisjoin: egen b84count = max(b84miss)
bysort gisjoin: egen c53count = max(c53miss)
	               
drop if c53count > 0

/* Making a first category - using motor power versus everything else */
/* We also want to again reshape things so that we have everything in */
/* terms of transit choices. So, we should have categories for each   */

tostring(year), gen(yearst)

gen gisyear = gisjoin + yearst

local stubs aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at
scalar N = 1
foreach k of local stubs {
   rename c53`k' c53m`=N'
   scalar N = `=N' + 1
  }
  
reshape long c53m, i(gisyear) j(alt)


label define choicevars 1 `""Car, Truck, Van""' 2 `""CTV - Alone""' 3 `""CTV - Carpooled""' 4 ///
             `""CTV - Carpool - 2people""' 5 `""CTV - Carpool - 3people""' 6 `""CTV - Carpool - 4 people""' ///
			 7 `""CTV - Carpool - 5,6 people""' 8 `""CTV - Carpool - 7 or more people""' 9 `""Pub Transit""' 10 ///
			 `""Pub Trans - Bus""' 11 `""Pub Trans - Streetcar""' 12 `""Pub Trans -Subway""' 13 `""Pub Trans - Train""' ///
			 14 `""Pub Trans - Ferry""' 15 `""Taxi""' 16 `""Motorcycle""' 17 `""Bicycle""' 18 `""Walked""' 19 ///
			 `""Other""' 20 `""Worked From Home""', replace
			 
label values alt  choicevars

drop yearst

/* Calculation of total commuters requires considering only some categories */

gen talleycats = 0 
replace talleycats = 1 if alt == 1 | alt == 9 | alt == 15 | alt == 15 | ///
    alt == 16 | alt == 17 | alt == 18 | alt == 19 | alt == 20   

bysort state county year: egen commuters = total(c53m*talleycats)

order state county year alt c53m commuters

/* Drop the carpool breakdown and just go with "carpool" as a category */

drop if alt == 4 | alt == 5 | alt == 6 | alt == 7 | alt == 8

/* If an alternative is never chosen, it probably isn't available */

drop if c53m == 0 

/* Now, let's form a count of available transit alternatives */

bysort state county year: egen options = count(alt)

order state county year options alt c53m commuters

bysort state county year: gen last = _n == _N

/* let's look at some of these adding-up arrangements to see what we have */

list state county year b78aa cw0aa commuters b84ad b84ae if last

/* These variables are, respectively, Total Population, commuters (reported), 
  commuters (calculated) civilian labor force, unemployed in civilian labor force */   
  
/* Let's just get a quick feel for some of these variables so we can see how they */
/* interact */

gen unemp = b84ae / b84ad  
sum unemp if last

/* Some screwy numbers are in the above, but let's let it slide - they are 
   all less than one */

/* what about labor force as a population fraction? */

gen lfp = b84ad / b78aa  

/* again, all the numbers seem sensible...what about what happens next? */

/* what about commuters and how it relates to lfp? Here is reported commuters */

gen comshare = cw0aa / b84ad

/* The above don't even come close to adding up. */

gen comshare2 = cw0aa / (b84ad - b84ae)

  
/* In light of these things, my instinct is to do the following */
/* calculate the labor force participation rate in each state */
/* then, model choice as a situation in which we have a basic */
/* this is something to work out mathematically. */ 

/* Some tests of adding up */

/* This test verifies that the car, truck, van category can be broken into 
   "drove alone" and "carpooled" categories */

   
   
bysort state county year: egen test = total( (alt == 2)*c53m + (alt == 3)*c53m)
sum test c53m if alt == 1

assert test == c53m if alt == 1
drop test

/* This test verifies that the public transit category adds up to its components */
/* So, there are multiple ways of viewing this */

bysort state county year: egen test = total( (alt == 10)*c53m + (alt == 13)*c53m + ///
                        (alt == 11)*c53m + (alt == 12)*c53m + (alt == 14)*c53m )

assert test == c53m if alt == 9

/* First blush - get rid of aggregate data and reassemble it based on nests */

drop if alt == 1 | alt == 9

/* FIRST NESTING SCHEME */

/* Self-motorization group */

gen selfmotor = alt == 2 | alt == 3 | alt == 16 | alt == 15
gen public    = alt == 10 | alt == 13 | alt == 11 | alt == 12 | alt == 14
gen selfpower = alt == 17 | alt == 18 | alt == 19 

gen altcount = 0
replace altcount = 1 if selfmotor 
replace altcount = 2 if public
replace altcount = 3 if selfpower

bysort state county year: egen smtotal = total(selfmotor*c53m)
bysort state county year: egen publictot = total(public*c53m)
bysort state county year: egen powertot  = total(selfpower*c53m)

bysort state county year: egen outside = total( (alt==20)*c53m)
replace outside = 1 if outside == 0
gen s0 = outside / commuters
drop if alt == 20

/* Two questions: one is the nesting structure, the other is what to
   do about the endogeneity of the choice set...why would this matter */
   
/* We care about all sets with two or more alternatives that are not identical */
/* So, in our case, we have: */

drop options

bysort state county year: egen options = count(year)

/* Now, we have too many options and should try to reduce the number to 9, say */
/* So, maybe we can combine the streetcar and bus categories, the bike/walked */
/* categories, and the train and subway categories so we have 9 total options */

/* Let's first just pare it down to 10 options */

gen togroup = inlist(alt, 17, 18) /* creates a group for walking and biking */
bysort togroup state county year : replace c53m = c53m[1] + c53m[2] if togroup & _N == 2

drop togroup
drop if alt == 17 /* No need to retain biked as a category, as lumped in with walked now */

sort state county year alt

/* Let's also collapse streetcar and bus */

gen togroup = inlist(alt, 10, 11)
bysort togroup state county year: replace c53m = c53m[1] + c53m[2] if togroup & _N == 2
drop togroup
drop if alt == 10

/* Let's also collapse subway and train */

gen togroup = inlist(alt, 12, 13)
bysort togroup state county year: replace c53m = c53m[1] + c53m[2] if togroup & _N == 2
drop togroup
drop if alt == 12

/* And let's drop collapse motorcycle */

gen togroup = inlist(alt, 2, 16)
bysort togroup state county year: replace c53m = c53m[1] + c53m[2] if togroup & _N == 2
drop togroup
drop if alt == 2

sort state county year alt

label define choicevars 1 `""Car, Truck, Van""' 2 `""CTV - Alone""' 3 `""CTV - Carpooled""' 4 ///
             `""CTV - Carpool - 2people""' 5 `""CTV - Carpool - 3people""' 6 `""CTV - Carpool - 4 people""' ///
			 7 `""CTV - Carpool - 5,6 people""' 8 `""CTV - Carpool - 7 or more people""' 9 `""Pub Transit""' 10 ///
			 `""Pub Trans - Bus""' 11 `""Pub Trans - Bus or Streetcar""' 13 `""Pub Trans - Subway or Train""' ///
			 14 `""Pub Trans - Ferry""' 15 `""Taxi""' 16 `""Alone Car or Motorcycle""' 18 `""Biked or Walked""' 19 ///
			 `""Other""' 20 `""Worked From Home""', replace
			 
label values alt  choicevars

tab alt



/************************************************************/
/**** now, read in sets of alternatives and create possibilities */

mata:
    st_view(X=.,.,"alt")
	x = rows(uniqrows(X))

	Menus = asarray_create("real", 1)
	for (n=2;n<=x;n++) {
	    asarray(Menus, n, mm_subsets(x, n))
	}
end

/* So we now have a list of all the possible menus */
/* Now, let's think about iterating over all the keys in the menu */

/* What about partitions? */
/* the following gives us all possible partitions */

mata:
   p = mm_partitions(x)
end    

/* the problem is this basically gives us groupings and now we have to get the groupings */
/* the first row is where everything is grouped together, while the second row has a group */
/* now, we select a column of the above, which basically says "partition the choice set */
/* into a set of the given sizes */

mata:
    col = select(p[.,15], p[.,15]:>0)

    ph = J(0,1,0)
    for (i=1;i<=rows(col);i++) {
        ph = ph \ J(col[i],1,i)
    }
end

/* this last snippet creates a panel structure, so each group belongs with each other group */
/* very cute...now, create variables for the partitions */

mata: 
	
	perms = J(rows(ph),1,0)
	info = cvpermutesetup(ph)
	
	perms = J(rows(ph),0,0)
	i = 0
	while ((p=cvpermute(info)) != J(0,1,.)) {
		i ++
		perms = perms, p
		}

end
/* So, now we have all the ways of partitioning a set - so what about computing intersections */
/* with a menu? */

/* Here we can make a list of all the ways in which to partition...*/

mata:
   p   = mm_partitions(x)
   
group_pone = J(rows(p),0,0)   
   
   for (i=1;i<=cols(p);i++) {
       
	   col = select(p[.,i], p[.,i]:>0)
	   ph = J(0,1,0)
	   for (k=1;k<=rows(col);k++) {
	       ph = ph \ J(col[k],1,k)
	   }
	   group_pone = group_pone , ph
    }
  
end

/* The previous code gives us a code for working with different partitions, but we now need to */
/* iterate through to jumble these things up. */

mata:

groups = J(rows(p),0,0)

for (i=1;i<=cols(group_pone); i++) {
    info = cvpermutesetup(group_pone[.,i])
	perms = J(rows(ph), 0, 0)
    while ((z=cvpermute(info)) != J(0,1,.)) { 
	    perms = perms, z
	}
	groups = groups, perms
	i
}

end

/* The above takes awhile, that's for sure! */
/* Let's think about the computationally simpler approach */

/* This requires us to do something like the following, which is compute r_A(a,b) */
/* The way to start is with alternatives a, and b */

/* So, if we jump here after using our original data, we get the following */

egen mode = group(alt)

mata:

st_view(X=.,.,"state county year mode c53m")

x = uniqrows(X[.,4])

Menus = asarray_create("real", 1)
	for (n=2;n<=rows(x);n++) {
	    asarray(Menus, n, mm_subsets(rows(x), n))
	}

end
/* Matrix-based method */

Menus = J(0,rows(x),0)

for (n=2;n<=rows(x);n++) {
    ss = mm_subsets(rows(x), n)'
	
	ta = ss,J(rows(ss), rows(x)-cols(ss), .)
	Menus = Menus \ ta
	}
	
	
/* Picking out two alternatives to work with */

a = 1
b = 2

/* Getting sets with both a and b in them */

Asets = J(0,rows(x),.0)
for (n=1;n<=rows(A);n++) {
    if (any(A[n,.]:==a) & any(A[n,.]:==b)) Asets = Asets \ A[n,.]
	}
	
/* Now, making rows and columns of each set */
/* What is the best way to pair the sets?   */
/* We first would like a way to calculate the quantity we need */


















		
		

	



















/*
/* Base dependent variable */

gen si = c53m / commuters
gen lnsis0 = ln(si) - ln(s0)

/* Group shares */

gen smshare = smtotal / commuters
gen spshare = publictot / commuters
gen soshare = powertot / commuters 

/* Share within group  - right hand side variable */

gen lnswg = . 
replace lnswg = ln(si) - ln(smshare) if selfmotor
replace lnswg = ln(si) - ln(spshare) if public
replace lnswg = ln(si) - ln(soshare) if selfpower




//////*********************** So ends the data arrangement ******////////////

/*

/* Some basic models */

tab alt, gen(td)

reg lnsis0 lnswg td*

gen lnswg1 = lnswg*(public)
gen lnswg2 = lnswg*(selfpower) 

reg lnsis0 lnswg lnswg1 lnswg2 td* year



capture program drop mlfunbase
program mlfunbase
    version 14.1
    args lnf mu rho1 sigma
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho1'^2*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + $ML_y4 * ($ML_y3 - 1)*ln(1 - `rho1'^2)
end


/* To use this, we need counts of the number of items in each group */

bysort state county year altcount: egen ng = count(alt)
bysort state county year altcount: gen glast = _n == _N
   

ml model lf mlfunbase (mu: lnsis0 lnswg ng last = ) (rho1: ) (sigma:)
ml maximize   
   
ml model lf mlfunbase (mu: lnsis0 lnswg ng last = year) (rho1: = public selfpower) (sigma:)
ml maximize  

tab state, gen(statedum) 
   
ml model lf mlfunbase (mu: lnsis0 lnswg ng last = year statedum*) (rho1: = public selfpower) (sigma:)
ml maximize    
  
ml model lf mlfunbase (mu: lnsis0 lnswg ng last = year statedum*) (rho1: = public selfpower) (sigma: = public selfpower)
ml maximize  
