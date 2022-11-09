/* Just moving on from the last one */

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
local cvars
scalar N = 1
foreach k of local stubs {
   rename c53`k' choice`=N'
   local cvars "`cvars' choice`=N'"
   scalar N = `=N' + 1
  }
  
  
/* So, we now have all the choices for each county-year pair listed, and */
/* what we want to do next is to describe menus. We will do this in mata */
 
mata: st_view(C=.,.,"`cvars'")
mata: rows(C)
mata: rowsmissing(C)

/* So, we now have a dataset where we have 20 alternatives arranged in each row */
/* What we then can do next is see what happens when we form all possible sets  */

Menus = J(0,cols(C),0)

for (n=2;n<=cols(C);n++) {
    ss = mm_subsets(cols(C), n)'
	ta = ss,J(rows(ss), cols(C)-cols(ss), .)
	Menus = Menus \ ta
	}
	
	
/* Picking out two alternatives to work with */

a = 1
b = 2
	
cone = rowsum( (Menus:-a):== 0)
ctwo = rowsum( (Menus:-b):== 0)

/* These two lines of code basically select the two things where a occurs, and */
/* where b occurs. */

setind = cone:*ctwo

SubMenu = select(Menus, setind)

/* So, now we have a way of calculating all the choices...but what we need to do */
/* Is now think about computing the things that go into the routine */

/* We first need a means of computing the probability of a given set A. Try this */

/* Pick out a menu number - although we are going to have to figure out a way to only */
/* select situations in which the options were available...if any of the Menus are zero */
/* we can't use that option - easy enough */
/* We can probably tighten up the code a bit as well, so that we don't double count */
/* that is, we could do j = i+1 and go from there */

tn   = 1
term = 0

for (i=1;i<=rows(SubMenu);i++) {
    for (j=1;j<=rows(SubMenu);j++) {
	    
		mnA = i
        mnB = j
        
		cworkA = mm_which(SubMenu[mnA,]:!=.)
        cworkB = mm_which(SubMenu[mnB,]:!=.)
		
		SMA = SubMenu[mnA, cworkA]
        SMB = SubMenu[mnB, cworkB]

        paA = C[,a] :/ rowsum(C[,SMA]) 
        pbA = C[,b] :/ rowsum(C[,SMA])
        rabA = mean(paA)/mean(pbA)

        paB = C[,a] :/ rowsum(C[,SMB])
        pbB = C[,b] :/ rowsum(C[,SMB])
        rabB = mean(paB)/mean(pbB)

		term = term + (ln(rabA) - ln(rabB))^2
		tn++
	}
}


/* Also, we need to think about 






  
 
