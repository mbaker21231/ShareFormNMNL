/* This file uses the one developed in CreateData.ipynb */

capture use "C:\Users\matthew\Downloads\medicare.dta", clear
capture use "C:\Users\\mjbaker\\Downloads\\medicare.dta", clear

set more off

drop if Enrolled_y == "nan"
drop if Eligibles  == "nan"

drop index

rename Enrolled_x plan_enr
rename Enrolled_y enr_tot
rename Eligibles elig

replace enr_tot = subinstr(enr_tot, ",", "", .)
replace enr_tot = subinstr(enr_tot, "*", "", .)
destring enr_tot, replace

replace elig = subinstr(elig, ",", "", .)
destring elig, replace

duplicates report
duplicates drop

bysort State County year month: egen enr_tot_calc = total(plan_enr)
bysort State County year month: gen last = _n == _N

drop if State == "AS" | State == "GB" | State == "AK" | State == "GU" |  ///
        State == "PR" | State == "VI" | State == "nan"

drop if County == "Bedford City" /* Not sure what's going on here... */
drop if County == "Pinal" & State=="AZ" /* this is also screwy */

/* Make a functioning date variable */

gen day = 1
gen double date = mdy(month, day, year)
drop day
format date %d

/* There are some duplicates, which seem as though enrollments are broken out */
/* into two parts. Here is a fix */

/* Plans with no enrollment we assume aren't offered */
/* These plans in fact have no id numbers */

assert Contract_ID == "nan" if plan_enr == . 
assert plan_enr == . if Contract_ID == "nan"

drop if plan_enr == . 

/* Some plans do not have choice characteristics */
/* These four plans only occur as singletons and should probably be removed */
/* from the choice set */

gen no_choice = 0
replace no_choice = 1 if Plan_Type == "Continuing Care Retirement Community"
replace no_choice = 1 if Plan_Type == "ESRD I - PFFS"
replace no_choice = 1 if Plan_Type == "ESRD II - HMOPOS"
replace no_choice = 1 if Plan_Type == "Employer/Union Only Direct Contract PFFS"
replace no_choice = 1 if Plan_Type == "PSO (State License)"
replace no_choice = 1 if Plan_Type == "nan" /* Just don't know what's going on here...*/

/* Total of those not really making choices */

bysort State County year month: egen really_no_choice = total(no_choice * plan_enr)

gen enr_tot_calc_adj = enr_tot_calc - really_no_choice
gen elig_adj = elig - really_no_choice
gen lnelig = ln(elig_adj)

/* Now, we can get rid of those plans that don't really involve any choice */

drop if no_choice == 1

/* Now, we can generate plan types, enrollments, etc. at the county-time-plan level */

bysort State County year month Plan_Type: egen plan_type_cou = count(Plan_Type)
bysort State County year month Plan_Type: egen plan_type_enr = total(plan_enr)

/* Make IDS and get rid of duplicates if there are any */

egen numid = group(State County Plan_Type Contract_ID)
bysort numid date: egen gC = count(numid)
bysort numid date: gen gn = _n

bysort State County year month Plan_Type Contract_ID: egen plan_enrph = total(plan_enr)

replace plan_enr = plan_enrph if gC == 2

drop if gn == 2
drop gn gC plan_enrph

duplicates report numid date

xtset numid date

/* Now, we can start plotting the number of plans for certain counties to get
   a feel for the data */
   
/* Los Angeles */

/* How many people are enrolled of plans of a given type in a place? */

gen plan_type_sha = plan_type_enr/enr_tot_calc_adj /* Share of enrolled in a particular type of plan */

bysort State County year month Plan_Type: gen last_plan = _n == _N

preserve

keep if County == "Bronx" & State == "NY"
keep if last_plan

egen tempid = group(Plan_Type)
xtset tempid date

xtline plan_type_cou, ylabel(none) xlabel( , angle(25)) overlay saving(cou, replace) legend(off)
xtline plan_type_enr, ylabel(none) xlabel( , angle(25)) overlay saving(enr, replace) legend(off)
xtline plan_type_sha, ylabel(none) xlabel( , angle(25)) overlay saving(sha, replace) legend(off)

graph combine cou.gph enr.gph sha.gph, 

restore 

/* Note there are some plans that never occur as more than one choice. These include: */
/* Continuing Care Retirement Community, ESRD I - PFFS, ESRD II - HMOPOS, Employer/Union Only */
/* Direct Contact PFFS, and nan plans, which are really classified by their Organization_Type */

/* These can all be split out as not really part of the medicare advantage choice */ 
/* The best approach is really to subtract anyone in one of these plans as not part of the population, I think */

/* Now, we have a bunch of plans. Which ones are of which type? */

/* Since we have dropped out the special cases, each of these plans could be */
/* considered as either an HMO plan, a PPO plan, or a PFFS plan. */

/* These are some broader categories that we might want to use */

gen HMO = 0
gen PPO = 0
gen PFS = 0
gen COS = 0
gen PCE = 0
gen MSA = 0

replace COS = 1 if Plan_Type == "1876 Cost"
replace COS = 1 if Plan_Type == "HCPP - 1833 Cost"

replace HMO = 1 if Plan_Type == "Medicare-Medicaid Plan HMO/HMOPOS"
replace HMO = 1 if Plan_Type == "HMO/HMOPOS"

replace PPO = 1 if Plan_Type == "Regional PPO"
replace PPO = 1 if Plan_Type == "Local PPO"

replace PFS = 1 if Plan_Type == "PFFS"
replace PFS = 1 if Plan_Type == "RFB PFFS"

replace MSA = 1 if Plan_Type == "MSA"
replace PCE = 1 if Plan_Type == "National PACE"

/* A grouping variable for the six types of plans */

gen PT = 1

replace PT = 2 if Plan_Type == "Medicare-Medicaid Plan HMO/HMOPOS"
replace PT = 2 if Plan_Type == "HMO/HMOPOS"

replace PT = 3 if Plan_Type == "Regional PPO"
replace PT = 3 if Plan_Type == "Local PPO"

replace PT = 4 if Plan_Type == "PFFS"
replace PT = 4 if Plan_Type == "RFB PFFS"

replace PT = 5 if Plan_Type == "MSA"
replace PT = 6 if Plan_Type == "National PACE"

/* Label for the variables */

label define plan_label 1 "Cost Plan" 2 "HMO" 3 "PPO" 4 "PFFS" 5 "MSA" 6 "PACE"

label values PT plan_label


/*****************************************************************************/

/* Old stuff to add */

gen share_MA = enr_tot_calc_adj / elig

bysort State County year month: egen total_COS = total((PT == 1) * plan_enr)
bysort State County year month: egen total_HMO = total((PT == 2) * plan_enr)
bysort State County year month: egen total_PPO = total((PT == 3) * plan_enr)
bysort State County year month: egen total_PFS = total((PT == 4) * plan_enr)
bysort State County year month: egen total_MSA = total((PT == 5) * plan_enr)
bysort State County year month: egen total_PCE = total((PT == 6) * plan_enr)


gen swg_COS = plan_enr * (PT == 1) / total_COS
gen swg_HMO = plan_enr * (PT == 2) / total_HMO
gen swg_PPO = plan_enr * (PT == 3) / total_PPO
gen swg_PFS = plan_enr * (PT == 4) / total_PFS
gen swg_MSA = plan_enr * (PT == 5) / total_MSA
gen swg_PCE = plan_enr * (PT == 6) / total_PCE

bysort State County year month PT: egen N = count(PT)
bysort State County year month PT: gen lastg = _n == N

gen si = plan_enr / elig

gen so  = 1-share_MA

gen lnsi = ln(si)
gen lnso = ln(so)

gen     ln_swg = ln(swg_COS) if PT == 1
replace ln_swg = ln(swg_HMO) if PT == 2
replace ln_swg = ln(swg_PPO) if PT == 3
replace ln_swg = ln(swg_PFS) if PT == 4
replace ln_swg = ln(swg_MSA) if PT == 5
replace ln_swg = ln(swg_PCE) if PT == 6

gen y = lnsi - lnso

/*************************************************/
/*         BASE MODEL                            */

capture program drop mlfunbase
program mlfunbase
    version 14.1
    args lnf mu rho sigma
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho'*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + $ML_y4 * ($ML_y3 - 1)*ln(1 - `rho')
end

ml model lf mlfunbase (mu: y ln_swg N lastg = year PPO PFS COS PCE MSA) (rho:) (sigma:)
ml maximize

capture program drop mlfunbase2
program mlfunbase2
    version 14.1
    args lnf mu rho sigma
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho'^2*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + ($ML_y3 - 1)/$ML_y3 * ln(1 - `rho'^2)
end

ml model lf mlfunbase2 (mu: y ln_swg N = year PPO PFS COS PCE MSA) (rho:) (sigma:)
ml maximize



/* With year dummies */

tab year, gen(yd)
tab State, gen(sd)

ml model lf mlfunbase (mu: y ln_swg N lastg = PPO PFS COS PCE MSA yd* sd* lnelig) (rho: lnelig PPO PFS COS PCE MSA) (sigma:)
ml maximize


capture program drop mlfunbase
program mlfunbase
    version 14.1
    args lnf mu rho1 rho2 rho3 rho4 rho5 rho6 sigma
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho1'*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + $ML_y4 * ($ML_y3 - 1)*ln(1 - `rho1') if $ML_y5 == 1
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho2'*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + $ML_y4 * ($ML_y3 - 1)*ln(1 - `rho2') if $ML_y5 == 2
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho3'*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + $ML_y4 * ($ML_y3 - 1)*ln(1 - `rho3') if $ML_y5 == 3
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho4'*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + $ML_y4 * ($ML_y3 - 1)*ln(1 - `rho4') if $ML_y5 == 4
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho5'*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + $ML_y4 * ($ML_y3 - 1)*ln(1 - `rho5') if $ML_y5 == 5
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho6'*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + $ML_y4 * ($ML_y3 - 1)*ln(1 - `rho6') if $ML_y5 == 6

end

ml model lf mlfunbase (mu: y ln_swg N lastg PT = yd* sd* PPO PFS COS PCE MSA) (rho1:) (rho2:) (rho3:) (rho4:) (rho5:) (rho6:) (sigma:)
ml maximize

capture program drop mlfunbase
program mlfunbase
    version 14.1
    args lnf mu rho1 rho2 rho3 rho4 rho5 rho6 sigma
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho1'^2*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + $ML_y4 * ($ML_y3 - 1)*ln(1 - `rho1'^2) if $ML_y5 == 1
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho2'^2*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + $ML_y4 * ($ML_y3 - 1)*ln(1 - `rho2'^2) if $ML_y5 == 2
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho3'^2*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + $ML_y4 * ($ML_y3 - 1)*ln(1 - `rho3'^2) if $ML_y5 == 3
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho4'^2*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + $ML_y4 * ($ML_y3 - 1)*ln(1 - `rho4'^2) if $ML_y5 == 4
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho5'^2*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + $ML_y4 * ($ML_y3 - 1)*ln(1 - `rho5'^2) if $ML_y5 == 5
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho6'^2*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + $ML_y4 * ($ML_y3 - 1)*ln(1 - `rho6'^2) if $ML_y5 == 6

end

ml model lf mlfunbase (mu: y ln_swg N lastg PT = lnelig yd* sd* PPO PFS COS PCE MSA) (rho1:) (rho2:) (rho3:) (rho4:) (rho5:) (rho6:) (sigma: PPO PFS COS PCE MSA)
ml maximize



/* Before doing any serious stuff, let's get some graphs going */

gen aetna = strpos(Organization_Name, "AETNA") > 0
gen ahfmco = strpos(Organization_Name, "AHF MCO") > 0

gen blbs = strpos(Organization_Name, "BLUE CROSS") > 0
replace blbs = strpos(Organization_Name, "BLUECROSS") > 0 if blbs == 0

ml model lf mlfunbase (mu: y ln_swg N lastg PT = yd* sd* PPO PFS COS PCE MSA aetna ahfmco blbs large) ///
                (rho1:) (rho2:) (rho3:) (rho4:) (rho5:) (rho6:) (sigma:)
ml maximize


gen time = ym(year, month)

gen co_st = County + State

egen cobsno = group(co_st)

tab year, gen(yd)
tab month, gen(md)

local timevars 
forvalues i = 1/11 {
    gen ydmd`i' = yd`i'*md`i'
    local timevars "`timevars' yd`i' md`i' ydmd`i' "
}

gmm ( (lns - {b0} - {b1}*g2 - {b2}*g3 - {rho}*lnswg)/({sigma=1}^2) ) ///
    ( (lns - {b0} - {b1}*g2 - {b2}*g3 - {rho}*lnswg)^2 - {sigma}^2 ) ///
	( lnswg*(lns - {b0} - {b1}*g2 - {b2}*g3 - {rho}*lnswg)/{sigma}^2 - (N-1)/N*1/(1-{rho}) ), ///
	 instruments(1: g2 g3) instruments(2: ) instruments(3: ) winitial(unadjusted, independent) conv_maxiter(50)

capture program drop mlfunbase2
program mlfunbase2
    version 14.1
    args lnf mu rho1 rho2 rho3 rho4 rho5 rho6 sigma
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho1'*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + ($ML_y3 - 1) / $ML_y3*ln(1 - `rho1') if $ML_y4 == 1
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho2'*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + ($ML_y3 - 1) / $ML_y3*ln(1 - `rho2') if $ML_y4 == 2
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho3'*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + ($ML_y3 - 1) / $ML_y3*ln(1 - `rho3') if $ML_y4 == 3
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho4'*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + ($ML_y3 - 1) / $ML_y3*ln(1 - `rho4') if $ML_y4 == 4
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho5'*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + ($ML_y3 - 1) / $ML_y3*ln(1 - `rho5') if $ML_y4 == 5
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho6'*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + ($ML_y3 - 1) / $ML_y3*ln(1 - `rho6') if $ML_y4 == 6

end	 
	 
ml model lf mlfunbase2 (mu: y ln_swg N PT = yd* sd* PPO PFS COS PCE MSA aetna ahfmco blbs large) ///
                (rho1:) (rho2:) (rho3:) (rho4:) (rho5:) (rho6:) (sigma:)
ml maximize
	 
	 
