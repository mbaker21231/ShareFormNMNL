capture use "C:\Users\matthew\Downloads\medicare.dta", clear
capture use "C:\Users\\mjbaker\\Downloads\\medicare.dta", clear

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
		
		
/* Notes: it looks like contract ID and Organization_Name contain the information
that we want about type */

drop if Plan_Type == "nan"

gen ptype   = 0
replace ptype = 1 if Plan_Type == "1876 Cost"
replace ptype = 1 if Plan_Type == "HCPP - 1833 Cost" 
replace ptype = 1 if Plan_Type == "HMO/HMOPOS"
replace ptype = 1 if Plan_Type == "ESRD II - HMOPOS"
replace ptype = 1 if Plan_Type == "Local PPO"
replace ptype = 1 if Plan_Type == "PSO (State License)"
replace ptype = 1 if Plan_Type == "Medicare-Medicaid Plan HMO/HMOPOS"
replace ptype = 1 if Plan_Type == "Regional PPO"
replace ptype = 2 if Plan_Type == "MSA"
replace ptype = 3 if Plan_Type == "PFFS"
replace ptype = 3 if Plan_Type == "RFB PFFS"
replace ptype = 3 if Plan_Type == "ESRD I - PFFS"
replace ptype = 3 if Plan_Type == "Employer/Union Only Direct Contract PFFS"
replace ptype = 4 if Plan_Type == "National PACE"
replace ptype = 4 if Plan_Type == "Continuing Care Retirement Community"


/* What sorts of shares are we looking at here? */

gen share_MA = enr_tot_calc / elig

bysort State County year month: egen total_CCP = total((ptype == 1) * plan_enr)
bysort State County year month: egen total_MSA = total((ptype == 2) * plan_enr)
bysort State County year month: egen total_PFS = total((ptype == 3) * plan_enr)

gen swg_CCP = plan_enr * (ptype == 1) / total_CCP
gen swg_MSA = plan_enr * (ptype == 2) / total_MSA
gen swg_PFS = plan_enr * (ptype == 3) / total_PFS

gen si = plan_enr / elig

gen so  = 1-share_MA

gen aetna = strpos(Organization_Name, "AETNA") > 0
gen ahfmco = strpos(Organization_Name, "AHF MCO") > 0

gen blbs = strpos(Organization_Name, "BLUE CROSS") > 0
replace blbs = strpos(Organization_Name, "BLUECROSS") > 0 if blbs == 0

gen lnsi = ln(si)
gen lnso = ln(so)

gen ln_swg = ln(si)

replace ln_swg = ln(swg_CCP) if ptype == 1
replace ln_swg = ln(swg_MSA) if ptype == 2
replace ln_swg = ln(swg_PFS) if ptype == 3

bysort State County year month ptype: egen N=count(ptype)
bysort State County year month ptype: gen lastg = _n == N

gen y = lnsi - lnso

gen CCP = ptype == 1
gen MSA = ptype == 2
gen PFS = ptype == 3

gen size = ln(elig)

quietly tab State, gen(sd)

/* Before doing any serious stuff, let's get some graphs going */

gen time = ym(year, month)

gen co_st = County + State

egen obsno = group(co_st)

preserve 

bysort obsno time: gen type1 = ptype==1
bysort obsno time: gen type2 = ptype==2
bysort obsno time: gen type3 = ptype==3
bysort obsno time: gen type4 = ptype==4

collapse (count) si (mean) type1 type2 type3 type4, by(obsno time)




capture program drop mlfunbase
program mlfunbase
    version 14.1
    args lnf mu rho1 sigma
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho1'*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + $ML_y4 * ($ML_y3 - 1)*ln(1 - `rho1')
end

/* With state-specific effects */

ml model lf mlfunbase (mu: y ln_swg N lastg = year CCP MSA PFS sd*) (rho1:) (sigma:)
ml maximize

/* With state-specific effects and plan-specific time trends */

gen yearCCP = year*CCP
gen yearMSA = year*MSA
gen yearPFS = year*PFS

ml model lf mlfunbase (mu: y ln_swg N lastg = year CCP MSA PFS yearCCP yearMSA yearPFS sd*) (rho1:) (sigma:)
ml maximize

/* With year dummies instead of a linear trend */

tab year, gen(yd)

ml model lf mlfunbase (mu: y ln_swg N lastg = yd* CCP MSA PFS yearCCP yearMSA yearPFS sd*) (rho1:) (sigma:)
ml maximize

capture program drop mlfun2
program mlfun2
    version 14.1
    args lnf mu rho1 rho2 rho3 rho4 sigma
	tempvar rho
	
	quietly gen double `rho' = `rho1'*( $ML_y5 == 1) + `rho2'*($ML_y5 == 2) + `rho3'*($ML_y5 == 3) + `rho4'*($ML_y5 == 4)
	
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho'*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + ///
	    ($ML_y5 == 1)*$ML_y4 * ($ML_y3 - 1)*ln(1 - `rho1') + ///
	    ($ML_y5 == 2)*$ML_y4 * ($ML_y3 - 1)*ln(1 - `rho2') + ///
	    ($ML_y5 == 3)*$ML_y4 * ($ML_y3 - 1)*ln(1 - `rho3') + ///
	    ($ML_y5 == 4)*$ML_y4 * ($ML_y3 - 1)*ln(1 - `rho4') 
end

ml model lf mlfun2 (mu: y ln_swg N lastg ptype = yd* CCP MSA PFS yearCCP yearMSA yearPFS sd*) ///
    (rho1:) (rho2:) (rho3:) (rho4:) (sigma:)
ml maximize

ml model lf mlfun2 (mu: y ln_swg N lastg ptype = yd* CCP MSA PFS yearCCP yearMSA yearPFS sd*) ///
    (rho1:) (rho2:) (rho3:) (rho4:) (sigma: CCP MSA PFS)
ml maximize


/*




ivregress gmm y CCP MSA PFS (ln_swg = size), first

bysort State County: gen lastc = _n == _N
bysort State: egen Ncounties = total(lastc)

ivregress gmm y CCP MSA PFS (ln_swg = size Ncounties), first

*/
