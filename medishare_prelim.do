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
replace date = mofd(date)
drop day
format date %tm

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

gen sgg_COS = total_COS/enr_tot_calc_adj
gen sgg_HMO = total_HMO/enr_tot_calc_adj
gen sgg_PPO = total_PPO/enr_tot_calc_adj
gen sgg_PFS = total_PFS/enr_tot_calc_adj
gen sgg_MSA = total_MSA/enr_tot_calc_adj
gen sgg_PCE = total_PCE/enr_tot_calc_adj

bysort State County year month PT: egen N = count(PT)
bysort State County year month PT: gen lastg = _n == N

/* Counts of the grouping variable */
/* Counts of total N for the purposes of grouping */

bysort State County year month: egen G = total(lastg)    /* total groups */
bysort State County year month: egen GN = count(lastg)   /* total plans  */

gen si = plan_enr / elig

gen so  = 1-share_MA

gen lnsi = ln(si)
gen lnso = ln(so)
gen lnsmed = ln(share_MA)

gen     ln_swg = ln(swg_COS) if PT == 1
replace ln_swg = ln(swg_HMO) if PT == 2
replace ln_swg = ln(swg_PPO) if PT == 3
replace ln_swg = ln(swg_PFS) if PT == 4
replace ln_swg = ln(swg_MSA) if PT == 5
replace ln_swg = ln(swg_PCE) if PT == 6

gen     ln_sgg = ln(sgg_COS) if PT == 1
replace ln_sgg = ln(sgg_HMO) if PT == 2
replace ln_sgg = ln(sgg_PPO) if PT == 3
replace ln_sgg = ln(sgg_PFS) if PT == 4
replace ln_sgg = ln(sgg_MSA) if PT == 5
replace ln_sgg = ln(sgg_PCE) if PT == 6


gen y = lnsi - lnso

/*************************************************/
/*         BASE MODEL                            */

capture program drop mlfunbase
program mlfunbase
    version 14.1
    args lnf mu rho sigma
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho'*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + $ML_y4 * ($ML_y3 - 1)*ln(1 - `rho')
end

ml model lf mlfunbase (mu: y ln_swg N lastg = PPO PFS COS PCE MSA) (rho:) (sigma: ) 
ml maximize

gmm ( (y - {b0} - {xb: PPO PFS COS PCE MSA} - {rho}*ln_swg)/({sigma=1}^2) )      ///
    ( (y - {b0} - {xb:}                     - {rho}*ln_swg )^2 - {sigma}^2 )     ///
	( ln_swg*(y - {b0} - {xb:} - {rho:}*ln_swg)/{sigma}^2 - (N-1)/N*1/(1-{rho}) ) , ///
	instruments(1: PPO PFS COS PCE MSA) instruments(2: ) instruments(3: ) winitial(unadjusted, independent) conv_maxiter(50)

/* With year dummies */

tab year, gen(yd)
tab State, gen(sd)
tab month, gen(md)

ml model lf mlfunbase (mu: y ln_swg N lastg = PPO PFS COS PCE MSA yd* sd* md*) (rho:  PPO PFS COS PCE MSA) (sigma: PPO PFS COS PCE MSA md*) 
ml maximize

capture program drop mlfunbase2
program mlfunbase2
    version 14.1
    args lnf mu rho1 rho2 rho3 rho4 rho5 rho6 sigma
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho1'*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + $ML_y4 * ($ML_y3 - 1)*ln(1 - `rho1') if $ML_y5 == 1
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho2'*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + $ML_y4 * ($ML_y3 - 1)*ln(1 - `rho2') if $ML_y5 == 2
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho3'*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + $ML_y4 * ($ML_y3 - 1)*ln(1 - `rho3') if $ML_y5 == 3
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho4'*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + $ML_y4 * ($ML_y3 - 1)*ln(1 - `rho4') if $ML_y5 == 4
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho5'*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + $ML_y4 * ($ML_y3 - 1)*ln(1 - `rho5') if $ML_y5 == 5
	quietly replace `lnf' = -($ML_y1 - `mu' - `rho6'*$ML_y2)^2/(2*exp(`sigma')) - 1/2*`sigma' + $ML_y4 * ($ML_y3 - 1)*ln(1 - `rho6') if $ML_y5 == 6

end

ml model lf mlfunbase2 (mu: y ln_swg N lastg PT = PPO PFS COS PCE MSA yd* sd*) (rho1:) (rho2:) (rho3:) (rho4:) (rho5:) (rho6:) (sigma:) if year == 2016 
ml maximize
 

/* Reassuringly, the two approaches give us the same answer */
/* What about if we estimate via GMM? */

/* Potential instruments for N */
/* Lagged group share    */


gmm ( (y - {b0} - {xb: PPO PFS COS PCE MSA} - {rho}*ln_swg)/({sigma=1}^2) )      ///
    ( (y - {b0} - {xb:}                     - {rho}*ln_swg )^2 - {sigma}^2 )     ///
	( ln_swg*(y - {b0} - {xb:} - {rho}*ln_swg)/{sigma}^2 - (N-1)/N*1/(1-{rho:}) ) ///
	if year == 2016 & month == 1, ///
	instruments(1: PPO PFS COS PCE MSA) instruments(2: ) instruments(3: lnelig ) winitial(unadjusted, independent) conv_maxiter(50)

/* These results replicate */

gmm ( (y - {b0} - {xb: PPO PFS COS PCE MSA} - {rho}*ln_swg)/({sigma=1}^2) )      ///
    ( (y - {b0} - {xb:}                     - {rho}*ln_swg )^2 - {sigma}^2 )     ///
	( ln_swg*(y - {b0} - {xb:} - {rho}*ln_swg)/{sigma}^2 - (N-1)/N*1/(1-{rho}) ) ///
	if year == 2016, ///
	instruments(PPO PFS COS PCE MSA lnelig) ///
	winitial(unadjusted, independent) conv_maxiter(50)
	
/* This takes a while to run */

gmm ( (y - {b0} - {xb: PPO PFS COS PCE MSA yd* sd* md*} - {rho}*ln_swg)/({sigma=1}^2) )      ///
    ( (y - {b0} - {xb:}                     - {rho}*ln_swg )^2 - {sigma}^2 )     ///
	( ln_swg*(y - {b0} - {xb:} - {rho}*ln_swg)/{sigma}^2 - (N-1)/N*1/(1-{rho}) ), ///
	instruments(PPO PFS COS PCE MSA lnelig yd* sd* md*) ///
	winitial(unadjusted, independent) conv_maxiter(50)

mata:
    void gmm_of(M, todo, b, obj, g, H)
    {
        real matrix y, Xb, Z, W, mX, m2, m3, ms,
		    ln_swg, N
		real scalar rho, sig
		
		y      = moptimize_util_depvar(M, 1)
		ln_swg = moptimize_util_depvar(M, 2)
		N      = moptimize_util_depvar(M, 3)
		
		Xb  = moptimize_util_xb(M, b, 1)
		rho = moptimize_util_xb(M, b, 2)
		rho = exp(rho)/(1+exp(rho))
		sig = exp(moptimize_util_xb(M, b, 3))
        
		Z   = moptimize_util_userinfo(M, 1)
		W   = moptimize_util_userinfo(M, 2)
		
	    mX = Z:*(y - Xb - rho*ln_swg)/sig^2
	    m2 = (y - Xb - rho*ln_swg):^2 :- sig^2
	    m3 = ln_swg:*(y - Xb - rho*ln_swg)/sig^2 :- (N:-1):/N*1/(1-rho)
	
        ms = mX, m2, m3
	
	ms = colsum(ms) / rows(ms) 
	
	obj = -ms*W*ms'		
    }
end

mata:
void gmm_of2(M, todo, b, obj, g, H)
{

        real matrix y, Xb, Z, W, mX, m3, ms,
		    ln_swg, N
		real scalar rho, sig2
		
		y      = moptimize_util_depvar(M, 1)
		ln_swg = moptimize_util_depvar(M, 2)
		N      = moptimize_util_depvar(M, 3)
		
		Xb  = moptimize_util_xb(M, b, 1)
		rho = moptimize_util_xb(M, b, 2)
        rho = exp(rho)/(1+exp(rho))
		Z   = moptimize_util_userinfo(M, 1)
		W   = moptimize_util_userinfo(M, 2)
		
		sig2 = mean((y - Xb - rho*ln_swg):^2)
		
	    mX = Z:*(y - Xb - rho*ln_swg)
	    m3 = ln_swg:*(y - Xb - rho*ln_swg)/sig2 :- (N:-1):/N*1/(1-rho)
	
        ms = mX, m3
	
	ms = colsum(ms)  
	
	obj = -ms*W*ms'		
}
end
local vars PPO PFS COS PCE MSA
local ct:  word count `vars'

mata:
    Model = moptimize_init()
    moptimize_init_trace_dots(Model,"on")
    moptimize_init_trace_params(Model,"on")
    moptimize_init_evaluator(Model,&gmm_of())
    moptimize_init_evaluatortype(Model,"d0")
    moptimize_init_which(Model,"max")
	
    moptimize_init_eq_indepvars(Model, 1, "`vars'")	
    moptimize_init_eq_indepvars(Model, 2, "")	
    moptimize_init_eq_indepvars(Model, 3, "")
    moptimize_init_depvar(Model, 1, "y")
	moptimize_init_depvar(Model, 2, "ln_swg")
	moptimize_init_depvar(Model, 3, "N")

	st_view(Z=., ., "PPO PFS COS PCE MSA")	
    Z   = Z, J(rows(Z), 1, 1)	
	moptimize_init_userinfo(Model, 1, Z)

	ct = strtoreal(st_local("ct"))
	W = I(ct + 3)
	W = invsym( Z'Z )
	W = W \ J(2, cols(W), 0)
	W = W , J(rows(W), 2, 0)
	W[cols(Z) + 1, cols(Z) + 1] = 1
	W[cols(Z) + 2, cols(Z) + 2] = 1
	
	moptimize_init_userinfo(Model, 2, W)
	
    moptimize_evaluate(Model)
	boo = moptimize(Model)
end
mata	
	alginfo = "d0", "moptimize", "mwg"
	b_mwg = amcmc(alginfo, &gmm_of(), J(1,8,0), I(8), 500, 100, 2/3, .4, 
	        arate=., vals=., lambda=., ., Model, "noisy")
	
end

local vars PPO PFS COS PCE MSA
local ct:  word count `vars'

mata:
    Model = moptimize_init()
    moptimize_init_trace_dots(Model,"on")
    moptimize_init_trace_params(Model,"on")
    moptimize_init_evaluator(Model,&gmm_of2())
    moptimize_init_evaluatortype(Model,"d0")
    moptimize_init_which(Model,"max")
	
    moptimize_init_eq_indepvars(Model, 1, "`vars'")	
    moptimize_init_eq_indepvars(Model, 2, "")	
    moptimize_init_depvar(Model, 1, "y")
	moptimize_init_depvar(Model, 2, "ln_swg")
	moptimize_init_depvar(Model, 3, "N")

	st_view(Z=., ., "PPO PFS COS PCE MSA")	
    Z   = Z, J(rows(Z), 1, 1)
	moptimize_init_userinfo(Model, 1, Z)

	ct = strtoreal(st_local("ct"))
	W = I(ct + 2)
	
	W = invsym( (Z, J(rows(Z), 1, 1/rows(Z)))'(Z, J(rows(Z), 1, 1/rows(Z))) )
	moptimize_init_userinfo(Model, 2, W)
	
    moptimize_evaluate(Model)
	boo = moptimize(Model)
end
mata	
	alginfo = "d0", "moptimize", "mwg"
	b_mwg = amcmc(alginfo, &gmm_of2(), J(1,7,0), I(7)/10, 500, 100, 2/3, .4, 
	        arate=., vals=., lambda=., ., Model, "noisy")
	
end

/* Mocking up derivatives */


mata:
    st_view(X=.,.,"PPO PFS COS PCE MSA")
	X = X, J(rows(X),1,1)
	st_view(y=.,.,"y")
	Z = X
	st_view(ln_swg=.,.,"ln_swg")
	st_view(N=.,.,"N")
	
	beta = J(1, cols(X), .1)
	rho = .1
	sig =  1
	
	W = I(cols(X) + 2)
	
	/* First moment */
	
	Xb = X*beta'
	
	
	mX = Z:*(y - Xb - rho*ln_swg)/sig^2
	
	/* Upper right block of Hessian (regular coefficients) */
	
	H11 = -X'X/sig^2
    H21 = -2*X'(y- Xb-rho*ln_swg)
    H31 = -X'(y - Xb - rho*ln_swg)/sig^2
 	
	H1 = H11, H21, H31
	
	H2 = (H21, H31)'
	
	Hsigsig = -2*sig
	Hsigrho = -2*ln_swg'(y - Xb - rho*ln_swg)
	Hrhorho = -ln_swg:*ln_swg/sig^2 :+ (N:-1):/N*(1/(1-rho)^2)
	
	
	m2 = (y - Xb - rho*ln_swg):^2 :- sig^2
	    m3 = ln_swg:*(y - Xb - rho*ln_swg)/sig^2 :- (N:-1):/N*1/(1-rho)
	
        ms = mX, m2, m3
	
end



/* Here, we are going to try and work things out by hand in sequential fashion.   */
/* We also would want to think about instruments and where they need be applied   */
/* Typically, wherever we are worried about variables correlated with instruments */

mata:
    st_view(X=.,.,"PPO PFS COS PCE MSA")
	X = X, J(rows(X),1,1)
	st_view(y=.,.,"y")
	st_view(ln_swg=.,.,"ln_swg")
	st_view(N=.,.,"N")
	
	rho   = 0.25    /* initial guess at rho */
	
	yhat = y - rho*ln_swg
	
	beta = invsym(X'X)*(X'yhat)   /* beta given rho */
	
	sig2  = sum((y - X*beta - rho* ln_swg):^2)/rows(y)   /* variance parameter given rho and beta */
	
	K1 = sum( ln_swg:*(y - X*beta) / (sig2)) / rows(y)
	K2 = sum( ln_swg:^2/ (sig2) ) / rows(y)
	K3 = sum( (N:-1):/N ) / rows(y)
	
	rho1 = ((K2 + K1) + sqrt((K2+K1)^2 - 4*K2*(K1 - K3))) / (2*K2)
	rho  = ((K2 + K1) - sqrt((K2+K1)^2 - 4*K2*(K1 - K3))) / (2*K2)   /* Typically, the correct value */
	
	/* Next iteration */
    /* It fucking works! */
	
	rho = 0.25
	
	for (i=1;i<=100;i++) {
    	
	    yhat = y - rho*ln_swg
	    beta = invsym(X'X)*X'yhat
	    sig2 = sum((y - X*beta - rho * ln_swg):^2) / rows(y)
	
	    K1 = sum( ln_swg:*(y - X*beta) / (sig2)) / rows(y)
	    K2 = sum( ln_swg:^2/ (sig2) ) / rows(y)
	    K3 = sum( (N:-1):/N ) / rows(y)	
	
	    rho  = ((K2 + K1) - sqrt((K2+K1)^2 - 4*K2*(K1 - K3))) / (2*K2) 
	    rho, sig2
	}
	
	
	
	

	
	
	
	
	






