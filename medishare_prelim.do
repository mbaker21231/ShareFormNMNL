/* This file uses the one developed in CreateData.ipynb */

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

bysort State County Plan_Type Contract_ID year month: egen enrollph = total(plan_enr)
egen numid = group(State County Plan_Type Contract_ID)
bysort numid date: egen gC = count(numid)
bysort numid date: gen gn = _n

replace plan_enr = enrollph if gC == 2

drop if gn == 2
drop gn gC enrollph

duplicates report numid date

xtset numid date

/* Now, we can start plotting the number of plans for certain counties to get
   a feel for the data */
   
/* Los Angeles */

/* How many people are enrolled of plans of a given type in a place? */

bysort State County year month Plan_Type: egen plan_type_enr = total(plan_enr)
gen plan_type_sha = plan_type_enr/enr_tot_calc

bysort State County year month Plan_Type: egen plan_type_cou = count(Plan_Type)
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










