use "C:\Users\mjbaker\Downloads\tempstuff\medishare.dta", clear

drop if Enrolled_x == "nan"

destring Enrolled_x share, replace

/* Notes: it looks like contract ID and Organization_Name contain the information
that we want about type */

gen ptype   = 0
replace ptype = 1 if Plan_Type == "1876 Cost"
replace ptype = 1 if Plan_Type == "HCPP - 1833 Cost" 
replace ptype = 1 if Plan_Type == "HMO/HMOPOS"
replace ptype = 1 if Plan_Type == "Local PPO"
replace ptype = 1 if Plan_Type == "Medicare-Medicaid Plan HMO/HMOPOS"
replace ptype = 1 if Plan_Type == "Regional PPO"
replace ptype = 2 if Plan_Type == "MSA"
replace ptype = 3 if Plan_Type == "PFFS"
replace ptype = 4 if Plan_Type == "National PACE"

/* What sorts of shares are we looking at here? */

bysort State County: egen MA = total(Enrolled_y)
bysort State County: gen last = _n == _N

gen share_MA = MA / Eligibles

bysort State County: egen total_CCP = total((ptype == 1) * Enrolled_y)
bysort State County: egen total_MSA = total((ptype == 2) * Enrolled_y)
bysort State County: egen total_PFS = total((ptype == 3) * Enrolled_y)

gen swg_CCP = Enrolled_y * (ptype == 1) / total_CCP
gen swg_MSA = Enrolled_y * (ptype == 2) / total_MSA
gen swg_PFS = Enrolled_y * (ptype == 3) / total_PFS

gen si = Enrolled_y / Eligibles

gen aetna = strpos(Organization_Name, "AETNA") > 0
gen ahfmco = strpos(Organization_Name, "AHF MCO") > 0

gen blbs = strpos(Organization_Name, "BLUE CROSS") > 0
replace blbs = strpos(Organization_Name, "BLUECROSS") > 0 if blbs == 0

gen lnsi = ln(si)

gen ln_swg = ln(si)

replace ln_swg = ln(swg_CCP) if ptype == 1
replace ln_swg = ln(swg_MSA) if ptype == 2
replace ln_swg = ln(swg_PFS) if ptype == 3

bysort State County ptype: egen N=count(ptype)



