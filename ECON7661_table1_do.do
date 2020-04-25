********************************************************************************
********************************************************************************
** The following do file generate the results for Table 1. This is the third  **
** do file in the sequence. First, I create the variables necessary for       **
** specifications (1) and (2), then I estimate those regressions. Two items   **
** are noteworthy here, and will be expanded upon in the methods paper. One,  **
** I use reghedfe to estimate the regressions. Two, to weight the regressions,**
** I use the option [aweights].	We use the -reghdfe- package here because it  **
** gives us a few advantages over onboard regression commands. For starters,  **
** the gradient descent algorithm that solves the least-squares optimization  **
** problem converges much faster than other regression algorithms. More 	  **
** importantly, -reghdfe- allows us to specify multi-level fixed effects in a **
** superior fashion to, say, xtreg. Multi-level fixed effects can cause 	  **
** dimensionality issues that -reghdfe- is better suited to handle. I'm not   **
** certain of exactly how the computation works, but I think it's related to  **
** the algorithm that -reghdfe- uses to solve the normal equations and compute**
** (X'X)^1. A full description is beyond the scope of the project, but I      **
** believe its use warrants explanation here. It's possible that different    **
** states and counties will have time-invariant, hetergeneous characteristics **
** in the sample period that may be related to insurance rates, medicaid      **
** expansion, accident responde times that we want to difference out using    **
** fixed effects. Therefore, we would like the flexibility to model different **
** slopes for every county and state, and -reghdfe- affords us that 		  **
** flexbility by specifying fixed effects with i.covariate#c.covariate. 	  ** 
** Furthermore, -reghdfe- automamtically drops groups with only one 		  **
** observation (singletons). This has the effect of changing the			  **
** variance-covariance matrix in the error term and corrects standard errors  **
** by making an adjustment to the asymptotic variance of the coefficient      **
** estimators when clustering in the presence of multi-level fixed effects.   **
** All of my research here can be corroborated by Sergio Correia's website and**
** links found therein. I confess I don't fully understand the package, but I **
** think I understand it well enough to apply to the specifications in this   **
** paper. A fun exercise, someday, would be to work through algorithm used by **
** -reghdfe- to peek under the hood.			     						  ** 
********************************************************************************
********************************************************************************

********************************************************************************
********************************************************************************
** Below is a list of expansion states and their dates of expansion, as well  **
** as a list of non-expansion states. These are taken from the Kaiser Family  **
** Foundation at the website: https://www.kff.org/medicaid/issue-brief/       **
** status-of-state-medicaid-expansion-decisions-interactive-map/.			  **
** Footnote 14 requires clarification regarding states that expanded after    **
** 1 Jan 14. The footnote states that seven states (NH, PA, IN, MT, LA, MI,   **
** AK) expanded after 1 Jan 2014. While this is true, MT and LA did not,      **
** according to the FKK website cited herein, expand coverage until 1 Jan 16, **
** which is outside the sample period. According to                           **
** https://www.healthinsurance.org/louisiana-medicaid/#expansiondate, it seems**
** that enrollment in expanded Medicaid coverage did not take place until 2016**
** in LA despite legislative action to provision funds for Medicaid expansion **
** during the previous year. By contrast, according to                        **
** https://www.healthinsurance.org/montana-medicaid/, enrollment in MT began  **
** on 2 Nov 15, after the Center for Medicare & Medicaid Services approved the**
** ACA expansion waiver, and coverage began in 2016. Therefore, I consider LA **
** to be outside the sample period, and I consider MT to be inside the sample **
** period with an expansion date of 2 Nov 15. Furthermore, as a robustness    **
** check, I will estimate (1) and (2) both with and without LA. My replicated **
** tables, however, will only reflect the sample WITHOUT LA.       			  **
********************************************************************************
********************************************************************************
/*
STATE 	FIPS 	Exp 	ExpDate
AL		1		No
AK 		2 		Yes		01sep15
AZ		4 		Yes		01jan14
AR		5 		Yes		01jan14
CA		6 		Yes		01jan14
CO		8 		Yes		01jan14
CT		9		Yes		01jan14
DE		10		Yes		01jan14
DC		11		Yes		01jan14
FL		12		No
GA		13		No
HI		15		Yes		01jan14
ID		16		No
IL		17		Yes		01jan14
IN		18		Yes		01feb15
IA		19		Yes		01jan14
KS		20		No
KY		21		Yes		01jan14
LA		22		No
ME		23		No
MD		24		Yes		01jan14
MA		25		Yes		01jan14
MI		26		Yes		01apr14
MN		27		Yes		01jan14
MS		28		No
MO		29		No
MT		30		Yes		02nov15
NE		31		No
NV		32		Yes		01jan14
NH		33		Yes		15aug14
NJ		34		Yes 	01jan14
NM		35		Yes		01jan14
NY		36		Yes		01jan14
NC		37		No
ND		38		Yes		01jan14
OH		39		Yes		01jan14
OK		40		No
OR		41		Yes		01jan14
PA		42		Yes		01jan15
RI		44		Yes		01jan14
SC		45		No
SD		46		No
TN		47		No
TX		48		No
UT		49		No
VT		50		Yes		01jan14
VA		51		No
WA		53		Yes		01jan14
WV		54		Yes		01jan14
WI		55		No
WY		56		No
*/

clear
set more off
set trace off //useful for debugging

// need reghdfe and dependencies
// ssc install ftools
// ssc install reghdfe

version //display stata version
reghdfe, version //display installed version of reghdfe

// set working directory
cd "C:\Users\zacharyj\OneDrive - The University of Colorado Denver\HealthEconStataProject\DataFiles"

capture log close
log using table1log.log, replace

use merged_data.dta

************************************************
** create post variable and interaction terms **
** for DD estimators in (1) and (2)           **
************************************************      

// create Post variable
// 2014 expansion states
local exp2014_states 4 5 6 8 9 10 11 15 17 19 21 24 25 26 27 32 33 34 35 ///
					 36 38 39 41 44 50 53 54

// 2015 expansion states
local exp2015_states 2 18 22 30 42

// non-expansion states
local nonexp_states 1 12 13 16 20 23 28 29 31 37 40 45 46 47 48 49 ///
					51 55 56
					
generate int post = 0 if year < 2014 // all states are 0 prior to 2014
label variable post "Indicator for before or after 2014"

generate int medexp = .
label variable medexp "Indicator for whether a state expanded Medicaid"

generate int post_medexp = .
label variable post_medexp "Ineraction of Post with Medicaid expansion"

// the following loops iterate over the previous assigned lists and assign
// 0/1 values for Post according to expansion year and for Medicaid expansion
// according to whether the state expanded Medicaid
foreach i in `nonexp_states' {
	replace post = 1 if statefips == `i' & year >= 2014
	replace medexp = 0 if statefips == `i'
	}

foreach i in `exp2014_states' {
	replace post = 1 if statefips == `i' & year >= 2014
	replace medexp = 1 if statefips == `i'
	}

// for 2015 expansion states, post = 0 unless interacted with medicaid expansion
foreach i in `exp2015_states' {
	replace post = 0 if statefips == `i' & year == 2014
	replace post = 1 if statefips == `i' & year == 2015
	replace medexp = 1 if statefips == `i'
	}
	
replace post_medexp = post*medexp

count if post == . // 0 values still missing
tabulate year post, missing // check that it worked

//hist post if year == 2014 & medexp == 1 & statefips != 11
//hist post if year == 2015 & medexp == 1 & statefips != 11
// i'm not sure what to make of these graphs

count if post_medexp == . // 0 values missing
tabulate year post_medexp, missing

//hist post_medexp if year == 2014 & pop2010 >= 10000 & statefips != 11
//hist post_medexp if year == 2015 & pop2010 >= 10000 & statefips != 11
 
// create post*uninsured2013 variable for specification (2)
generate _unins2013 = unins_rate if year == 2013
by statefips countyfips, sort: egen unins2013 = mean(_unins2013)
label variable unins2013 "Count-level uninsured rate in 2013"
drop _unins2013

//hist unins2013

generate post_unins2013 = post*unins2013
label variable post_unins2013 "Interaction of Post with Uninsured2013"

//hist post_unins2013

generate post_unins2013_medexp = post*unins2013*medexp
label variable post_unins2013_medexp "Interaction of Post with Uninsured2013 and Medicaid Expansion"

//hist post_unins2013_medexp

save merged_ins_rates.dta, replace // need these variables in Tables 4, 5

// investigate parallel trends for insurance coverage
by year medexp, sort: egen mean_ins_rate = mean(ins_rate) if pop2010 >= 10000
graph twoway line mean_ins_rate year if medexp == 1 || ///
			 line mean_ins_rate year if medexp == 0, ///
			 xline(2014) legend(label(1 Expansion) label(2 Non-expansion)) ///
			 title("State-level Insured Rates by Year")

// it's not clear to me that there's a differential effect in and after 2014

// investigate parallel trends for uninsured (a verification exercise)
by year medexp, sort: egen mean_unins_rate = mean(unins_rate) if pop2010 >= 10000
graph twoway line mean_unins_rate year if medexp == 1 || ///
			 line mean_unins_rate year if medexp == 0, ///
			 xline(2014) legend(label(1 Expansion) label(2 Non-expansion)) ///
			 title("State-level Uninsured Rates by Year")

// exactly what one would expect: uninsured rates go down

drop mean_ins_rate mean_unins_rate

**************************************************************
** estimate specifications (1) and (2) and recreate Table 1 **
**************************************************************

// response variable and covariates of interest
local covariates ins_rate unins_rate post medexp post_medexp unins2013 	///	
	  post_unins2013 post_unins2013_medexp

// economic indicators
local econ_ind povertyrate unemprate medianhouseholdincome

// collapse data to county level
collapse `covariates' `econ_ind' pop2010 , by(year statefips countyfips)

// get pre-treatment mean
tabstat ins_rate if year <= 2013 & statefips != 11 & pop2010 >= 10000, stat(mean)

// it's not clear to me that DC (statefips 11) is included in the sample, so i'm
// excluding it here (see footnote 14; results are, in fact, robust to inclusion
// or exclusion of DC)

**************************************
** regression for specification (1) **
**************************************
// regression weighting (footnote 15) is accomplished with [aweight = pop2010^2]
// see https://www.stata.com/support/faqs/statistics/
// analytical-weights-with-linear-regression/
// since aweight by default weights by the square root, we have to use the
// squaring function here

generate w_j = pop2010^2 // create weights
// 10 observations have missing population data, so they do not have weights
// see note in mergedata.do about missing population data for AK, SD, and VA
// due to census area changes within the sample period

#delimit ;

reghdfe ins_rate post post_medexp `econ_ind' [aweight = w_j] 
		if statefips != 11 & pop2010 >= 10000, 
		absorb(c.countyfips#i.medexp i.pop2010) vce(cluster statefips);

#delimit cr

// absorbing countyfips#medexp and pop2010 here because i want to allow for 
// (unobserved) time-invariant heterogeneous characteristics between counties, 
// and this is interacted with medicaid expansion because counties in states
// that expanded medicaid may be different from counties in states that did not
// in a way that is associated with insurance rates (i.e. - a source of ommitted
// variable bias)

// i use pop2012 in absorb() here to force STATA to soak up extra time-invariant
// characteristics that may be associated with counties

lincom post + post_medexp

**************************************
** regression for specification (2) **
**************************************
// the fixed effects here reflect abosrbing unobserved, time-invariant, 
// heterogeneous characteristics among counties and states differentiated by
// medicaid expansion and non-expansion that may be sources of endogeneity

#delimit ;

reghdfe ins_rate post_unins2013 post_unins2013_medexp `econ_ind' 
		[aweight = w_j] if statefips != 11 & pop2010 >= 10000, 
		absorb(c.countyfips#i.medexp c.statefips#c.year#i.medexp i.pop2010) 
		vce(cluster statefips);

#delimit cr

lincom post_unins2013
local beta_1 = r(estimate)
display `beta_1'*unins2013

lincom post_unins2013_medexp
local beta_2 = r(estimate)
display `beta_2'*unins2013

lincom post_unins2013 + post_unins2013_medexp
local post_coef2 = r(estimate)
display `post_coef2'*unins2013

clear

**********************************************************
** Results are robust to inclusion or exclustion of LA. **
**********************************************************

log close