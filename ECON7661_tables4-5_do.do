********************************************************************************
********************************************************************************
** The following do file produces the results for Tables 4 and 5. This is the **
** fifth and final do file in the sequence. It begins with updating the Post  **
** variable and the variables interacted with it. Then it proceeds to estimate**
** regressions (3) and (4). The condition_controls.dta file created in        **
** table3.do supplies the data.                                               **
********************************************************************************
********************************************************************************

clear
set more off
set trace off // useful for debugging

//ssc install ftools
//ssc install reghdfe

version //display stata version
reghdfe, version //display installed version of reghdfe

cd "C:\Users\zacharyj\OneDrive - The University of Colorado Denver\HealthEconStataProject\DataFiles"

capture log close
log using tables4-5log.log, replace

use ins_rates_condition_controls.dta

***********************************************************
** change the post variable to reflect date of expansion **
***********************************************************
// create expansion date variable
local exp1jan2014_states 4 5 6 8 9 10 11 15 17 19 21 24 25 27 32 34 35 36 ///
						 38 39 41 44 50 53 54

generate exp_date = .
label variable exp_date "Date of Medicaid Expansion"

foreach i in `exp1jan2014_states' {
	replace exp_date = date("01jan2014", "DMY") if statefips == `i'
	}
	
replace exp_date = date("01sep2015", "DMY") if statefips == 2 // AK
replace exp_date = date("01feb2015", "DMY") if statefips == 18 // IN
replace exp_date = date("01apr2014", "DMY") if statefips == 26 // MI
replace exp_date = date("02nov2015", "DMY") if statefips == 30 // MT
replace exp_date = date("15aug2014", "DMY") if statefips == 33 // NH
replace exp_date = date("01jan2015", "DMY") if statefips == 42 // PA
format exp_date %td

// non-expansion states remain missing for exp_date

// create accident date variables
generate acc_day_str = string(day, "%02.0f")
generate acc_mon_str = string(month, "%02.0f")
egen _acc_date = concat(acc_mon_str acc_day_str year), punct("/")

generate acc_date = date(_acc_date, "MDY")
label variable acc_date "Date of Accident"

format acc_date %td
drop _acc_date

// update post for expansion states
replace post = 0 if year >= 2014 & exp_date != . & acc_date <= exp_date
replace post = 1 if year >= 2014 & exp_date != . & acc_date > exp_date

// update post for non-expansion states
replace post = 0 if year <= 2013 & exp_date == .
replace post = 1 if year > 2013 & exp_date == .

// account for missing accident date
replace post = . if year > 2013 & acc_date == . 

tabulate year post, missing

// update interaction terms
replace post_medexp = post*medexp
replace post_unins2013 = post*unins2013
replace post_unins2013_medexp = post*unins2013*medexp

// response time cutoffs
local resp_cutoffs slow4 slow8 slow13 slow20

// economic controls
local econ_ind povertyrate unemprate medianhouseholdincome

// weather controls (not including sun to avoid dummy variable trap)
local weather_ind rain sleet snow fog cloudy blowing_snow freezing_rain wind ///
				  blowing_dirt other_inclement
				  
// night and weekend indicators
local night_weekend_ind night_acc weekend_acc

// create quarter variable
generate quarter = .
label variable quarter "Quarter of Accident"

replace quarter = 1 if inlist(month, 1, 2, 3)
replace quarter = 2 if inlist(month, 4, 5, 6)
replace quarter = 3 if inlist(month, 7, 8, 9)
replace quarter = 4 if inlist(month, 10, 11, 12)

tabstat resp_time if resp_time > 0 & year <= 2013 & pop2010 >= 10000 ///
		& statefips != 11, stat(mean)

*********************************************************
** estimating specification (3) and recreating table 4 **
*********************************************************
// with respect to absorb here, i'm unsure how to specify the fixed effects
// i have tried multiple different combinations, and i can't seem to find a
// combination that is consistent with the purpose of fixed effects and produces
// the magnitudes reported in the paper

// i am hesitant to alter the regression any further because i don't want to
// deviate too far afield from the hypothesis and the identification strategy

// the hypothesis reflected in my specification is that i am differencing out
// unobserved, time-invariant heterogeneity among counties and states 
// with respect to medicaid expansion. within counties, quarters are allowed
// heterogeneous slopes, and within states, years are allowed heterogeneous
// slopes, while quarters are not. the only explanation i have for this is that
// maybe there's something about the county-level interaction where accidents
// are different between different quarters, while that ceases to be important
// at the state level

// note that i maintain these fixed effects for estimating the remaining
// regressions

#delimit ;

// column (1)
reghdfe resp_time post_unins2013 post_unins2013_medexp
		if resp_time > 0 & resp_time < 180 & pop2010 >= 10000 & statefips != 11,
		absorb(c.countyfips#c.quarter#i.medexp c.pop2010#i.medexp
		c.statefips#c.year#i.quarter)
		vce(cluster statefips);

lincom post_unins2013;
local beta_1 = r(estimate);
display `beta_1'*unins2013;

lincom post_unins2013_medexp;
local beta_2 = r(estimate);
display `beta_2'*unins2013;

lincom post_unins2013 + post_unins2013_medexp;
local post_coef1 = r(estimate);
display `post_coef1'*unins2013;

// column (2)
reghdfe resp_time post_unins2013 post_unins2013_medexp `econ_ind' `weather_ind'
		`night_weekend_ind' if resp_time > 0 & resp_time < 180 
		& pop2010 >= 10000 & statefips != 11,
		absorb(c.countyfips#c.quarter#i.medexp c.pop2010#i.medexp
		c.statefips#c.year#i.quarter)
		vce(cluster statefips);

lincom post_unins2013;
local beta_1 = r(estimate);
display `beta_1'*unins2013;

lincom post_unins2013_medexp;
local beta_2 = r(estimate);
display `beta_2'*unins2013;

lincom post_unins2013 + post_unins2013_medexp;
local post_coef2 = r(estimate);
display `post_coef2'*unins2013;
		
***************************************;
** estimating specification (3) with **;
** response time cutoffs and         **;
** recreating table 5                **;
***************************************;
foreach x in `resp_cutoffs' {;
	tabstat `x' if resp_time > 0 & year <= 2013 & pop2010 >= 10000 
			& statefips != 11, stat(mean);
		
	reghdfe `x' post_unins2013 post_unins2013_medexp `econ_ind' `weather_ind'
			`night_weekend_ind' if resp_time > 0 & resp_time < 180 
			& pop2010 >= 10000 & statefips != 11,
			absorb(c.countyfips#c.quarter#i.medexp c.pop2010#i.medexp
			c.statefips#c.year#i.quarter)
			vce(cluster statefips);
	
	lincom post_unins2013;
	local beta_1 = r(estimate);
	display `beta_1'*unins2013;

	lincom post_unins2013_medexp;
	local beta_2 = r(estimate);
	display `beta_2'*unins2013;

	lincom post_unins2013 + post_unins2013_medexp;
	local post_coef = r(estimate);
	display `post_coef'*unins2013;
	};

#delimit cr
clear

log close