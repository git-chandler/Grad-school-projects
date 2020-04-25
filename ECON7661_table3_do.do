********************************************************************************
********************************************************************************
** The following do file produces the results for Table 3. It is the fourth   **
** do file in the sequence. It begins with the .dta file saved in table1.do   **
** and condstructs indicators for weather and light conditions, night and     **
** weekend accidents, and ambulance response times. The comments follow my    **
** calculations used to determine what observations are considered not useful.**
** The added variables are saved for use in building tables 4 and 5.          **
********************************************************************************
********************************************************************************

/* list of statefips codes in case i need it to verify data
1 2 4 5 6 8 9 10 11 12 13 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
32 33 34 35 36 37 38 39 40 41 42 44 45 46 47 48 49 50 51 53 54 55 56 */

clear
set more off
set trace off // useful for debugging

version // display stata version

cd "C:\Users\zacharyj\OneDrive - The University of Colorado Denver\HealthEconStataProject\DataFiles"

capture log close
log using table3log.log, replace

use merged_ins_rates.dta

*******************************************************
** create variable representing time of notification **
*******************************************************

// unknown/missing time values are indicated by 88, 98, 99,
generate notif_time = . 
replace notif_time = not_hour*60 + not_min
replace notif_time = . if inlist(not_hour, 88, 98, 99, .) | ///
inlist(not_min, 88, 98, 99, .)
label variable notif_time "Time of notification in minutes"

// count if notif_time != . // 98,137 missing notification times
// count if inlist(not_hour, 88, 98, 99, .) | inlist(not_min, 88, 98, 99, .) // 87,516
// display 98137 + 87516 // 185,653, which matches the total number of 
// observations in the data

// the biggest the 'full sample' can be is 98,137 b.c this is the largest number
// of observations for which there is meaningful notification time data

// hist notif_time
// for comparison, 8am is 8*60=480 minutes, and 8pm, is 20*60=1,200 minutes
// the highest this number can be is 24*60=1,440 minutes, and the graph doesn't
// seem to exceed that value 

*********************************************************************
** create variable representing time of ambulance arrival on scene **
*********************************************************************
// need to adjust some arrival times based on the time of notifcation,
// ie - if notification occurs at 23 hours and arrival occers at 00 hours, then
// the arrival time needs to be adjusted to reflect this

generate _arr_hour = arr_hour
replace _arr_hour = 24 if not_hour == 23 & arr_hour == 0 // there are 508 of these
replace _arr_hour = 25 if not_hour == 23 & arr_hour == 1 // there are 8 of these
replace _arr_hour = 26 if not_hour == 23 & arr_hour == 2 // there are 3 of these

generate scene_time = .
replace scene_time = _arr_hour*60 + arr_min
replace scene_time = . if inlist(_arr_hour, 88, 98, 99, .) | ///
inlist(arr_min, 88, 98, 99, .)
label variable scene_time "Time of arrival on scene in minutes"

// hist scene_time // i think this is consistent with the way i've coded it

*******************************************************************
** create response time, dependent variable in specification (3) **
*******************************************************************
generate resp_time = .
replace resp_time = scene_time - notif_time
label variable resp_time "Response time to scene after notification in minutes"

// count if notif_time == . & resp_time != . // 0 missing response times for
// non-missing notification times

// hist resp_time // need to investigate negative values

/*
count if resp_time < 0 // 89
count if resp_time == 0 // 1,268, these are weird
count if resp_time == . // 95,148
count if resp_time != . & resp_time > 0 // only 89,148 observations have a response time

// examine the distributions of the negative and zero response times
hist(year) if resp_time < 0, bin(5) // seems disproportionate in 2015
hist(statefips) if resp_time < 0
hist(statefips) if resp_time < 0, bin(25)
hist(statefips) if resp_time < 0, bin(51)

// had i been more thoughtful, i could have devised a test to dig into this

hist(year) if resp_time == 0, bin(5) // nothing too weird here; higher again in 2015
hist(statefips) if resp_time == 0
hist(statefips) if resp_time == 0, bin(25)
hist(statefips) if resp_time == 0, bin(51)

// similar to the negative response times; maybe more rural states or less
// dense states are more prone to error? how would one test this?

*/
// display (1268+89)/(1268+89+89139) // 0.014995, so about 1.5% of non-missing
// response times are fishy

// i think the best way to move forward is to include only positive response times

****************************
** hospital arrival times **
****************************
generate _hosp_hour = hosp_hr
replace _hosp_hour = 24 if not_hour == 23 & hosp_hr == 0 // 1,039 of these
replace _hosp_hour = 25 if not_hour == 23 & hosp_hr == 1 // 115 of these
replace _hosp_hour = 26 if not_hour == 23 & hosp_hr == 2 // 8 of these
replace _hosp_hour = 26 if not_hour == 23 & hosp_hr == 3 // 4 of these

generate hosp_time = .
replace hosp_time = _hosp_hour*60 + hosp_mn
replace hosp_time = . if inlist(_hosp_hour, 88, 98, 99, .) | ///
inlist(hosp_mn, 88, 98, 99, .)
label variable hosp_time "Time of arrival at hospital in minutes"

generate hosp_arr_time = .
replace hosp_arr_time = hosp_time - notif_time
label variable hosp_arr_time "Time of Arrival at Hospital after Notification"

/*
count if hosp_time < 0 // 0
count if hosp_time == 0 // 49

count if hosp_time != . & hosp_time > 0 // 53,008
display 49/(49+53008) // 0.0009, about 0.1% of non-missing data are suspect

// examine the distributions of the zero response times
hist(year) if hosp_time == 0, bin(5) // nothing weird here
hist(statefips) if hosp_time == 0
hist(statefips) if hosp_time == 0, bin(25)
hist(statefips) if hosp_time == 0, bin(51) // this is revealing, a large
// proportion for statefips 6, 7, or 8

count if resp_time != . & resp_time > 0 & hosp_time == . // 37,422
// this is a concerning number

// similar story here to that of response times; i will only include positive
// hospital arrival times; however, this is only one line item and not
// required to estimate for the assigment, so it's less of a concern
*/

*********************************************************************
** generate indicators for ambulance times on scene within cutoffs **
*********************************************************************
foreach i in 4 8 13 20 {
	generate slow`i' = (resp_time > `i')
	label variable slow`i' "Ambulance arrives on scene in more than `i' minutes"
	}

local times resp_time slow4 slow8 slow13 slow20

********************************************************************************
** response times descriptive statistics and t-tests for mean differences     **
********************************************************************************
// the hopsital arrival times are coded in a separate line because there is
// missing data for hospital arrival times in many cases where response times
// have non-missing, positive data

// number of observations is based on response time, and the means and standard 
// deviations reported in my replicated table 3 are taken from the tabstat
// output, not the t-test output

// note that i include the topcoding of response time at 180 minutes

#delimit ;

foreach i in `times' {;
	display "Descriptive Stats of `i' for Full Sample";
	tabstat `i' if resp_time > 0 & resp_time < 180 & statefips != 11 
			& pop2010 >= 10000, stat(mean sd N);
	display "Descriptive Stats of `i' for Expansion States";
	tabstat `i' if medexp == 1 & resp_time > 0 & resp_time < 180 & statefips != 11
			& pop2010 >= 10000, stat(mean sd N);
	display "Descriptive Stats of `i' for Non-Expansion States";
	tabstat `i' if medexp == 0 & resp_time > 0  & resp_time < 180 & statefips != 11
			& pop2010 >= 10000, stat(mean sd N);
	display "t-test for mean differces in `i' between medicaid non/expansion states";
	ttest `i', by(medexp) unequal;
	display "Descriptive Stats of `i' for Pre-ACA";
	tabstat `i' if year <= 2013 & resp_time > 0  & resp_time < 180 & statefips != 11
			& pop2010 >= 10000, stat(mean sd N);
	display "Descriptive Stats of `i' for Post-ACA";
	tabstat `i' if year > 2013 & resp_time > 0  & resp_time < 180 & statefips != 11
			& pop2010 >= 10000, stat(mean sd N);
	display "t-test for mean differces in `i' before and after ACA";
	ttest `i', by(post) unequal;
	};

// hospital arrival times
tabstat hosp_arr_time if resp_time > 0 & resp_time < 180 & hosp_time > 0 
		& statefips != 11 & pop2010 >= 10000, stat(mean sd N); // full sample

tabstat hosp_arr_time if medexp == 1 & resp_time > 0  & resp_time < 180
		& hosp_time > 0 & statefips != 11 & pop2010 >= 10000, stat(mean sd N); 
		// expansion states

tabstat hosp_arr_time if medexp == 0 & resp_time > 0  & resp_time < 180 
		& hosp_time > 0 & statefips != 11 & pop2010 >= 10000, stat(mean sd N); 		
		//non-expansion states

ttest hosp_arr_time, by(medexp) unequal;

tabstat hosp_arr_time if year <= 2013 & resp_time > 0  & resp_time < 180 
		& hosp_time > 0 & statefips != 11 & pop2010 >= 10000, stat(mean sd N); 
		// pre-ACA

tabstat hosp_arr_time if year > 2013 & resp_time > 0  & resp_time < 180
		& hosp_time > 0 & statefips != 11 & pop2010 >= 10000, stat(mean sd N); 
		// post-ACA

ttest hosp_arr_time, by(post) unequal;

#delimit cr

********************************************************************************
** weather conditions descriptive statistics and t-tests for mean differences **
********************************************************************************
// create weather conditions indicators
generate sun = (weather == 1) // including sun to create mutually exclusive
// and exhaustive weather categories
generate rain = (weather == 2)
generate sleet = (weather == 3)
generate snow = (weather == 4)
generate fog = (weather == 5)
generate cloudy = (weather == 10)
generate blowing_snow = (weather == 11)
generate freezing_rain = (weather == 12)
generate wind = (weather == 6)
generate blowing_dirt = (weather == 7)
generate other_inclement = (weather == 8)

local atm_cond rain sleet snow fog cloudy blowing_snow freezing_rain ///
			   wind blowing_dirt other_inclement
			   
foreach i in `atm_cond' {
	replace `i' = . if inlist(weather, 98, 99, .)
	}

generate taco = sun + rain + sleet + snow + fog + cloudy + blowing_snow ///
				+ freezing_rain + wind + blowing_dirt + other_inclement

assert taco == 1 if taco != . // true

#delimit ;

foreach i in `atm_cond' {;
	display "Descriptive Stats of `i' for Full Sample";
	tabstat `i' if resp_time > 0  & resp_time < 180 & statefips != 11 
			& pop2010 >= 10000, stat(mean sd N);
	display "Descriptive Stats of `i' for Expansion States";
	tabstat `i' if medexp == 1 & resp_time > 0 & resp_time < 180 & statefips != 11 
			& pop2010 >= 10000, stat(mean sd N);
	display "Descriptive Stats of `i' for Non-Expansion States";
	tabstat `i' if medexp == 0 & resp_time > 0 & resp_time < 180 & statefips != 11
			& pop2010 >= 10000, stat(mean sd N);
	display "t-test for mean differces in `i' non/expansion states";
	ttest `i', by(medexp) unequal;
	display "Descriptive Stats of `i' for Pre-ACA";
	tabstat `i' if year <= 2013 & resp_time > 0 & resp_time < 180 & statefips != 11
			& pop2010 >= 10000, stat(mean sd N);
	display "Descriptive Stats of `i' for Post-ACA";
	tabstat `i' if year > 2013 & resp_time > 0 & resp_time < 180 & statefips != 11
			& pop2010 >= 10000, stat(mean sd N);
	display "t-test for mean differces in `i' before and after ACA";
	ttest `i', by(medexp) unequal;
	};

*******************************************************************************;
** night/weekend descriptive statistics and t-tests for mean differences     **;
*******************************************************************************;
generate night_acc = (lgt_cond == 2 | lgt_cond == 3);
replace night_acc = . if inlist(lgt_cond, 4, 5, 6, 7, 8, 9, .);

generate weekend_acc = (day_week == 1 | day_week == 7);
replace weekend_acc = . if day_week == .;

foreach i in night_acc weekend_acc {;
	display "Descriptive Stats of `i' for Full Sample";
	tabstat `i' if resp_time > 0 & resp_time < 180 & statefips != 11
			& pop2010 >= 10000, stat(mean sd N);
	display "Descriptive Stats of `i' for Expansion States";
	tabstat `i' if medexp == 1 & resp_time > 0 & resp_time < 180
			& statefips != 11 & pop2010 >= 10000, stat(mean sd N);
	display "Descriptive Stats of `i' for Non-Expansion States";
	tabstat `i' if medexp == 0 & resp_time > 0 & resp_time < 180
			& statefips != 11 & pop2010 >= 10000, stat(mean sd N);
	display "t-test for mean differces in `i' non/expansion states";
	ttest `i', by(medexp) unequal;
	display "Descriptive of `i' Stats for Pre-ACA";
	tabstat `i' if year <= 2013 & resp_time > 0 & resp_time < 180
			& statefips != 11 & pop2010 >= 10000, stat(mean sd N);
	display "Descriptive Stats of `i' for Post-ACA";
	tabstat `i' if year > 2013 & resp_time > 0 & resp_time < 180
			& statefips != 11 & pop2010 >= 10000, stat(mean sd N);
	display "t-test for mean differces in `i' before and after ACA";
	ttest `i', by(medexp) unequal;
	};

#delimit cr
	
save ins_rates_condition_controls.dta, replace
clear

log close