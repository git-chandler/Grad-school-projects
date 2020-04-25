********************************************************************************
********************************************************************************
** The following do file converts .csv files in the working directory to .dta **
** files that will later be merged by mergedata.do. This is the first in the  **
** sequence of do files to perform the analysis. The SAHIE and SAPIE data     **
** were originally downloaded as Excel files and had extraneous rows that     **
** contained information useful for interpreting the data, such as definitions**
** and footnotes. I removed these rows and any columns that seemed obviously  **
** extraneous (more about my reasoning process in the paper) and saved them as**
** .csv files. The FARS ACCIDENT file did not require any manipulation so it  **
** remains in its original form. I also created a .csv file containing        **
** statefips codes and expansion dates. Saving everything as .csv allows me to**
** take advantage of a loop for converting files to .dta rather than making   **
** this conversion manually, one at a time. The notes accompanying the code   **
** generally outline my decision-making process, and I expand on this where   **
** necessary in the methods paper.                                            **
********************************************************************************
********************************************************************************

/*File index:
ambtimes_year: ambulance response time information (this is the ACCIDENT file 
			   from the NHTSA ftp site); st_case is a unique identifier

laucnty0_year: local area unemployment statistics with headers modified

sahie0_year: small area health insurance estimates without code description 
			 information

saipe0_year: small area income and poverty estimates with table description,
			 postal, county names, confidence intervals removed; removed commas;
			 year column added

medexpdate: medicaid expansion dates (constructed in .csv from information I 
			took from the Kaiser Foundation)*/

/* list of statefips codes in case i need it to verify data
1 2 4 5 6 8 9 10 11 12 13 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
32 33 34 35 36 37 38 39 40 41 42 44 45 46 47 48 49 50 51 53 54 55 56 */

clear
set more off
set trace off // this can be useful for debugging

version // display stata version

capture log close
log using combinelog.log, replace

// set working directory
cd "C:\Users\zacharyj\OneDrive - The University of Colorado Denver\HealthEconStataProject\DataFiles"

/*This first loop converts .csv files to .dta files using a for loop.*/

local myfilelist : dir . files"*.csv"

foreach file of local myfilelist {
drop _all
import delimited using `"`file'"'
local outfile = subinstr("`file'",".csv","",.)
save `"`outfile'"', replace
}

clear

/*Next, we loop over files to append all files of similar data in different years 
into single files.*/

*****************************************************
** ambulance response times and driving conditions **
*****************************************************
use ambtimes_2010.dta // initializing a file to set up columns
save ambtimes.dta, replace

forval i = 2011/2015 {
use ambtimes.dta
append using ambtimes_`i'.dta
save ambtimes.dta, replace
}

// i am convinced that the 'state' and 'county' geographic identifiers in the
// accident file are actually fips codes in disguise, but i'm unsure how to
// verify it; this is based on brief research i did about fips codes and
// geographic identifiers in general; it seems that fips codes are still 
// maintained, but they no longer enjoy the use they once did and have been 
// converted to other geographic area locators; however, i can not locate an 
// authoritative source that explains this; therefore, i rename state and 
// county columns to statefips and countyfips, respectively

rename state statefips // common variable name statefips for all files
rename county countyfips // common variable name countyfips for all files

tabulate statefips, missing // check for missing values, none
hist statefips, discrete // this looks good

count if countyfips == . // 0 missing; histogram not really helpful here

// according to documentation, 0 indicates 'not applicable', and 
// 997, 998, 999 are topcodes

count if countyfips == 0 // 12 of these; not clear why these are 'not applicable'
// looks like some variables associated with notification and arrival times have top 
// codes for these; i think they can be safely deleted because it's a small
// subset

count if inlist(countyfips, 997, 998, 999) // 8 observations; lots of topcodes in
// other variables; almost all are AZ, which expanded medicaid

count if inlist(city, ., 0, 9997, 9998, 9999) // 95,559
// the analysis is performed at the county level, so we can keep these
// observations, but there's no clear reason why these are labeled as such
// a useful analysis would be to find characteristics, such as medicaid expansion,
// un/insurance rates, and any of the controls, that predict whether a city is 
// topcoded or missing, but i think that's beyond the scope of what we're doing

drop if inlist(countyfips, 0, 997, 998, 999) // dropping 20 observations

save ambtimes.dta, replace
clear

****************************************
** labor data for economic conditions **
****************************************
use laucnty0_2010.dta
save laucnty.dta, replace

forval i = 2011/2015 {
use laucnty.dta
append using laucnty0_`i'.dta
save laucnty.dta, replace
}

rename statefipscode statefips // common variable name statefips for all files
rename countyfipscode countyfips // common variable name countyfips for all files

generate unemprate = unemploymentrate/100 // changing % to decimals

tabulate statefips, missing // 468 observations from Puerto Rico
// 6 observations for DC; keeping for now
hist statefips, discrete // this looks good

drop if statefips == 72 // dropping Puerto Rico

hist countyfips, discrete // checking for topcodes; i think this looks good
// based on the Census Bureau's website, it looks like there are no topcodes
// for countyfips, and BLS data comes from the Census Bureau

count if countyfips == 0 // looking for weird values; none

hist unemprate // this doesn't look suspect; most values are between 0 and 0.2,
// nothing higher than 0.3ish

drop laborforce employed unemployed unemploymentrate // drop what i don't need
save laucnty.dta, replace
clear

*******************************************
** small area health insurance estimates **
** un/insured rates are outcomes of      **
** interest in the analysis              **
*******************************************
use sahie0_2010.dta
save sahie.dta, replace

forval i = 2011/2015 {
use sahie.dta
append using sahie0_`i'.dta
save sahie.dta, replace
}

tabulate statefips, missing // nothing weird here; no observations from
// Puerto Rico; 2,288 observations from DC

// missing data for 15-005 (HI, Kalawao County) according to documentation;
// these are in the saipe data, also; 6 values total

drop if statefips == 15 & countyfips == 5 // drops 478 observations total, for
// each demographic subgroup in each year

// i'm collapsing the sahie data here b.c it's best practice to avoid m:m
// merges, and this will reduce the m:m merges later
// the collapse statement eliminates the demographic subgroup information and
// retains the county level information, which is constant at the
// county-by-year level

sort year statefips countyfips
drop if geocat == 40 // dropping state-level aggregates

count if countyfips == 0
// 0 (check for remaining state-level info)
hist countyfips, discrete // no weird values

keep if agecat == 0 & racecat == 0 & sexcat == 0 & iprcat == 0 
// keeping county-level aggregate information for all demographic groups

hist pctui // nothing below 0 or over 100
hist pctic // nothing below 0 or over 100

collapse (mean) pctui pctic, by(year statefips countyfips) // these are 
// outcomes of interest in the analysis

generate ins_rate = pctic/100 // changing % to decimals
label variable ins_rate "County insured rate"

generate unins_rate = pctui/100 // changing % to decimals
label variable unins_rate "County uninsured rate"

hist ins_rate // most values between 0.6 and 1
hist unins_rate // most values between 0 and 0.4
// these correspond to the histograms pre-transformation

drop pctui pctic
save sahie.dta, replace
clear

*******************************************
** small area income & poverty estimates **
** economic conditions used as controls  **
** in the analysis                       **
*******************************************
use saipe0_2010.dta
save saipe.dta, replace

forval i = 2011/2015 {
use saipe.dta
append using saipe0_`i'.dta
save saipe.dta, replace
}

// in 2010 and 2011, fips codes are identified in the columns "statefips"
// and "countyfips," and in remaining years, fips codes are identified in the
// the columns "statefipscode" and "countyfipscode"; i achieve a single, combined
// variable for each using a simple max() function b.c it will ignore null values

generate statefips_combined = max(statefips, statefipscode)
drop statefips statefipscode
rename statefips_combined statefips // common statefips variable name

generate countyfips_combined = max(countyfips, countyfipscode)
drop countyfips countyfipscode
rename countyfips_combined countyfips // common countyfips variable name

tabulate statefips, missing // looks good
hist countyfips // nothing weird
count if countyfips == 0 // state-level aggregates
drop if countyfips == 0

drop if statefips == 15 & countyfips == 5 // dropping Kalawao, HI 
// (see previous note in SAHIE)

hist povertypercentallages // nothing weird
generate povertyrate = povertypercentallages/100 // change % to decimals
hist povertyrate // most values between 0 and 0.4

// dropping all the variables i won't need
keep year state countyfips povertyrate medianhouseholdincome

save saipe.dta, replace
clear

log close