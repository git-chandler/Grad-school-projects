********************************************************************************
********************************************************************************
** The following do file constructs the final merged data set from the .dta   **
** files created in combinedata.do. This is the second do file in the sequence**
** of do files that perform the analysis. Generally, my notes make clear my   **
** process, and details are outlined in the methods paper. I did my best to   **
** make as few m:m merges as possible because this is best practice. Note     **
** that I sometimes interchange the words 'ambulance' and 'accident' when     **
** referring to the file that contains the ambulance response times. FARS     **
** calls it ACCIDENT on their website. In my notes, they are one and the same.**
********************************************************************************
********************************************************************************

clear
set more off
set trace off

version

// set working directory
cd "C:\Users\zacharyj\OneDrive - The University of Colorado Denver\HealthEconStataProject\DataFiles"

capture log close
log using mergedatalog.log, replace

// based on combinedata.do, there should be no state-aggregated obersvations
// neither should there be any obervations from Puerto Rico

******************************************
** merge the small area estimates first **
******************************************
use sahie
sort year statefips countyfips

merge 1:1 year statefips countyfips using saipe

drop _merge // 0 unmatched
save sae_merged.dta, replace

**********************************************
** merge small area estimates w. labor data **
**********************************************
merge 1:1 year statefips countyfips using laucnty

// inspect the unmatched
// browse if _merge == 1

// in 2015, state/county fips 02-270 (Wade Hampton census area) 
// changed to 02-158 (Kusilvak Census Area); reference the Census Bureau
// all employment data for this unit is listed in all years as 02-158,
// but there are sae data for 02-158 that do not match the sae data for 02-270

// browse if statefips == 2 & inlist(countyfips, 158, 270)

// state fips code 46, county fips code 102 is Oglala Lakota County, S. Dakota
// this area has observations for all years in the labor and unemployment data,
// but not in the saipe data, where it only has observations for 2015
// it also has only 2015 observations in the sahie data

// in 2015, state/county fips 46-113 (Shannon County) 
// changed to 46-102 (Oglala Lakota County); reference the Census Bureau

// browse if statefips == 46 & inlist(countyfips, 102, 113)

// ideally, we would merge all the values into the 46-102 observations, but
// there's no easy way to do this and make it repeatable (that i can find)
// keep an eye on it b.c these observations may fall out based on conditions
// defined later, such as population in 2010

// in 2013, 51-019, bedford county, va, added the formerly independent city of
// 51-515, bedford, va; net added population is 6,222

// browse if statefips == 51 & inlist(countyfips, 19, 515)

// browse if _merge == 2 // keeping these for now

// needs to be determined for AK, SD, and VA whether there are meaningful
// accident-level data prior to census area changes before deleting or excluding
// observations

drop _merge // remaining unmatched obs are mostly aggregated data
save saeLau_merged.dta, replace

****************************************************************
** merge sae-labor data with county population data from 2010 **
** counties with population < 10,000 in 2010 are excluded     **
** according to the note to Table 1; my population data are   **
** from American FactFinder, maintined by the Census Bureau   **
****************************************************************
merge m:1 statefips countyfips using cntypop2010.dta

// inspect unmatched observations
// browse if _merge == 1 // see note above about census areas that have changed
// browse if _merge == 2 // Kalawao, HI; can be dropped; see combinedata.do

drop if _merge == 2 // only 1 observation

rename april12010census pop2010 // renaming this variable for easier typing
// i'm using this because i think it will be more accurate
// since it's a result of an actual count

//drop id2 geography april12010estimatesbase populationestimateasofjuly12010

// inspecting the observations for AK, SD, and VA (see note above)
sort year statefips countyfips

// browse if statefips == 2 & inlist(countyfips, 158, 270)
// population data are under 10,000, so they will not be included, but we should
// verify that they do not have usfeul accident-level data before excluding 

// browse if statefips == 46 & inlist(countyfips, 102, 113)
// population data are greater than 10,000, so these should not be excluded

// browse if statefips == 51 & inlist(countyfips, 19, 515)
// population data are greater than 10,000 for bedford county prior to
// incorporating and adds a net population of 6,222 after incorporating;
// this suggests that unincorporated Bedford will be dropped from the analysis

local temp_list ins_rate unins_rate medianhouseholdincome povertyrate ///
			    unemprate pop2010

foreach i in `temp_list' {
	by year, sort: egen _`i' = max(`i') ///
		if statefips == 2 & inlist(countyfips, 158, 270) ///
			| statefips == 46 & inlist(countyfips, 102, 113)
	replace `i' = _`i' if statefips == 2 & inlist(countyfips, 158, 270) ///
							| statefips == 46 & inlist(countyfips, 102, 113)
	drop _`i'
	}

drop _merge
save saeLauPop_merged.dta, replace

**********************************************************
** merge sae-labor-population data with ambulance times **
**********************************************************
merge 1:m year statefips countyfips using ambtimes

// inspect unmatched observations
// browse if _merge == 1 // these don't have any accident observations, so they
// don't appear in the accident data

// browse if _merge == 2 // most of these reflect census areas that were changed
// and have accidents for years prior to their change; some of these have 0 as
// their countyfips code, which is 'not applicable' in the accident file; i did
// not drop these in combinedata.do

// in total, there are 1,719 unmatched observations, which is just abve 1% of
// the cumulative sample size

// inspecting the observations for AK (see notes above)
sort year statefips countyfips

// browse if statefips == 2 & inlist(countyfips, 158, 270)
// there are accident-level data here, but we can drop countyfips 158 observations
// before 2015 because those do not have accident-level data
// countyfips 270 have accident-level data in 2015 but no population data
// most of the accident data are topcoded, and they will be dropped by the
// population restriction
drop if statefips == 2 & inlist(countyfips, 158, 270) & year <= 2014

// browse if statefips == 46 & inlist(countyfips, 102, 113)
// similarly, we can drop countyfips 102 here prior to 2015; countyfips 113
// has no population data, so it will be dropped; it also has topcoded
// accident-level data in 2015
drop if statefips == 46 & inlist(countyfips, 102, 113) & year <= 2014

// browse if statefips == 51 & inlist(countyfips, 19, 515)
// we can't drop countyfips 515 here because there are meaningful accident-level
// data prior to 2013

drop _merge
save merged_data.dta, replace

clear

log close