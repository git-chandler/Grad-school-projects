ssc install estout, replace // print regression tables

clear
eststo clear
set more off
*set trace on

cd "F:\"
capture log close
log using regresslog.log, replace

use census_coverage.dta, clear

/* this bit of code generates the exposure function which effect is being measured */
generate progYear = Birthyear - enrol_year // exposure is a function of this quantity
generate Exposure = .
replace Exposure = 0 if progYear >= -30 & progYear <= -19
replace Exposure = 0 if progYear >= -18 & progYear <= -16 & Birthyear <= 1982
replace Exposure = 1 if progYear >= -18 & progYear <= -16 & Birthyear == 1983
replace Exposure = 2 if progYear >= -18 & progYear <= -16 & Birthyear == 1984
replace Exposure = 3 if progYear >= -18 & progYear <= -16 & Birthyear >= 1985
replace Exposure = 2 if progYear == -15 & Birthyear == 1983
replace Exposure = 3 if progYear == -15 & Birthyear == 1984
replace Exposure = 4 if progYear == -15 & Birthyear >= 1985
replace Exposure = 4 if progYear == -14 & Birthyear == 1984
replace Exposure = 5 if progYear == -14 & Birthyear >= 1985
replace Exposure = 6 if progYear == -13
replace Exposure = 7 if progYear == -12
replace Exposure = 8 if progYear == -11
replace Exposure = 9 if progYear == -10
replace Exposure = 10 if progYear >= -9 & progYear <= -6

/* grade completion variables */
/* primary: grades 1-6
   secondary: grades 6-9
   high school: grades 10-12
   some college: 13 and above */
generate Primary = 1 if yrschool >= 6 // completed primary but not secondary
replace Primary = 0 if yrschool < 6
generate Secondary = 1 if yrschool >= 9 // completed secondary but not high school
replace Secondary = 0 if yrschool >= 6 & yrschool < 9
generate HighSchool = 1 if yrschool >= 12 // completed high school but no college
replace HighSchool = 0 if yrschool >= 9 & yrschool < 12
generate SomeCollege = 1 if yrschool >= 13 // at least some college
replace SomeCollege = 0 if yrschool >= 12 & yrschool < 13

save exposure.dta, replace

//=======================================================================
/* check the exposure variable */
/*
summarize Exposure, detail
tabulate Exposure
histogram Exposure

tabulate Birthyear enrol_year // 78% are in enrolled in 1998 and 1999 at all birthyears
tabulate Birthyear Exposure
tabulate Birthyear, sum(Exposure) // this looks like what i expected
tabulate enrol_year Exposure // matches the tab birthyear exposure, above
tabulate enrol_year, sum(Exposure) // this looks like what i expected
*/
//=======================================================================
/* summary statistics */
tabstat Exposure Female Literate Rural, by(Birthyear) stat(mean sd)

tabstat yrschool Employed Skilled lnwage, by(Birthyear) stat(mean sd)

/*		
//=======================================================================
/* exploring and graphing relationships */
generate fullExp = 1 if Exposure == 10
replace fullExp = 0 if Exposure < 10

generate halfExp = 1 if Exposure >= 5 & Exposure <= 9
replace halfExp = 0 if Exposure < 5 | Exposure > 9

generate someExp = 1 if Exposure >= 1 & Exposure <= 4
replace someExp = 0 if Exposure < 1 | Exposure > 4

generate noExp = 1 if Exposure == 0
replace noExp = 0 if Exposure > 0

graph twoway (lfit Primary fullExp) (lfit Primary halfExp) (lfit Primary someExp) ///
			 (lfit Primary noExp)

graph twoway (lfit Secondary fullExp) (lfit Secondary halfExp) (lfit Secondary someExp) ///
			 (lfit Secondary noExp)

graph twoway (lfit HighSchool fullExp) (lfit HighSchool halfExp) (lfit HighSchool someExp) ///
			 (lfit HighSchool noExp)
			 
graph twoway (lfit SomeCollege fullExp) (lfit SomeCollege halfExp) (lfit SomeCollege someExp) ///
			 (lfit SomeCollege noExp)
			 
graph twoway (lfit lnwage fullExp) (lfit lnwage halfExp) (lfit lnwage someExp) ///
			 (lfit lnwage noExp)
*/

//=============================================================================
/* schooling regressions */
/* naive regressions */
eststo prim: regress Primary Exposure, vce(cluster cve_mun)
eststo secon: regress Secondary Exposure, vce(cluster cve_mun)
eststo high: regress HighSchool Exposure, vce(cluster cve_mun)
eststo coll: regress SomeCollege Exposure, vce(cluster cve_mun)

/* municipality fixed effects regressions */
eststo primm: xtreg Primary Exposure, fe i(cve_mun) vce(cluster cve_mun)
eststo seconm: xtreg Secondary Exposure, fe i(cve_mun) vce(cluster cve_mun)
eststo highm: xtreg HighSchool Exposure, fe i(cve_mun) vce(cluster cve_mun)
eststo collm: xtreg SomeCollege Exposure, fe i(cve_mun) vce(cluster cve_mun)

/* municipality + birth year fixed effects regressions */
eststo primmby: xtreg Primary Exposure i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)
eststo seconmby: xtreg Secondary Exposure i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)
eststo highmby: xtreg HighSchool Exposure i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)
eststo collmby: xtreg SomeCollege Exposure i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)

/* print main table */
#delimit ;
esttab primmby seconmby highmby collmby using schMain_regression.tex, replace se
	   star(* 0.1 ** 0.05 *** 0.001) noconstant label booktabs 
	   alignment(D{.}{.}{-1}) width(0.8\hsize) title("Completed Levels of Schooling")
	   mtitles("Primary" "Secondary" "High School" "Some College");

/* print appendix table */
esttab prim* using schoolApp_regression.tex, replace se star(* 0.1 ** 0.05 *** 0.001)
	   noconstant label booktabs alignment(D{.}{.}{-1}) width(0.8\hsize)
	   title("Completed Primary School")
	   mtitles("Whole Sample" "Skilled" "Unskilled" "Whole Sample" "Skilled" "Unskilled" "Whole Sample" "Skilled" "Unskilled");

esttab secon* using schoolApp_regression.tex, append se star(* 0.1 ** 0.05 *** 0.001)
	   noconstant label booktabs alignment(D{.}{.}{-1}) width(0.8\hsize)
	   title("Completed Primary School")
	   mtitles("Whole Sample" "Skilled" "Unskilled" "Whole Sample" "Skilled" "Unskilled" "Whole Sample" "Skilled" "Unskilled");

esttab high* using schoolApp_regression.tex, append se star(* 0.1 ** 0.05 *** 0.001)
	   noconstant label booktabs alignment(D{.}{.}{-1}) width(0.8\hsize)
	   title("Completed Primary School")
	   mtitles("Whole Sample" "Skilled" "Unskilled" "Whole Sample" "Skilled" "Unskilled" "Whole Sample" "Skilled" "Unskilled");

esttab coll* using schoolApp_regression.tex, append se star(* 0.1 ** 0.05 *** 0.001)
	   noconstant label booktabs alignment(D{.}{.}{-1}) width(0.8\hsize)
	   title("Completed Primary School")
	   mtitles("Whole Sample" "Skilled" "Unskilled" "Whole Sample" "Skilled" "Unskilled" "Whole Sample" "Skilled" "Unskilled");

#delimit cr

tabstat Primary Secondary HighSchool SomeCollege, stat(mean)

//=============================================================================
/* employment regressions */
eststo employ: regress Employed Exposure, vce(cluster cve_mun)
eststo employm: xtreg Employed Exposure, fe i(cve_mun) vce(cluster cve_mun)
eststo employmby: xtreg Employed Exposure i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)

//=============================================================================
/* skill regressions */
eststo skill: regress Skilled Exposure, vce(cluster cve_mun)
eststo skillm: xtreg Skilled Exposure, fe i(cve_mun) vce(cluster cve_mun)
eststo skillmby: xtreg Skilled Exposure i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)
	   
//==============================================================================
/* wage regressions */
eststo wage: regress lnwage Exposure, vce(cluster cve_mun)
eststo wagem: xtreg lnwage Exposure, fe i(cve_mun) vce(cluster cve_mun)
eststo wagemby: xtreg lnwage Exposure i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)

/* print main table */
#delimit ;
esttab employmby skillmby wagemby using laborMain_regression.tex, replace se
	   star(* 0.1 ** 0.05 *** 0.001) noconstant booktabs 
	   alignment(D{.}{.}{-1}) width(0.8\hsize)
	   label title("Labor Outcomes")
	   mtitles("Employment" "Skilled" "ln(Hourly Wage)");

/* print appendix tables */
esttab employ* using laborApp_regression.tex, replace se star(* 0.1 ** 0.05 *** 0.001)
	   noconstant booktabs alignment(D{.}{.}{-1}) width(0.8\hsize)
	   label title("Likelihood of Being Employmed");
	   
esttab skill* using laborApp_regression.tex, append se star(* 0.1 ** 0.05 *** 0.001)
	   noconstant booktabs alignment(D{.}{.}{-1}) width(0.8\hsize)
	   label title("Likelihood of Being Employmed");

esttab wage* using laborApp_regression.tex, append se star(* 0.1 ** 0.05 *** 0.001)
	   noconstant booktabs alignment(D{.}{.}{-1}) width(0.8\hsize)
	   label title("Likelihood of Being Employmed");

#delimit cr

tabstat Employed Skilled, stat(mean)

/* check for spurious regression [Don't include this in anything. Just a check.] */
xtreg Indigenous Exposure i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)
	   
//=============================================================================
/* robustness checks */
/* restricted sample: conditioned on being exposed, what is the effect? */
eststo primRob1: xtreg Primary Exposure i.Birthyear if Exposure > 0, fe i(cve_mun) vce(cluster cve_mun)
eststo seconRob1: xtreg Secondary Exposure i.Birthyear if Exposure > 0, fe i(cve_mun) vce(cluster cve_mun)
eststo highRob1: xtreg HighSchool Exposure i.Birthyear if Exposure > 0, fe i(cve_mun) vce(cluster cve_mun)
eststo collRob1: xtreg SomeCollege Exposure i.Birthyear if Exposure > 0, fe i(cve_mun) vce(cluster cve_mun)

eststo employRob1: xtreg Employed Exposure i.Birthyear if Exposure > 0, fe i(cve_mun) vce(cluster cve_mun)
eststo skillRob1: xtreg Skilled Exposure i.Birthyear if Exposure > 0, fe i(cve_mun) vce(cluster cve_mun)
eststo wageRob1: xtreg lnwage Exposure i.Birthyear if Exposure > 0, fe i(cve_mun) vce(cluster cve_mun)

/* print table */
#delimit ;
esttab primRob1 seconRob1 highRob1 collRob1 employRob1 skillRob1 wageRob1 using robust_regression.tex,
	   replace se star(* 0.1 ** 0.05 *** 0.001) noconstant booktabs
	   alignment(D{.}{.}{-1}) width(0.8\hsize) label title("Restricted Sample")
	   mtitles("Primary" "Secondary" "High School" "Some College" "Employed" "Skilled" "ln(Hourly Wage)");

#delimit cr

/* state linear time trends */
eststo primRob2: xtreg Primary Exposure i.Birthyear c.Birthyear#i.cve_edo, fe vce(cluster cve_mun)
eststo seconRob2: xtreg Secondary Exposure i.Birthyear c.Birthyear#i.cve_edo, fe vce(cluster cve_mun)
eststo highRob2: xtreg HighSchool Exposure i.Birthyear c.Birthyear#i.cve_edo, fe vce(cluster cve_mun)
eststo collRob2: xtreg SomeCollege Exposure i.Birthyear c.Birthyear#i.cve_edo, fe vce(cluster cve_mun)

eststo employRob2: xtreg Employed Exposure i.Birthyear c.Birthyear#i.cve_edo, fe vce(cluster cve_mun)
eststo skillRob2: xtreg Skilled Exposure i.Birthyear c.Birthyear#i.cve_edo, fe vce(cluster cve_mun)
eststo wageRob2: xtreg lnwage Exposure i.Birthyear c.Birthyear#i.cve_edo, fe vce(cluster cve_mun)

#delimit ;
esttab primRob2 seconRob2 highRob2 collRob2 employRob2 skillRob2 wageRob2 using robust_regression.tex,
	   append se star(* 0.1 ** 0.05 *** 0.001) noconstant booktabs
	   alignment(D{.}{.}{-1}) width(0.8\hsize) label title("State Linear Time Trends")
	   mtitles("Primary" "Secondary" "High School" "Some College" "Employed" "Skilled" "ln(Hourly Wage)");

#delimit cr
/*
/* municipality linear time trends [Try this w. STATA 15/SE or MP]*/
eststo primRob3: xtreg Primary Exposure i.Birthyear c.Birthyear#i.cve_mun, fe vce(cluster cve_mun)
eststo seconRob3: xtreg Secondary Exposure i.Birthyear c.Birthyear#i.cve_mun, fe vce(cluster cve_mun)
eststo highRob3: xtreg HighSchool Exposure i.Birthyear c.Birthyear#i.cve_mun, fe vce(cluster cve_mun)
eststo collRob3: xtreg SomeCollege Exposure i.Birthyear c.Birthyear#i.cve_mun, fe vce(cluster cve_mun)

eststo employRob3: xtreg Employed Exposure i.Birthyear c.Birthyear#i.cve_mun, fe vce(cluster cve_mun)
eststo skillRob3: xtreg Skilled Exposure i.Birthyear c.Birthyear#i.cve_mun, fe vce(cluster cve_mun)
eststo wageRob3: xtreg lnwage Exposure i.Birthyear c.Birthyear#i.cve_mun, fe vce(cluster cve_mun)

#delimit ;
esttab primRob3 seconRob3 highRob3 collRob3 employRob3 skillRob3 wageRob3 using robust_regression.csv,
	   append se star(* 0.1 ** 0.05 *** 0.001) noconstant label title("Muncipality Linear Time Trends")
	   mtitles("Primary" "Secondary" "High School" "Some College" "Employed" "Skilled" "ln(Hourly Wage)");

#delimit cr
*/
/* altering the definition of Skilled */
// reducing skilled occupations
generate Skilled2 = (occisco == 1 | occisco == 2)
replace Skilled2 = . if occisco == .

// increasing skilled occupations
generate Skilled3 = (occisco == 1 | occisco == 2 | occisco == 3 | occisco == 4 | ///
occisco == 5 | occisco == 6 | occisco == 7 | occisco == 8)
replace Skilled3 = . if occisco == .

generate ColDeg = 1 if yrschool >= 16 // earned a college degree
replace ColDeg = 0 if yrschool >= 12 & yrschool < 16

eststo ColDeg: xtreg ColDeg Exposure i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)
eststo skill2: xtreg Skilled2 Exposure i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)
eststo skill3: xtreg Skilled3 Exposure i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)

#delimit ;
esttab collmby ColDeg skillmby skill2 skill3 using robust_regression.tex, append se 
	   star(* 0.1 ** 0.05 *** 0.001) noconstant booktabs 
	   alignment(D{.}{.}{-1}) width(0.8\hsize) label title("Changing Definition of Skilled")
	   mtitles("Skilled" "Skilled2" "Skilled3");

#delimit cr

tabstat SomeCollege ColDeg Skilled Skilled2 Skilled3

/* controlling for migration */
eststo primRob4: xtreg Primary Exposure migrate5 i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)
eststo seconRob4: xtreg Secondary Exposure migrate5 i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)
eststo highRob4: xtreg HighSchool Exposure migrate5 i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)
eststo collRob4: xtreg SomeCollege Exposure migrate5 i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)

eststo employRob4: xtreg Employed Exposure migrate5 i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)
eststo skillRob4: xtreg Skilled Exposure migrate5 i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)
eststo wageRob4: xtreg lnwage Exposure migrate5 i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)

#delimit ;
esttab primRob4 seconRob4 highRob4 collRob4 employRob4 skillRob4 wageRob4 using robust_regression.tex,
	   append se star(* 0.1 ** 0.05 *** 0.001) noconstant booktabs
	   alignment(D{.}{.}{-1}) width(0.8\hsize)
	   label title("Controlling for Migration")
	   mtitles("Primary" "Secondary" "High School" "Some College" "Employed" "Skilled" "ln(Hourly Wage)");

#delimit cr

//============================================================================
/* heterogeneity analysis */
/* restricted sample: Rural == 1 */
eststo primRur: xtreg Primary Exposure i.Birthyear if Rural == 1, fe i(cve_mun) vce(cluster cve_mun)
eststo seconRur: xtreg Secondary Exposure i.Birthyear if Rural == 1, fe i(cve_mun) vce(cluster cve_mun)
eststo highRur: xtreg HighSchool Exposure i.Birthyear if Rural == 1, fe i(cve_mun) vce(cluster cve_mun)
eststo collRur: xtreg SomeCollege Exposure i.Birthyear if Rural == 1, fe i(cve_mun) vce(cluster cve_mun)

eststo employRur: xtreg Employed Exposure i.Birthyear if Rural == 1, fe i(cve_mun) vce(cluster cve_mun)
eststo skillRur: xtreg Skilled Exposure i.Birthyear if Rural == 1, fe i(cve_mun) vce(cluster cve_mun)
eststo wageRur: xtreg lnwage Exposure i.Birthyear if Rural == 1, fe i(cve_mun) vce(cluster cve_mun)

/* print table */
#delimit ;
esttab primRur seconRur highRur collRur employRur skillRur wageRur using extra_regression.tex,
	   replace se star(* 0.1 ** 0.05 *** 0.001) noconstant booktabs
	   alignment(D{.}{.}{-1}) width(0.8\hsize) label title("Rural == 1")
	   mtitles("Primary" "Secondary" "High School" "Some College" "Employed" "Skilled" "ln(Hourly Wage)");

#delimit cr

tabstat Primary Secondary HighSchool SomeCollege Employed Skilled lnwage if Rural == 1

/* restricted sample: Male == 1 */
eststo primMal: xtreg Primary Exposure i.Birthyear if Male == 1, fe i(cve_mun) vce(cluster cve_mun)
eststo seconMal: xtreg Secondary Exposure i.Birthyear if Male == 1, fe i(cve_mun) vce(cluster cve_mun)
eststo highMal: xtreg HighSchool Exposure i.Birthyear if Male == 1, fe i(cve_mun) vce(cluster cve_mun)
eststo collMal: xtreg SomeCollege Exposure i.Birthyear if Male == 1, fe i(cve_mun) vce(cluster cve_mun)

eststo employMal: xtreg Employed Exposure i.Birthyear if Male == 1, fe i(cve_mun) vce(cluster cve_mun)
eststo skillMal: xtreg Skilled Exposure i.Birthyear if Male == 1, fe i(cve_mun) vce(cluster cve_mun)
eststo wageMal: xtreg lnwage Exposure i.Birthyear if Male == 1, fe i(cve_mun) vce(cluster cve_mun)

/* print table */
#delimit ;
esttab primMal seconMal highMal collMal employMal skillMal wageMal using extra_regression.tex,
	   append se star(* 0.1 ** 0.05 *** 0.001) noconstant booktabs
	   alignment(D{.}{.}{-1}) width(0.8\hsize) label title("Male == 1")
	   mtitles("Primary" "Secondary" "High School" "Some College" "Employed" "Skilled" "ln(Hourly Wage)");

#delimit cr

tabstat Primary Secondary HighSchool SomeCollege Employed Skilled lnwage if Male == 1

/* restricted sample: Female == 1 */
eststo primFem: xtreg Primary Exposure i.Birthyear if Female == 1, fe i(cve_mun) vce(cluster cve_mun)
eststo seconFem: xtreg Secondary Exposure i.Birthyear if Female == 1, fe i(cve_mun) vce(cluster cve_mun)
eststo highFem: xtreg HighSchool Exposure i.Birthyear if Female == 1, fe i(cve_mun) vce(cluster cve_mun)
eststo collFem: xtreg SomeCollege Exposure i.Birthyear if Female == 1, fe i(cve_mun) vce(cluster cve_mun)

eststo employFem: xtreg Employed Exposure i.Birthyear if Female == 1, fe i(cve_mun) vce(cluster cve_mun)
eststo skillFem: xtreg Skilled Exposure i.Birthyear if Female == 1, fe i(cve_mun) vce(cluster cve_mun)
eststo wageFem: xtreg lnwage Exposure i.Birthyear if Female == 1, fe i(cve_mun) vce(cluster cve_mun)

/* print table */
#delimit ;
esttab primFem seconFem highFem collFem employFem skillFem wageFem using extra_regression.tex,
	   append se star(* 0.1 ** 0.05 *** 0.001) noconstant booktabs
	   alignment(D{.}{.}{-1}) width(0.8\hsize) label title("Female == 1")
	   mtitles("Primary" "Secondary" "High School" "Some College" "Employed" "Skilled" "ln(Hourly Wage)");

#delimit cr

tabstat Primary Secondary HighSchool SomeCollege Employed Skilled lnwage if Female == 1

//============================================================================
/* additional analysis */
/* non-linear transformations of exposure */
generate Exp2 = Exposure^2

eststo primExp2: xtreg Primary Exp2 i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)
eststo seconExp2: xtreg Secondary Exp2 i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)
eststo highExp2: xtreg HighSchool Exp2 i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)
eststo collExp2: xtreg SomeCollege Exp2 i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)

eststo employExp2: xtreg Employed Exp2 i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)
eststo skillExp2: xtreg Skilled Exp2 i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)
eststo wageExp2: xtreg lnwage Exp2 i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)

generate ExpSR = sqrt(Exposure)

eststo primExpSR: xtreg Primary ExpSR i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)
eststo seconExpSR: xtreg Secondary ExpSR i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)
eststo highExpSR: xtreg HighSchool ExpSR i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)
eststo collExpSR: xtreg SomeCollege ExpSR i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)

eststo employExpSR: xtreg Employed ExpSR i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)
eststo skillExpSR: xtreg Skilled ExpSR i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)
eststo wageExpSR: xtreg lnwage ExpSR i.Birthyear, fe i(cve_mun) vce(cluster cve_mun)

/* print table */
#delimit ;
esttab primExp2 seconExp2 highExp2 collExp2 employExp2 skillExp2 wageExp2 using extra_regression.tex,
	   append se star(* 0.1 ** 0.05 *** 0.001) noconstant booktabs
	   alignment(D{.}{.}{-1}) width(0.8\hsize) label title("Exposure Squared")
	   mtitles("Primary" "Secondary" "High School" "Some College" "Employed" "Skilled" "ln(Hourly Wage)");

esttab primExpSR seconExpSR highExpSR collExpSR employExpSR skillExpSR wageExpSR using extra_regression.tex,
	   append se star(* 0.1 ** 0.05 *** 0.001) noconstant booktabs
	   alignment(D{.}{.}{-1}) width(0.8\hsize) label title("Square Root of Exposure")
	   mtitles("Primary" "Secondary" "High School" "Some College" "Employed" "Skilled" "ln(Hourly Wage)");

#delimit cr

set trace off
set more on
log close

*view regresslog.log
