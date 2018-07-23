clear all
set more off

cd "C:\Users\ammichau\Desktop\ModelOutputAnalysis"
global OUT_dir "OUT"
global Preview_dir "Preview"

*simDatado.do
******************************************************************************
*og: @A.michaud 12/07/2017 for DIoption w/ D.wiczer 
*	this version: @A.michaud 6/12/2018 trimming excess stuff
*---------------------------------------------------------------------------------------------------
* This file analyzes our model simulated dataset
*	1) Stats about who goes on SSDI to compare to the data
*		-Occupation
*		-Age X health
*		-Assets (?)
*		-Work history: past 10 years of employment and wages
*		-Three model mechanisms: low alpha, low wtr. 
*	2) Dynamic Paths
*		-Wages + their parts
*		-Employment + Involuntary Job Loss
*	3) Do the Wage trends look reasonable?
***************************************************************************************************************
* Files needed 
*	-DTsim.dta: created by FORTRAN simulation
*------------------------------------------------------------------------------
use "DTsim.dta", clear
rename alpha w_alpha
*WUNDR
drop if (status==-1 & age>0)
gen allDI=(status==4)
gen vocDI =(hlthvoc==1)
gen U=(status==2)
gen LTU=(status==3)
gen allU=(status==3 | status==2)
gen WouldWorkNoDI=(workdif>0)
gen Apply=(appdif>0 & status==3)
gen Utag=(alpha_int==1)
gen Emp=(status==1)

replace d=0 if d==1
replace d=1 if d==2
replace d=2 if d==3
gen currentDI_d2=(allDI==1 & d==2)
gen currentDI_d1=(allDI==1 & d==1)
gen currentDI_d0=(allDI==1 & d==0)
gen currentApp_d2=(Apply==1 & d==2)
gen currentApp_d1=(Apply==1 & d==1)
gen currentApp_d0=(Apply==1 & d==0)

*Generate year variable
gen year=.
forvalues y = 1(1)32 {
		local t1= (`y'-1)*12
		local t2= `y'*12+1
	replace year = 1983+`y' if (t>`t1' & t<`t2')
}


*Create leads and lag indicators of major events.
xtset id t
	gen newDI=(status==4 & L.status~=4)
	gen newd1=(d==1 & L.d==0)
	gen newd2=(d==2 & L.d~=2)
	gen newDOB= year if (age==1 & L.age==0)
	*Measure first application date in a spell as first in last year.
	gen firstApp=(Apply==1 & L.Apply==0 & L2.Apply==0 & L3.Apply==0 & L4.Apply==0 & L5.Apply==0 & L6.Apply==0 & L7.Apply==0 & L8.Apply==0 & L9.Apply==0 & L10.Apply==0 & L11.Apply==0 & L12.Apply==0)
	gen Reject=(allDI==0 & L12.firstApp==1)
	gen Reapply=(Apply==1 & L12.Reject==1)
	
drop if status==-1


	*Create quantiles of raw wages and occupation component
ssc install egenmore 
foreach var of varlist wage wtr {
	egen `var'_quant=xtile(`var'), n(5) by(year)	
}

gen currentDI_wageQ1=(allDI==1 & wage_quant==1)
gen newDI_wageQ1=(newDI==1 & wage_quant==1)
gen currentApp_wageQ1=(Apply==1 & wage_quant==1)
gen currentDI_occwageQ1=(allDI==1 & wtr_quant==1)
gen newDI_occwageQ1=(newDI==1 & wtr_quant==1)
gen currentApp_occwageQ1=(Apply==1 & wtr_quant==1)

gen counter=1	


save "DTsim_v2.dta", replace


***************************************
*ELASTICITY REG- part 1: indvidual
***************************************
preserve
	collapse (mean) age wage wtr (max) newDI firstApp recIndic Utag, by(id year)
	
	xtset year id
	gen lnwage=ln(wage)

	*logit newDI age wtr wage recIndic Utag if age>2 & age<6 
	*logit newDI age wtr wage L.recIndic L.Utag if age>2 & age<6 
	logit firstApp age wtr wage recIndic Utag if age>2 & age<6 
	*logit firstApp age wtr wage L.recIndic L.Utag if age>2 & age<6 
		
restore		


*****************************************************************************************************************************************************************************
*****************************************************************************************************************************************************************************
*	TIME SERIES-GRAPHS
****************************************************************************************

**************************************************************************************
* Agg time series
*--------------------------------------------------------------------------------------
cd "C:\Users\ammichau\Desktop\ModelOutputAnalysis"
use "DTsim_v2.dta", clear
*preserve
collapse (sum) allDI counter newDI* newd1 newd2 vocDI U LTU allU Utag firstApp Apply Emp currentDI* currentApp* Reject Reapply, by(year)
drop if year==1984

	*Rates
		*DI stock
		gen gTrate = allDI/counter
		gen gVrate = vocDI/counter
		gen gd2share_DI = currentDI_d2/allDI
		gen gd1share_DI = currentDI_d2/allDI
		gen gd0share_DI = currentDI_d2/allDI
		gen glowWshare_DI = currentDI_wageQ1/allDI
		gen glowOccWshare_DI = currentDI_occwageQ1/allDI

		*New DI
		gen gNrate = 1000*newDI/counter
		gen gVratio = vocDI/newDI
		gen gd2share_newDI = newd2/newDI
		gen gd1share_newDI = newd1/newDI
		gen gd0share_newDI = 1-gd2share_newDI-gd1share_newDI
		gen glowWshare_newDI = newDI_wageQ1/newDI
		gen glowOccWshare_newDI = newDI_occwageQ1/newDI
		
		*Applications
		gen AppPoprate = Apply/counter
		gen gApprate = Apply/(counter-allDI)
		gen gd2share_App= currentApp_d2/Apply
		gen gd1share_App = currentApp_d1/Apply
		gen gd0share_App = currentApp_d0/Apply
		gen glowWshare_App = currentApp_wageQ1/Apply
		gen glowOccWshare_App = currentApp_occwageQ1/Apply		
		
		*Labor Markets
		gen LTUrate = LTU/counter
		gen allUrate = allU/counter
		gen InvU = Utag/counter
		gen NonEmprate = 1-Emp/counter
		gen UnotApp = (allU-Apply)/counter
		label variable allUrate "Unemployment Rate"
		label variable InvU "Involuntary"	
		label variable LTUrate "Long-Term"	
		
		foreach v of newlist Trate Vrate d2share_DI d1share_DI d0share_DI lowWshare_DI lowOccWshare_DI Apprate d2share_App d1share_App d0share_App lowWshare_App lowOccWshare_App Nrate Vratio d2share_newDI d1share_newDI d0share_newDI lowWshare_newDI lowOccWshare_newDI {
			cd "C:\Users\ammichau\Google Drive\Disability\DisabilityOption\ModelOutputAnalysis\OUT\AggTrends"
			twoway (line g`v' year, lwidth(thick)),  ///
			xtitle("Year") ytitle("Percentage") ///
			graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot))  			
			graph export `v'.eps, as(eps) preview(on) replace
			graph save `v', replace		
			cd "C:\Users\ammichau\Google Drive\Disability\DisabilityOption\ModelOutputAnalysis\Preview\AggTrends"
			graph export `v'.pdf, as(pdf) replace
		}
	
	cd "C:\Users\ammichau\Google Drive\Disability\DisabilityOption\ModelOutputAnalysis\OUT\AggTrends"
	twoway (line gTrate year, lwidth(thick)) (line gApprate year, lwidth(thick)),    ///
		xtitle("Year") ytitle("Share of Population") legend(ring(0) lab(1 "Stock of Beneficiaries") lab(2 "Applications")) ///
		graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot))  ///
		saving($OUT_dir\StockNApps, replace)
		graph export $OUT_dir\StockNApps.eps, as(eps) preview(on) replace
		graph export $Preview_dir\StockNApps.pdf, as(pdf) replace
		
	
	twoway (line allUrate year, lwidth(thick)) (line InvU year, lwidth(thick) lpattern(dash) lcolor(red)) (line LTUrate year, lwidth(thick) lpattern(dash_dot) lcolor(blue)),      ///
		 xtitle("Year") ytitle("Unemployment Rate") legend(ring(0) lab(1 "Total") lab(2 "Involuntary") lab(3 "Long Term")) ///
		graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot))  ///
		saving($OUT_dir\Urate, replace)
		graph export $OUT_dir\Urate.eps, as(eps) preview(on) replace
		graph export $Preview_dir\Urate.pdf, as(pdf) replace
		
	twoway (line NonEmprate year, lwidth(thick)) (line UnotApp year, lwidth(thick) lpattern(dash) lcolor(red)) ///
		   (line AppPoprate year, lwidth(thick) lpattern(longdash_dot) lcolor(blue)) (line gTrate year, lwidth(thick) lpattern(shortdash_dot) lcolor(blue)),      ///
		 xtitle("Year") ytitle("Share of Population") legend(ring(0) lab(1 "Total Nonemployed") lab(2 "Unemployed- not applying") lab(3 "Applying") lab(4 "On SSDI")) ///
		graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot))  ///
		saving($OUT_dir\NonEdecomp, replace)
		graph export $OUT_dir\NonEdecomp.eps, as(eps) preview(on) replace
		graph export $Preview_dir\NonEdecomp.pdf, as(pdf) replace		

*----------------------------------------------------------------------------------------------------------
					

restore

****************************************************************************************
* Occ time series
*--------------------------------------------------------------------------------------

preserve
collapse (sum) allDI counter newDI vocDI U LTU allU Utag, by(occ year)

drop if year==1984

		label define SOC_lbl 1 `"Managerial"'
		label define SOC_lbl 2 `"Professional"', add
		label define SOC_lbl 3 `"Sales"', add
		label define SOC_lbl 4 `"Clerical"', add
		label define SOC_lbl 5 `"Service: clean & maint"', add
		label define SOC_lbl 6 `"Service: protection"', add
		label define SOC_lbl 7 `"Service: food"', add
		label define SOC_lbl 8 `"Service: Health"', add
		label define SOC_lbl 9 `"Service: Personal"', add
		label define SOC_lbl 10 `"Farm, forest, fish"', add
		label define SOC_lbl 11 `"Mechanics and repair"', add
		label define SOC_lbl 12 `"Construction & Mining"', add
		label define SOC_lbl 13 `"Precision production"', add
		label define SOC_lbl 14 `"Operators: machine"', add
		label define SOC_lbl 15 `"Operators: transport"', add
		label define SOC_lbl 16 `"Operators: handlers"', add
		
		label values occ SOC_lbl 
xtset occ year

foreach x in all new voc {
gen occ_tot= `x'DI if occ==1
	by year, sort: egen occ_`x'1=max(occ_tot) 
	drop occ_tot

	gen run`x' =occ_`x'1
	
forvalues o=2/16 {
 local oo = `o'-1
gen occ_tot= `x'DI if occ==`o'
	by year, sort: egen occ_`x'`o'=max(occ_tot) 
	replace occ_`x'`o'= occ_`x'`o'+run`x'
	replace run`x'= occ_`x'`o'
	drop occ_tot
}
gen occ_`x'=occ_`x'1 if occ==1

forvalues o=2/16 {
 replace occ_`x'=occ_`x'`o' if occ==`o'
}
}


*Rates within Occupation
	gen Trate = 1000*allDI/counter
	gen Nrate = 1000*newDI/counter
	gen Vrate = 1000*vocDI/counter
	gen Vratio = vocDI/newDI
	gen Unemp = U/counter
	gen LTUrate = LTU/counter
	gen allUrate = allU/counter
	gen InvU = Utag/counter
	label variable allUrate "Unemployment Rate"
	label variable InvU "Involuntary"	
	label variable LTUrate "Long-Term"	


	xtline Trate,     ///
		 xtitle("Year") ytitle("Total Beneficiaries per 1,000 Insured") ///
		graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot))  ///
		saving($OUT_dir\Trate_occ, replace)
		graph export $OUT_dir\Trate_occ.eps, as(eps) preview(on) replace
		graph export $Preview_dir\Trate_occ.pdf, as(pdf) replace	
	
	xtline Nrate,      ///
		xtitle("Year") ytitle("New Awards per 1,000 Insured") ///
		graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot))  ///
		saving($OUT_dir\Nrate_occ, replace)
		graph export $OUT_dir\Nrate_occ.eps, as(eps) preview(on) replace
		graph export $Preview_dir\Nrate_occ.pdf, as(pdf) replace
		
	xtline Vrate,      ///
		xtitle("Year") ytitle("New Vocational Awards per 1,000 Insured") ///
		graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot))  ///
		saving($OUT_dir\Vrate_occ, replace)
		graph export $OUT_dir\Vrate_occ.eps, as(eps) preview(on) replace
		graph export $Preview_dir\Vrate_occ.pdf, as(pdf) replace
		
	xtline Vratio,     ///
		xtitle("Year") ytitle("Vocational Share of all New Awards") ///
		graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot))  ///
		saving($OUT_dir\Vratio_occ, replace)
		graph export $OUT_dir\Vratio_occ.eps, as(eps) preview(on) replace
		graph export $Preview_dir\Vratio_occ.pdf, as(pdf) replace
		

*----------------------------------------------------------------------------------------------------------
	
	
	twoway (line allUrate year, by(occ) lwidth(thick)) (line InvU year, by(occ) lwidth(thick) lpattern(dash) lcolor(red)) (line LTUrate year, by(occ) lwidth(thick) lpattern(dash_dot) lcolor(blue)),      ///
		xtitle("Year") ytitle("Unemployment Rate") ///
		graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot))  ///
		saving($OUT_dir\Urate_occ, replace)
		graph export $OUT_dir\Urate_occ.eps, as(eps) preview(on) replace
		graph export $Preview_dir\Urate_occ.pdf, as(pdf) replace
			
			

restore



****************************************************************************************
* Age time series
*--------------------------------------------------------------------------------------
preserve

collapse (sum) allDI counter newDI vocDI U LTU allU Utag, by(age year)

drop if age==6
xtset age year

		label define age_lbl 1 `"30-44"'
		label define age_lbl 2 `"45-49"', add
		label define age_lbl 3 `"50-54"', add
		label define age_lbl 4 `"55-59"', add
		label define age_lbl 5 `"60-65"', add
		
		label values age age_lbl 


*Rates within Age
	gen Trate = 1000*allDI/counter
	gen Nrate = 1000*newDI/counter
	gen Vrate = 1000*vocDI/counter
	gen Vratio = vocDI/newDI
	gen Unemp = U/counter
	gen LTUrate = LTU/counter
	gen allUrate = allU/counter
	gen InvU = Utag/counter
	label variable allUrate "Unemployment Rate"
	label variable InvU "Involuntary"	
	label variable LTUrate "Long-Term"	


	xtline Trate,     ///
		title("Model Simulation- Males") xtitle("Year") ytitle("Total Beneficiaries per 1,000 Insured") ///
		graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot))  ///
		saving($OUT_dir\Trate_age, replace)
		graph export $OUT_dir\Trate_age.eps, as(eps) preview(on) replace
		graph export $Preview_dir\Trate_age.pdf, as(pdf) replace	
	
	xtline Nrate,      ///
		title("Model Simulation- Males") xtitle("Year") ytitle("New Awards per 1,000 Insured") ///
		graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot))  ///
		saving($OUT_dir\Nrate_age, replace)
		graph export $OUT_dir\Nrate_age.eps, as(eps) preview(on) replace
		graph export $Preview_dir\Nrate_age.pdf, as(pdf) replace
		
	xtline Vrate,      ///
		title("Model Simulation- Males") xtitle("Year") ytitle("New Vocational Awards per 1,000 Insured") ///
		graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot))  ///
		saving($OUT_dir\Vrate_age, replace)
		graph export $OUT_dir\Vrate_age.eps, as(eps) preview(on) replace
		graph export $Preview_dir\Vrate_age.pdf, as(pdf) replace
		
	xtline Vratio,     ///
		title("Model Simulation- Males") xtitle("Year") ytitle("Vocational Share of all New Awards") ///
		graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot))  ///
		saving($OUT_dir\Vratio_age, replace)
		graph export $OUT_dir\Vratio_age.eps, as(eps) preview(on) replace
		graph export $Preview_dir\Vratio_age.pdf, as(pdf) replace
		
*----------------------------------------------------------------------------------------------------------
	
	twoway (line allUrate year, by(age) lwidth(thick)) (line InvU year, by(age) lwidth(thick) lpattern(dash) lcolor(red)) (line LTUrate year, by(age) lwidth(thick) lpattern(dash_dot) lcolor(blue)),      ///
		title("Model Simulation- Males") xtitle("Year") ytitle("Unemployment Rate") ///
		graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot))  ///
		saving($OUT_dir\Urate_age, replace)
		graph export $OUT_dir\Urate_age.eps, as(eps) preview(on) replace
		graph export $Preview_dir\Urate_age.pdf, as(pdf) replace
			

*********************************************************************

restore



**********************

*****************************************************************************************************************************************************************************
*****************************************************************************************************************************************************************************
*	WHO GOES ON?
****************************************************************************************


*-------------------------------------------------
* Path graphs
*-----------------------------------------------
*Setup-----------------------------------------
use "DTsim_v2.dta", clear

	*Transform to dummy for lowest quantile
	foreach var of varlist asset_quant w_alpha_quant  wage_quant  wtr_quant  AIME_quant {
		replace `var' = 0 if (`var'>1 & `var'<6)
	}
	
	*Calculate component of wage
	gen w_health = 0 if d==0
		replace w_health = -0.2661 if d==2
		replace w_health = -0.0969 if d==1
		
	gen Emp=(status==1)
		
collapse (mean) age asset* worksurplus alpha_int w_alpha* wage* wtr* AIME* allU LTU Utag w_health Emp (max) d newDI newd1 newd2 allDI vocDI WouldWorkExPost WouldWorkNoDI, by(id year)

xtset id year
	gen newDI_Lt = .
	gen newd1_Lt =. 
	gen newd2_Lt = .
	
	foreach var of varlist newDI newd1 newd2 {
		replace `var'_Lt=0 if `var'==1
		forvalues t = 1(1)10 {
			*gen `var'_F= F`t'.`var'
			*gen `var'_F`t'=(L`t'.`var'==1)
		*Continuous is probably more useful
			replace `var'_Lt = -1*`t' if (F`t'.`var'==1)
			replace `var'_Lt = `t' if (L`t'.`var'==1)	
		}
		}

	preserve
	
		collapse (mean) age asset* worksurplus alpha_int w_alpha* wage* wtr* AIME* allU LTU Utag  w_health Emp  (max) d allDI vocDI WouldWorkExPost WouldWorkNoDI, by(newDI_Lt)
		
		rename newDI_Lt time2event
		foreach var of varlist age-WouldWorkNoDI {
			rename `var' DI_`var'
		}
		
		save "paths.dta", replace
		
	restore		
	preserve
	
		collapse (mean) age asset* worksurplus alpha_int w_alpha* wage* wtr* AIME* allU LTU Utag  w_health Emp  (max) d allDI vocDI WouldWorkExPost WouldWorkNoDI, by(newd1_Lt)
		
		rename newd1_Lt time2event
		foreach var of varlist age-WouldWorkNoDI {
			rename `var' d1_`var'
		}
		
			merge 1:1 time2event using "paths.dta" , nogenerate

			save "paths.dta", replace

	restore	
	preserve
	
		collapse (mean) age asset* worksurplus alpha_int w_alpha* wage* wtr* AIME* allU LTU Utag  w_health Emp  (max) d allDI vocDI WouldWorkExPost WouldWorkNoDI, by(newd2_Lt)
		
		rename newd2_Lt time2event
		foreach var of varlist age-WouldWorkNoDI {
			rename `var' d2_`var'
		}
		
			merge 1:1 time2event using "paths.dta" , nogenerate

			save "paths.dta", replace		
		
	restore	
	preserve
	
		keep if (newd1_Lt>-1 & d==1 | d==2)
		collapse (mean) age asset* worksurplus alpha_int w_alpha* wage* wtr* AIME* allU LTU Utag  w_health Emp  (max) d allDI vocDI WouldWorkExPost WouldWorkNoDI, by(newd1_Lt)
		
		rename newd1_Lt time2event
		drop if time2event<0
		foreach var of varlist age-WouldWorkNoDI {
			rename `var' d1stay_`var'
		}
		
			merge 1:1 time2event using "paths.dta" , nogenerate

			save "paths.dta", replace

	restore	
	preserve
	
		keep if (newd2_Lt>-1 &  d==2)
		collapse (mean) age asset* worksurplus alpha_int w_alpha* wage* wtr* AIME* allU LTU Utag  w_health Emp  (max) d allDI vocDI WouldWorkExPost WouldWorkNoDI, by(newd1_Lt)
		
		rename newd1_Lt time2event
		drop if time2event<0
		foreach var of varlist age-WouldWorkNoDI {
			rename `var' d2stay_`var'
		}
		
			merge 1:1 time2event using "paths.dta" , nogenerate

			save "paths.dta", replace		
		
	restore
	
		*Reference group: 50-60 yrs
		keep if (age==3 | age==4)
		collapse (mean) age asset* worksurplus alpha_int w_alpha* wage* wtr* AIME* allU LTU Utag  w_health Emp  (max) d allDI vocDI WouldWorkExPost WouldWorkNoDI
		
		foreach var of varlist age-WouldWorkNoDI {
			rename `var' ref_`var'
		}
			append using "paths.dta"

			save "paths.dta", replace			
	
	

*Plots-----------------------------------------	
cd "C:\Users\ammichau\Desktop\ModelOutputAnalysis"
use "paths.dta", clear

	foreach v of newlist asset asset_quant worksurplus w_alpha w_alpha_quant wage wage_quant wtr wtr_quant AIME AIME_quant allU LTU Utag w_health Emp WouldWorkExPost WouldWorkNoDI {
	egen max_X = max(ref_`v')
	replace ref_`v' = max_X
	drop max_X
	sort time2event
	replace DI_`v' =. if time2event>0
	twoway (line ref_`v' time2event,  lcolor(grey)  ) ///
	 (line DI_`v' time2event, lwidth(thick)  lcolor(black)  ) ///
	 (line d1_`v' time2event, lwidth(thick)  lcolor(blue) lpattern(dash) ) ///
	 (line d1stay_`v' time2event, lwidth(thick)  lcolor(blue)   ) ///
	 (line d2_`v' time2event, lwidth(thick)  lcolor(red) lpattern(dash) ) ///
	 (line d2stay_`v' time2event, lwidth(thick)  lcolor(red)   ), ///
		title(`v') xtitle("Years Prior to Event") ytitle("Average") ///
	 legend(lab(1 "All Age 50-60") lab(2 "New SDDI") lab(3 "Moderate Limitation") lab(4 "Still Moderate Limitation") lab(5 "Severe Limitation") lab(6 "Still Severe Limitation")  ) ///
	 graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) tline(0, lc(red))
	 cd "C:\Users\ammichau\Google Drive\Disability\DisabilityOption\ModelOutputAnalysis\OUT"
		graph export `v'.eps, as(eps) preview(on) replace
		graph save `v', replace		
	 cd "C:\Users\ammichau\Google Drive\Disability\DisabilityOption\ModelOutputAnalysis\Preview"
		graph export `v'.pdf, as(pdf) replace
	}
	
	*wage progression by component
*twoway (line DI_wage time2event,  lwidth(thick)  lcolor(black)   ) ///
	tsset time2event
	replace DI_w_alpha = L1.DI_w_alpha if time2event==0
	twoway (line DI_w_health time2event if time2event<1, lwidth(thick)  lcolor(grey ) lpattern(solid) ) ///
	 (line DI_w_alpha time2event if time2event<1, lwidth(thick)  lcolor(grey) lpattern(dash)) ///
	 (line DI_wtr time2event if time2event<1, lwidth(thick)  lcolor(grey) lpattern(longdash dot)), ///
		title(`v') xtitle("Years Prior to DI Award") ytitle("Average") ///
	 legend( lab(1 "Health Component") lab(2 "Idiosyncratic Component") lab(3 "Occupation Component") ) ///
	 graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) tline(0, lc(red))
	 cd "C:\Users\ammichau\Google Drive\Disability\DisabilityOption\ModelOutputAnalysis\OUT"
		graph save DI_wagecomponent, replace
		graph export DI_wagecomponent.eps, as(eps) preview(on) replace
	 cd "C:\Users\ammichau\Google Drive\Disability\DisabilityOption\ModelOutputAnalysis\Preview"
		graph export DI_wagecomponent.pdf, as(pdf) replace	
	
***************************************************************************************************************
* DO THE WAGE TRENDS LOOK REASONABLE?
*------------------------------------------------------------------------------
cd "C:\Users\ammichau\Desktop\ModelOutputAnalysis"
global OUT_dir "OUT"
global Preview_dir "Preview"
use "DTsim_v2.dta", clear

*Wage quantile variable
forvalues q = 1(1)5 {
		gen wage_quant_q`q'= wage if wage_quant==`q'
	}

*Prime age only
	drop if allDI==1 | age==6 | age==1
	
	collapse (mean) wage wage_quant_q*, by(year)	

tsset year

		twoway (tsline wage_quant_q1) (tsline wage_quant_q2) (tsline wage_quant_q3) /*
	*/ (tsline wage_quant_q4) (tsline wage_quant_q5) if year>1986 ,  /*
	*/  ytitle("Wage") xtitle("Year") legend( lab(1 "Q1") lab(2 "Q2") lab(3 "Q3") lab(4 "Q4") lab(5 "Q5") ) /*
	*/ graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) 	
		graph export $OUT_dir\Wtrend_Occ.eps, as(eps) preview(on) replace
		graph export $OUT_dir\Wtrend_Occ.pdf, as(pdf) replace	

		
