*SumStats_WhoGoes.do
*-------------------v.3-16-2018; @A. Michaud for DIoption w/ DW---------------------------------*
***************************************************************************************************************
* This code computes statistics about DI claims from Model data.
* The analagous file for the PSID is "Disability\DisabilityOption\Data\Calibration\PSID\PostEstimationDos\SumStats_WhoGoes.do"
***************************************************************************************************************
clear all
set more off

cd "C:\Users\ammichau\Desktop\ModelOutputAnalysis"
global OUT_dir "OUT"
global Preview_dir "Preview"
use "DTsim_v2.dta", clear
*************************************************************************************************
*Coding Chunk 1: Weeks unemployed & employed prior 10 years, excluding year before DI
*------------------------------------------------------------------------------------------------
*------------------------------------------------------------------------------------------------		
xtset id t
* Months unemployed
	gen MonthsU = 0

	sort id t
	forvalues m = 13(1)120 {
		by id, sort: replace MonthsU =  MonthsU + L`m'.allU if t>120
		}

* Weeks employed
	gen MonthsE = 108-MonthsU
	
*************************************************************************************************
*Involuntary Job Loss
*------------------------------------------------------------------------------------------------
*------------------------------------------------------------------------------------------------		
	*Ever in past 2 years
	gen InvU5 = 0

	sort id t
	*forvalues m = 1(1)60 {
	*	by id, sort: replace InvU5 = 1 if L`m'.Utag==1 & t>60
   *	}
   		replace InvU5=1 if L12.Utag==1 
		replace InvU5=1 if L24.Utag==1 
		replace InvU5=1 if L36.Utag==1 
		replace InvU5=1 if L48.Utag==1 
		replace InvU5=1 if L60.Utag==1 
	replace InvU5 = . if t<60
			
************

*save "DTsim_Who.dta", replace	
*********************************************************************************
*------------------------------------------------------
*Cleaning Done. Analysis Starts Here.
*------------------------------------------------------
********************************************************************************
egen everDI = max(allDI), by(id) 
gen L1newDI = L1.newDI

drop if age==6
svyset _n

*****************************************************
* Table 2 in paper *
*****************************************************
	tab d if (L1newDI==1)
	tab d if ( age>1 )

	gen riskiestocc=(occ==10 | occ==12 | occ==13 | occ==16)
	tab riskiestocc if (L1newDI==1)
	tab riskiestocc if ( age>1 )
	
	gen lowestwagegrowthocc=(occ==5 | occ==11 | occ==13 | occ==14)
	tab lowestwagegrowthocc if (L1newDI==1)
	tab lowestwagegrowthocc if ( age>1 )
****************************************************	


************
*Income:
*		Want to figure out where SSDI claimaints are in the income distribution.
************	
*Total Labor income	
	by t, sort: egen LabIq = xtile(wage) , nq(5)
	
	*=1 if wage< 20th percentile prior year
		gen LabIncLess20=(LabIq==1)

	*=1 if wage< 20th percentile any of prior 5 years
	xtset id t
	gen y5LabIncLess20= 0
		replace y5LabIncLess20=1 if L12.LabIncLess20==1 
		replace y5LabIncLess20=1 if L24.LabIncLess20==1 
		replace y5LabIncLess20=1 if L36.LabIncLess20==1 
		replace y5LabIncLess20=1 if L48.LabIncLess20==1 
		replace y5LabIncLess20=1 if L60.LabIncLess20==1 		
		*replace y5LabIncLess20 = 0 if (LabIncLess20<1)
		
			replace y5LabIncLess20=. if t<61
*****************************************************
* Table 2 in paper *
*****************************************************
	tab y5LabIncLess20 if (L1newDI==1)
	tab y5LabIncLess20 if ( age>1 & age<5)
****************************************************

*****Tab for DI receipients and general population

	svy: tab y5LabIncLess20 if (L1newDI==1)
			estimate store DILabInc5_newDI	
	svy: tab y5LabIncLess20 if ( age>2)
			estimate store DILabInc5_pop	
	svy: tab y5LabIncLess20 if ( age>2 & t<190 & L1newDI==1)
			estimate store DILabInc5_early
	svy: tab y5LabIncLess20 if ( age>2 & t>190 & L1newDI==1)
			estimate store DILabInc5_late
/*			
esttab DILabInc5_pop DILabInc5_newDI DILabInc5_early DILabInc5_late ///
                   using $OUT_dir\SumStatsDI_labInc.tex ///
		   , se ar2 b(4) stat(N , fmt(0 4 4) labels("Observations" ) ) ///
		    title(MODEL: Labor income less than 20th percentile in any of past 5 years; DI receipients and reference pop.)       ///
		   nodepvars gaps label substitute(\hline\hline \hline \hline "\hline  "  ///
                   "\sym{\sym{\dagger}}" " $^{\dagger}$" "\sym{\sym{*}}" " $^{*}$" "\sym{\sym{**}}" " $^{**}$") ///
		     nonumbers mtitles("Population over 50" "New DI Recipients" "DI prior to 2000" "After 2000") ///
	           star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace	
	*/		   			   
************
*Separations:
*		Want to figure out if SSDI claimaints were involuntary or voluntary separated
************
*Ever InvU
*****************************************************
* Table 2 in paper *
*****************************************************
	tab InvU5 if (L1newDI==1)
	tab InvU5 if ( age>1 & age<5)
****************************************************

	*By DI
	svy: tab InvU if (L1newDI==1)
			estimate store InvU_newDI	
	svy: tab InvU if ( age>2)
			estimate store InvU_pop	
	svy: tab InvU if (  t<190 & L1newDI==1)
			estimate store InvU_early
	svy: tab InvU if (  t>190 & L1newDI==1)
			estimate store InvU_late
	
esttab InvU_pop InvU_newDI InvU_early InvU_late ///
                   using $OUT_dir\SumStatsDI_InvU.tex ///
		   , se ar2 b(4) stat(N , fmt(0 4 4) labels("Observations" ) ) ///
		    title(MODEL: Involuntary Unemployed in any of past 10 years.)       ///
		   nodepvars gaps label substitute(\hline\hline \hline \hline "\hline  "  ///
                   "\sym{\sym{\dagger}}" " $^{\dagger}$" "\sym{\sym{*}}" " $^{*}$" "\sym{\sym{**}}" " $^{**}$") ///
		     nonumbers mtitles("Population over 50" "New DI Recipients" "DI prior to 2000" "After 2000") ///
	           star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace	
			   
			   
************
*Unemployed- 24 months or more out of the past 10 years, excluding year before DI
************
gen U24=(MonthsU>23)
	*By DI
	svy: tab U24 if (L1newDI==1)
			estimate store U24_newDI	
	svy: tab U24 if ( age>2)
			estimate store U24_pop	
	svy: tab U24 if ( age>2 & t<190 & L1newDI==1)
			estimate store U24_early
	svy: tab U24 if ( age>2 & t>190 & L1newDI==1)
			estimate store U24_late
	
esttab U24_pop U24_newDI U24_early U24_late ///
                   using $OUT_dir\SumStatsDI_U24.tex ///
		   , se ar2 b(4) stat(N , fmt(0 4 4) labels("Observations" ) ) ///
		    title(MODEL: Nonemployed 24 months or more over th past 10 years.)       ///
		   nodepvars gaps label substitute(\hline\hline \hline \hline "\hline  "  ///
                   "\sym{\sym{\dagger}}" " $^{\dagger}$" "\sym{\sym{*}}" " $^{*}$" "\sym{\sym{**}}" " $^{**}$") ///
		     nonumbers mtitles("Population over 50" "New DI Recipients" "DI prior to 2000" "After 2000") ///
	           star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace	



************
*Health Status 
************

	*By DI
	svy: tab d if (L1newDI==1)
			estimate store Hlth_newDI	
	svy: tab d if ( age>2)
			estimate store Hlth_pop	
	svy: tab d if ( age>2 & t<190 & L1newDI==1)
			estimate store Hlth_early
	svy: tab d if ( age>2 & t>190 & L1newDI==1)
			estimate store Hlth_late
	
esttab Hlth_pop Hlth_newDI Hlth_early Hlth_late ///
                   using $OUT_dir\SumStatsDI_Hlth.tex ///
		   , se ar2 b(4) stat(N , fmt(0 4 4) labels("Observations" ) ) ///
		    title(MODEL: Health Status.)       ///
		   nodepvars gaps label substitute(\hline\hline \hline \hline "\hline  "  ///
                   "\sym{\sym{\dagger}}" " $^{\dagger}$" "\sym{\sym{*}}" " $^{*}$" "\sym{\sym{**}}" " $^{**}$") ///
		     nonumbers mtitles("Population over 50" "New DI Recipients" "DI prior to 2000" "After 2000") ///
	           star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace	

save "DTsim_Who.dta", replace				   
********************************************************************************
*Repeat Income stuff by breaking into components:
*	-Regression FE and residual
*	-Occupation wage trend
************

xtset id t

xtreg wage i.age if status<4, fe 
	predict wage_FE, u
	predict wage_RESID, residuals

*Lower 20% FE & Resid
    egen wage_FEq = xtile(wage_FE) , nq(5)
	 egen wage_FEqFill = max(wage_FEq), by(id) 
	 replace wage_FEq = wage_FEqFill
	 drop wage_FEqFill

	*=1 if wage< 20th percentile prior year
		gen wage_FELess20temp=(wage_FEq==1)
		egen wage_FELess20 = max(wage_FELess20temp), by(id) 
**HERE 		
	by t, sort: egen wage_RESIDq = xtile(wage_RESID) , nq(5)
	*=1 if wage< 20th percentile prior year
		gen wage_RESIDLess20=(wage_RESIDq==1)
	
	*Five year window	
	sort id t
	gen y5wage_RESIDLess20=  wage_RESIDLess20
		replace y5wage_RESIDLess20=1 if L12.wage_RESIDLess20==1 
		replace y5wage_RESIDLess20=1 if L24.wage_RESIDLess20==1 
		replace y5wage_RESIDLess20=1 if L36.wage_RESIDLess20==1 
		replace y5wage_RESIDLess20=1 if L48.wage_RESIDLess20==1
		replace y5wage_RESIDLess20=1 if L60.wage_RESIDLess20==1 		
		replace y5wage_RESIDLess20 = . if t<61
		
	by t, sort: egen wage_OCCq = xtile(wtr) , nq(5)
	*=1 if wage< 20th percentile prior year
		gen wage_OCCLess20=(wage_OCCq==1)

	*Five year window	
	xtset id t
	
	gen y5wage_OCCLess20=  wage_OCCLess20
		replace y5wage_OCCLess20=1 if L12.wage_OCCLess20==1 
		replace y5wage_OCCLess20=1 if L24.wage_OCCLess20==1 
		replace y5wage_OCCLess20=1 if L36.wage_OCCLess20==1 
		replace y5wage_OCCLess20=1 if L48.wage_OCCLess20==1
		replace y5wage_OCCLess20=1 if L60.wage_OCCLess20==1 			
		replace y5wage_OCCLess20 = . if t<61		

save "DTsim_Who.dta", replace			
*****wage_FE Tab for DI receipients and general population
svyset _n
	svy: tab wage_FELess20 if (L1newDI==1)
			estimate store DIwFE_newDI	
	svy: tab wage_FELess20 if ( age>2)
			estimate store DIwFE_pop	
	svy: tab wage_FELess20 if ( age>2 & t<190 & L1newDI==1)
			estimate store DIwFE_early
	svy: tab wage_FELess20 if ( age>2 & t>190 & L1newDI==1)
			estimate store DIwFE_late
			
esttab DIwFE_pop DIwFE_newDI DIwFE_early DIwFE_late ///
                   using $OUT_dir\SumStatsDI_wFE.tex ///
		   , se ar2 b(4) stat(N , fmt(0 4 4) labels("Observations" ) ) ///
		    title(MODEL: Individual Fixed Effect in labor income less than 20th percentile; DI receipients and reference pop.)       ///
		   nodepvars gaps label substitute(\hline\hline \hline \hline "\hline  "  ///
                   "\sym{\sym{\dagger}}" " $^{\dagger}$" "\sym{\sym{*}}" " $^{*}$" "\sym{\sym{**}}" " $^{**}$") ///
		     nonumbers mtitles("Population over 50" "New DI Recipients" "DI prior to 2000" "After 2000") ///
	           star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace	
			   
*****wage_RESID Tab for DI receipients and general population

	svy: tab y5wage_RESIDLess20 if (L1newDI==1)
			estimate store DIwRESID_newDI	
	svy: tab y5wage_RESIDLess20 if ( age>2)
			estimate store DIwRESID_pop	
	svy: tab y5wage_RESIDLess20 if ( age>2 & t<190 & L1newDI==1)
			estimate store DIwRESID_early
	svy: tab y5wage_RESIDLess20 if ( age>2 & t>190 & L1newDI==1)
			estimate store DIwRESID_late
			
esttab DIwRESID_pop DIwRESID_newDI DIwRESID_early DIwRESID_late ///
                   using $OUT_dir\SumStatsDI_wRESID.tex ///
		   , se ar2 b(4) stat(N , fmt(0 4 4) labels("Observations" ) ) ///
		    title(MODEL: Residual labor income less than 20th percentile in any of past 5 years; DI receipients and reference pop.)       ///
		   nodepvars gaps label substitute(\hline\hline \hline \hline "\hline  "  ///
                   "\sym{\sym{\dagger}}" " $^{\dagger}$" "\sym{\sym{*}}" " $^{*}$" "\sym{\sym{**}}" " $^{**}$") ///
		     nonumbers mtitles("Population over 50" "New DI Recipients" "DI prior to 2000" "After 2000") ///
	           star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace	
			   
*****Tab for DI receipients and general population

	svy: tab y5wage_OCCLess20 if (L1newDI==1)
			estimate store wOCC_newDI	
	svy: tab y5wage_OCCLess20 if ( age>2)
			estimate store wOCC_pop	
	svy: tab y5wage_OCCLess20 if ( age>2 & t<190 & L1newDI==1)
			estimate store wOCC_early
	svy: tab y5wage_OCCLess20 if ( age>2 & t>190 & L1newDI==1)
			estimate store wOCC_late
			
esttab wOCC_pop wOCC_newDI wOCC_early wOCC_late ///
                   using $OUT_dir\SumStatsDI_labInc.tex ///
		   , se ar2 b(4) stat(N , fmt(0 4 4) labels("Observations" ) ) ///
		    title(MODEL: Occupation component of labor income less than 20th percentile in any of past 5 years; DI receipients and reference pop.)       ///
		   nodepvars gaps label substitute(\hline\hline \hline \hline "\hline  "  ///
                   "\sym{\sym{\dagger}}" " $^{\dagger}$" "\sym{\sym{*}}" " $^{*}$" "\sym{\sym{**}}" " $^{**}$") ///
		     nonumbers mtitles("Population over 50" "New DI Recipients" "DI prior to 2000" "After 2000") ///
	           star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace				   

save "DTsim_Who.dta", replace				   
*********************************************************************************
*------------------------------------------------------
*GRAPHICS- Pretty picture time!! 
*------------------------------------------------------
********************************************************************************	
use "C:\Users\ammichau\Desktop\ModelOutputAnalysis\DTsim_Who.dta" , clear
*More interpretable measure of health risk: % in d=2 @ age 60 by occupation. 

egen max_d = max(d) if age<6, by(id) 
	gen max_d2 = max_d
	replace max_d2=0 if max_d2==1
egen Occd2rate = mean(max_d2), by(occ)
	drop max_d2
	gen max_d1 = max_d
	replace max_d1=1 if max_d==2
egen Occd1rate = mean(max_d1), by(occ)	
    drop max_d1

	egen d2q = xtile(Occd2rate) , nq(5)	
	
*Make labels
	
        lab def Qlabel 1 "Q1- Lowest"
		lab def Qlabel 2 "Q2", add
		lab def Qlabel 3 "Q3", add
		lab def Qlabel 4 "Q4", add
        lab def Qlabel 5 "Q5- Highest", add
		
		foreach l of varlist LabIq wage_FEq wage_RESIDq wage_OCCq d2q {
			label values `l' Qlabel
		}
		
		lab def YNlabel 1 "Yes"
		lab def YNlabel 2 "No", add
		foreach l of varlist InvU U24 {
			label values `l' YNlabel
		}	

		label variable d2q   "Work Limitation Risk- Quintile" 
		label variable LabIq   "Wage- Quintile" 
		label variable InvU   "Involuntary U in past 5 yrs" 
		label variable U24   "Unemployed >20% of last 10 yrs" 
		label variable wage_FEq   "Individual FE Component of Wage- Quintile" 
		label variable wage_RESIDq   "Residual Component of Wage- Quintile" 
		label variable wage_OCCq   "Occ Time Trend Component of Wage- Quintile" 
		
			   
*Put things into matrices
	tab d2q LabIq if (L1newDI==1), cell matcell(aHlth_LabIq_DI) nofreq	
		sum LabIq if (L1newDI==1)
		sca n = r(N)
		mat Hlth_LabIq_DI = 100*aHlth_LabIq_DI/n
		matrix colnames Hlth_LabIq_DI = Q1 Q2 Q3 Q4 Q5
		matrix rownames Hlth_LabIq_DI = Q1 Q2 Q3 Q4 Q5
				
	tab d2q LabIq  if ((age==4 | age==5) & status<4), cell matcell(aHlth_LabIq) nofreq
		sum LabIq if ((age==4 | age==5) & status<4)
		sca n = r(N)
		mat Hlth_LabIq = 100*aHlth_LabIq/n
		matrix colnames Hlth_LabIq = Q1 Q2 Q3 Q4 Q5
		matrix rownames Hlth_LabIq = Q1 Q2 Q3 Q4 Q5
		
	tab d2q InvU  if (L1newDI==1), cell matcell(aHlth_InvU_DI)	nofreq
		sum InvU if (L1newDI==1)
		sca n = r(N)
		mat Hlth_InvU_DI = 100*aHlth_InvU_DI/n
		matrix colnames Hlth_InvU_DI = No Yes
		matrix rownames Hlth_InvU_DI = Q1 Q2 Q3 Q4 Q5
		
	tab d2q InvU  if ((age==4 | age==5) & status<4), cell matcell(aHlth_InvU) nofreq
		sum InvU if ((age==4 | age==5) & status<4)
		sca n = r(N)
		mat Hlth_InvU = 100*aHlth_InvU/n
		matrix colnames Hlth_InvU = No Yes
		matrix rownames Hlth_InvU = Q1 Q2 Q3 Q4 Q5
		
	tab d2q U24  if (L1newDI==1), cell matcell(aHlth_U24_DI) nofreq	
		sum U24 if (L1newDI==1)
		sca n = r(N)
		mat Hlth_U24_DI = 100*aHlth_U24_DI/n
		matrix colnames Hlth_U24_DI = No Yes
		matrix rownames Hlth_U24_DI = Q1 Q2 Q3 Q4 Q5
		
	tab d2q U24  if ((age==4 | age==5) & status<4), cell matcell(aHlth_U24) nofreq
		sum U24 if ((age==4 | age==5) & status<4)
		sca n = r(N)
		mat Hlth_U24 = 100*aHlth_U24/n
		matrix colnames Hlth_U24 = No Yes
		matrix rownames Hlth_U24 = Q1 Q2 Q3 Q4 Q5
		
	tab d2q wage_FEq  if (L1newDI==1), cell matcell(aHlth_wage_FEq_DI)	nofreq
		sum wage_FEq if (L1newDI==1)
		sca n = r(N)
		mat Hlth_wage_FEq_DI = 100*aHlth_wage_FEq_DI/n
		matrix colnames Hlth_wage_FEq_DI = Q1 Q2 Q3 Q4 Q5
		matrix rownames Hlth_wage_FEq_DI = Q1 Q2 Q3 Q4 Q5	
		
	tab d2q wage_FEq  if ((age==4 | age==5) & status<4), cell matcell(aHlth_wage_FEq) nofreq
		sum wage_FEq if ((age==4 | age==5) & status<4)
		sca n = r(N)
		mat Hlth_wage_FEq = 100*aHlth_wage_FEq/n
		matrix colnames Hlth_wage_FEq = Q1 Q2 Q3 Q4 Q5
		matrix rownames Hlth_wage_FEq = Q1 Q2 Q3 Q4 Q5
		
	tab d2q wage_RESIDq  if (L1newDI==1), cell matcell(aHlth_wage_RESIDq_DI)	nofreq
		sum wage_RESIDq if (L1newDI==1)
		sca n = r(N)
		mat Hlth_wage_RESIDq_DI = 100*aHlth_wage_RESIDq_DI/n
		matrix colnames Hlth_wage_RESIDq_DI = Q1 Q2 Q3 Q4 Q5
		matrix rownames Hlth_wage_RESIDq_DI = Q1 Q2 Q3 Q4 Q5	
		
	tab d2q wage_RESIDq  if ((age==4 | age==5) & status<4), cell matcell(aHlth_wage_RESIDq) nofreq
		sum wage_RESIDq if ((age==4 | age==5) & status<4)
		sca n = r(N)
		mat Hlth_wage_RESIDq = 100*aHlth_wage_RESIDq/n
		matrix colnames Hlth_wage_RESIDq = Q1 Q2 Q3 Q4 Q5
		matrix rownames Hlth_wage_RESIDq = Q1 Q2 Q3 Q4 Q5	
		
	tab d2q wage_OCCq  if (L1newDI==1), cell matcell(aHlth_wage_OCCq_DI)	nofreq
		sum wage_OCCq if (L1newDI==1)
		sca n = r(N)
		mat Hlth_wage_OCCq_DI = 100*aHlth_wage_OCCq_DI/n
		matrix colnames Hlth_wage_OCCq_DI = Q1 Q2 Q3 Q4 Q5
		matrix rownames Hlth_wage_OCCq_DI = Q1 Q2 Q3 Q4 Q5	
		
	tab d2q wage_OCCq  if ((age==4 | age==5) & status<4), cell matcell(aHlth_wage_OCCq)	nofreq
		sum wage_OCCq if ((age==4 | age==5) & status<4)
		sca n = r(N)
		mat Hlth_wage_OCCq = 100*aHlth_wage_OCCq/n
		matrix colnames Hlth_wage_OCCq = Q1 Q2 Q3 Q4 Q5
		matrix rownames Hlth_wage_OCCq = Q1 Q2 Q3 Q4 Q5		
		

		
		
*Pretty Graphs!
ssc install plotmatrix
*matrix list Hlth_LabIq_DI
*matrix drop _all
* split( 0(1)20 )
		plotmatrix, mat(Hlth_LabIq_DI) color(green) ytitle("Work Limit Risk") xtitle("Wage") split(0 1 2 3 4 5 10 15 20 30 40 50 70 90 ) ///
										allcolors(green*0 green*0.05 green*0.15 green*0.2 green*0.3 green*0.4 green*0.5 green*0.6 green*0.7 green*0.8 green*0.9 green*1 red*1) 
			graph export $OUT_dir\D_Hlth_LabIq_DI.eps, as(eps) preview(on) replace
			graph export $OUT_dir\D_Hlth_LabIq_DI.pdf, as(pdf) replace		
		plotmatrix, mat(Hlth_LabIq) color(green) ytitle("Work Limit Risk") xtitle("Wage") split(0 1 2 3 4 5 10 15 20 30 40 50 70 90 ) ///
										allcolors(green*0 green*0.05 green*0.15 green*0.2 green*0.3 green*0.4 green*0.5 green*0.6 green*0.7 green*0.8 green*0.9 green*1 red*1) 
			graph export $OUT_dir\D_Hlth_LabIq.eps, as(eps) preview(on) replace
			graph export $OUT_dir\D_Hlth_LabIq.pdf, as(pdf) replace		  

		plotmatrix, mat(Hlth_InvU_DI) color(green) ytitle("Work Limit Risk") xtitle("Involuntary Unemployment") split(0 1 2 3 4 5 10 15 20 30 40 50 70 90 ) ///
										allcolors(green*0 green*0.05 green*0.15 green*0.2 green*0.3 green*0.4 green*0.5 green*0.6 green*0.7 green*0.8 green*0.9 green*1 red*1) 
			graph export $OUT_dir\D_Hlth_InvU_DI.eps, as(eps) preview(on) replace
			graph export $OUT_dir\D_Hlth_InvU_DI.pdf, as(pdf) replace		
		plotmatrix, mat(Hlth_InvU) color(green) ytitle("Work Limit Risk") xtitle("Involuntary Unemployment") split(0 1 2 3 4 5 10 15 20 30 40 50 70 90 ) ///
										allcolors(green*0 green*0.05 green*0.15 green*0.2 green*0.3 green*0.4 green*0.5 green*0.6 green*0.7 green*0.8 green*0.9 green*1 red*1) 
			graph export $OUT_dir\D_Hlth_InvU.eps, as(eps) preview(on) replace
			graph export $OUT_dir\D_Hlth_InvU.pdf, as(pdf) replace		
			
		plotmatrix, mat(Hlth_U24_DI) color(green) ytitle("Work Limit Risk") xtitle("Unemployed >20% Last 10 Yrs") split(0 1 2 3 4 5 10 15 20 30 40 50 70 90 ) ///
										allcolors(green*0 green*0.05 green*0.15 green*0.2 green*0.3 green*0.4 green*0.5 green*0.6 green*0.7 green*0.8 green*0.9 green*1 red*1) 
			graph export $OUT_dir\D_Hlth_U24_DI.eps, as(eps) preview(on) replace
			graph export $OUT_dir\D_Hlth_U24_DI.pdf, as(pdf) replace		
		plotmatrix, mat(Hlth_U24) color(green) ytitle("Work Limit Risk") xtitle("Unemployed >20% Last 10 Yrs") split(0 1 2 3 4 5 10 15 20 30 40 50 70 90 ) ///
										allcolors(green*0 green*0.05 green*0.15 green*0.2 green*0.3 green*0.4 green*0.5 green*0.6 green*0.7 green*0.8 green*0.9 green*1 red*1) 
			graph export $OUT_dir\D_Hlth_U24.eps, as(eps) preview(on) replace
			graph export $OUT_dir\D_Hlth_U24.pdf, as(pdf) replace		
			
		plotmatrix, mat(Hlth_wage_FEq_DI) color(green) ytitle("Work Limit Risk") xtitle("Individual Fixed Effect- Wage") split(0 1 2 3 4 5 10 15 20 30 40 50 70 90 ) ///
										allcolors(green*0 green*0.05 green*0.15 green*0.2 green*0.3 green*0.4 green*0.5 green*0.6 green*0.7 green*0.8 green*0.9 green*1 red*1) 
			graph export $OUT_dir\D_Hlth_wage_FEq_DI.eps, as(eps) preview(on) replace
			graph export $OUT_dir\D_Hlth_wage_FEq_DI.pdf, as(pdf) replace		
		plotmatrix, mat(Hlth_wage_FEq) color(green) ytitle("Work Limit Risk") xtitle("Individual Fixed Effect- Wage") split(0 1 2 3 4 5 10 15 20 30 40 50 70 90 ) ///
										allcolors(green*0 green*0.05 green*0.15 green*0.2 green*0.3 green*0.4 green*0.5 green*0.6 green*0.7 green*0.8 green*0.9 green*1 red*1) 
			graph export $OUT_dir\D_Hlth_wage_FEq.eps, as(eps) preview(on) replace
			graph export $OUT_dir\D_Hlth_wage_FEq.pdf, as(pdf) replace		
			
		plotmatrix, mat(Hlth_wage_RESIDq_DI) color(green) ytitle("Work Limit Risk") xtitle("Residual Wage") split(0 1 2 3 4 5 10 15 20 30 40 50 70 90 ) ///
										allcolors(green*0 green*0.05 green*0.15 green*0.2 green*0.3 green*0.4 green*0.5 green*0.6 green*0.7 green*0.8 green*0.9 green*1 red*1) 
			graph export $OUT_dir\D_Hlth_wage_RESIDq_DI.eps, as(eps) preview(on) replace
			graph export $OUT_dir\D_Hlth_wage_RESIDq_DI.pdf, as(pdf) replace		
		plotmatrix, mat(Hlth_wage_RESIDq) color(green) ytitle("Work Limit Risk") xtitle("Residual Wage") split(0 1 2 3 4 5 10 15 20 30 40 50 70 90 ) ///
										allcolors(green*0 green*0.05 green*0.15 green*0.2 green*0.3 green*0.4 green*0.5 green*0.6 green*0.7 green*0.8 green*0.9 green*1 red*1) 
			graph export $OUT_dir\D_Hlth_wage_RESIDq.eps, as(eps) preview(on) replace
			graph export $OUT_dir\D_Hlth_wage_RESIDq.pdf, as(pdf) replace		
			
		plotmatrix, mat(Hlth_wage_OCCq_DI) color(green) ytitle("Work Limit Risk") xtitle("Occupation Wage Trend") split(0 1 2 3 4 5 10 15 20 30 40 50 70 90 ) ///
										allcolors(green*0 green*0.05 green*0.15 green*0.2 green*0.3 green*0.4 green*0.5 green*0.6 green*0.7 green*0.8 green*0.9 green*1 red*1) 
			graph export $OUT_dir\D_Hlth_wage_OCCq_DI.eps, as(eps) preview(on) replace
			graph export $OUT_dir\D_Hlth_wage_OCCq_DI.pdf, as(pdf) replace		
		plotmatrix, mat(Hlth_wage_OCCq) color(green) ytitle("Work Limit Risk") xtitle("Occupation Wage Trend") split(0 1 2 3 4 5 10 15 20 30 40 50 70 90 ) ///
										allcolors(green*0 green*0.05 green*0.15 green*0.2 green*0.3 green*0.4 green*0.5 green*0.6 green*0.7 green*0.8 green*0.9 green*1 red*1) 
			graph export $OUT_dir\D_Hlth_wage_OCCq.eps, as(eps) preview(on) replace
			graph export $OUT_dir\D_Hlth_wage_OCCq.pdf, as(pdf) replace					

**************************************************************************************************************************************			
**************************************************************************************************************************************
*Repeat by actual health status, not risk
**************************************************************************************************************************************
**************************************************************************************************************************************

	tab d LabIq if (L1newDI==1), cell matcell(ad3_LabIq_DI) nofreq	
		sum LabIq if (L1newDI==1)
		sca n = r(N)
		mat d3_LabIq_DI = 100*ad3_LabIq_DI/n
		matrix colnames d3_LabIq_DI = Q1 Q2 Q3 Q4 Q5
		matrix rownames d3_LabIq_DI = None Moderate Severe
				
	tab d LabIq  if ((age==4 | age==5) & status<4), cell matcell(ad3_LabIq) nofreq
		sum LabIq if ((age==4 | age==5) & status<4)
		sca n = r(N)
		mat d3_LabIq = 100*ad3_LabIq/n
		matrix colnames d3_LabIq = Q1 Q2 Q3 Q4 Q5
		matrix rownames d3_LabIq = None Moderate Severe
		
	tab d InvU  if (L1newDI==1), cell matcell(ad3_InvU_DI)	nofreq
		sum InvU if (L1newDI==1)
		sca n = r(N)
		mat d3_InvU_DI = 100*ad3_InvU_DI/n
		matrix colnames d3_InvU_DI = No Yes
		matrix rownames d3_InvU_DI = None Moderate Severe
		
	tab d InvU  if ((age==4 | age==5) & status<4), cell matcell(ad3_InvU) nofreq
		sum InvU if ((age==4 | age==5) & status<4)
		sca n = r(N)
		mat d3_InvU = 100*ad3_InvU/n
		matrix colnames d3_InvU = No Yes
		matrix rownames d3_InvU = None Moderate Severe
		
	tab d U24  if (L1newDI==1), cell matcell(ad3_U24_DI) nofreq	
		sum U24 if (L1newDI==1)
		sca n = r(N)
		mat d3_U24_DI = 100*ad3_U24_DI/n
		matrix colnames d3_U24_DI = No Yes
		matrix rownames d3_U24_DI = None Moderate Severe
		
	tab d U24  if ((age==4 | age==5) & status<4), cell matcell(ad3_U24) nofreq
		sum U24 if ((age==4 | age==5) & status<4)
		sca n = r(N)
		mat d3_U24 = 100*ad3_U24/n
		matrix colnames d3_U24 = No Yes
		matrix rownames d3_U24 = None Moderate Severe
		
	tab d wage_FEq  if (L1newDI==1), cell matcell(ad3_wage_FEq_DI)	nofreq
		sum wage_FEq if (L1newDI==1)
		sca n = r(N)
		mat d3_wage_FEq_DI = 100*ad3_wage_FEq_DI/n
		matrix colnames d3_wage_FEq_DI = Q1 Q2 Q3 Q4 Q5
		matrix rownames d3_wage_FEq_DI = None Moderate Severe
		
	tab d wage_FEq  if ((age==4 | age==5) & status<4), cell matcell(ad3_wage_FEq) nofreq
		sum wage_FEq if ((age==4 | age==5) & status<4)
		sca n = r(N)
		mat d3_wage_FEq = 100*ad3_wage_FEq/n
		matrix colnames d3_wage_FEq = Q1 Q2 Q3 Q4 Q5
		matrix rownames d3_wage_FEq = None Moderate Severe
		
	tab d wage_RESIDq  if (L1newDI==1), cell matcell(ad3_wage_RESIDq_DI)	nofreq
		sum wage_RESIDq if (L1newDI==1)
		sca n = r(N)
		mat d3_wage_RESIDq_DI = 100*ad3_wage_RESIDq_DI/n
		matrix colnames d3_wage_RESIDq_DI = Q1 Q2 Q3 Q4 Q5
		matrix rownames d3_wage_RESIDq_DI = None Moderate Severe	
		
	tab d wage_RESIDq  if ((age==4 | age==5) & status<4), cell matcell(ad3_wage_RESIDq) nofreq
		sum wage_RESIDq if ((age==4 | age==5) & status<4)
		sca n = r(N)
		mat d3_wage_RESIDq = 100*ad3_wage_RESIDq/n
		matrix colnames d3_wage_RESIDq = Q1 Q2 Q3 Q4 Q5
		matrix rownames d3_wage_RESIDq = None Moderate Severe
		
	tab d wage_OCCq  if (L1newDI==1), cell matcell(ad3_wage_OCCq_DI)	nofreq
		sum wage_OCCq if (L1newDI==1)
		sca n = r(N)
		mat d3_wage_OCCq_DI = 100*ad3_wage_OCCq_DI/n
		matrix colnames d3_wage_OCCq_DI = Q1 Q2 Q3 Q4 Q5
		matrix rownames d3_wage_OCCq_DI = None Moderate Severe	
		
	tab d wage_OCCq  if ((age==4 | age==5) & status<4), cell matcell(ad3_wage_OCCq)	nofreq
		sum wage_OCCq if ((age==4 | age==5) & status<4)
		sca n = r(N)
		mat d3_wage_OCCq = 100*ad3_wage_OCCq/n
		matrix colnames d3_wage_OCCq = Q1 Q2 Q3 Q4 Q5
		matrix rownames d3_wage_OCCq = None Moderate Severe		
		

		
		
*Pretty Graphs!
*matrix list d3_LabIq_DI
*matrix drop _all
* split( 0(1)20 )
		plotmatrix, mat(d3_LabIq_DI) color(green) ytitle("Work Limitation") xtitle("Wage") split(0 1 2 3 4 5 10 15 20 30 40 50 70 90 ) ///
										allcolors(green*0 green*0.05 green*0.15 green*0.2 green*0.3 green*0.4 green*0.5 green*0.6 green*0.7 green*0.8 green*0.9 green*1 red*1) 
			graph export $OUT_dir\D_d3_LabIq_DI.eps, as(eps) preview(on) replace
			graph export $OUT_dir\D_d3_LabIq_DI.pdf, as(pdf) replace		
		plotmatrix, mat(d3_LabIq) color(green) ytitle("Work Limitation") xtitle("Wage") split(0 1 2 3 4 5 10 15 20 30 40 50 60 70 ) ///
										allcolors(green*0 green*0.05 green*0.15 green*0.2 green*0.3 green*0.4 green*0.5 green*0.6 green*0.7 green*0.8 green*0.9 green*1 red*1) 
			graph export $OUT_dir\D_d3_LabIq.eps, as(eps) preview(on) replace
			graph export $OUT_dir\D_d3_LabIq.pdf, as(pdf) replace		  

		plotmatrix, mat(d3_InvU_DI) color(green) ytitle("Work Limitation") xtitle("Involuntary Unemployment")  split(0 1 2 3 4 5 10 15 20 30 40 50 70 90 ) ///
										allcolors(green*0 green*0.05 green*0.15 green*0.2 green*0.3 green*0.4 green*0.5 green*0.6 green*0.7 green*0.8 green*0.9 green*1 red*1) 
			graph export $OUT_dir\D_d3_InvU_DI.eps, as(eps) preview(on) replace
			graph export $OUT_dir\D_d3_InvU_DI.pdf, as(pdf) replace		
		plotmatrix, mat(d3_InvU) color(green) ytitle("Work Limitation") xtitle("Involuntary Unemployment")  split(0 1 2 3 4 5 10 15 20 30 40 50 70 90 ) ///
										allcolors(green*0 green*0.05 green*0.15 green*0.2 green*0.3 green*0.4 green*0.5 green*0.6 green*0.7 green*0.8 green*0.9 green*1 red*1) 
			graph export $OUT_dir\D_d3_InvU.eps, as(eps) preview(on) replace
			graph export $OUT_dir\D_d3_InvU.pdf, as(pdf) replace		
			
		plotmatrix, mat(d3_U24_DI) color(green) ytitle("Work Limitation") xtitle("Unemployed >20% Last 10 Yrs") split(0 1 2 3 4 5 10 15 20 30 40 50 70 90 ) ///
										allcolors(green*0 green*0.05 green*0.15 green*0.2 green*0.3 green*0.4 green*0.5 green*0.6 green*0.7 green*0.8 green*0.9 green*1 red*1) 
			graph export $OUT_dir\D_d3_U24_DI.eps, as(eps) preview(on) replace
			graph export $OUT_dir\D_d3_U24_DI.pdf, as(pdf) replace		
		plotmatrix, mat(d3_U24) color(green) ytitle("Work Limitation") xtitle("Unemployed >20% Last 10 Yrs") split(0 1 2 3 4 5 10 15 20 30 40 50 70 90 ) ///
										allcolors(green*0 green*0.05 green*0.15 green*0.2 green*0.3 green*0.4 green*0.5 green*0.6 green*0.7 green*0.8 green*0.9 green*1 red*1) 
			graph export $OUT_dir\D_d3_U24.eps, as(eps) preview(on) replace
			graph export $OUT_dir\D_d3_U24.pdf, as(pdf) replace		
			
		plotmatrix, mat(d3_wage_FEq_DI) color(green) ytitle("Work Limitation") xtitle("Individual Fixed Effect- Wage") split(0 1 2 3 4 5 10 15 20 30 40 50 70 90 ) ///
										allcolors(green*0 green*0.05 green*0.15 green*0.2 green*0.3 green*0.4 green*0.5 green*0.6 green*0.7 green*0.8 green*0.9 green*1 red*1) 
			graph export $OUT_dir\D_d3_wage_FEq_DI.eps, as(eps) preview(on) replace
			graph export $OUT_dir\D_d3_wage_FEq_DI.pdf, as(pdf) replace		
		plotmatrix, mat(d3_wage_FEq) color(green) ytitle("Work Limitation") xtitle("Individual Fixed Effect- Wage") split(0 1 2 3 4 5 10 15 20 30 40 50 70 90 ) ///
										allcolors(green*0 green*0.05 green*0.15 green*0.2 green*0.3 green*0.4 green*0.5 green*0.6 green*0.7 green*0.8 green*0.9 green*1 red*1) 
			graph export $OUT_dir\D_d3_wage_FEq.eps, as(eps) preview(on) replace
			graph export $OUT_dir\D_d3_wage_FEq.pdf, as(pdf) replace		
			
		plotmatrix, mat(d3_wage_RESIDq_DI) color(green) ytitle("Work Limitation") xtitle("Residual Wage") split(0 1 2 3 4 5 10 15 20 30 40 50 70 90 ) ///
										allcolors(green*0 green*0.05 green*0.15 green*0.2 green*0.3 green*0.4 green*0.5 green*0.6 green*0.7 green*0.8 green*0.9 green*1 red*1) 
			graph export $OUT_dir\D_d3_wage_RESIDq_DI.eps, as(eps) preview(on) replace
			graph export $OUT_dir\D_d3_wage_RESIDq_DI.pdf, as(pdf) replace		
		plotmatrix, mat(d3_wage_RESIDq) color(green) ytitle("Work Limitation") xtitle("Residual Wage") split(0 1 2 3 4 5 10 15 20 30 40 50 70 90 ) ///
										allcolors(green*0 green*0.05 green*0.15 green*0.2 green*0.3 green*0.4 green*0.5 green*0.6 green*0.7 green*0.8 green*0.9 green*1 red*1) 
			graph export $OUT_dir\D_d3_wage_RESIDq.eps, as(eps) preview(on) replace
			graph export $OUT_dir\D_d3_wage_RESIDq.pdf, as(pdf) replace		
			
		plotmatrix, mat(d3_wage_OCCq_DI) color(green) ytitle("Work Limitation") xtitle("Occupation Wage Trend") split(0 1 2 3 4 5 10 15 20 30 40 50 70 90 ) ///
										allcolors(green*0 green*0.05 green*0.15 green*0.2 green*0.3 green*0.4 green*0.5 green*0.6 green*0.7 green*0.8 green*0.9 green*1 red*1) 
			graph export $OUT_dir\D_d3_wage_OCCq_DI.eps, as(eps) preview(on) replace
			graph export $OUT_dir\D_d3_wage_OCCq_DI.pdf, as(pdf) replace		
		plotmatrix, mat(d3_wage_OCCq) color(green) ytitle("Work Limitation") xtitle("Occupation Wage Trend") split(0 1 2 3 4 5 10 15 20 30 40 50 70 90 ) ///
										allcolors(green*0 green*0.05 green*0.15 green*0.2 green*0.3 green*0.4 green*0.5 green*0.6 green*0.7 green*0.8 green*0.9 green*1 red*1) 
			graph export $OUT_dir\D_d3_wage_OCCq.eps, as(eps) preview(on) replace
			graph export $OUT_dir\D_d3_wage_OCCq.pdf, as(pdf) replace					
