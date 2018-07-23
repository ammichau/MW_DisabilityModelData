clear all
set more off

cd "C:\Users\ammichau\Desktop\ModelOutputAnalysis"
global OUT_dir "OUT"
global Preview_dir "Preview"
global Diagnos_dir "C:\Users\ammichau\Google Drive\Disability\DisabilityOption\ModelOutputAnalysis\Diagnose"

*simTransdo.do
******************************************************************************
*og: @A.michaud 4/23/2018 for DIoption w/ D.wiczer 
*	this version: @A.michaud 4/23/2018 
*---------------------------------------------------------------------------------------------------
* This file analyzes our model simulated dataset
*	1) Computes how demographics change along the transition
***************************************************************************************************************
* Files needed 
*	-DTsim_v2.dta: created by simDatado
*------------------------------------------------------------------------------

use "DTsim_v2.dta", clear

*regs

gen brn=(newDOB~=.)
gen coT = newDOB-1984

areg asset i.d i.alpha_int i.del i.newDOB if brn==1, absorb(t)

*Diagnosis Plots
histogram newDOB, discrete
	graph export "C:\Users\ammichau\Google Drive\Disability\DisabilityOption\ModelOutputAnalysis\Diagnose\BirthBar.pdf", as(pdf) replace

*Age bin Sizes

preserve
	collapse (sum) counter (mean) asset d w_alpha wage AIME Utag, by(newDOB)

	graph bar (mean) counter, over(newDOB) 
		graph export "C:\Users\ammichau\Google Drive\Disability\DisabilityOption\ModelOutputAnalysis\Diagnose\BirthBar.pdf", as(pdf) replace	
		
	graph bar (mean) asset, over(newDOB)
		graph export "C:\Users\ammichau\Google Drive\Disability\DisabilityOption\ModelOutputAnalysis\Diagnose\BirthAsset.pdf", as(pdf) replace
		
	graph bar (mean) d, over(newDOB)
		graph export "C:\Users\ammichau\Google Drive\Disability\DisabilityOption\ModelOutputAnalysis\Diagnose\BirthHealth.pdf", as(pdf) replace	
		
	graph bar (mean) wage, over(newDOB)
		graph export "C:\Users\ammichau\Google Drive\Disability\DisabilityOption\ModelOutputAnalysis\Diagnose\BirthWage.pdf", as(pdf) replace	
		
	graph bar (mean) w_alpha, over(newDOB)
		graph export "C:\Users\ammichau\Google Drive\Disability\DisabilityOption\ModelOutputAnalysis\Diagnose\BirthAlpha.pdf", as(pdf) replace
		
	graph bar (mean) AIME, over(newDOB)
		graph export "C:\Users\ammichau\Google Drive\Disability\DisabilityOption\ModelOutputAnalysis\Diagnose\BirthAIME.pdf", as(pdf) replace	
		
	graph bar (mean) Utag, over(newDOB)
		graph export "C:\Users\ammichau\Google Drive\Disability\DisabilityOption\ModelOutputAnalysis\Diagnose\BirthUtag.pdf", as(pdf) replace			
	
restore	
	
preserve
	collapse (sum) counter (mean) asset d w_alpha wage AIME Utag, by(age year)
	
	xtset age year
	
	xtline counter
		graph export "C:\Users\ammichau\Google Drive\Disability\DisabilityOption\ModelOutputAnalysis\Diagnose\AgeBar.pdf", as(pdf) replace	
		
	xtline asset
		graph export "C:\Users\ammichau\Google Drive\Disability\DisabilityOption\ModelOutputAnalysis\Diagnose\AgeAsset.pdf", as(pdf) replace
		
	xtline d
		graph export "C:\Users\ammichau\Google Drive\Disability\DisabilityOption\ModelOutputAnalysis\Diagnose\AgeHealth.pdf", as(pdf) replace	
		
	xtline wage
		graph export "C:\Users\ammichau\Google Drive\Disability\DisabilityOption\ModelOutputAnalysis\Diagnose\AgeWage.pdf", as(pdf) replace	
		
	xtline w_alpha
		graph export "C:\Users\ammichau\Google Drive\Disability\DisabilityOption\ModelOutputAnalysis\Diagnose\AgeAlpha.pdf", as(pdf) replace
		
	xtline AIME
		graph export "C:\Users\ammichau\Google Drive\Disability\DisabilityOption\ModelOutputAnalysis\Diagnose\AgeAIME.pdf", as(pdf) replace	
		
	xtline Utag
		graph export "C:\Users\ammichau\Google Drive\Disability\DisabilityOption\ModelOutputAnalysis\Diagnose\AgeUtag.pdf", as(pdf) replace		

	
	
restore	
