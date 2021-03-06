/*******************************************************************************
								MASTER DO-FILE
********************************************************************************
													   Applied Microeconometrics
															   Empirical Project
																	  Do-File 01
	
		PURPOSE:	Root file that manages the execution of all 
					subordinated do-files.
		
		OUTLINE:	PART 1:	Prepare Folder Paths
					PART 2: Descriptive Analysis
					PART 3: PSM
					PART 4: Robustness Checks 
					
********************************************************************************
			PART 1: Prepare Folder Paths
*******************************************************************************/

	clear all

*------------------------------------------------------------------------------*
*	PART 1.1: Set globals for do-file routines
*------------------------------------------------------------------------------*

//	Adjust root file:	
	global root	"C:/Users/Emilie/Documents/Emilie/Uni/Master/Nottingham/2_Appl_Microeconometrics/fdimatching_clean"

	global input	"$root/01_input"
	global scripts	"$root/02_scripts"
	global log	"$root/03_log"
	global results	"$root/04_results"
	
	use "$input/FDI_project"

*------------------------------------------------------------------------------*
*	PART 1.2: Adjust variable labels
*------------------------------------------------------------------------------*

	label var OWN "Ownership"
	label var TECH "Technology intensity"
	label var PORT "Access to port"
	label var logwages2015 "Log wages"
	label var TFP2015 "TFP"
	label var logemp2015 "Log employment"
	label var DEBTS2015 "Log debts"
	label var EXP2015 "Export intensity"
	label var RD2015 "R&D dummy"
	label var logwages2017 "Log wages"
	label var TFP2017 "TFP"

********************************************************************************
*			PART 2: Descriptive Analysis
********************************************************************************

	cap log close
	log using $log/02_Descriptive_Analysis, replace

			do $scripts/02_Descriptive_Analysis
	
	log close
	translate $log/02_Descriptive_Analysis.smcl $log/02_Descriptive_Analysis.pdf , ///
	trans(smcl2pdf) replace 	
	
	erase $log/02_Descriptive_Analysis.smcl


********************************************************************************
*			PART 3: Matching
********************************************************************************

*------------------------------------------------------------------------------*
*	PART 3.1: PSM (1 neighbour)
*------------------------------------------------------------------------------*
	
	cap log close
	log using $log/03a_PSM, replace

			do $scripts/03a_PSM
	
	log close
	translate $log/03a_PSM.smcl $log/03a_PSM.pdf , ///
	trans(smcl2pdf) replace 	
	
	erase $log/03a_PSM.smcl

*------------------------------------------------------------------------------*
*	PART 3.2: NNM (>1 neighbours, including caliper specifications)
*------------------------------------------------------------------------------*

	cap log close
	log using $log/03b_NNM, replace

			do $scripts/03b_NNM
	
	log close
	translate $log/03b_NNM.smcl $log/03b_NNM.pdf , ///
	trans(smcl2pdf) replace 	
	
	erase $log/03b_NNM.smcl


*------------------------------------------------------------------------------*
*	PART 3.3: AIPW
*------------------------------------------------------------------------------*

	cap log close
	log using $log/03c_AIPW, replace

			do $scripts/03c_AIPW
	
	log close
	translate $log/03c_AIPW.smcl $log/03c_AIPW.pdf , ///
	trans(smcl2pdf) replace 	
	
	erase $log/03c_AIPW.smcl
	
	
********************************************************************************
*			PART 4: Robustness Checks 
********************************************************************************

	cap log close
	log using $log/04a_Robustness, replace

			do $scripts/04a_Robustness
	
	log close
	translate $log/04a_Robustness.smcl $log/04a_Robustness.pdf , ///
	trans(smcl2pdf) replace 	
	
	erase $log/04a_Robustness.smcl



