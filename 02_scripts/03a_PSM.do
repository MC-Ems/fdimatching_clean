/*******************************************************************************
								PSM DO-FILE
********************************************************************************
													   Applied Microeconometrics
															   Empirical Project
																	 Do-File 03a
		
		PURPOSE:	Perform Propensity Score Matching
					Effect of FDI on TFP
		
		OUTLINE:	PART 1:	Complete Model
					PART 2: Improved Model (w/o TECH)
														
	
********************************************************************************
					PART 1:	Complete Model
*******************************************************************************/

// unlogging employment

gen emp15 = exp(logemp2015)


// standardizing TFP

gen TFPST = (TFP2017 -  3.656046) / 2.056464

//	Setting globals for interaction terms
	global F "OWN TECH PORT"	// Dummies with TECH
	global C "logwages2015 TFP2015 emp15 RD2015 EXP2015"

*------------------------------------------------------------------------------*
*	PART 1.1: No interactions
*------------------------------------------------------------------------------*

*========*
* Logit
*========*

*	ATE:
*	----
	cap drop osa1 
	cap drop p1 
	teffects psmatch (TFP2017) ///
					 (FDI2016 i.OWN i.TECH PORT ///
					  logwages2015 TFP2015 logemp2015 DEBTS2015 EXP2015 RD2015),	///
					  osample(osa1) generate(p1)
	// Significant ATE			

	teffects overlap, ptlevel(1) saving($results/03a_PSM/overl_log_comp1.gph, replace)
	graph export $results/03a_PSM/overl_log_comp1.pdf, as(pdf) replace
	// Really bad overlap
	
	tebalance summarize
	// SD catastrophy. VR not good either.
			  
					  
*========*
* Probit
*========*
*	ATE:
*	----	
	cap drop osa1 
	cap drop p1 	
	cap teffects psmatch (TFP2017) ///
					 (FDI2016 i.OWN i.TECH PORT ///
					  logwages2015 TFP2015 logemp2015 DEBTS2015 EXP2015 RD2015, probit),	///
					  osample(osa1) generate(p1)
				   // violation of overlap assumption for 389 obs 	

	// Reestimate			   
	teffects psmatch (TFP2017) ///
					 (FDI2016 i.OWN i.TECH PORT ///
					  logwages2015 TFP2015 logemp2015 DEBTS2015 EXP2015 RD2015, probit)	///
					  if osa1 == 0
					  
	tebalance summarize
	// SD catastrophy. VR not good either.
					  
	teffects overlap, ptlevel(1) saving($results/03a_PSM/overl_prob_comp1.gph, replace)
	graph export $results/03a_PSM/overl_prob_comp1.pdf, as(pdf) replace
	// Really bad overlap

		
*------------------------------------------------------------------------------*
*	PART 1.2: Interacting dummies 
*------------------------------------------------------------------------------*
/* 	From now on only probit, bc. no large differences and previous pscore 
	estimations consistently gave higher R2.								*/
	
*	ATE:
*	----	
	cap drop osa1 
	cap drop p1 	
	cap teffects psmatch (TFP2017) ///
					 (FDI2016 i.($F)##i.($F) $C, probit),	///
					  osample(osa1) generate(p1)
				   // violation of overlap assumption for 415 obs 	

	// Reestimate			   
	teffects psmatch (TFP2017) ///
					 (FDI2016 i.($F)##i.($F) $C, probit)	///
					 if osa1 == 0
					  
	tebalance summarize
	// SD catastrophy. VR not good.
					  
	teffects overlap, ptlevel(1)
	// Really bad overlap

*------------------------------------------------------------------------------*
*	PART 1.3: Interacting continuous variables
*------------------------------------------------------------------------------*

*	ATE:
*	----	
	cap drop osa1 
	cap drop p1 	
	cap teffects psmatch (TFP2017) ///
					 (FDI2016 i.($F) c.($C)##c.($C), probit),	///
					  osample(osa1) generate(p1)
				   // violation of overlap assumption for 517 obs 	

	// Reestimate			   
	teffects psmatch (TFP2017) ///
					 (FDI2016 i.($F) c.($C)##c.($C), probit)	///
					 if osa1 == 0
					  
	tebalance summarize
	// SD catastrophy. VR ok.
					  
	teffects overlap, ptlevel(1)


*------------------------------------------------------------------------------*
*	PART 1.4: Interacting all variables
*------------------------------------------------------------------------------*

*	ATE:
*	----	
	cap drop osa1 
	cap drop p1 	
	cap teffects psmatch (TFP2017) ///
					 (FDI2016 i.($F)##c.($C) i.($F)#i.($F) c.($C)#c.($C), probit),	///
					  osample(osa1) generate(p1)
				   // violation of overlap assumption for 998 obs 	

	// Reestimate			   
	teffects psmatch (TFP2017) ///
					 (FDI2016 i.($F)##c.($C) i.($F)#i.($F) c.($C)#c.($C), probit)	///
					 if osa1 == 0

	tebalance summarize
	// SD catastrophy. VR too.
					  
	teffects overlap, ptlevel(1) 
	// Really bad overlap
	

/*	There is no way of getting good overlap with the complete model, EXP is 
	too good at explaining who gets the treatment. Redo this all with improved
	model (i.e. excluding EXP). 				*/
	
	
	
********************************************************************************
*					PART 2:	Improved Model (w/o TECH)
*******************************************************************************/

//	Setting global for interaction terms
	global D "logwages2015 TFP2015 logemp2015 RD2015"			// Dummies without TECH

*------------------------------------------------------------------------------*
*	PART 2.1: No interactions
*------------------------------------------------------------------------------*	

*========*
* Logit
*========*	
	cap drop osa1 
	cap drop p1 
	teffects psmatch (TFP2017) ///
					 (FDI2016 $D  i.($F) ) ,	///
					  osample(osa1) generate(p1)
					  
	teffects overlap, ptlevel(1) saving($results/03a_PSM/overl_log_noTECH.gph, replace)
	graph export $results/03a_PSM/overl_log_noTECH.pdf, as(pdf) replace
	// Much better overlap
	
	tebalance summarize
	// pretty decent balance and overlap

*========*
* Probit
*========*	
	cap drop osa1 
	cap drop p1 
	teffects psmatch (TFP2017) ///
					 (FDI2016  $D  i.($F) , probit),	///
					  osample(osa1) generate(p1)

	 outreg2 using $results\test_1.tex, replace dec(3)  
					  
	teffects overlap, ptlevel(1) saving($results/03a_PSM/overl_prob_noTECH.gph, replace)
	graph export $results/03a_PSM/overl_prob_noTECH.pdf, as(pdf) replace
	// Much better overlap
	
	tebalance summarize
	// same as logit

	


	
	
	
	
*------------------------------------------------------------------------------*
*	PART 2.2: Interacting dummies
*------------------------------------------------------------------------------*	
	
	cap drop osa1 
	cap drop p1 
	teffects psmatch (TFP2017) ///
					 (FDI2016 i.($F)##i.($F) $D, probit), ///
					  osample(osa1) generate(p1)

	outreg2 using $results\test_1.tex, append dec(3) 	
		 
	teffects overlap, ptlevel(1) saving($results/03a_PSM/overl_prob_noTECH#d.gph, replace)
	graph export $results/03a_PSM/overl_prob_noTECH#d.pdf, as(pdf) replace
	
	tebalance summarize
	// VR of employment bad.

*------------------------------------------------------------------------------*
*	PART 2.3: Interacting continuous variables
*------------------------------------------------------------------------------*	

	cap drop osa1 
	cap drop p1 
	cap teffects psmatch (TFP2017) ///
					 (FDI2016 i.($F) c.($D)##c.($D), probit), ///
					  osample(osa1) generate(p1)
	teffects psmatch (TFP2017) ///
					 (FDI2016 i.($F) c.($D)##c.($D), probit) if osa1==0, ///
					  osample(osa2) generate(p2)
					  
	teffects overlap, ptlevel(1) saving($results/03a_PSM/overl_prob_noTECH#c.gph, replace)
	graph export $results/03a_PSM/overl_prob_noTECH#c.pdf, as(pdf) replace
	
	tebalance summarize
	// emp gets worse

*------------------------------------------------------------------------------*
*	PART 2.4: Interacting all variables
*------------------------------------------------------------------------------*	

	cap drop osa1 
	cap drop p1 
	cap teffects psmatch (TFP2017) ///
					 (FDI2016 i.($F)##c.($D) i.($F)##i.($F) c.($D)##c.($D), logit), ///
					  osample(osa1) generate(p1)
					  // Treatment overlap assumption violated by 1 obs
	
	// Reestimate				  
	teffects psmatch (TFP2017) ///
					 (FDI2016 i.($F)##c.($D) i.($F)#i.($F) c.($D)#c.($D), logit) ///
					  if osa1 == 0
	
	tebalance summarize
	// SD above 10% for some interactions. VR fine.
					  
	teffects overlap, ptlevel(1) saving($results/03a_PSM/overl_prob_noTECH#all.gph, replace)
	graph export $results/03a_PSM/overl_prob_noTECH#all.pdf, as(pdf) replace
	
	