/*******************************************************************************
								NNM DO-FILE
********************************************************************************
													   Applied Microeconometrics
															   Empirical Project
																	 Do-File 03b
		
		PURPOSE:	Perform Nearest-Neighbour Matching
		
		OUTLINE:	PART 1:	NN: Wages and TFP--> Table1
					PART 2:	5NN and Caliper: Wages
					PART 3: 5NN and Caliper; IPW; AIPW: TFP --> Table 2
		
														
	
********************************************************************************
					PART 1:	NN, logit;
********************************************************************************
to Install Outreg run: ssc install outreg2
*------------------------------------------------------------------------------*
*	PART 1.2: WAGES
*------------------------------------------------------------------------------*//
cap drop osa1 
	cap drop p1* 
	eststo wages:  cap teffects psmatch (logwages2017) ///
					 (FDI2016 i.OWN /*i.TECH*/ PORT ///
					  logwages2015 TFP2015 logemp2015 DEBTS2015 EXP2015 RD2015, logit),	///
					  osample(osa1) generate(p1)
	
	teffects overlap, ptlevel(1) saving($results/03b_NNM/WAGES_overl_nn1.gph, replace)
	graph export $results/03b_NNM/WAGES_overl_nn1.pdf, as(pdf) replace
	
	
	
	// SD way below 10% for all variables. VR fine.

*------------------------------------------------------------------------------*
*	PART 1.3: TFP2015
*------------------------------------------------------------------------------*
cap drop osa1 
	cap drop p1* 
	eststo TFP: cap teffects psmatch (TFP2017) ///
					 (FDI2016 i.OWN /*i.TECH*/ PORT ///
					  logwages2015 TFP2015 logemp2015 DEBTS2015 EXP2015 RD2015, logit),	///
					  osample(osa1) generate(p1)			  
	
	teffects overlap, ptlevel(1) saving($results/03b_NNM/WAGES_overl_nn1.gph, replace)
	graph export $results/03b_NNM/TFP_overl_nn1.pdf, as(pdf) replace
	tebalance summarize
	
	// Generate Table 1 
	outreg2 [wages TFP] using $results/Table1_wagesTFP.tex, replace dec(3) 
	

********************************************************************************
*					PART 2: 5NN and Caliper .05 [WAGES] Logit
********************************************************************************
//

//	All specifications logit without TECH	

//	Setting globals for interaction terms
	global D "OWN PORT" /*TECH*/
	global C "logwages2015 TFP2015 logemp2015 DEBTS2015 EXP2015 RD2015"
		
*------------------------------------------------------------------------------*
*	PART 2.1: No interactions
*------------------------------------------------------------------------------*	
	
	cap drop osa1 
	cap drop p1* 
	teffects psmatch (logwages2017) ///
					 (FDI2016 i.OWN /*i.TECH*/ PORT ///
					  logwages2015 TFP2015 logemp2015 DEBTS2015 EXP2015 RD2015, logit),	///
					  nneighbor(5) caliper(.05) osample(osa1) generate(p1)
					  // have fewer than 6 propensity-score matches within caliper .05
	
	// Reestimate
	estto NN5: teffects psmatch (logwages2017) ///
					 (FDI2016 i.OWN /*i.TECH*/ PORT ///
					  logwages2015 TFP2015 logemp2015 DEBTS2015 EXP2015 RD2015, logit) if osa1==0,	///
					  nneighbor(5) caliper(.05)  generate(p1) 
	
	teffects overlap, ptlevel(1) saving($results/03b_NNM/WAGES_overl_nn5.gph, replace)
	graph export $results/03b_NNM/WAGES_overl_nn5.pdf, as(pdf) replace
	// Much better overlap
	
	tebalance summarize
	outreg2 using $04_results/031_PSM_wages, replace dec(3) 
	using $results\Table1.tex, replace dec(3) addnote("This is a note") stats(ATE POmean)
	// SD way below 10% for all variables. VR fine.

	
*------------------------------------------------------------------------------*
*	PART 2.2: Including interactions #dc
*------------------------------------------------------------------------------*	
	
	cap drop osa1 
	cap drop p1* 
	cap teffects psmatch (logwages2017) ///
						 (FDI2016 i.($D)##c.($C), logit),	///
						  nneighbor(5) caliper(.05) osample(osa1) generate(p1)
						  // 2 observation with pscore too low
	
	// Reestimate
	teffects psmatch (logwages2017) ///
					 (FDI2016 i.($D)##c.($C), logit) if osa1==0,	///
					  nneighbor(5) caliper(.05) generate(p1) 

	teffects overlap, ptlevel(1) saving($results/03b_NNM/WAGES_overl_nn5#cd.gph, replace)
	graph export $results/03b_NNM/WAGES_overl_nn5#cd.pdf, as(pdf) replace
	// Much better overlap
	
	tebalance summarize
	// SD way below 10% for all variables. VR fine.

	
	
********************************************************************************
*					PART 3: 5NN and Caliper .05 [TFP]
********************************************************************************

*------------------------------------------------------------------------------*
*	PART 2.2: No interactions 
*------------------------------------------------------------------------------*	
*------------------------5NN and Caliper .05-----------------------------------*
	
	cap drop osa1 
	cap drop p1* 
	cap teffects psmatch (TFP2017) ///
						(FDI2016 i.OWN /*i.TECH*/ PORT ///
						logwages2015 TFP2015 logemp2015 DEBTS2015 EXP2015 RD2015, logit),	///
						nneighbor(5) caliper(.05) osample(osa1) generate(p1)
						// 5 observations violate caliper
	 
	// Reestimate
	estto 5NN: teffects psmatch (TFP2017) ///
					 (FDI2016 i.OWN /*i.TECH*/ PORT ///
					  logwages2015 TFP2015 logemp2015 DEBTS2015 EXP2015 RD2015, logit) if osa1==0,	///
					  nneighbor(5) caliper(.05)  generate(p1) 
	
	teffects overlap, ptlevel(1) saving($results/03b_NNM/TFP_overl_nn5.gph, replace)
	graph export $results/03b_NNM/TFP_overl_nn5.pdf, as(pdf) replace
	// Much better overlap
	
	tebalance summarize
	// SD way below 10% for all variables. VR fine.

	
*------------------------IPW---------------------------------------------------*




*------------------------------------------------------------------------------*
*	PART 3.2: Including interactions #dc
*------------------------------------------------------------------------------*	

	cap drop osa1 
	cap drop p1* 
	cap teffects psmatch (TFP2017) ///
						(FDI2016 i.($D)##c.($C), logit),	///
						nneighbor(5) caliper(.05) osample(osa1) generate(p1)
						// 2 observation with pscore too low
	
	//Reesimate 
	teffects psmatch (TFP2017) ///
					 (FDI2016 i.($D)##c.($C), logit) if osa1==0,	///
					  nneighbor(5) caliper(.05) generate(p1) 

	teffects overlap, ptlevel(1) saving($results/03b_NNM/TFP_overl_nn5#cd.gph, replace)
	graph export $results/03b_NNM/TFP_overl_nn5#cd.pdf, as(pdf) replace
	// Much better overlap
	
	tebalance summarize
	// SD way below 10% for all variables. VR fine.	
