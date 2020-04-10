/*******************************************************************************
								AIPW DO-FILE
********************************************************************************
													   Applied Microeconometrics
															   Empirical Project
																	 Do-File 03c
		
		PURPOSE:	Perform AIPW Matching
		
		OUTLINE:	PART 1:	
		
		asdasdsdasdasd												
	
********************************************************************************
					PART 1:	AIPW (THEA part)
*******************************************************************************/
cap gen emp15 = exp(logemp2015)
cap gen TFPS17 =  (TFP2017 -  3.656046) / 2.056464
sum TFPS17
	global S "OWN TECH PORT"	// Dummies with TECH
	global P "logwages2015 TFP2015 emp15  RD2015 DEBTS2015"
	
*------------------------------------------------------------------------------*
*	PART 1.1: IPW, AIPW without type
*------------------------------------------------------------------------------*
cap drop osa1
teffects aipw (TFPS17 )(FDI2016 i.($S) $P  ) ,  osample(osa1)  
tebalance summarize
teffects overlap


cap drop osa1
teffects ipw (TFPS17 )(FDI2016 i.($S) $P  ) , osample(osa1)  
tebalance summarize
teffects overlap

cap drop osa1
teffects ipw (TFPS17 )(FDI2016 i.($S) $P  ) , atet osample(osa1)  
tebalance summarize
teffects overlap


cap drop osa1
cap drop p1*
teffects psmatch (TFPS17)(FDI2016 i.($S) logwages2015 TFP2015 logemp2015  RD2015 DEBTS2015  ) , osample(osa1) generate(p1)
tebalance summarize
teffects overlap



cap drop osa1
teffects aipw (TFP2017 $S $P )(FDI2016   $S $P ) , osample(osa1) 
tebalance summarize
teffects overlap


/********************************************************************************
					PART 2:	AIPW by type  (Georg part)
*******************************************************************************/

	*AIPW by type
	cap drop osa1
	teffects aipw (TFP2017  i.($S) c.($P) )(FDITYPE2016  i.($S) c.($P) ) , osample(osa1)
	outreg2 using $results/04_bytype/bytype_mlog_1.tex, replace dec(3) drop(i.OWN i.PORT logwages2015 TFP2015 logemp2015 DEBTS2015 EXP2015 RD2015 ) nocon eqdrop(OME0 OME1 OME2 OME3 TME1 TME2 TME3) lab()
	tebalance summarize 
	teffects overlap
	graph export $results/04_bytype/overl_mlog_ab.pdf, as(pdf) replace
	*worst is 8% diff for independent, 0.7 for logemp


	*ipw by type
	cap drop osa1
	teffects ipw (TFP2017 )(FDITYPE2016  i.($S) c.($P) ) , osample(osa1) 
	outreg2  using $results/04_bytype/bytype_mlog_1.tex, append dec(3) drop(i.OWN i.PORT logwages2015 TFP2015 logemp2015 DEBTS2015 EXP2015 RD2015 ) nocon eqdrop(OME 0 OME1 OME2 OME3 TME1 TME2 TME3)
	tebalance summarize
	teffects overlap
	graph export $results/04_bytype/overl_mlog_ib.pdf, as(pdf) replace



*seperate logits

*AIPW Logit type1 
cap drop osa1
teffects aipw (TFP2017   i.($S) c.($P) )(FDI2016 c.($P) i.($S)  ) if FDITYPE2016==1 | FDITYPE2016==0, osample(osa1)
outreg2 using $results/04_bytype/bytype_sep_1.tex, replace dec(3) drop(i.OWN i.PORT logwages2015 TFP2015 logemp2015 DEBTS2015 EXP2015 RD2015 ) nocon eqdrop(OME0 OME1 TME1)

*IPW logit type1
cap drop osa1
teffects ipw (TFP2017)  (FDI2016 c.($P) i.($S)  ) if FDITYPE2016==1 | FDITYPE2016==0, osample(osa1)
outreg2 using $results/04_bytype/bytype_sep_1.tex, append dec(3) drop(i.OWN i.PORT logwages2015 TFP2015 logemp2015 DEBTS2015 EXP2015 RD2015 ) nocon eqdrop(TME1)

*AIPW Logit type2
cap drop osa1
teffects aipw (TFP2017   i.($S) c.($P) )(FDI2016 c.($P) i.($S)  ) if FDITYPE2016==2 | FDITYPE2016==0, osample(osa1)
outreg2 using $results/04_bytype/bytype_sep_1.tex, append dec(3) drop(i.OWN i.PORT logwages2015 TFP2015 logemp2015 DEBTS2015 EXP2015 RD2015 ) nocon eqdrop(OME0 OME1 TME1)

*IPW logit type 2
cap drop osa1
teffects ipw (TFP2017)  (FDI2016 c.($P) i.($S)  ) if FDITYPE2016==2 | FDITYPE2016==0, osample(osa1)
outreg2 using $results/04_bytype/bytype_sep_1.tex, append dec(3) drop(i.OWN i.PORT logwages2015 TFP2015 logemp2015 DEBTS2015 EXP2015 RD2015 ) nocon eqdrop(TME1)

*AIPW Logit type3
cap drop osa1
teffects aipw (TFP2017   i.($S) c.($P) )(FDI2016 c.($P) i.($S)  ) if FDITYPE2016==3 | FDITYPE2016==0, osample(osa1)
outreg2 using $results/04_bytype/bytype_sep_1.tex, append dec(3) drop(i.OWN i.PORT logwages2015 TFP2015 logemp2015 DEBTS2015 EXP2015 RD2015 ) nocon eqdrop(OME0 OME1 TME1)

*IPW logit type3
cap drop osa1
teffects ipw (TFP2017)   (FDI2016 c.($P) i.($S)  ) if FDITYPE2016==3 | FDITYPE2016==0, osample(osa1)
outreg2 using $results/04_bytype/bytype_sep_1.tex, append dec(3) drop(i.OWN i.PORT logwages2015 TFP2015 logemp2015 DEBTS2015 EXP2015 RD2015 ) nocon eqdrop(TME1)





*seperate probits with dropping of variables
cap drop osa1
teffects aipw (TFP2017   i.($S) c.($P) )(FDITYPE2016 c.($P) i.($S), probit  ) if FDITYPE2016==1 | FDITYPE2016==0, osample(osa1)
outreg2 using $results/04_bytype/bytype_prob_1.tex, replace dec(3) drop(i.OWN i.PORT logwages2015 TFP2015 logemp2015 DEBTS2015 EXP2015 RD2015 ) nocon eqdrop(OME0 OME1 TME1)

cap drop osa1
teffects aipw (TFP2017   i.($S) c.($P) )(FDITYPE2016 c.($P) i.($S), probit ) if FDITYPE2016==2 | FDITYPE2016==0, osample(osa1)
outreg2 using $results/04_bytype/bytype_prob_1.tex, append dec(3) drop(i.OWN i.PORT logwages2015 TFP2015 logemp2015 DEBTS2015 EXP2015 RD2015 ) nocon eqdrop(OME0 OME1 TME1)

cap drop osa1
teffects aipw (TFP2017   i.($S) c.($P) )(FDITYPE2016 c.($P) i.($S), probit ) if FDITYPE2016==3 | FDITYPE2016==0, osample(osa1)
outreg2 using $results/04_bytype/bytype_prob_1.tex, append dec(3) drop(i.OWN i.PORT logwages2015 TFP2015 logemp2015 DEBTS2015 EXP2015 RD2015 ) nocon eqdrop(OME0 OME1 TME1)

