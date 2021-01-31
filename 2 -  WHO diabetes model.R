setwd("~/Box/Research/Research projects/WHO diabetes")
rm(list=ls())
library(tidyverse)

load("dfimp")
cost = read_csv("cost.csv")
cnt_reg = read_csv("cnt_reg.csv")

dfrep = do.call("rbind", replicate(25, dfimp, simplify = FALSE))


dxdf = dfrep %>%
  mutate(detected=(und_dia==0)) %>%
  group_by(Country) %>%
  mutate(positionInCategory = 1:n(),
         current_level = sum(und_dia==0)/n()) %>%
  ungroup()


# PEN coverage just for diagnosed subset
start_time <- Sys.time()

for (screens in  1:3){


screening_goal = ifelse(screens==1,0,ifelse(screens==2,0.6,0.8))
newscreening  = (screening_goal-dxdf$current_level)
newscreening[newscreening<0]= 0

dxdf$newlydetected = 1*(dxdf$detected==1)+newscreening*(dxdf$detected==0)

dxdf = left_join(dxdf,cost)

ncon  = length(table(dxdf$Country))

outmat_cvdevents = NULL
outmat_chfevents = NULL
outmat_nephevents = NULL
outmat_retinevents = NULL
outmat_neuroevents = NULL

outmat_cvddeaths = NULL
outmat_chfdeaths = NULL
outmat_nephdeaths = NULL
outmat_retindeaths = NULL
outmat_neurodeaths = NULL

outmat_rxbpcosts = NULL
outmat_rxdmcosts = NULL
outmat_rxstatincosts = NULL

outmat_cvdcosts = NULL
outmat_chfcosts = NULL
outmat_nephcosts = NULL
outmat_retincosts = NULL
outmat_neurocosts = NULL

outmat_cvddalys = NULL
outmat_chfdalys = NULL
outmat_nephdalys = NULL
outmat_retindalys = NULL
outmat_neurodalys = NULL


K = 1
C = 0.1658 
r = 0.03
D_cvd = mean(c(0.422,0.056,0.021,0.076,0.312,0.539,0.567)) # equal probability of each CVD outcome
D_chf = mean(c(0.037, 0.070, 0.186)) # equal probability of each chf outcome
D_neph = mean(c(0.573, 0.105)) # dialysis or stage 4 ckd
D_retin = mean(c(0.195)) # blindness
D_neuro = mean(c(0.099)) 
b = 0.04


iters = 10000

for (iter in 1:iters) {
  print(paste("On iteration", iter))
  
  
  
  dxdf$rxbpcosts_base = dxdf$bprx*dxdf$rxbp_cost*dxdf$detected
  dxdf$rxdmcosts_base = dxdf$oralrx*dxdf$rxdm_cost*dxdf$detected
  dxdf$rxstatincosts_base = (as.numeric(dxdf$statin)-1)*dxdf$rxstatin_cost*dxdf$detected
  
  
  set.seed(iter)
  dxdf$cvdevents_base = rbinom(length(dxdf$cvdrisk),1,dxdf$cvdrisk/100)
  dxdf$cvdcosts_base = dxdf$cvdevents_base*dxdf$cvdevents_cost
  dxdf$cvddeaths_base = dxdf$cvdevents_base*0.2#rbinom(length(dxdf$cvdrisk),1,0.2) #20% case fatality, https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3819990/
  
  set.seed(iter)
  dxdf$chfevents_base = rbinom(length(dxdf$chfrisk),1,dxdf$chfrisk/100)
  dxdf$chfcosts_base = dxdf$chfevents_base*dxdf$chfevents_cost
  dxdf$chfdeaths_base = dxdf$chfevents_base*0.5#rbinom(length(dxdf$chfrisk),1,0.5) #50% case fatality at 5 years, https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3033496/
  
  set.seed(iter)
  dxdf$nephevents_base = rbinom(length(dxdf$nephrisk),1,dxdf$nephrisk/100)
  dxdf$nephcosts_base = dxdf$nephevents_base*dxdf$nephevents_cost
  dxdf$nephdeaths_base = dxdf$nephevents_base*1#rbinom(length(dxdf$nephrisk),1,1) #100% case fatality
  
  set.seed(iter)
  dxdf$retinevents_base = rbinom(length(dxdf$retinrisk),1,dxdf$retinrisk/100)
  dxdf$retincosts_base = dxdf$retinevents_base*dxdf$retinevents_cost
  dxdf$retindeaths_base = dxdf$retinevents_base*0 #rbinom(length(dxdf$retinrisk),1,0) 
  
  set.seed(iter)
  dxdf$neuroevents_base = rbinom(length(dxdf$neurorisk),1,dxdf$neurorisk/100)
  dxdf$neurocosts_base = dxdf$neuroevents_base*dxdf$neuroevents_cost
  dxdf$neurodeaths_base = dxdf$neuroevents_base*0.1#rbinom(length(dxdf$neurorisk),1,0.1) #10% case fatality at 10 yrs, https://care.diabetesjournals.org/content/28/3/617#T4
  
  
  dxdf$a = dxdf$age+5
  dxdf$L = 57.44-dxdf$age
  dxdf$L[dxdf$L<0]=5
  
  dxdf$YLL_cvd = ((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                      (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L)))
  dxdf$YLD_cvd = D_cvd*(((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                             (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L))))
  dxdf$DALYS_cvd_base = (dxdf$cvddeaths_base*dxdf$YLL_cvd+dxdf$cvdevents_base*dxdf$YLD_cvd)
  
  
  dxdf$YLL_chf = ((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                      (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L)))
  dxdf$YLD_chf = D_chf*(((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                             (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L))))
  dxdf$DALYS_chf_base = (dxdf$chfdeaths_base*dxdf$YLL_chf+dxdf$chfevents_base*dxdf$YLD_chf)
  
  dxdf$YLL_neph = ((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                       (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L)))
  dxdf$YLD_neph = D_neph*(((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                               (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L))))
  dxdf$DALYS_neph_base = (dxdf$nephdeaths_base*dxdf$YLL_neph+dxdf$nephevents_base*dxdf$YLD_neph)
  
  
  dxdf$YLL_retin = ((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                        (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L)))
  dxdf$YLD_retin = D_retin*(((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                                 (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L))))
  dxdf$DALYS_retin_base = (dxdf$retindeaths_base*dxdf$YLL_retin+dxdf$retinevents_base*dxdf$YLD_retin)
  
  dxdf$YLL_neuro = ((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                        (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L)))
  dxdf$YLD_neuro = D_neuro*(((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                                 (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L))))
  dxdf$DALYS_neuro_base = (dxdf$neurodeaths_base*dxdf$YLL_neuro+dxdf$neuroevents_base*dxdf$YLD_neuro)
  
  
  
  parsed = dxdf  %>%
    add_count(Country) %>%
    group_by(Country) %>%
    summarise(cvdevents_base = sum(cvdevents_base)/n,
              chfevents_base = sum(chfevents_base)/n,
              nephevents_base = sum(nephevents_base)/n,
              retinevents_base = sum(retinevents_base)/n,
              neuroevents_base = sum(neuroevents_base)/n,
              cvddeaths_base = sum(cvddeaths_base)/n,
              chfdeaths_base = sum(chfdeaths_base)/n,
              nephdeaths_base = sum(nephdeaths_base)/n,
              retindeaths_base = sum(retindeaths_base)/n,
              neurodeaths_base = sum(neurodeaths_base)/n,
              rxbpcosts_base = sum(rxbpcosts_base)/n,
              rxdmcosts_base = sum(rxdmcosts_base)/n,
              rxstatincosts_base = sum(rxstatincosts_base)/n,
              cvdcosts_base = sum(cvdcosts_base)/n,
              chfcosts_base = sum(chfcosts_base)/n,
              nephcosts_base = sum(nephcosts_base)/n,
              retincosts_base = sum(retincosts_base)/n,
              neurocosts_base = sum(neurocosts_base)/n,
              cvddalys_base = sum(DALYS_cvd_base)/n,
              chfdalys_base = sum(DALYS_chf_base)/n,
              nephdalys_base = sum(DALYS_neph_base)/n,
              retindalys_base = sum(DALYS_retin_base)/n,
              neurodalys_base = sum(DALYS_neuro_base)/n) %>%
    distinct()
  
  
  cvdevents_mat = parsed %>% select(Country, cvdevents_base)
  cvdevents_mat = as.matrix(cvdevents_mat)
  chfevents_mat = parsed %>% select(Country, chfevents_base)
  chfevents_mat = as.matrix(chfevents_mat)
  nephevents_mat = parsed %>% select(Country, nephevents_base)
  nephevents_mat = as.matrix(nephevents_mat)
  retinevents_mat = parsed %>% select(Country, retinevents_base)
  retinevents_mat = as.matrix(retinevents_mat)
  neuroevents_mat = parsed %>% select(Country, neuroevents_base)
  neuroevents_mat = as.matrix(neuroevents_mat)
  
  cvddeaths_mat = parsed %>% select(Country, cvddeaths_base)
  cvddeaths_mat = as.matrix(cvddeaths_mat)
  chfdeaths_mat = parsed %>% select(Country, chfdeaths_base)
  chfdeaths_mat = as.matrix(chfdeaths_mat)
  chfdeaths_mat = parsed %>% select(Country, chfdeaths_base)
  nephdeaths_mat = parsed %>% select(Country, nephdeaths_base)
  nephdeaths_mat = as.matrix(nephdeaths_mat)
  retindeaths_mat = parsed %>% select(Country, retindeaths_base)
  retindeaths_mat = as.matrix(retindeaths_mat)
  neurodeaths_mat = parsed %>% select(Country, neurodeaths_base)
  neurodeaths_mat = as.matrix(neurodeaths_mat)
  
  rxbpcosts_mat = parsed %>% select(Country, rxbpcosts_base)
  rxbpcosts_mat = as.matrix(rxbpcosts_mat)
  rxdmcosts_mat = parsed %>% select(Country, rxdmcosts_base)
  rxdmcosts_mat = as.matrix(rxdmcosts_mat)
  rxstatincosts_mat = parsed %>% select(Country, rxstatincosts_base)
  rxstatincosts_mat = as.matrix(rxstatincosts_mat)
  
  cvdcosts_mat = parsed %>% select(Country, cvdcosts_base)
  cvdcosts_mat = as.matrix(cvdcosts_mat)
  chfcosts_mat = parsed %>% select(Country, chfcosts_base)
  chfcosts_mat = as.matrix(chfcosts_mat)
  nephcosts_mat = parsed %>% select(Country, nephcosts_base)
  nephcosts_mat = as.matrix(nephcosts_mat)
  retincosts_mat = parsed %>% select(Country, retincosts_base)
  retincosts_mat = as.matrix(retincosts_mat)
  neurocosts_mat = parsed %>% select(Country, neurocosts_base)
  neurocosts_mat = as.matrix(neurocosts_mat)
  
  
  cvddalys_mat = parsed %>% select(Country, cvddalys_base)
  cvddalys_mat = as.matrix(cvddalys_mat)
  chfdalys_mat = parsed %>% select(Country, chfdalys_base)
  chfdalys_mat = parsed %>% select(Country, chfdalys_base)
  nephdalys_mat = parsed %>% select(Country, nephdalys_base)
  nephdalys_mat = as.matrix(nephdalys_mat)
  retindalys_mat = parsed %>% select(Country, retindalys_base)
  retindalys_mat = as.matrix(retindalys_mat)
  neurodalys_mat = parsed %>% select(Country, neurodalys_base)
  neurodalys_mat = as.matrix(neurodalys_mat)
  



  
  for (i  in 1:2) {

    for (j in 1:2) {
      
      ptreat = 0.6+(i-1)/5
      pctrl = 0.6+(j-1)/5
      print(paste("ptreat=",ptreat,"pctrl=",pctrl))    
      
      # treat BP to <130/80, w/ thiazide/ACEI, and if age>=40 or tchol>=8 give statin, and a1c<7mmol/L w/ mtf + sulf
      
      
      
      
      dxdf$rxbp0 = ((dxdf$sbp>=130) | (dxdf$dbp>=80))*dxdf$newlydetected
      dxdf$rxstatin0 = ((dxdf$age>=40)|(dxdf$tchol>=8))*dxdf$newlydetected
      dxdf$rxdm0 = (dxdf$a1c>=7)*dxdf$newlydetected
      
      set.seed(iter)
      controlled = rbinom(length(dxdf$rxbp0),1,pctrl)
      set.seed(iter)
      controlledbg = rbinom(length(dxdf$rxdm0),1,pctrl)
      
      set.seed(iter)
      dxdf$rxbp = dxdf$rxbp0*rbinom(length(dxdf$rxbp0),1,ptreat)
      dxdf$rxbp_costs = dxdf$rxbp*dxdf$rxbp_cost*(1*(controlled==0)+3*(controlled==1))
      set.seed(iter)
      dxdf$rxdm = dxdf$rxdm0*rbinom(length(dxdf$rxdm0),1,ptreat)
      dxdf$rxdm_costs = dxdf$rxdm*dxdf$rxdm_cost*(1*(controlledbg==0)+2.5*(controlledbg==1))
      set.seed(iter)
      dxdf$rxstatin = dxdf$rxstatin0*rbinom(length(dxdf$rxstatin0),1,ptreat)
      dxdf$rxstatin_costs = dxdf$rxstatin*dxdf$rxstatin_cost
      
      bpeff = 1*(controlled==1)*3*8.8 +  0.25*(controlled==0)*4*8.8 #1 med for uncontrolled vs 3 for controlled
      dxdf$newsbp[(dxdf$sbp>=130)] = 130*controlled[dxdf$sbp>=130] + (dxdf$sbp[dxdf$sbp>=130]-bpeff[dxdf$sbp>=130])*(1-controlled[dxdf$sbp>=130])
      dxdf$newsbp[(dxdf$sbp<130)] = dxdf$sbp[(dxdf$sbp<130)]
      dxdf$newsbp[(dxdf$sbp>=130) & (dxdf$newsbp<130)] = 130
      dxdf$deltasbp = (dxdf$newsbp - dxdf$sbp)
      #dxdf$deltasbp[dxdf$deltasbp<(-8.8*4)]=(-8.8*4)
      
      dxdf$newa1c = dxdf$a1c
      dxdf$newa1c[dxdf$a1c>7] = 7*controlledbg[dxdf$a1c>7] + (dxdf$a1c[dxdf$a1c>7]-1.5)*(1-controlled[dxdf$a1c>7]) # 1.5% impact of 1 med
      dxdf$deltaa1c = (dxdf$newa1c - dxdf$a1c)
      dxdf$neworalrx = dxdf$rxdm
      dxdf$newa1c = dxdf$newa1c
      dxdf$newa1c[dxdf$newa1c>dxdf$a1c]=dxdf$a1c[dxdf$newa1c>dxdf$a1c]
      dxdf$deltaa1c = (dxdf$newa1c - dxdf$a1c)
      #dxdf$deltaa1c[dxdf$deltaa1c<(-5)]=(-5)
      
       
      dxdf$cvdrisk_opt = dxdf$cvdrisk*(1-dxdf$rxbp)*(1-dxdf$rxstatin)+
        dxdf$cvdrisk*(2^(dxdf$deltasbp*(-0.0000184775*dxdf$age^2+0.001584*dxdf$age+0.028672)))*dxdf$rxbp*(1-dxdf$rxstatin)+
        dxdf$cvdrisk*(1-0.261-0.056)*dxdf$rxstatin*(1-dxdf$rxbp)+
        dxdf$cvdrisk*(2^(dxdf$deltasbp*(-0.0000184775*dxdf$age^2+0.001584*dxdf$age+0.028672)))*(1-0.261)*dxdf$rxbp*dxdf$rxstatin
      dxdf$deltacvdrisk = dxdf$cvdrisk -  dxdf$cvdrisk_opt
      
      dxdf$chfrisk_opt = dxdf$chfrisk*(1-dxdf$rxbp)+
        dxdf$chfrisk*(1-0.22)*dxdf$rxbp # http://www.nejm.org/doi/full/10.1056/NEJM199108013250501
      dxdf$deltachfrisk =  dxdf$chfrisk-dxdf$chfrisk_opt
      
      
      dxdf$nephrisk_opt = dxdf$nephrisk*(1-dxdf$rxbp)*(1-dxdf$rxdm)+
        dxdf$nephrisk*((dxdf$newsbp/dxdf$sbp)^2.5640)*dxdf$rxbp*(1-dxdf$rxdm)+ # JAMA Vijan beta coef
        dxdf$nephrisk*((dxdf$newa1c/dxdf$a1c)^1.1025)*dxdf$rxdm*(1-dxdf$rxbp)+
        dxdf$nephrisk*((dxdf$newsbp/dxdf$sbp)^2.5640)*dxdf$rxbp*((dxdf$newa1c/dxdf$a1c)^1.4325)*dxdf$rxbp*dxdf$rxdm
      dxdf$nephrisk_opt[is.na(dxdf$nephrisk_opt)==T]=dxdf$nephrisk[is.na(dxdf$nephrisk_opt)==T]
      #dxdf$nephrisk_opt[dxdf$nephrisk_opt>dxdf$nephrisk] = dxdf$nephrisk[dxdf$nephrisk_opt>dxdf$nephrisk]
      dxdf$deltanephrisk = dxdf$nephrisk - dxdf$nephrisk_opt 
      
      
      dxdf$retinrisk_opt = dxdf$retinrisk*(1-dxdf$rxbp)*(1-dxdf$rxdm)+
        dxdf$retinrisk*((dxdf$newsbp/dxdf$sbp)^6.4249)*dxdf$rxbp*(1-dxdf$rxdm)+ # JAMA Vijan beta coef
        dxdf$retinrisk*((dxdf$newa1c/dxdf$a1c)^2.5144)*dxdf$rxdm*(1-dxdf$rxbp)+
        dxdf$retinrisk*((dxdf$newsbp/dxdf$sbp)^6.4249)*dxdf$rxbp*((dxdf$newa1c/dxdf$a1c)^2.5144)*dxdf$rxbp*dxdf$rxdm
      dxdf$retinrisk_opt[is.na(dxdf$retinrisk_opt)==T]=dxdf$retinrisk[is.na(dxdf$retinrisk_opt)==T]
      #dxdf$retinrisk_opt[dxdf$retinrisk_opt>dxdf$retinrisk] = dxdf$retinrisk[dxdf$retinrisk_opt>dxdf$retinrisk]
      dxdf$deltaretinrisk =  dxdf$retinrisk - dxdf$retinrisk_opt 
      
      
      dxdf$neurorisk_opt = dxdf$neurorisk*(1-dxdf$rxdm)+
        dxdf$neurorisk*((dxdf$newa1c/dxdf$a1c)^1.4325)*dxdf$rxdm
      dxdf$neurorisk_opt[is.na(dxdf$neurorisk_opt)==T]=dxdf$neurorisk[is.na(dxdf$neurorisk_opt)==T]
      #dxdf$neurorisk_opt[dxdf$neurorisk_opt>dxdf$neurorisk] = dxdf$neurorisk[dxdf$neurorisk_opt>dxdf$neurorisk]
      dxdf$deltaneurorisk = dxdf$neurorisk - dxdf$neurorisk_opt 
      
      dxdf_tab = dxdf
      varsToFactor <- c("rxbp", "rxdm", "rxstatin")
      dxdf_tab[varsToFactor] <- lapply(dxdf_tab[varsToFactor], factor)
      
      # vars =c("rxbp", "rxdm", "rxstatin","deltasbp", "deltaa1c", "deltacvdrisk", "deltachfrisk", "deltanephrisk", "deltaretinrisk", "deltaneurorisk")
      # tableThree <- CreateTableOne(vars = vars, data = dxdf_tab, strata="Country")
      # tab3 = print(tableThree, nonnormal = c("deltasbp", "deltaa1c", "deltacvdrisk", "deltachfrisk", "deltanephrisk", "deltaretinrisk", "deltaneurorisk"))
      # 
      # 
      # dxdfsvy_diag =  svydesign(id  = ~positionInCategory,  weights = ~p_wt, data = dxdf_tab)
      # tableThreew <- svyCreateTableOne(vars = vars, data = dxdfsvy_diag, strata="Region")
      # tab3w = print(tableThreew, nonnormal = c("deltasbp", "deltaa1c", "deltacvdrisk", "deltachfrisk", "deltanephrisk", "deltaretinrisk", "deltaneurorisk"))
      
      
      set.seed(iter)
      dxdf$cvdevents_opt = rbinom(length(dxdf$cvdrisk),1,dxdf$cvdrisk_opt/100)  # rbinom(length(dxdf$cvdrisk_opt),1,dxdf$cvdrisk_opt/100)
      dxdf$cvdcosts_opt = dxdf$cvdevents_opt*dxdf$cvdevents_cost
      set.seed(iter)
      dxdf$cvddeaths_opt = dxdf$cvdevents_opt*0.2 #rbinom(length(dxdf$cvdrisk_opt),1,0.2) #20% case fatality, https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3819990/
      
      set.seed(iter)
      dxdf$chfevents_opt = rbinom(length(dxdf$chfrisk),1,dxdf$chfrisk_opt/100) #rbinom(length(dxdf$chfrisk_opt),1,dxdf$chfrisk_opt/100)
      dxdf$chfcosts_opt = dxdf$chfevents_opt*dxdf$chfevents_cost
      set.seed(iter)
      dxdf$chfdeaths_opt = dxdf$chfevents_opt*0.5  # rbinom(length(dxdf$chfrisk_opt),1,0.5) #50% case fatality at 5 years, https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3033496/
      
      set.seed(iter)
      dxdf$nephevents_opt = rbinom(length(dxdf$nephrisk),1,dxdf$nephrisk_opt/100)  #rbinom(length(dxdf$nephrisk_opt),1,dxdf$nephrisk_opt/100)
      dxdf$nephcosts_opt = dxdf$nephevents_opt*dxdf$nephevents_cost
      set.seed(iter)
      dxdf$nephdeaths_opt = dxdf$nephevents_opt*1 #rbinom(length(dxdf$nephrisk_opt),1,1) #100% case fatality
      
      set.seed(iter)
      dxdf$retinevents_opt = rbinom(length(dxdf$retinrisk),1,dxdf$retinrisk_opt/100)  #rbinom(length(dxdf$retinrisk_opt),1,dxdf$retinrisk_opt/100)
      dxdf$retincosts_opt = dxdf$retinevents_opt*dxdf$retinevents_cost
      set.seed(iter)
      dxdf$retindeaths_opt = dxdf$retinevents_opt*0 #rbinom(length(dxdf$retinrisk_opt),1,0) 
      
      set.seed(iter)
      dxdf$neuroevents_opt = rbinom(length(dxdf$neurorisk),1,dxdf$neurorisk_opt/100)  #rbinom(length(dxdf$neurorisk_opt),1,dxdf$neurorisk_opt/100)
      dxdf$neurocosts_opt = dxdf$neuroevents_opt*dxdf$neuroevents_cost
      set.seed(iter)
      dxdf$neurodeaths_opt = dxdf$neuroevents_opt*0.1 #rbinom(length(dxdf$neurorisk_opt),1,0.1) #10% case fatality at 10 yrs, https://care.diabetesjournals.org/content/28/3/617#T4
      
      dxdf$DALYS_cvd_opt = (dxdf$cvddeaths_opt*dxdf$YLL_cvd+dxdf$cvdevents_opt*dxdf$YLD_cvd)
      
      dxdf$DALYS_chf_opt = (dxdf$chfdeaths_opt*dxdf$YLL_chf+dxdf$chfevents_opt*dxdf$YLD_chf)
      
      dxdf$DALYS_neph_opt = (dxdf$nephdeaths_opt*dxdf$YLL_neph+dxdf$nephevents_opt*dxdf$YLD_neph)
      
      dxdf$DALYS_retin_opt = (dxdf$retindeaths_opt*dxdf$YLL_retin+dxdf$retinevents_opt*dxdf$YLD_retin)
      
      dxdf$DALYS_neuro_opt = (dxdf$neurodeaths_opt*dxdf$YLL_neuro+dxdf$neuroevents_opt*dxdf$YLD_neuro)
      
      
      
      parsed = dxdf  %>%
        add_count(Country) %>%
        group_by(Country) %>%
        summarise(cvdevents_opt = sum(cvdevents_opt)/n,
                  chfevents_opt = sum(chfevents_opt)/n,
                  nephevents_opt = sum(nephevents_opt)/n,
                  retinevents_opt = sum(retinevents_opt)/n,
                  neuroevents_opt = sum(neuroevents_opt)/n,
                  cvddeaths_opt = sum(cvddeaths_opt)/n,
                  chfdeaths_opt = sum(chfdeaths_opt)/n,
                  nephdeaths_opt = sum(nephdeaths_opt)/n,
                  retindeaths_opt = sum(retindeaths_opt)/n,
                  neurodeaths_opt = sum(neurodeaths_opt)/n,
                  rxbp_costs = sum(rxbp_costs)/n,
                  rxdm_costs = sum(rxdm_costs)/n,
                  rxstatin_costs = sum(rxstatin_costs)/n,
                  cvdcosts_opt = sum(cvdcosts_opt)/n,
                  chfcosts_opt = sum(chfcosts_opt)/n,
                  nephcosts_opt = sum(nephcosts_opt)/n,
                  retincosts_opt = sum(retincosts_opt)/n,
                  neurocosts_opt = sum(neurocosts_opt)/n,
                  cvddalys_opt = sum(DALYS_cvd_opt)/n,
                  chfdalys_opt = sum(DALYS_chf_opt)/n,
                  nephdalys_opt = sum(DALYS_neph_opt)/n,
                  retindalys_opt = sum(DALYS_retin_opt)/n,
                  neurodalys_opt = sum(DALYS_neuro_opt)/n) %>%
        distinct()
      
      
      cvdevents_mat = cbind(cvdevents_mat,as.double(parsed$cvdevents_opt))
      chfevents_mat = cbind(chfevents_mat,as.double(parsed$chfevents_opt))
      nephevents_mat = cbind(nephevents_mat,as.double(parsed$nephevents_opt))
      retinevents_mat = cbind(retinevents_mat,as.double(parsed$retinevents_opt))
      neuroevents_mat = cbind(neuroevents_mat,as.double(parsed$neuroevents_opt))
      
      rxbpcosts_mat = cbind(rxbpcosts_mat,as.double(parsed$rxbp_costs))
      rxdmcosts_mat = cbind(rxdmcosts_mat,as.double(parsed$rxdm_costs))
      rxstatincosts_mat = cbind(rxstatincosts_mat,as.double(parsed$rxstatin_costs))
      
      cvdcosts_mat = cbind(cvdcosts_mat,as.double(parsed$cvdcosts_opt))
      chfcosts_mat = cbind(chfcosts_mat,as.double(parsed$chfcosts_opt))
      nephcosts_mat = cbind(nephcosts_mat,as.double(parsed$nephcosts_opt))
      retincosts_mat = cbind(retincosts_mat,as.double(parsed$retincosts_opt))
      neurocosts_mat = cbind(neurocosts_mat,as.double(parsed$neurocosts_opt))
      
      cvddeaths_mat = cbind(cvddeaths_mat,as.double(parsed$cvddeaths_opt))
      chfdeaths_mat = cbind(chfdeaths_mat,as.double(parsed$chfdeaths_opt))
      nephdeaths_mat = cbind(nephdeaths_mat,as.double(parsed$nephdeaths_opt))
      retindeaths_mat = cbind(retindeaths_mat,as.double(parsed$retindeaths_opt))
      neurodeaths_mat = cbind(neurodeaths_mat,as.double(parsed$neurodeaths_opt))
      
      cvddalys_mat = cbind(cvddalys_mat,as.double(parsed$cvddalys_opt))
      chfdalys_mat = cbind(chfdalys_mat,as.double(parsed$chfdalys_opt))
      nephdalys_mat = cbind(nephdalys_mat,as.double(parsed$nephdalys_opt))
      retindalys_mat = cbind(retindalys_mat,as.double(parsed$retindalys_opt))
      neurodalys_mat = cbind(neurodalys_mat,as.double(parsed$neurodalys_opt))
      
      
    }
  }
  
  
  
  labels = c("Country",
             "baseline", 
             "60% trt, 60% ctrl",
             "60% trt, 80% ctrl",
             "80% trt, 60% ctrl",
             "80% trt, 80% ctrl")
  
  colnames(rxbpcosts_mat) = labels
  colnames(rxdmcosts_mat) = labels
  colnames(rxstatincosts_mat) = labels
  colnames(cvdcosts_mat) = labels
  colnames(chfcosts_mat) = labels
  colnames(nephcosts_mat) = labels
  colnames(retincosts_mat) = labels
  colnames(neurocosts_mat) = labels
  colnames(cvdevents_mat) = labels
  colnames(chfevents_mat) = labels
  colnames(nephevents_mat) = labels
  colnames(retinevents_mat) = labels
  colnames(neuroevents_mat) = labels
  colnames(cvddeaths_mat) = labels
  colnames(chfdeaths_mat) = labels
  colnames(nephdeaths_mat) = labels
  colnames(retindeaths_mat) = labels
  colnames(neurodeaths_mat) = labels
  colnames(cvddalys_mat) = labels
  colnames(chfdalys_mat) = labels
  colnames(nephdalys_mat) = labels
  colnames(retindalys_mat) = labels
  colnames(neurodalys_mat) = labels
  
  outmat_cvdevents = bind_rows(outmat_cvdevents,as_tibble(cvdevents_mat))
  outmat_chfevents = bind_rows(outmat_chfevents,as_tibble(chfevents_mat))
  outmat_nephevents = bind_rows(outmat_nephevents,as_tibble(nephevents_mat))
  outmat_retinevents = bind_rows(outmat_retinevents,as_tibble(retinevents_mat))
  outmat_neuroevents = bind_rows(outmat_neuroevents,as_tibble(neuroevents_mat))
  
  outmat_cvddeaths = bind_rows(outmat_cvddeaths,as_tibble(cvddeaths_mat))
  outmat_chfdeaths = bind_rows(outmat_chfdeaths,as_tibble(chfdeaths_mat))
  outmat_nephdeaths = bind_rows(outmat_nephdeaths,as_tibble(nephdeaths_mat))
  outmat_retindeaths = bind_rows(outmat_retindeaths,as_tibble(retindeaths_mat))
  outmat_neurodeaths = bind_rows(outmat_neurodeaths,as_tibble(neurodeaths_mat))
  
  outmat_rxbpcosts = bind_rows(outmat_rxbpcosts,as_tibble(rxbpcosts_mat))
  outmat_rxdmcosts = bind_rows(outmat_rxdmcosts,as_tibble(rxdmcosts_mat))
  outmat_rxstatincosts = bind_rows(outmat_rxstatincosts,as_tibble(rxstatincosts_mat))
  
  outmat_cvdcosts = bind_rows(outmat_cvdcosts,as_tibble(cvdcosts_mat))
  outmat_chfcosts = bind_rows(outmat_chfcosts,as_tibble(chfcosts_mat))
  outmat_nephcosts = bind_rows(outmat_nephcosts,as_tibble(nephcosts_mat))
  outmat_retincosts = bind_rows(outmat_retincosts,as_tibble(retincosts_mat))
  outmat_neurocosts = bind_rows(outmat_neurocosts,as_tibble(neurocosts_mat))
  
  outmat_cvddalys = bind_rows(outmat_cvddalys,as_tibble(cvddalys_mat))
  outmat_chfdalys = bind_rows(outmat_chfdalys,as_tibble(chfdalys_mat))
  outmat_nephdalys = bind_rows(outmat_nephdalys,as_tibble(nephdalys_mat))
  outmat_retindalys = bind_rows(outmat_retindalys,as_tibble(retindalys_mat))
  outmat_neurodalys = bind_rows(outmat_neurodalys,as_tibble(neurodalys_mat))
  
}

setwd("~/Box/Research/Research projects/WHO diabetes/Results matrices/")

save(outmat_rxbpcosts,file=paste0("outmat_rxbpcosts",screens))
save(outmat_rxdmcosts,file=paste0("outmat_rxdmcosts",screens))
save(outmat_rxstatincosts,file=paste0("outmat_rxstatincosts",screens))

save(outmat_cvdcosts,file=paste0("outmat_cvdcosts",screens))
save(outmat_chfcosts,file=paste0("outmat_chfcosts",screens))
save(outmat_nephcosts,file=paste0("outmat_nephcosts",screens))
save(outmat_retincosts,file=paste0("outmat_retincosts",screens))
save(outmat_neurocosts,file=paste0("outmat_neurocosts",screens))

save(outmat_cvdevents,file=paste0("outmat_cvdevents",screens))
save(outmat_chfevents,file=paste0("outmat_chfevents",screens))
save(outmat_nephevents,file=paste0("outmat_nephevents",screens))
save(outmat_retinevents,file=paste0("outmat_retinevents",screens))
save(outmat_neuroevents,file=paste0("outmat_neuroevents",screens))

save(outmat_cvddeaths,file=paste0("outmat_cvddeaths",screens))
save(outmat_chfdeaths,file=paste0("outmat_chfdeaths",screens))
save(outmat_nephdeaths,file=paste0("outmat_nephdeaths",screens))
save(outmat_retindeaths,file=paste0("outmat_retindeaths",screens))
save(outmat_neurodeaths,file=paste0("outmat_neurodeaths",screens))

save(outmat_cvddalys,file=paste0("outmat_cvddalys",screens))
save(outmat_chfdalys,file=paste0("outmat_chfdalys",screens))
save(outmat_nephdalys,file=paste0("outmat_nephdalys",screens))
save(outmat_retindalys,file=paste0("outmat_retindalys",screens))
save(outmat_neurodalys,file=paste0("outmat_neurodalys",screens))

}
end_time <- Sys.time()
end_time - start_time


