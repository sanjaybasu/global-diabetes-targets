setwd("~/Downloads")
rm(list=ls())
library(tidyverse)

load("dfimp.RData")
cost = read_csv("cost.csv")
cnt_reg = read_csv("cnt_reg.csv")

dfrep = do.call("rbind", replicate(25, dfimp, simplify = FALSE))


dxdf = dfrep %>%
  mutate(detected=(und_dia==0)) %>%
  group_by(Country) %>%
  mutate(positionInCategory = 1:n(),
         current_level = sum(und_dia==0)/n()) %>%
  ungroup()




screening_goal = mean(dxdf$current_level)
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


dxdf$rxbpcosts_base = dxdf$bprx*dxdf$rxbp_cost*dxdf$detected/(1.03^10)
dxdf$rxdmcosts_base = dxdf$oralrx*dxdf$rxdm_cost*dxdf$detected/(1.03^10)
dxdf$rxstatincosts_base = (as.numeric(dxdf$statin)-1)*dxdf$rxstatin_cost*dxdf$detected/(1.03^10)


set.seed(iter)
dxdf$cvdevents_base = rbinom(length(dxdf$cvdrisk),1,dxdf$cvdrisk/100)
dxdf$cvdcosts_base = dxdf$cvdevents_base*dxdf$cvdevents_cost/(1.03^10)
dxdf$cvddeaths_base = dxdf$cvdevents_base*0.2#rbinom(length(dxdf$cvdrisk),1,0.2) #20% case fatality, https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3819990/

set.seed(iter)
dxdf$chfevents_base = rbinom(length(dxdf$chfrisk),1,dxdf$chfrisk/100)
dxdf$chfcosts_base = dxdf$chfevents_base*dxdf$chfevents_cost/(1.03^10)
dxdf$chfdeaths_base = dxdf$chfevents_base*0.5#rbinom(length(dxdf$chfrisk),1,0.5) #50% case fatality at 5 years, https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3033496/

set.seed(iter)
dxdf$nephevents_base = rbinom(length(dxdf$nephrisk),1,dxdf$nephrisk/100)
dxdf$nephcosts_base = dxdf$nephevents_base*dxdf$nephevents_cost/(1.03^10)
dxdf$nephdeaths_base = dxdf$nephevents_base*1#rbinom(length(dxdf$nephrisk),1,1) #100% case fatality

set.seed(iter)
dxdf$retinevents_base = rbinom(length(dxdf$retinrisk),1,dxdf$retinrisk/100)
dxdf$retincosts_base = dxdf$retinevents_base*dxdf$retinevents_cost/(1.03^10)
dxdf$retindeaths_base = dxdf$retinevents_base*0 #rbinom(length(dxdf$retinrisk),1,0) 

set.seed(iter)
dxdf$neuroevents_base = rbinom(length(dxdf$neurorisk),1,dxdf$neurorisk/100)
dxdf$neurocosts_base = dxdf$neuroevents_base*dxdf$neuroevents_cost/(1.03^10)
dxdf$neurodeaths_base = dxdf$neuroevents_base*0.1#rbinom(length(dxdf$neurorisk),1,0.1) #10% case fatality at 10 yrs, https://care.diabetesjournals.org/content/28/3/617#T4


dxdf$a = dxdf$age+5
dxdf$L = 57.44-dxdf$age
dxdf$L[dxdf$L<0]=5

dxdf$YLL_cvd = ((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                    (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L)))
dxdf$YLD_cvd = D_cvd*(((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                           (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L))))
dxdf$DALYS_cvd_base = (dxdf$cvddeaths_base*dxdf$YLL_cvd+dxdf$cvdevents_base*dxdf$YLD_cvd)/(1.03^10)


dxdf$YLL_chf = ((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                    (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L)))
dxdf$YLD_chf = D_chf*(((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                           (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L))))
dxdf$DALYS_chf_base = (dxdf$chfdeaths_base*dxdf$YLL_chf+dxdf$chfevents_base*dxdf$YLD_chf)/(1.03^10)

dxdf$YLL_neph = ((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                     (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L)))
dxdf$YLD_neph = D_neph*(((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                             (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L))))
dxdf$DALYS_neph_base = (dxdf$nephdeaths_base*dxdf$YLL_neph+dxdf$nephevents_base*dxdf$YLD_neph)/(1.03^10)


dxdf$YLL_retin = ((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                      (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L)))
dxdf$YLD_retin = D_retin*(((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                               (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L))))
dxdf$DALYS_retin_base = (dxdf$retindeaths_base*dxdf$YLL_retin+dxdf$retinevents_base*dxdf$YLD_retin)/(1.03^10)

dxdf$YLL_neuro = ((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                      (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L)))
dxdf$YLD_neuro = D_neuro*(((K*C*(exp(r*dxdf$a)))/((r+b)^2))*(((exp(-(r+b)*(dxdf$L+dxdf$a)))*(-(r+b)*(dxdf$L+dxdf$a)-1))-
                                                               (exp(-(r+b)*dxdf$a))*(-(r+b)*dxdf$a-1))+(((1-K)/r)*(1-exp(-r*dxdf$L))))
dxdf$DALYS_neuro_base = (dxdf$neurodeaths_base*dxdf$YLL_neuro+dxdf$neuroevents_base*dxdf$YLD_neuro)/(1.03^10)



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


dxdf$rxbp =  dxdf$bprx*(dxdf$bprx==1) + rbinom(dxdf$bprx==0,1,0)*(dxdf$bprx==0) #0.041
dxdf$rxdm =  dxdf$oralrx*(dxdf$oralrx==1) + rbinom(dxdf$oralrx==0,1,0.075)*(dxdf$oralrx==0) #.075
dxdf$rxstatin =  (as.numeric(dxdf$statin)-1)*(dxdf$statin==1) + rbinom(dxdf$statin==0,1,0)*(dxdf$statin==0) #.013
controlled = rbinom(dim(dxdf)[1],1,(sum(dxdf$sbp<130 & dxdf$dbp<80 & dxdf$bprx==1)/(dim(dxdf)[1]))) #0.1+
controlledbg = rbinom(dim(dxdf)[1],1,(sum((dxdf$a1c<=7 | dxdf$fbg<7) & dxdf$oralrx==1))/(dim(dxdf)[1]))#0.1+

prop.table(table(dxdf$bprx,dxdf$rxbp),2)
prop.table(table(dxdf$oralrx,dxdf$rxdm),2)
prop.table(table(dxdf$statin,dxdf$rxstatin),2)

prop.table(table(dxdf$rxstatin,(dxdf$age>=40 & dxdf$cvdrisk>20)),2)
prop.table(table(((dxdf$sbp_avg<130 & dxdf$dbp_avg<80) & dxdf$bprx==1),dxdf$bprx),2)



bpeff = 1*(controlled==1)*3*8.8 +  0.25*(controlled==0)*4*8.8 #1 med for uncontrolled vs 3 for controlled
dxdf$newsbp = dxdf$sbp
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

summary(dxdf$cvdrisk_opt)
summary(dxdf$cvdrisk_opt)-summary(dxdf$cvdrisk)
summary(dxdf$neurorisk_opt)
summary(dxdf$neurorisk_opt)-summary(dxdf$neurorisk)
summary(dxdf$nephrisk_opt)
summary(dxdf$nephrisk_opt)-summary(dxdf$nephrisk)
summary(dxdf$retinrisk_opt)
summary(dxdf$retinrisk_opt)-summary(dxdf$retinrisk)
summary(dxdf$chfrisk_opt)
summary(dxdf$chfrisk_opt)-summary(dxdf$chfrisk)





