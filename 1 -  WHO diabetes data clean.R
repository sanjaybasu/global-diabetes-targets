rm(list=ls())
library(haven)
library(tidyverse)
library(tableone)
library(matrixStats)
library(mice)
library(vcd)
library(Hmisc)

setwd("~/Downloads")

df = read_dta("Diabetes med dataset cut 2021.04.18.dta")
cost = read_csv("cost.csv")
cnt_reg = read_csv("cnt_reg.csv")
cvdriskchart = read_csv("cvdriskchart.csv")
cvdriskchart = cvdriskchart %>%
  filter(dm==1)

df$Country = df$country
df$p_wt = df$w_all
df$sex = df$sex
df$age = df$age
df$hypt_med =  df$hypt_med_new
df$sbp_avg = df$sbp_avg
df$dbp_avg = df$dbp_avg
df$dia_med = df$dia_med_new
df$insulin  = df$insulin_new
df$fbp = df$fbg_new
df$bmi = df$bmi
df$csmoke = df$csmoke
df$hba1c_p = df$hba1c_p
df$hba1c_m = 10.929*(df$hba1c_p -2.15)
df$und_dia  = 1-df$hbg_new
df$clin_dia =  df$clin_dia
df$tchol_mgdl = df$tchol_mgdl
df$tchol_mmoll = df$tchol_mgdl/38.67
df$hdl_mgdl = df$hdl_mgdl
df$hdl_mmoll = df$hdl_mgdl/38.67
df$ldl_mgdl = df$ldl_mgdl
df$ldl_mmoll = df$ldl_mgdl/38.67
df$mi = df$mi
df$statin = df$statin
df$und_hypt = 1-df$hypt_new
df$clin_hypt =  df$clin_hypt
df$hbg = df$hbg_new
df$fbg = df$fbg_new

df$hypt_med[df$hypt_med>1] = 0
df$sbp_avg[df$sbp_avg>300] = NA
df$dbp_avg[df$dbp_avg>300] = NA
df$hbg[df$hbg>1] = 0
df$dia_med[df$dia_med>1] = 0
df$insulin[df$insulin>1] = 0
df$fbg[df$fbg>33] = NA
df$csmoke[df$csmoke>1] = 0
df$csmoke[df$csmoke<0] = 0
df$hba1c_p[df$hba1c_p>17] = NA
df$hba1c_p[df$hba1c_p<3] = NA
df$statin[df$statin!=0 & df$statin!=1] = 0
df$mi[df$mi!=0 & df$mi!=1] = 0

dfsub = df %>%
  filter(clin_dia==1) %>%  # 1 if (fbg>7.0 mmol/L AND fast=1) OR (fbg>11.1 mmol/L AND fast=0) OR hba1c_p≥6.5% OR dia_med==1 OR insulin==1. Else=0
  select(Country, p_wt, sex, age, hypt_med, sbp_avg, dbp_avg, dia_med, insulin, fbg, bmi, csmoke, 
         hba1c_m, hba1c_p, und_dia, clin_dia, tchol_mmoll, tchol_mgdl, hdl_mmoll, hdl_mgdl, ldl_mmoll, ldl_mgdl, 
         mi, statin, und_hypt, clin_hypt) %>%
  filter(Country!="India" & Country!="Namibia" & Country!="Uganda") %>%
  left_join(cnt_reg,by="Country") 



save(dfsub, file="dfsub.RData")

load("dfsub.RData")

varsToFactor <- c("sex","hypt_med", "dia_med", "insulin", "csmoke", "und_dia", "clin_dia", "mi", "statin", "und_hypt", "clin_hypt")
dfsub[varsToFactor] <- lapply(dfsub[varsToFactor], factor)

vars =c("sex", "age", "hypt_med", "sbp_avg", "dbp_avg", "dia_med", "insulin", "fbg", "bmi", "csmoke", "hba1c_m", "hba1c_p", "und_dia", "clin_dia", "tchol_mmoll", "tchol_mgdl", "hdl_mmoll", "hdl_mgdl",
        "mi", "statin", "und_hypt", "clin_hypt")
tableOne <- CreateTableOne(vars = vars, data = dfsub)
print(tableOne, nonnormal = c("age", "sbp_avg", "dbp_avg", "fbg","fbp", "bmi", "hba1c_p", "hba1c_m", "tchol_mmoll", "tchol_mgdl", "hdl_mmoll", "hdl_mgdl"), quote=T)
summary(dfsub$hba1c_m)
summary(dfsub$mi)
dfsub =  dfsub %>%
  mutate(sex = as.factor(sex),
         hypt_med = as.factor(hypt_med),
         dia_med = as.factor(dia_med),
         insulin = as.factor(insulin),
         csmoke = as.factor(csmoke),
         und_dia = as.factor(und_dia),
         mi = as.factor(mi),
         statin = as.factor(statin),
         und_hypt = as.factor(und_hypt),
         clin_hypt = as.factor(clin_hypt))
tempData <- mice(dfsub,m=1,maxit=1,seed=1, method = "cart")
dfimp <- complete(tempData,1)
dfimp = dfimp %>%
  filter(Country!="Swaziland" & Country!="South Africa DHS") 
dfimp$hba1c_p = (dfimp$hba1c_m/10.929)+2.15
dfimp$tchol_mgdl = dfimp$tchol_mmoll*38.67
dfimp$hdl_mgdl = dfimp$hdl_mmoll*38.67
dfimp$ldl_mgdl = dfimp$ldl_mmoll*38.67
dfimp =dfimp %>% mutate(diagnosed_dx = 1-as.numeric(und_dia),
                        diagnosed_htn =  1-as.numeric(und_hypt))
dfimp$diagnosed_dx = as.factor(dfimp$diagnosed_dx+1)
dfimp$diagnosed_htn = as.factor(dfimp$diagnosed_htn+1)
dfimp$dm_rx_rate = (dfimp$dia_med==1 & dfimp$diagnosed_dx==1)
dfimp$dm_ctrl = ((dfimp$fbg<7 | dfimp$hba1c_p<=7))
dfimp$dm_ctrl_rate = ((dfimp$fbg<7 | dfimp$hba1c_p<=7) & dfimp$dia_med==1)
dfimp$htn_rx_rate = (dfimp$hypt_med==1 & dfimp$diagnosed_htn==1)
dfimp$htn_ctrl = ((dfimp$sbp_avg<130 & dfimp$dbp_avg<80))
dfimp$htn_ctrl_rate = ((dfimp$sbp_avg<130 & dfimp$dbp_avg<80) & dfimp$hypt_med==1)

varsToFactor <- c("sex","hypt_med", "dia_med", "insulin", "csmoke", "und_dia", "clin_dia", "mi", "statin", "und_hypt", "clin_hypt","dm_ctrl","htn_ctrl","dm_ctrl_rate","htn_ctrl_rate",
                  "dm_rx_rate","htn_rx_rate")
dfimp[varsToFactor] <- lapply(dfimp[varsToFactor], factor)
vars =c("sex", "age","bmi", "csmoke", "mi", "sbp_avg", "dbp_avg", "fbg", "hba1c_m", "hba1c_p", "tchol_mmoll", "tchol_mgdl", "hdl_mmoll", "hdl_mgdl", "ldl_mmoll", "ldl_mgdl", "clin_dia", "diagnosed_dx", "dia_med",  "insulin",
        "dm_ctrl", "clin_hypt", "diagnosed_htn","hypt_med", "htn_ctrl","dm_ctrl_rate","htn_ctrl_rate", 
        "statin","dm_rx_rate","htn_rx_rate")
tableTwo <- CreateTableOne(vars = vars, data = dfimp, strata="Region")
tab1r=print(tableTwo, nonnormal = c("age", "sbp_avg", "dbp_avg","fbg", "fbp", "bmi", "hba1c_p", "hba1c_m", "tchol_mmoll", "tchol_mgdl", "hdl_mmoll", "hdl_mgdl", "ldl_mmoll", "ldl_mgdl"), quote=T)
tableTwo <- CreateTableOne(vars = vars, data = dfimp, strata="Country")
tab1c=print(tableTwo, nonnormal = c("age", "sbp_avg", "dbp_avg","fbg", "fbp", "bmi", "hba1c_p", "hba1c_m", "tchol_mmoll", "tchol_mgdl", "hdl_mmoll", "hdl_mgdl", "ldl_mmoll", "ldl_mgdl"), quote=T)
tableTwo <- CreateTableOne(vars = vars, data = dfimp)
tab1a=print(tableTwo, nonnormal = c("age", "sbp_avg", "dbp_avg","fbg", "fbp", "bmi", "hba1c_p", "hba1c_m", "tchol_mmoll", "tchol_mgdl", "hdl_mmoll", "hdl_mgdl", "ldl_mmoll", "ldl_mgdl"), quote=T)
 

dfimp$sex = as.numeric(dfimp$sex)-1
dfimp$csmoke = as.numeric(dfimp$csmoke)-1
dfimp$dia_med = as.numeric(dfimp$dia_med)-1
dfimp$insulin = as.numeric(dfimp$insulin)-1
dfimp$und_dia = as.numeric(dfimp$und_dia)-1
dfimp$statin = as.numeric(dfimp$statin)-1
dfimp$und_hypt = as.numeric(dfimp$und_hypt)-1
dfimp$clin_hypt = as.numeric(dfimp$clin_hypt)-1
dfimp$hypt_med = as.numeric(dfimp$hypt_med)-1
dfimp$mi = as.numeric(dfimp$mi)-1


dfimp$sbp = dfimp$sbp_avg
dfimp$dbp = dfimp$dbp_avg
dfimp$tchol = dfimp$tchol_mmoll
dfimp$tob = dfimp$csmoke
dfimp$dm = 1
dfimp$bprx = dfimp$hypt_med
dfimp$cvdhx = dfimp$mi
dfimp$hdl = dfimp$hdl_mmoll
dfimp$oralrx = dfimp$dia_med
dfimp$a1c = dfimp$hba1c_p
dfimp$p_wt[dfimp$p_wt>1]=1


save(dfimp, file = "dfimp.RData")

load("dfimp.RData")

dfimp$cvdrisk=0
for (i in 1:length(dfimp$age)){
  dfimp$cvdrisk[i] = max(cvdriskchart$cvdrisk[(dfimp$age[i]>=cvdriskchart$agemin & dfimp$Region[i]==cvdriskchart$Region)&
                                                (dfimp$age[i]<cvdriskchart$agemax & dfimp$Region[i]==cvdriskchart$Region)&
                                                (dfimp$sbp[i]>=cvdriskchart$sbpmin & dfimp$Region[i]==cvdriskchart$Region)&
                                                (dfimp$sbp[i]<cvdriskchart$sbpmax & dfimp$Region[i]==cvdriskchart$Region)&
                                                (dfimp$tchol[i]>=cvdriskchart$tcholmin & dfimp$Region[i]==cvdriskchart$Region)&
                                                (dfimp$tchol[i]<cvdriskchart$tcholmax & dfimp$Region[i]==cvdriskchart$Region)&
                                                (dfimp$tob[i]==cvdriskchart$tob & dfimp$Region[i]==cvdriskchart$Region)&
                                                (dfimp$dm[i]==cvdriskchart$dm & dfimp$Region[i]==cvdriskchart$Region)&
                                                (dfimp$sex[i]==cvdriskchart$sex & dfimp$Region[i]==cvdriskchart$Region)],0)
}

dfimp$statin_ind = (dfimp$age>40 | dfimp$cvdrisk>20)
varsToFactor <- c("statin_ind")
dfimp[varsToFactor] <- lapply(dfimp[varsToFactor], factor)
vars =c("statin_ind")
tableTwo <- CreateTableOne(vars = vars, data = dfimp, strata="Region")
tab1r=print(tableTwo, quote=T)
tableTwo <- CreateTableOne(vars = vars, data = dfimp, strata="Country")
tab1c=print(tableTwo, quote=T)
tableTwo <- CreateTableOne(vars = vars, data = dfimp)
tab1a=print(tableTwo, quote=T)


dfimp$chfrisk = (100*(1-0.96^exp(0 + 
                                   5.268e-02 * dfimp$age + 
                                   2.529e-01 * as.numeric(dfimp$sex) + 
                                   -4.969e-02 * as.numeric(dfimp$Region == "CAR" |
                                                             dfimp$Region == "ESSA" |
                                                             dfimp$Region == "SSSA" |
                                                             dfimp$Region == "WSSA" ) + 
                                   2.905e-01*as.numeric(dfimp$tob)+
                                   1.217e-03 * dfimp$sbp + 
                                   6.389e-01 * dfimp$bprx + 
                                   1.007e00 * as.numeric(dfimp$cvdhx)+
                                   -1.175e-01*dfimp$statin+
                                   7.365e-01*0+
                                   4.142e-04 * 5*99.2/11.3 + 
                                   8.214e-01 * 70*0.9/78.6 + 
                                   -1.358e-03 * dfimp$tchol*183.2/4.7 + 
                                   -1.758e-02 * dfimp$hdl*41.8/1.1 +
                                   2.092e-01 * dfimp$a1c +
                                   -5.15)))
dfimp$chfrisk[dfimp$chfhx==1]=1


dfimp$nephrisk = (100*(1-.973^exp(0 + 
                                    -0.0193838993 * dfimp$age + 
                                    -0.0112943865 * as.numeric(dfimp$sex) + 
                                    -0.0881241594 * as.numeric(dfimp$Region == "CAR" |
                                                                 dfimp$Region == "ESSA" |
                                                                 dfimp$Region == "SSSA" |
                                                                 dfimp$Region == "WSSA" ) + 
                                    0.2337712368 * as.numeric(dfimp$Region == "ALA" |
                                                                dfimp$Region == "CLA" |
                                                                dfimp$Region == "SLA"  ) + 
                                    0.0030271330 * dfimp$sbp + 
                                    -0.0795168593 * dfimp$bprx + 
                                    0.1483078052 * as.numeric(dfimp$tob) +
                                    -0.0216363649 * as.numeric(dfimp$cvdhx) + 
                                    -0.1255530728 * dfimp$oralrx + 
                                    0.8608801402 * 70*0.9/78.6 + 
                                    -0.0011121510 * dfimp$tchol*183.2/4.7+ 
                                    0.0062888253 * dfimp$hdl*41.8/1.1 +
                                    0.0319895697 * 0 + 
                                    0.1369126389 * dfimp$a1c +
                                    0.0003615507*5*99.2/11.3
                                  -0.2269629)))


dfimp$retinrisk = (100*(1-.921^exp(0 + 
                                     0.0228504061 * dfimp$age + 
                                     0.2264337097 * as.numeric(dfimp$sex) + 
                                     -0.1676573729 * as.numeric(dfimp$Region == "CAR" |
                                                                  dfimp$Region == "ESSA" |
                                                                  dfimp$Region == "SSSA" |
                                                                  dfimp$Region == "WSSA" ) + 
                                     0.0082431088 * dfimp$sbp + 
                                     0.0639339678 * dfimp$bprx + 
                                     0.1127372373 * as.numeric(dfimp$cvdhx) + 
                                     -0.2348989478 * dfimp$oralrx + 
                                     0.6946500975 * 70*0.9/78.6 + 
                                     -0.0001676169 * dfimp$tchol*183.2/4.7 + 
                                     0.0054470159 * dfimp$hdl*41.8/1.1 +
                                     0.1449446673 * dfimp$a1c +
                                     0.0001991881*5*99.2/11.3
                                   -4.563441)))



dfimp$neurorisk = (100*(1-0.87^exp(0 + 
                                     3.022e-02 * dfimp$age + 
                                     -1.868e-01 * as.numeric(dfimp$sex) + 
                                     -9.448e-02 * as.numeric(dfimp$Region == "CAR" |
                                                               dfimp$Region == "ESSA" |
                                                               dfimp$Region == "SSSA" |
                                                               dfimp$Region == "WSSA" ) + 
                                     4.561e-03 * dfimp$sbp + 
                                     1.819e-01 * dfimp$bprx + 
                                     2.667e-01 * as.numeric(dfimp$cvdhx) + 
                                     -2.575e-01 * dfimp$oralrx+ 
                                     6.044e-01 * 70*0.9/78.6 + 
                                     2.185e-03 * dfimp$tchol*183.2/4.7 + 
                                     -5.389e-03 * dfimp$hdl*41.8/1.1 +
                                     1.887e-01 * dfimp$a1c +
                                     -4.746261)))

dfimp$sumhypogrisk = -8.8533+
  dfimp$age*0.0136+
  dfimp$sex*0.2835+
  dfimp$a1c*0.6870+
  max(0,(dfimp$a1c-7))*0.1323+
  dfimp$sbp*(-0.0026)+
  92.5*(-0.0472)+
  (dfimp$neurorisk>50)*0.5126+
  (dfimp$insulin==1)*dfimp$bmi*(1.76^2)*0.64*0.005+
  (dfimp$oralrx==1)*(-0.3323)+
  (dfimp$retinrisk>50)*0.0226+
  0.85*1.1783+
  max(0,(dfimp$age-45))*0.0391


dfimp$hypogrisk = 2*100*(1/(1+exp(-dfimp$sumhypogrisk)))
dfimp$hypogrisk[dfimp$hypogrisk>100]=100

# kg/m^2 gain/loss
set.seed(1)
dfimp$bmirisk = dfimp$oralrx*rbinom(dfimp$oralrx,1,0.5)*(2.2/(1.66^2)) # 50% of oral rx is sulfonylurea, average weight gain 2.2 kg per av height # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6318231/

dfimp$genitrisk = mean(c(2.6,6.1))
dfimp$dkarisk = 0.75/1000*100

dfimp$amprisk = mean(c(2.8,4.2))/1000*100

dfimp$gidrisk = 0




varsToFactor <- c("hypt_med", "dia_med", "statin")
dfimp[varsToFactor] <- lapply(dfimp[varsToFactor], factor)

vars =c("sbp_avg", "dbp_avg", "hba1c_p","hypt_med", "dia_med", "statin", "cvdrisk", "chfrisk", "nephrisk", "retinrisk", "neurorisk", "hypogrisk","bmirisk","genitrisk","dkarisk","amprisk","gidrisk")
tableTwo <- CreateTableOne(vars = vars, data = dfimp, strata="Country")
tab2c = print(tableTwo, nonnormal = c("sbp_avg", "dbp_avg", "hba1c_p","cvdrisk", "chfrisk", "nephrisk", "retinrisk", "neurorisk", "hypogrisk","bmirisk","genitrisk","dkarisk","amprisk","gidrisk"))

dfimpr = dfimp %>%
  group_by(Region) 

tableTwor <- CreateTableOne(vars = vars, data = dfimp, strata="Region")
tab2r = print(tableTwor, nonnormal = c("sbp_avg", "dbp_avg", "hba1c_p","cvdrisk", "chfrisk", "nephrisk", "retinrisk", "neurorisk", "hypogrisk","bmirisk","genitrisk","dkarisk","amprisk","gidrisk"))

tableTwoa <- CreateTableOne(vars = vars, data = dfimp)
tab2a = print(tableTwoa, nonnormal = c("sbp_avg", "dbp_avg", "hba1c_p","cvdrisk", "chfrisk", "nephrisk", "retinrisk", "neurorisk", "hypogrisk","bmirisk","genitrisk","dkarisk","amprisk","gidrisk"))

prop.table(table(dfimp$diagnosed_htn, (dfimp$sbp_avg>=140 & dfimp$dbp_avg>=90)),2)
prop.table(table(dfimp$hypt_med, dfimp$diagnosed_htn),2)
prop.table(table(dfimp$hypt_med, (dfimp$sbp_avg>=140 | dfimp$dbp_avg>=90)),2)
prop.table(table(dfimp$hypt_med, ((dfimp$diagnosed_htn==1 |  dfimp$sbp_avg>=140 | dfimp$dbp_avg>=90))),2)
prop.table(table(dfimp$hypt_med, (dfimp$sbp_avg<130 & dfimp$dbp_avg<80)),1)
prop.table(table(dfimp$dia_med, dfimp$diagnosed_dx),2)
prop.table(table(dfimp$dia_med, (dfimp$fbg>=7 | dfimp$hba1c_p>7)),2)
prop.table(table(dfimp$dia_med, (dfimp$fbg<7 | dfimp$hba1c_p<=7)),1)
prop.table(table(dfimp$statin, (dfimp$age>=40 | dfimp$cvdrisk>20)),2)


dfm1  =  as.data.frame(dfimp) %>%
  mutate(Diagnosed = diagnosed_dx,
         Treated = dia_med,
         Controlled = hba1c_p<=7 & fbg<7)
dfm2 = as.data.frame(dfimp) %>%
  mutate(Diagnosed = diagnosed_htn,
         Treated  = hypt_med,
         Controlled= sbp_avg<130 & dbp_avg<80)
dfm1$Diagnosed[dfm1$Treated==1 & dfm1$Diagnosed==0] = 1
dfm2$Diagnosed[dfm2$Treated==1 & dfm2$Diagnosed==0] = 1
dfm1$Diagnosed = dfm1$Diagnosed==1
dfm2$Diagnosed = dfm2$Diagnosed==1

dmfunnel = xtabs( ~ Treated + Controlled + Diagnosed, data = dfm1)
cotabplot(dmfunnel,
          set_labels=list(Diagnosed = c("Undiagnosed DM", "Diagnosed DM"),
                          Treated = c("Untreated", "Treated"),
                          Controlled = c("Uncontrolled", "Controlled")))

htnfunnel = xtabs( ~ Treated + Controlled + Diagnosed, data = dfm2)
cotabplot(htnfunnel,
          set_labels=list(Diagnosed = c("Undiagnosed HTN", "Diagnosed HTN"),
                          Treated = c("Untreated", "Treated"),
                          Controlled = c("Uncontrolled", "Controlled")))




currrates = dfimp %>%
  group_by(Country) %>%
  summarise(n_dm = n(),
            n_bprx = sum(bprx==1),
            n_dmrx = sum(oralrx==1),
            n_strx = sum(statin==1),
            n_htn  = sum(diagnosed_htn==1 | sbp_avg>=140 | dbp_avg>=90 | bprx==1),
            n_hba1c7  = sum(hba1c_p>7  | fbg>=7),
            n_stat = sum(age>40 | cvdrisk>20),
            dx_dm = sum(und_dia==0),
            dx_htn = sum(diagnosed_htn==1),
            treated_bp_n  = sum((bprx==1) & (diagnosed_htn==1 )),
            treated_dm_n = sum((oralrx==1) & (und_dia==0)),
            treated_st_n = sum((statin==1) & (age>40 | cvdrisk>20)),
            control_bp_n = sum(sbp_avg<130 & dbp_avg<80 & ((bprx==1) & diagnosed_htn==1)),
            control_dm_n = sum((hba1c_p<=7 | fbg<7) & oralrx==1 & (und_dia==0))) %>%
  select(Country,
         n_dm, n_bprx, n_dmrx, n_strx, n_htn, n_hba1c7, n_stat,
         dx_dm,dx_htn,treated_bp_n, treated_dm_n, treated_st_n,  control_bp_n, control_dm_n)





currrates_r = dfimp %>%
  group_by(Region) %>%
  summarise(n_dm = n(),
            n_bprx = sum(bprx==1),
            n_dmrx = sum(oralrx==1),
            n_strx = sum(statin==1),
            n_htn  = sum(diagnosed_htn==1 | sbp_avg>=140 | dbp_avg>=90  | bprx==1),
            n_hba1c7  = sum(hba1c_p>7  | fbg>=7),
            n_stat = sum(age>40 | cvdrisk>20),
            dx_dm = sum(und_dia==0),
            dx_htn = sum(diagnosed_htn==1),
            treated_bp_n  = sum((bprx==1) & (diagnosed_htn==1 )),
            treated_dm_n = sum((oralrx==1) & (und_dia==0)),
            treated_st_n = sum((statin==1) & (age>40 | cvdrisk>20)),
            control_bp_n = sum(sbp_avg<130 & dbp_avg<80 & ((bprx==1) & diagnosed_htn==1)),
            control_dm_n = sum((hba1c_p<=7 | fbg<7) & oralrx==1 & (und_dia==0))) %>%
  select(Region,
         n_dm, n_bprx, n_dmrx, n_strx, n_htn, n_hba1c7, n_stat,
         dx_dm,dx_htn,treated_bp_n, treated_dm_n, treated_st_n,  control_bp_n, control_dm_n)


currrates_a = dfimp %>%
  summarise(n_dm = n(),
            n_bprx = sum(bprx==1),
            n_dmrx = sum(oralrx==1),
            n_strx = sum(statin==1),
            n_htn  = sum(diagnosed_htn==1 | sbp_avg>=140 | dbp_avg>=90  | bprx==1),
            n_hba1c7  = sum(hba1c_p>7  | fbg>=7),
            n_stat = sum(age>40 | cvdrisk>20),
            dx_dm = sum(und_dia==0),
            dx_htn = sum(diagnosed_htn==1),
            treated_bp_n  = sum((bprx==1) & (diagnosed_htn==1 )),
            treated_dm_n = sum((oralrx==1) & (und_dia==0)),
            treated_st_n = sum((statin==1) & (age>40 | cvdrisk>20)),
            control_bp_n = sum(sbp_avg<130 & dbp_avg<80 & ((bprx==1) & diagnosed_htn==1)),
            control_dm_n = sum((hba1c_p<=7 | fbg<7) & oralrx==1 & (und_dia==0))) %>%
  select(n_dm, n_bprx, n_dmrx, n_strx, n_htn, n_hba1c7, n_stat,
         dx_dm,dx_htn,treated_bp_n, treated_dm_n, treated_st_n,  control_bp_n, control_dm_n)


dfimp$cvdrisk[is.na(dfimp$cvdrisk)] = mean(na.omit(dfimp$cvdrisk))
dfimp$chfrisk[is.na(dfimp$chfrisk)] = mean(na.omit(dfimp$chfrisk))
dfimp$nephrisk[is.na(dfimp$nephrisk)] = mean(na.omit(dfimp$nephrisk))
dfimp$retinrisk[is.na(dfimp$retinrisk)] = mean(na.omit(dfimp$retinrisk))
dfimp$neurorisk[is.na(dfimp$neurorisk)] = mean(na.omit(dfimp$neurorisk))
dfimp$hypogrisk[is.na(dfimp$hypogrisk)] = mean(na.omit(dfimp$hypogrisk))

save(dfimp, file = "dfimp.RData")

