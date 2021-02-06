rm(list=ls())
library(haven)
library(tidyverse)
library(tableone)
library(matrixStats)
library(mice)
library(sjPlot)
library(sjmisc)

setwd("~/Box/Research/Research projects/WHO diabetes")
  
df <- read_dta("2021_02_04 - Appended dataset microsimulation.dta") #read_dta("HPACC_2019data_CLEAN_2020-09-23.dta")
cost = read_csv("cost.csv")
cnt_reg = read_csv("cnt_reg.csv")
cvdriskchart = read_csv("cvdriskchart.csv")
cvdriskchart = cvdriskchart %>%
  filter(dm==1)

df$Country = df$country
df$p_wt = df$wstep1_micsim
df$sex = df$sex_micsim
df$age = df$age_micsim
df$hypt_med =  df$hypt_med_micsim
df$sbp_avg = df$sbp_avg_micsim
df$dbp_avg = df$dbp_avg_micsim
df$dia_med = df$dia_med_micsim
df$insulin  = df$insulin_micsim
df$fbp = df$fbg_micsim
df$bmi = df$bmi_micsim
df$csmoke = df$csmoke_micsim
df$hba1c_p = df$hba1c_p_micsim
df$hba1c_m = 10.929*(df$hba1c_p -2.15)
df$und_dia  = 1-df$hbg_micsim
df$clin_dia =  df$clin_dia_micsim
df$tchol_mgdl = df$tchol_mgdl_micsim
df$tchol_mmoll = df$tchol_mgdl/38.67
df$hdl_mgdl = df$hdl_mgdl_micsim
df$hdl_mmoll = df$hdl_mgdl/38.67
df$ldl_mgdl =  df$ldl_mgdl_micsim
df$ldl_mmoll = df$ldl_mgdl*.02586
df$mi = df$mi_micsim
df$statin = df$statin_micsim
df$und_hypt = 1-df$hypt_micsim
df$clin_hypt =  df$clin_hypt_micsim
df$hbg = df$hbg_micsim
df$fbg = df$fbg_micsim

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
         hba1c_m, hba1c_p, und_dia, clin_dia, tchol_mmoll, tchol_mgdl, hdl_mmoll, hdl_mgdl, ldl_mgdl, ldl_mmoll,
         mi, statin, und_hypt, clin_hypt) %>%
  left_join(cnt_reg,by="Country")

varsToFactor <- c("sex","hypt_med", "dia_med", "insulin", "csmoke", "und_dia", "clin_dia", "mi", "statin", "und_hypt", "clin_hypt")
dfsub[varsToFactor] <- lapply(dfsub[varsToFactor], factor)

vars =c("sex", "age", "hypt_med", "sbp_avg", "dbp_avg", "dia_med", "insulin", "fbg", "bmi", "csmoke", "hba1c_m", "hba1c_p", "und_dia", "clin_dia", "tchol_mmoll", "tchol_mgdl", "hdl_mmoll", "hdl_mgdl", "ldl_mgdl", "ldl_mmoll",
        "mi", "statin", "und_hypt", "clin_hypt")
tableOne <- CreateTableOne(vars = vars, data = dfsub)
print(tableOne, nonnormal = c("age", "sbp_avg", "dbp_avg", "fbg","fbp", "bmi", "hba1c_p", "hba1c_m", "tchol_mmoll", "tchol_mgdl", "hdl_mmoll", "hdl_mgdl", "ldl_mgdl", "ldl_mmoll"), quote=T)
summary(dfsub$hba1c_m)
summary(dfsub$ldl_mmoll)
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
dfimp$hba1c_p = (dfimp$hba1c_m/10.929)+2.15
dfimp$tchol_mgdl = dfimp$tchol_mmoll*38.67
dfimp$hdl_mgdl = dfimp$hdl_mmoll*38.67
dfimp$ldl_mmoll = dfimp$ldl_mgdl*.02586
dfimp =dfimp %>% mutate(diagnosed_dx = 1-as.numeric(und_dia),
                        diagnosed_htn =  1-as.numeric(und_hypt))
dfimp$diagnosed_dx = as.factor(dfimp$diagnosed_dx+1)
dfimp$diagnosed_htn = as.factor(dfimp$diagnosed_htn+1)

dfimp[varsToFactor] <- lapply(dfimp[varsToFactor], factor)
vars =c("sex", "age", "hypt_med", "sbp_avg", "dbp_avg", "dia_med", "insulin", "fbg", "bmi", "csmoke", "hba1c_m", "hba1c_p", "diagnosed_dx", "clin_dia", "tchol_mmoll", "tchol_mgdl", "hdl_mmoll", "hdl_mgdl", "ldl_mgdl", "ldl_mmoll",
  "mi", "statin", "diagnosed_htn", "clin_hypt")
tableTwo <- CreateTableOne(vars = vars, data = dfimp, strata="Region")
tab1r=print(tableTwo, nonnormal = c("age", "sbp_avg", "dbp_avg","fbg", "fbp", "bmi", "hba1c_p", "hba1c_m", "tchol_mmoll", "tchol_mgdl", "hdl_mmoll", "hdl_mgdl", "ldl_mgdl", "ldl_mmoll"), quote=T)
tableTwo <- CreateTableOne(vars = vars, data = dfimp, strata="Country")
tab1c=print(tableTwo, nonnormal = c("age", "sbp_avg", "dbp_avg","fbg", "fbp", "bmi", "hba1c_p", "hba1c_m", "tchol_mmoll", "tchol_mgdl", "hdl_mmoll", "hdl_mgdl", "ldl_mgdl", "ldl_mmoll"), quote=T)
tableTwo <- CreateTableOne(vars = vars, data = dfimp)
tab1a=print(tableTwo, nonnormal = c("age", "sbp_avg", "dbp_avg","fbg", "fbp", "bmi", "hba1c_p", "hba1c_m", "tchol_mmoll", "tchol_mgdl", "hdl_mmoll", "hdl_mgdl", "ldl_mgdl", "ldl_mmoll"), quote=T)


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



varsToFactor <- c("hypt_med", "dia_med", "statin")
dfimp[varsToFactor] <- lapply(dfimp[varsToFactor], factor)

vars =c("sbp_avg", "dbp_avg", "hba1c_p","hypt_med", "dia_med", "statin", "cvdrisk", "chfrisk", "nephrisk", "retinrisk", "neurorisk")
tableTwo <- CreateTableOne(vars = vars, data = dfimp, strata="Country")
tab2c = print(tableTwo, nonnormal = c("sbp_avg", "dbp_avg", "hba1c_p","cvdrisk", "chfrisk", "nephrisk", "retinrisk", "neurorisk"))

dfimpr = dfimp %>%
  group_by(Region) 

tableTwor <- CreateTableOne(vars = vars, data = dfimp, strata="Region")
tab2r = print(tableTwor, nonnormal = c("sbp_avg", "dbp_avg", "hba1c_p","cvdrisk", "chfrisk", "nephrisk", "retinrisk", "neurorisk"))

tableTwoa <- CreateTableOne(vars = vars, data = dfimp)
tab2a = print(tableTwoa, nonnormal = c("sbp_avg", "dbp_avg", "hba1c_p","cvdrisk", "chfrisk", "nephrisk", "retinrisk", "neurorisk"))

# library(survey)
# dfimpsvy = dfimp %>%
#   group_by(Country) %>%
#   mutate(positionInCategory = 1:n())
#   
#   
# dfimpsvy =  svydesign(id  = ~positionInCategory,  weights = ~p_wt, data = dfimpsvy)
# tableTwow <- svyCreateTableOne(vars = vars, data = dfimpsvy, strata="Country")
# tab2w = print(tableTwow, nonnormal = c("sbp_avg", "dbp_avg", "hba1c_p","cvdrisk", "chfrisk", "nephrisk", "retinrisk", "neurorisk"))
# 
# dfimpsvy_diag = dfimp %>%
#   filter(und_dia==0) %>%
#   group_by(Country) %>%
#   mutate(positionInCategory = 1:n()) 
# 
# 
# dfimpsvy_diag =  svydesign(id  = ~positionInCategory,  weights = ~p_wt, data = dfimpsvy_diag)
# tableTwod <- svyCreateTableOne(vars = vars, data = dfimpsvy_diag, strata="Country")
# tab2d = print(tableTwod, nonnormal = c("sbp_avg", "dbp_avg", "hba1c_p","cvdrisk", "chfrisk", "nephrisk", "retinrisk", "neurorisk"))
# 
# 
# dfimpsvy_diagr = dfimp %>%
#   filter(und_dia==0) %>%
#   group_by(Region) %>%
#   mutate(positionInCategory = 1:n()) 
# 
# 
# dfimpsvy_diagr =  svydesign(id  = ~positionInCategory,  weights = ~p_wt, data = dfimpsvy_diagr) 
# tableTwodr <- svyCreateTableOne(vars = vars, data = dfimpsvy_diagr, strata="Region")
# tab2dr = print(tableTwodr, nonnormal = c("sbp_avg", "dbp_avg", "hba1c_p","cvdrisk", "chfrisk", "nephrisk", "retinrisk", "neurorisk"))




# current rate of being diagnosed with  DM
frq(dfimp$und_dia) # 33.5% are  diagnosed

# current treatment rates, all cases (not just diagnosed)
sjt.xtab(dfimp$sbp_avg>130,dfimp$bprx,show.cell.prc = T) # 9.2% for those with HTN  are treated
frq(dfimp$oralrx) # 29.1% on DM Rx
sjt.xtab(dfimp$age>40, dfimp$statin==1,show.cell.prc = T) # 0.3% above 40 yrs old on statin

# current control rates
sjt.xtab(dfimp$bprx, dfimp$sbp_avg<=130,show.cell.prc = T) # 8.4% HTN control
sjt.xtab(dfimp$hba1c_p<=7, dfimp$oralrx,show.cell.prc = T) # 14.3% A1c control

dfimp %>%
  summarise(n_dm = n(),
            n_bprx = sum(bprx==1),
            n_dmrx = sum(oralrx==1),
            n_strx = sum(statin==1),
            n_htn  = sum(sbp_avg>130 | dbp_avg>80),
            n_hba1c7  = sum(hba1c_p>7  | fbg>=7),
            n_stat = sum(age>40),
            dx_rate = sum(und_dia==0)/n(),
            treated_bp  = sum(bprx)/n_htn,
            treated_dm = sum(oralrx)/n(),
            treated_st = sum(statin==1)/sum(age>40 | cvdrisk>20),
            control_bp = sum(sbp_avg<130 & dbp_avg<80 & bprx==1)/max(1,sum(bprx)),
            control_dm = sum((hba1c_p<=7 | fbg<7) & oralrx==1)/max(1,sum(oralrx))) %>%
  select(n_dm, n_bprx, n_dmrx, n_strx, n_htn, n_hba1c7, n_stat,
         dx_rate,treated_bp, treated_dm, treated_st,  control_bp, control_dm)

currrates = dfimp %>%
  group_by(Country) %>%
  summarise(n_dm = n(),
            n_bprx = sum(bprx==1),
            n_dmrx = sum(oralrx==1),
            n_strx = sum(statin==1),
            n_htn  = sum(sbp_avg>130 | dbp_avg>80),
            n_hba1c7  = sum(hba1c_p>7  | fbg>=7),
            n_stat = sum(age>40),
            dx_rate = sum(und_dia==0)/n(),
            treated_bp  = sum(bprx)/n_htn,
            treated_dm = sum(oralrx)/n(),
            treated_st = sum(statin==1)/sum(age>40 | cvdrisk>20),
            control_bp = sum(sbp_avg<130 & dbp_avg<80 & bprx==1)/max(1,sum(bprx)),
            control_dm = sum((hba1c_p<=7 | fbg<7) & oralrx==1)/max(1,sum(oralrx))) %>%
  select(Country,
         n_dm, n_bprx, n_dmrx, n_strx, n_htn, n_hba1c7, n_stat,
         dx_rate,treated_bp, treated_dm, treated_st,  control_bp, control_dm)

currrates_r = dfimp %>%
  group_by(Region) %>%
  summarise(n_dm = n(),
            n_bprx = sum(bprx==1),
            n_dmrx = sum(oralrx==1),
            n_strx = sum(statin==1),
            n_htn  = sum(sbp_avg>130 | dbp_avg>80),
            n_hba1c7  = sum(hba1c_p>7  | fbg>=7),
            n_stat = sum(age>40),
            dx_rate = sum(und_dia==0)/n(),
            treated_bp  = sum(bprx)/n_htn,
            treated_dm = sum(oralrx)/n(),
            treated_st = sum(statin==1)/sum(age>40 | cvdrisk>20),
            control_bp = sum(sbp_avg<130 & dbp_avg<80 & bprx==1)/max(1,sum(bprx)),
            control_dm = sum((hba1c_p<=7 | fbg<7) & oralrx==1)/max(1,sum(oralrx))) %>%
  select(Region,
         n_dm, n_bprx, n_dmrx, n_strx, n_htn, n_hba1c7, n_stat,
         dx_rate,treated_bp, treated_dm, treated_st,  control_bp, control_dm)

dfimp$cvdrisk[is.na(dfimp$cvdrisk)] = mean(na.omit(dfimp$cvdrisk))
dfimp$chfrisk[is.na(dfimp$chfrisk)] = mean(na.omit(dfimp$chfrisk))

dfimp$nephrisk[is.na(dfimp$nephrisk)] = mean(na.omit(dfimp$nephrisk))

dfimp$retinrisk[is.na(dfimp$retinrisk)] = mean(na.omit(dfimp$retinrisk))

dfimp$neurorisk[is.na(dfimp$neurorisk)] = mean(na.omit(dfimp$neurorisk))

save(dfimp, file = "dfimp")

