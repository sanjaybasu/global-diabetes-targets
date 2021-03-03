
setwd("~/Downloads/Results matrices")

library(tidyverse)
library(ggplot2)
library(robustbase)
cnt_reg = read_csv("~/Downloads/cnt_reg.csv")


load("outmat_cvdevents3")
load("outmat_chfevents3")
load("outmat_nephevents3")
load("outmat_retinevents3")
load("outmat_neuroevents3")

load("outmat_cvddeaths3")
load("outmat_chfdeaths3")
load("outmat_nephdeaths3")
load("outmat_retindeaths3")
load("outmat_neurodeaths3")

load("outmat_cvddalys3")
load("outmat_chfdalys3")
load("outmat_nephdalys3")
load("outmat_retindalys3")
load("outmat_neurodalys3")

load("outmat_rxbpcosts3")
load("outmat_rxdmcosts3")
load("outmat_rxstatincosts3")

load("outmat_cvdcosts3")
load("outmat_chfcosts3")
load("outmat_nephcosts3")
load("outmat_retincosts3")
load("outmat_neurocosts3")


##### DALYS #####
cvddalys = outmat_cvddalys %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_all(median) %>% mutate(outcome = "cvd")
chfdalys = outmat_chfdalys %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_all(median) %>% mutate(outcome = "chf")
nephdalys = outmat_nephdalys %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_all(median) %>% mutate(outcome = "neph")
neurodalys = outmat_neurodalys %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_all(median) %>% mutate(outcome = "neuro")
retindalys = outmat_retindalys %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_all(median)  %>% mutate(outcome = "retin")

totdalys = rbind(cvddalys,chfdalys,nephdalys,neurodalys,retindalys)
tot = totdalys  %>%
  group_by(Country) %>%
  select(2:6)%>%
  summarize_all(sum)
summary(tot)
colMedians(sapply(tot[2:6],as.numeric))
totd=   totdalys %>%
  group_by(outcome) %>%
  select(2:6) %>%
  summarize_all(mean)
cbind(totd[,1],totd[,2:6]*1000)
colSums(totd[,2:6])
totd[,2:6]/colSums(totd[,2:6])
tottab = totdalys %>%
  left_join(cnt_reg) %>%
  group_by(Region) %>%
  select(2:6)%>%
  summarize_all(sum)


# 25th centile

cvddalys25 = outmat_cvddalys %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q1=~quantile(., probs = 0.25))) %>% mutate(outcome = "cvd")
chfdalys25 = outmat_chfdalys %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_at(vars(colnames(outmat_chfdalys[,2:6])), list(Q1=~quantile(., probs = 0.25))) %>% mutate(outcome = "chf")
nephdalys25 = outmat_nephdalys %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_at(vars(colnames(outmat_nephdalys[,2:6])), list(Q1=~quantile(., probs = 0.25))) %>% mutate(outcome = "neph")
neurodalys25 = outmat_neurodalys %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_at(vars(colnames(outmat_neurodalys[,2:6])), list(Q1=~quantile(., probs = 0.25))) %>% mutate(outcome = "neuro")
retindalys25 = outmat_retindalys %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_at(vars(colnames(outmat_retindalys[,2:6])), list(Q1=~quantile(., probs = 0.25)))  %>% mutate(outcome = "retin")


totdalys25 = rbind(cvddalys25,chfdalys25,nephdalys25,neurodalys25,retindalys25)
tot25 = totdalys25  %>%
  group_by(Country) %>%
  select(2:6)%>%
  summarize_all(sum)
summary(tot25)
colMedians(sapply(tot25[2:6],as.numeric))
totd25 =  totdalys25 %>%
  group_by(outcome) %>%
  select(2:6) %>%
  summarize_all(mean)
cbind(totd25[,1],totd25[,2:6]*1000)
colSums(totd25[,2:6])
totd25[,2:6]/colSums(totd25[,2:6])
tottab25 = totdalys25 %>%
  left_join(cnt_reg) %>%
  group_by(Region) %>%
  select(2:6)%>%
  summarize_all(sum)

#75th centile


cvddalys75 = outmat_cvddalys %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q1=~quantile(., probs = 0.75))) %>% mutate(outcome = "cvd")
chfdalys75 = outmat_chfdalys %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_at(vars(colnames(outmat_chfdalys[,2:6])), list(Q1=~quantile(., probs = 0.75))) %>% mutate(outcome = "chf")
nephdalys75 = outmat_nephdalys %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_at(vars(colnames(outmat_nephdalys[,2:6])), list(Q1=~quantile(., probs = 0.75))) %>% mutate(outcome = "neph")
neurodalys75 = outmat_neurodalys %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_at(vars(colnames(outmat_neurodalys[,2:6])), list(Q1=~quantile(., probs = 0.75))) %>% mutate(outcome = "neuro")
retindalys75 = outmat_retindalys %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_at(vars(colnames(outmat_retindalys[,2:6])), list(Q1=~quantile(., probs = 0.75)))  %>% mutate(outcome = "retin")


totdalys75 = rbind(cvddalys75,chfdalys75,nephdalys75,neurodalys75,retindalys75)
tot75 = totdalys75  %>%
  group_by(Country) %>%
  select(2:6)%>%
  summarize_all(sum)
summary(tot75)
colMedians(sapply(tot75[2:6],as.numeric))
totd75 =  totdalys75 %>%
  group_by(outcome) %>%
  select(2:6) %>%
  summarize_all(mean)
cbind(totd75[,1],totd75[,2:6]*1000)
colSums(totd75[,2:6])
totd75[,2:6]/colSums(totd75[,2:6])
tottab75 = totdalys75 %>%
  left_join(cnt_reg) %>%
  group_by(Region) %>%
  select(2:6)%>%
  summarize_all(sum)

# BY REGION AND OUTCOME

cvddalys = outmat_cvddalys %>% left_join(cnt_reg) %>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_all(median) %>% mutate(outcome = "cvd")
chfdalys = outmat_chfdalys %>% left_join(cnt_reg) %>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_all(median) %>% mutate(outcome = "chf")
nephdalys = outmat_nephdalys %>% left_join(cnt_reg) %>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_all(median) %>% mutate(outcome = "neph")
neurodalys = outmat_neurodalys %>% left_join(cnt_reg) %>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_all(median) %>% mutate(outcome = "neuro")
retindalys = outmat_retindalys %>% left_join(cnt_reg) %>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_all(median)  %>% mutate(outcome = "retin")

totdalys = rbind(cvddalys,chfdalys,nephdalys,neurodalys,retindalys)
totdalys %>% select(3:8) %>% group_by(outcome) %>% summarize_all(median)


# 25th centile

cvddalys25 = outmat_cvddalys %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q1=~quantile(., probs = 0.25))) %>% mutate(outcome = "cvd")
chfdalys25 = outmat_chfdalys %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_at(vars(colnames(outmat_chfdalys[,2:6])), list(Q1=~quantile(., probs = 0.25))) %>% mutate(outcome = "chf")
nephdalys25 = outmat_nephdalys %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_at(vars(colnames(outmat_nephdalys[,2:6])), list(Q1=~quantile(., probs = 0.25))) %>% mutate(outcome = "neph")
neurodalys25 = outmat_neurodalys %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_at(vars(colnames(outmat_neurodalys[,2:6])), list(Q1=~quantile(., probs = 0.25))) %>% mutate(outcome = "neuro")
retindalys25 = outmat_retindalys %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_at(vars(colnames(outmat_retindalys[,2:6])), list(Q1=~quantile(., probs = 0.25)))  %>% mutate(outcome = "retin")


totdalys25 = rbind(cvddalys25,chfdalys25,nephdalys25,neurodalys25,retindalys25)
totdalys25 %>% select(2:7) %>% group_by(outcome) %>% summarize_all(median)

#75th centile


cvddalys75 = outmat_cvddalys %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q1=~quantile(., probs = 0.75))) %>% mutate(outcome = "cvd")
chfdalys75 = outmat_chfdalys %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_at(vars(colnames(outmat_chfdalys[,2:6])), list(Q1=~quantile(., probs = 0.75))) %>% mutate(outcome = "chf")
nephdalys75 = outmat_nephdalys %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_at(vars(colnames(outmat_nephdalys[,2:6])), list(Q1=~quantile(., probs = 0.75))) %>% mutate(outcome = "neph")
neurodalys75 = outmat_neurodalys %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_at(vars(colnames(outmat_neurodalys[,2:6])), list(Q1=~quantile(., probs = 0.75))) %>% mutate(outcome = "neuro")
retindalys75 = outmat_retindalys %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_at(vars(colnames(outmat_retindalys[,2:6])), list(Q1=~quantile(., probs = 0.75)))  %>% mutate(outcome = "retin")


totdalys75 = rbind(cvddalys75,chfdalys75,nephdalys75,neurodalys75,retindalys75)
totdalys75 %>% select(2:7) %>% group_by(outcome) %>% summarize_all(median)



##### COSTS #####

rxbpcosts = outmat_rxbpcosts %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_all(median, na.rm = T) %>% mutate(outcome = "rxbp")
rxdmcosts = outmat_rxdmcosts %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_all(median, na.rm = T) %>% mutate(outcome = "rxdm")
rxstatincosts = outmat_rxstatincosts %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_all(median, na.rm = T) %>% mutate(outcome = "rxstatin")

cvdcosts = outmat_cvdcosts %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_all(median, na.rm = T) %>% mutate(outcome = "cvd")
chfcosts = outmat_chfcosts %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_all(median, na.rm = T) %>% mutate(outcome = "chf")
nephcosts = outmat_nephcosts %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_all(median, na.rm = T) %>% mutate(outcome = "neph")
neurocosts = outmat_neurocosts %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_all(median, na.rm = T) %>% mutate(outcome = "neuro")
retincosts = outmat_retincosts %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_all(median, na.rm = T)  %>% mutate(outcome = "retin")

totcosts = rbind(rxbpcosts,rxdmcosts,rxstatincosts,cvdcosts,chfcosts,nephcosts,neurocosts,retincosts)
tot = totcosts  %>%
  group_by(Country) %>%
  select(2:6)%>%
  summarize_all(sum)
summary(tot)
colMedians(sapply(tot[2:6],as.numeric))
totc=   totcosts %>%
  group_by(outcome) %>%
  select(2:6) %>%
  summarize_all(mean)
cbind(totc[,1],totc[,2:6]*1000)
colSums(totc[,2:6])*1000
totc[,2:6]/colSums(totc[,2:6])
tottab = totcosts %>%
  left_join(cnt_reg) %>%
  group_by(Region) %>%
  select(2:6)%>%
  summarize_all(sum)

# 25th centile

rxbpcosts = outmat_rxbpcosts %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q1=~quantile(., probs = 0.25))) %>% mutate(outcome = "rxbp")
rxdmcosts = outmat_rxdmcosts %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q1=~quantile(., probs = 0.25))) %>% mutate(outcome = "rxdm")
rxstatincosts = outmat_rxstatincosts %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q1=~quantile(., probs = 0.25))) %>% mutate(outcome = "rxstatin")

cvdcosts = outmat_cvdcosts %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q1=~quantile(., probs = 0.25))) %>% mutate(outcome = "cvd")
chfcosts = outmat_chfcosts %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q1=~quantile(., probs = 0.25))) %>% mutate(outcome = "chf")
nephcosts = outmat_nephcosts %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q1=~quantile(., probs = 0.25))) %>% mutate(outcome = "neph")
neurocosts = outmat_neurocosts %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q1=~quantile(., probs = 0.25))) %>% mutate(outcome = "neuro")
retincosts = outmat_retincosts %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q1=~quantile(., probs = 0.25)))  %>% mutate(outcome = "retin")

totcosts = rbind(rxbpcosts,rxdmcosts,rxstatincosts,cvdcosts,chfcosts,nephcosts,neurocosts,retincosts)
tot = totcosts  %>%
  group_by(Country) %>%
  select(2:6)%>%
  summarize_all(sum)
summary(tot)
colMedians(sapply(tot[2:6],as.numeric))
totc=   totcosts %>%
  group_by(outcome) %>%
  select(2:6) %>%
  summarize_all(mean)
cbind(totc[,1],totc[,2:6]*1000)
colSums(totc[,2:6])*1000
totc[,2:6]/colSums(totc[,2:6])
tottab = totcosts %>%
  left_join(cnt_reg) %>%
  group_by(Region) %>%
  select(2:6)%>%
  summarize_all(sum)


# 75th centile

rxbpcosts = outmat_rxbpcosts %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q3=~quantile(., probs = 0.75))) %>% mutate(outcome = "rxbp")
rxdmcosts = outmat_rxdmcosts %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q3=~quantile(., probs = 0.75))) %>% mutate(outcome = "rxdm")
rxstatincosts = outmat_rxstatincosts %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q3=~quantile(., probs = 0.75))) %>% mutate(outcome = "rxstatin")

cvdcosts = outmat_cvdcosts %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q3=~quantile(., probs = 0.75))) %>% mutate(outcome = "cvd")
chfcosts = outmat_chfcosts %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q3=~quantile(., probs = 0.75))) %>% mutate(outcome = "chf")
nephcosts = outmat_nephcosts %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q3=~quantile(., probs = 0.75))) %>% mutate(outcome = "neph")
neurocosts = outmat_neurocosts %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q3=~quantile(., probs = 0.75))) %>% mutate(outcome = "neuro")
retincosts = outmat_retincosts %>% mutate_at(2:6,as.numeric) %>% group_by(Country) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q3=~quantile(., probs = 0.75)))  %>% mutate(outcome = "retin")

totcosts = rbind(rxbpcosts,rxdmcosts,rxstatincosts,cvdcosts,chfcosts,nephcosts,neurocosts,retincosts)
tot = totcosts  %>%
  group_by(Country) %>%
  select(2:6)%>%
  summarize_all(sum)
summary(tot)
colMedians(sapply(tot[2:6],as.numeric))
totc=   totcosts %>%
  group_by(outcome) %>%
  select(2:6) %>%
  summarize_all(mean)
cbind(totc[,1],totc[,2:6]*1000)
colSums(totc[,2:6])*1000
totc[,2:6]/colSums(totc[,2:6])
tottab = totcosts %>%
  left_join(cnt_reg) %>%
  group_by(Region) %>%
  select(2:6)%>%
  summarize_all(sum)

# by region and by outcome

rxbpcosts = outmat_rxbpcosts %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_all(median, na.rm = T) %>% mutate(outcome = "rxbp")
rxdmcosts = outmat_rxdmcosts %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_all(median, na.rm = T) %>% mutate(outcome = "rxdm")
rxstatincosts = outmat_rxstatincosts %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_all(median, na.rm = T) %>% mutate(outcome = "rxstatin")

cvdcosts = outmat_cvdcosts %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_all(median, na.rm = T) %>% mutate(outcome = "cvd")
chfcosts = outmat_chfcosts %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_all(median, na.rm = T) %>% mutate(outcome = "chf")
nephcosts = outmat_nephcosts %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_all(median, na.rm = T) %>% mutate(outcome = "neph")
neurocosts = outmat_neurocosts %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_all(median, na.rm = T) %>% mutate(outcome = "neuro")
retincosts = outmat_retincosts %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_all(median, na.rm = T)  %>% mutate(outcome = "retin")

totcosts = rbind(rxbpcosts,rxdmcosts,rxstatincosts,cvdcosts,chfcosts,nephcosts,neurocosts,retincosts)
totcosts %>% select(3:8) %>% group_by(outcome) %>% summarize_all(median)


# 25th centile

rxbpcosts = outmat_rxbpcosts %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q1=~quantile(., probs = 0.25))) %>% mutate(outcome = "rxbp")
rxdmcosts = outmat_rxdmcosts %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q1=~quantile(., probs = 0.25))) %>% mutate(outcome = "rxdm")
rxstatincosts = outmat_rxstatincosts %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q1=~quantile(., probs = 0.25))) %>% mutate(outcome = "rxstatin")

cvdcosts = outmat_cvdcosts %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q1=~quantile(., probs = 0.25))) %>% mutate(outcome = "cvd")
chfcosts = outmat_chfcosts %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q1=~quantile(., probs = 0.25))) %>% mutate(outcome = "chf")
nephcosts = outmat_nephcosts %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q1=~quantile(., probs = 0.25))) %>% mutate(outcome = "neph")
neurocosts = outmat_neurocosts %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q1=~quantile(., probs = 0.25))) %>% mutate(outcome = "neuro")
retincosts = outmat_retincosts %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q1=~quantile(., probs = 0.25)))  %>% mutate(outcome = "retin")

totcosts = rbind(rxbpcosts,rxdmcosts,rxstatincosts,cvdcosts,chfcosts,nephcosts,neurocosts,retincosts)
totcosts %>% select(2:7) %>% group_by(outcome) %>% summarize_all(median)



# 75th centile

rxbpcosts = outmat_rxbpcosts %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q3=~quantile(., probs = 0.75))) %>% mutate(outcome = "rxbp")
rxdmcosts = outmat_rxdmcosts %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q3=~quantile(., probs = 0.75))) %>% mutate(outcome = "rxdm")
rxstatincosts = outmat_rxstatincosts %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q3=~quantile(., probs = 0.75))) %>% mutate(outcome = "rxstatin")

cvdcosts = outmat_cvdcosts %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q3=~quantile(., probs = 0.75))) %>% mutate(outcome = "cvd")
chfcosts = outmat_chfcosts %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q3=~quantile(., probs = 0.75))) %>% mutate(outcome = "chf")
nephcosts = outmat_nephcosts %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q3=~quantile(., probs = 0.75))) %>% mutate(outcome = "neph")
neurocosts = outmat_neurocosts %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q3=~quantile(., probs = 0.75))) %>% mutate(outcome = "neuro")
retincosts = outmat_retincosts %>% left_join(cnt_reg)%>% mutate_at(2:6,as.numeric) %>% group_by(Region) %>% summarise_at(vars(colnames(outmat_cvddalys[,2:6])), list(Q3=~quantile(., probs = 0.75)))  %>% mutate(outcome = "retin")

totcosts = rbind(rxbpcosts,rxdmcosts,rxstatincosts,cvdcosts,chfcosts,nephcosts,neurocosts,retincosts)
totcosts %>% select(2:7) %>% group_by(outcome) %>% summarize_all(median)






