# WHO diabetes plots

setwd("~/Box/Research/Research projects/WHO diabetes/Results matrices")

library(tidyverse)
library(ggplot2)
cnt_reg = read_csv("~/Box/Research/Research projects/WHO diabetes/cnt_reg.csv")


###### NO NEW  SCREENING ######

  
load("outmat_cvdevents1")
load("outmat_chfevents1")
load("outmat_nephevents1")
load("outmat_retinevents1")
load("outmat_neuroevents1")

load("outmat_cvddeaths1")
load("outmat_chfdeaths1")
load("outmat_nephdeaths1")
load("outmat_retindeaths1")
load("outmat_neurodeaths1")

load("outmat_cvddalys1")
load("outmat_chfdalys1")
load("outmat_nephdalys1")
load("outmat_retindalys1")
load("outmat_neurodalys1")

load("outmat_rxbpcosts1")
load("outmat_rxdmcosts1")
load("outmat_rxstatincosts1")

load("outmat_cvdcosts1")
load("outmat_chfcosts1")
load("outmat_nephcosts1")
load("outmat_retincosts1")
load("outmat_neurocosts1")

labels = c("Country",
           "baseline", 
           "60% trt, 60% ctrl",
           "60% trt, 80% ctrl",
           "80% trt, 60% ctrl",
           "80% trt, 80% ctrl")


cvdevents = gather(outmat_cvdevents, scenario, cvdevents, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(cvdevents= as.numeric(cvdevents)*1000) %>%
  left_join(cnt_reg,by="Country")
cvdevents$scenario = factor(cvdevents$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/cvdevents1.eps", dpi = "retina", plot = ggplot(cvdevents, aes(x = Region, y = cvdevents, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("CVD events per 1,000 people with diabetes") + ggtitle("CVD events", subtitle="No new screening"))

chfevents = gather(outmat_chfevents, scenario, chfevents, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(chfevents= as.numeric(chfevents)*1000) %>%
  left_join(cnt_reg,by="Country")
chfevents$scenario = factor(chfevents$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/chfevents1.eps", dpi = "retina", plot = ggplot(chfevents, aes(x = Region, y = chfevents, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("CHF events per 1,000 people with diabetes") + ggtitle("CHF events", subtitle="No new screening"))

nephevents = gather(outmat_nephevents, scenario, nephevents, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(nephevents= as.numeric(nephevents)*1000) %>%
  left_join(cnt_reg,by="Country")
nephevents$scenario = factor(nephevents$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/nephevents1.eps", dpi = "retina", plot = ggplot(nephevents, aes(x = Region, y = nephevents, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("ESRD events per 1,000 people with diabetes") + ggtitle("ESRD events", subtitle="No new screening"))

retinevents = gather(outmat_retinevents, scenario, retinevents, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(retinevents= as.numeric(retinevents)*1000) %>%
  left_join(cnt_reg,by="Country")
retinevents$scenario = factor(retinevents$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/retinevents1.eps", dpi = "retina", plot = ggplot(retinevents, aes(x = Region, y = retinevents, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("Vision loss events per 1,000 people with diabetes") + ggtitle("Vision loss events", subtitle="No new screening"))

neuroevents = gather(outmat_neuroevents, scenario, neuroevents, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(neuroevents= as.numeric(neuroevents)*1000) %>%
  left_join(cnt_reg,by="Country")
neuroevents$scenario = factor(neuroevents$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/neuroevents1.eps", dpi = "retina", plot = ggplot(neuroevents, aes(x = Region, y = neuroevents, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("Neuropathy events per 1,000 people with diabetes") + ggtitle("Neuropathy events", subtitle="No new screening"))



cvddeaths = gather(outmat_cvddeaths, scenario, cvddeaths, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(cvddeaths= as.numeric(cvddeaths)*1000) %>%
  left_join(cnt_reg,by="Country")
cvddeaths$scenario = factor(cvddeaths$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/cvddeaths1.eps", dpi = "retina", plot = ggplot(cvddeaths, aes(x = Region, y = cvddeaths, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("CVD deaths per 1,000 people with diabetes") + ggtitle("CVD deaths", subtitle="No new screening"))

chfdeaths = gather(outmat_chfdeaths, scenario, chfdeaths, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(chfdeaths= as.numeric(chfdeaths)*1000) %>%
  left_join(cnt_reg,by="Country")
chfdeaths$scenario = factor(chfdeaths$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/chfdeaths1.eps", dpi = "retina", plot = ggplot(chfdeaths, aes(x = Region, y = chfdeaths, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("CHF deaths per 1,000 people with diabetes") + ggtitle("CHF deaths", subtitle="No new screening"))

nephdeaths = gather(outmat_nephdeaths, scenario, nephdeaths, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(nephdeaths= as.numeric(nephdeaths)*1000) %>%
  left_join(cnt_reg,by="Country")
nephdeaths$scenario = factor(nephdeaths$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/nephdeaths1.eps", dpi = "retina", plot = ggplot(nephdeaths, aes(x = Region, y = nephdeaths, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("ESRD deaths per 1,000 people with diabetes") + ggtitle("ESRD deaths", subtitle="No new screening"))

retindeaths = gather(outmat_retindeaths, scenario, retindeaths, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(retindeaths= as.numeric(retindeaths)*1000) %>%
  left_join(cnt_reg,by="Country")
retindeaths$scenario = factor(retindeaths$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/retindeaths1.eps", dpi = "retina", plot = ggplot(retindeaths, aes(x = Region, y = retindeaths, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("Vision loss deaths per 1,000 people with diabetes") + ggtitle("Vision loss deaths", subtitle="No new screening"))

neurodeaths = gather(outmat_neurodeaths, scenario, neurodeaths, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(neurodeaths= as.numeric(neurodeaths)*1000) %>%
  left_join(cnt_reg,by="Country")
neurodeaths$scenario = factor(neurodeaths$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/neurodeaths1.eps", dpi = "retina", plot = ggplot(neurodeaths, aes(x = Region, y = neurodeaths, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("Neuropathy deaths per 1,000 people with diabetes") + ggtitle("Neuropathy deaths", subtitle="No new screening"))






cvddalys = gather(outmat_cvddalys, scenario, cvddalys, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(cvddalys= as.numeric(cvddalys)*1000) %>%
  left_join(cnt_reg,by="Country")
cvddalys$scenario = factor(cvddalys$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/cvddalys1.eps", dpi = "retina", plot = ggplot(cvddalys, aes(x = Region, y = cvddalys, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("CVD DALYs per 1,000 people with diabetes") + ggtitle("CVD DALYs", subtitle="No new screening"))

chfdalys = gather(outmat_chfdalys, scenario, chfdalys, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(chfdalys= as.numeric(chfdalys)*1000) %>%
  left_join(cnt_reg,by="Country")
chfdalys$scenario = factor(chfdalys$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/chfdalys1.eps", dpi = "retina", plot = ggplot(chfdalys, aes(x = Region, y = chfdalys, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("CHF DALYs per 1,000 people with diabetes") + ggtitle("CHF DALYs", subtitle="No new screening"))

nephdalys = gather(outmat_nephdalys, scenario, nephdalys, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(nephdalys= as.numeric(nephdalys)*1000) %>%
  left_join(cnt_reg,by="Country")
nephdalys$scenario = factor(nephdalys$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/nephdalys1.eps", dpi = "retina", plot = ggplot(nephdalys, aes(x = Region, y = nephdalys, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("ESRD DALYs per 1,000 people with diabetes") + ggtitle("ESRD DALYs", subtitle="No new screening"))

retindalys = gather(outmat_retindalys, scenario, retindalys, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(retindalys= as.numeric(retindalys)*1000) %>%
  left_join(cnt_reg,by="Country")
retindalys$scenario = factor(retindalys$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/retindalys1.eps", dpi = "retina", plot = ggplot(retindalys, aes(x = Region, y = retindalys, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("Vision loss DALYs per 1,000 people with diabetes") + ggtitle("Vision loss DALYs", subtitle="No new screening"))

neurodalys = gather(outmat_neurodalys, scenario, neurodalys, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(neurodalys= as.numeric(neurodalys)*1000) %>%
  left_join(cnt_reg,by="Country")
neurodalys$scenario = factor(neurodalys$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/neurodalys1.eps", dpi = "retina", plot = ggplot(neurodalys, aes(x = Region, y = neurodalys, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("Neuropathy DALYs per 1,000 people with diabetes") + ggtitle("Neuropathy DALYs", subtitle="No new screening"))


###### SCREENING 60% #######

load("outmat_cvdevents2")
load("outmat_chfevents2")
load("outmat_nephevents2")
load("outmat_retinevents2")
load("outmat_neuroevents2")

load("outmat_cvddeaths2")
load("outmat_chfdeaths2")
load("outmat_nephdeaths2")
load("outmat_retindeaths2")
load("outmat_neurodeaths2")

load("outmat_cvddalys2")
load("outmat_chfdalys2")
load("outmat_nephdalys2")
load("outmat_retindalys2")
load("outmat_neurodalys2")

load("outmat_rxbpcosts2")
load("outmat_rxdmcosts2")
load("outmat_rxstatincosts2")

load("outmat_cvdcosts2")
load("outmat_chfcosts2")
load("outmat_nephcosts2")
load("outmat_retincosts2")
load("outmat_neurocosts2")

labels = c("Country",
           "baseline", 
           "60% trt, 60% ctrl",
           "60% trt, 80% ctrl",
           "80% trt, 60% ctrl",
           "80% trt, 80% ctrl")


cvdevents = gather(outmat_cvdevents, scenario, cvdevents, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(cvdevents= as.numeric(cvdevents)*1000) %>%
  left_join(cnt_reg,by="Country")
cvdevents$scenario = factor(cvdevents$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/cvdevents2.eps", dpi = "retina", plot = ggplot(cvdevents, aes(x = Region, y = cvdevents, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("CVD events per 1,000 people with diabetes") + ggtitle("CVD events", subtitle="Screening 60% of population"))

chfevents = gather(outmat_chfevents, scenario, chfevents, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(chfevents= as.numeric(chfevents)*1000) %>%
  left_join(cnt_reg,by="Country")
chfevents$scenario = factor(chfevents$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/chfevents2.eps", dpi = "retina", plot = ggplot(chfevents, aes(x = Region, y = chfevents, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("CHF events per 1,000 people with diabetes") + ggtitle("CHF events", subtitle="Screening 60% of population"))

nephevents = gather(outmat_nephevents, scenario, nephevents, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(nephevents= as.numeric(nephevents)*1000) %>%
  left_join(cnt_reg,by="Country")
nephevents$scenario = factor(nephevents$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/nephevents2.eps", dpi = "retina", plot = ggplot(nephevents, aes(x = Region, y = nephevents, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("ESRD events per 1,000 people with diabetes") + ggtitle("ESRD events", subtitle="Screening 60% of population"))

retinevents = gather(outmat_retinevents, scenario, retinevents, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(retinevents= as.numeric(retinevents)*1000) %>%
  left_join(cnt_reg,by="Country")
retinevents$scenario = factor(retinevents$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/retinevents2.eps", dpi = "retina", plot = ggplot(retinevents, aes(x = Region, y = retinevents, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("Vision loss events per 1,000 people with diabetes") + ggtitle("Vision loss events", subtitle="Screening 60% of population"))

neuroevents = gather(outmat_neuroevents, scenario, neuroevents, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(neuroevents= as.numeric(neuroevents)*1000) %>%
  left_join(cnt_reg,by="Country")
neuroevents$scenario = factor(neuroevents$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/neuroevents2.eps", dpi = "retina", plot = ggplot(neuroevents, aes(x = Region, y = neuroevents, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("Neuropathy events per 1,000 people with diabetes") + ggtitle("Neuropathy events", subtitle="Screening 60% of population"))



cvddeaths = gather(outmat_cvddeaths, scenario, cvddeaths, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(cvddeaths= as.numeric(cvddeaths)*1000) %>%
  left_join(cnt_reg,by="Country")
cvddeaths$scenario = factor(cvddeaths$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/cvddeaths2.eps", dpi = "retina", plot = ggplot(cvddeaths, aes(x = Region, y = cvddeaths, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("CVD deaths per 1,000 people with diabetes") + ggtitle("CVD deaths", subtitle="Screening 60% of population"))

chfdeaths = gather(outmat_chfdeaths, scenario, chfdeaths, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(chfdeaths= as.numeric(chfdeaths)*1000) %>%
  left_join(cnt_reg,by="Country")
chfdeaths$scenario = factor(chfdeaths$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/chfdeaths2.eps", dpi = "retina", plot = ggplot(chfdeaths, aes(x = Region, y = chfdeaths, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("CHF deaths per 1,000 people with diabetes") + ggtitle("CHF deaths", subtitle="Screening 60% of population"))

nephdeaths = gather(outmat_nephdeaths, scenario, nephdeaths, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(nephdeaths= as.numeric(nephdeaths)*1000) %>%
  left_join(cnt_reg,by="Country")
nephdeaths$scenario = factor(nephdeaths$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/nephdeaths2.eps", dpi = "retina", plot = ggplot(nephdeaths, aes(x = Region, y = nephdeaths, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("ESRD deaths per 1,000 people with diabetes") + ggtitle("ESRD deaths", subtitle="Screening 60% of population"))

retindeaths = gather(outmat_retindeaths, scenario, retindeaths, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(retindeaths= as.numeric(retindeaths)*1000) %>%
  left_join(cnt_reg,by="Country")
retindeaths$scenario = factor(retindeaths$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/retindeaths2.eps", dpi = "retina", plot = ggplot(retindeaths, aes(x = Region, y = retindeaths, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("Vision loss deaths per 1,000 people with diabetes") + ggtitle("Vision loss deaths", subtitle="Screening 60% of population"))

neurodeaths = gather(outmat_neurodeaths, scenario, neurodeaths, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(neurodeaths= as.numeric(neurodeaths)*1000) %>%
  left_join(cnt_reg,by="Country")
neurodeaths$scenario = factor(neurodeaths$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/neurodeaths2.eps", dpi = "retina", plot = ggplot(neurodeaths, aes(x = Region, y = neurodeaths, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("Neuropathy deaths per 1,000 people with diabetes") + ggtitle("Neuropathy deaths", subtitle="Screening 60% of population"))






cvddalys = gather(outmat_cvddalys, scenario, cvddalys, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(cvddalys= as.numeric(cvddalys)*1000) %>%
  left_join(cnt_reg,by="Country")
cvddalys$scenario = factor(cvddalys$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/cvddalys2.eps", dpi = "retina", plot = ggplot(cvddalys, aes(x = Region, y = cvddalys, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("CVD DALYs per 1,000 people with diabetes") + ggtitle("CVD DALYs", subtitle="Screening 60% of population"))

chfdalys = gather(outmat_chfdalys, scenario, chfdalys, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(chfdalys= as.numeric(chfdalys)*1000) %>%
  left_join(cnt_reg,by="Country")
chfdalys$scenario = factor(chfdalys$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/chfdalys2.eps", dpi = "retina", plot = ggplot(chfdalys, aes(x = Region, y = chfdalys, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("CHF DALYs per 1,000 people with diabetes") + ggtitle("CHF DALYs", subtitle="Screening 60% of population"))

nephdalys = gather(outmat_nephdalys, scenario, nephdalys, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(nephdalys= as.numeric(nephdalys)*1000) %>%
  left_join(cnt_reg,by="Country")
nephdalys$scenario = factor(nephdalys$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/nephdalys2.eps", dpi = "retina", plot = ggplot(nephdalys, aes(x = Region, y = nephdalys, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("ESRD DALYs per 1,000 people with diabetes") + ggtitle("ESRD DALYs", subtitle="Screening 60% of population"))

retindalys = gather(outmat_retindalys, scenario, retindalys, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(retindalys= as.numeric(retindalys)*1000) %>%
  left_join(cnt_reg,by="Country")
retindalys$scenario = factor(retindalys$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/retindalys2.eps", dpi = "retina", plot = ggplot(retindalys, aes(x = Region, y = retindalys, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("Vision loss DALYs per 1,000 people with diabetes") + ggtitle("Vision loss DALYs", subtitle="Screening 60% of population"))

neurodalys = gather(outmat_neurodalys, scenario, neurodalys, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(neurodalys= as.numeric(neurodalys)*1000) %>%
  left_join(cnt_reg,by="Country")
neurodalys$scenario = factor(neurodalys$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/neurodalys2.eps", dpi = "retina", plot = ggplot(neurodalys, aes(x = Region, y = neurodalys, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("Neuropathy DALYs per 1,000 people with diabetes") + ggtitle("Neuropathy DALYs", subtitle="Screening 60% of population"))



###### 80% SCREENING ######


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

labels = c("Country",
           "baseline", 
           "60% trt, 60% ctrl",
           "60% trt, 80% ctrl",
           "80% trt, 60% ctrl",
           "80% trt, 80% ctrl")


cvdevents = gather(outmat_cvdevents, scenario, cvdevents, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(cvdevents= as.numeric(cvdevents)*1000) %>%
  left_join(cnt_reg,by="Country")
cvdevents$scenario = factor(cvdevents$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/cvdevents3.eps", dpi = "retina", plot = ggplot(cvdevents, aes(x = Region, y = cvdevents, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("CVD events per 1,000 people with diabetes") + ggtitle("CVD events", subtitle="Screening 80% of population"))

chfevents = gather(outmat_chfevents, scenario, chfevents, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(chfevents= as.numeric(chfevents)*1000) %>%
  left_join(cnt_reg,by="Country")
chfevents$scenario = factor(chfevents$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/chfevents3.eps", dpi = "retina", plot = ggplot(chfevents, aes(x = Region, y = chfevents, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("CHF events per 1,000 people with diabetes") + ggtitle("CHF events", subtitle="Screening 80% of population"))

nephevents = gather(outmat_nephevents, scenario, nephevents, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(nephevents= as.numeric(nephevents)*1000) %>%
  left_join(cnt_reg,by="Country")
nephevents$scenario = factor(nephevents$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/nephevents3.eps", dpi = "retina", plot = ggplot(nephevents, aes(x = Region, y = nephevents, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("ESRD events per 1,000 people with diabetes") + ggtitle("ESRD events", subtitle="Screening 80% of population"))

retinevents = gather(outmat_retinevents, scenario, retinevents, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(retinevents= as.numeric(retinevents)*1000) %>%
  left_join(cnt_reg,by="Country")
retinevents$scenario = factor(retinevents$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/retinevents3.eps", dpi = "retina", plot = ggplot(retinevents, aes(x = Region, y = retinevents, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("Vision loss events per 1,000 people with diabetes") + ggtitle("Vision loss events", subtitle="Screening 80% of population"))

neuroevents = gather(outmat_neuroevents, scenario, neuroevents, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(neuroevents= as.numeric(neuroevents)*1000) %>%
  left_join(cnt_reg,by="Country")
neuroevents$scenario = factor(neuroevents$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/neuroevents3.eps", dpi = "retina", plot = ggplot(neuroevents, aes(x = Region, y = neuroevents, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("Neuropathy events per 1,000 people with diabetes") + ggtitle("Neuropathy events", subtitle="Screening 80% of population"))



cvddeaths = gather(outmat_cvddeaths, scenario, cvddeaths, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(cvddeaths= as.numeric(cvddeaths)*1000) %>%
  left_join(cnt_reg,by="Country")
cvddeaths$scenario = factor(cvddeaths$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/cvddeaths3.eps", dpi = "retina", plot = ggplot(cvddeaths, aes(x = Region, y = cvddeaths, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("CVD deaths per 1,000 people with diabetes") + ggtitle("CVD deaths", subtitle="Screening 80% of population"))

chfdeaths = gather(outmat_chfdeaths, scenario, chfdeaths, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(chfdeaths= as.numeric(chfdeaths)*1000) %>%
  left_join(cnt_reg,by="Country")
chfdeaths$scenario = factor(chfdeaths$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/chfdeaths3.eps", dpi = "retina", plot = ggplot(chfdeaths, aes(x = Region, y = chfdeaths, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("CHF deaths per 1,000 people with diabetes") + ggtitle("CHF deaths", subtitle="Screening 80% of population"))

nephdeaths = gather(outmat_nephdeaths, scenario, nephdeaths, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(nephdeaths= as.numeric(nephdeaths)*1000) %>%
  left_join(cnt_reg,by="Country")
nephdeaths$scenario = factor(nephdeaths$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/nephdeaths3.eps", dpi = "retina", plot = ggplot(nephdeaths, aes(x = Region, y = nephdeaths, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("ESRD deaths per 1,000 people with diabetes") + ggtitle("ESRD deaths", subtitle="Screening 80% of population"))

retindeaths = gather(outmat_retindeaths, scenario, retindeaths, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(retindeaths= as.numeric(retindeaths)*1000) %>%
  left_join(cnt_reg,by="Country")
retindeaths$scenario = factor(retindeaths$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/retindeaths3.eps", dpi = "retina", plot = ggplot(retindeaths, aes(x = Region, y = retindeaths, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("Vision loss deaths per 1,000 people with diabetes") + ggtitle("Vision loss deaths", subtitle="Screening 80% of population"))

neurodeaths = gather(outmat_neurodeaths, scenario, neurodeaths, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(neurodeaths= as.numeric(neurodeaths)*1000) %>%
  left_join(cnt_reg,by="Country")
neurodeaths$scenario = factor(neurodeaths$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/neurodeaths3.eps", dpi = "retina", plot = ggplot(neurodeaths, aes(x = Region, y = neurodeaths, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("Neuropathy deaths per 1,000 people with diabetes") + ggtitle("Neuropathy deaths", subtitle="Screening 80% of population"))






cvddalys = gather(outmat_cvddalys, scenario, cvddalys, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(cvddalys= as.numeric(cvddalys)*1000) %>%
  left_join(cnt_reg,by="Country")
cvddalys$scenario = factor(cvddalys$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/cvddalys3.eps", dpi = "retina", plot = ggplot(cvddalys, aes(x = Region, y = cvddalys, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("CVD DALYs per 1,000 people with diabetes") + ggtitle("CVD DALYs", subtitle="Screening 80% of population"))

chfdalys = gather(outmat_chfdalys, scenario, chfdalys, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(chfdalys= as.numeric(chfdalys)*1000) %>%
  left_join(cnt_reg,by="Country")
chfdalys$scenario = factor(chfdalys$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/chfdalys3.eps", dpi = "retina", plot = ggplot(chfdalys, aes(x = Region, y = chfdalys, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("CHF DALYs per 1,000 people with diabetes") + ggtitle("CHF DALYs", subtitle="Screening 80% of population"))

nephdalys = gather(outmat_nephdalys, scenario, nephdalys, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(nephdalys= as.numeric(nephdalys)*1000) %>%
  left_join(cnt_reg,by="Country")
nephdalys$scenario = factor(nephdalys$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/nephdalys3.eps", dpi = "retina", plot = ggplot(nephdalys, aes(x = Region, y = nephdalys, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("ESRD DALYs per 1,000 people with diabetes") + ggtitle("ESRD DALYs", subtitle="Screening 80% of population"))

retindalys = gather(outmat_retindalys, scenario, retindalys, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(retindalys= as.numeric(retindalys)*1000) %>%
  left_join(cnt_reg,by="Country")
retindalys$scenario = factor(retindalys$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/retindalys3.eps", dpi = "retina", plot = ggplot(retindalys, aes(x = Region, y = retindalys, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("Vision loss DALYs per 1,000 people with diabetes") + ggtitle("Vision loss DALYs", subtitle="Screening 80% of population"))

neurodalys = gather(outmat_neurodalys, scenario, neurodalys, baseline:`80% trt, 80% ctrl`, convert = T, factor_key = T)  %>%
  mutate(neurodalys= as.numeric(neurodalys)*1000) %>%
  left_join(cnt_reg,by="Country")
neurodalys$scenario = factor(neurodalys$scenario, levels = labels)
ggsave(filename="~/Box/Research/Research projects/WHO diabetes/Figures/neurodalys3.eps", dpi = "retina", plot = ggplot(neurodalys, aes(x = Region, y = neurodalys, color = scenario)) + geom_boxplot(outlier.shape = NA) + theme_minimal() + ylab("Neuropathy DALYs per 1,000 people with diabetes") + ggtitle("Neuropathy DALYs", subtitle="Screening 80% of population"))



