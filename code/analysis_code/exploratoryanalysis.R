###############################
# exploratory analysis script
#
#this script loads the processed data and conducts exploratory analysis. 
#Results (tables and figures) are saved in results folder.


## ---- packages --------
#load needed packages
library(here) #for data loading/saving
library(dplyr)
library(skimr)
library(ggplot2) #for plots/figures
library(forcats) #for factoring


## ---- loaddata --------
#Path to data.
data_location <- here::here("data","processed_data","processeddata.rda")
#load data
load(data_location)

## ---- exploratorytables1 --------
#bcg coverage summary and save to file location
summary_bcg <- skimr::skim(bcg)
print(summary_bcg)
bcgsummarytable_file <- here("results", "summary tables", "bcgsummarytable.rds")
saveRDS(summary_bcg, file = bcgsummarytable_file)

#catastrophic cost summary and save to file location
summary_catacost <- skimr::skim(catacost)
print(summary_catacost)
catacostsummarytable_file <- here("results", "summary tables", "catacostsummarytable.rds")
saveRDS(summary_catacost, file = catacostsummarytable_file)

#case detection rate summary and save to file location
summary_cdr <- skimr::skim(cdr)
print(summary_cdr)
cdrsummarytable_file <- here("results", "summary tables", "cdrsummarytable.rds")
saveRDS(summary_cdr, file = cdrsummarytable_file)

#drug resistance summary and save to file location
summary_drug <- skimr::skim(drug_resistance)
print(summary_drug)
drugsummarytable_file <- here("results", "summary tables", "drugsummarytable.rds")
saveRDS(summary_drug, file = drugsummarytable_file)

## ---- exploratorytables2 --------
#incidence summary and save to file location
summary_inc <- skimr::skim(incidence)
print(summary_inc)
incsummarytable_file <- here("results", "summary tables", "incsummarytable.rds")
saveRDS(summary_inc, file = incsummarytable_file)

#mortality summary and save to file location
summary_mort <- skimr::skim(mortality)
print(summary_mort)
mortsummarytable_file <- here("results", "summary tables", "mortsummarytable.rds")
saveRDS(summary_mort, file = mortsummarytable_file)

## ---- exploratorytables3 --------
#prevalence to notification summary and save to file location
summary_pn <- skimr::skim(pn)
print(summary_pn)
pnsummarytable_file <- here("results", "summary tables", "pnsummarytable.rds")
saveRDS(summary_pn, file = pnsummarytable_file)

#prevalence summary and save to file location
summary_prev <- skimr::skim(prevalence_place)
print(summary_prev)
prevsummarytable_file <- here("results", "summary tables", "prevsummarytable.rds")
saveRDS(summary_prev, file = prevsummarytable_file)

## ---- exploratorytables4 --------
#attitude summary and save to file location
summary_att <- skimr::skim(tb_att)
print(summary_att)
attsummarytable_file <- here("results", "summary tables", "attsummarytable.rds")
saveRDS(summary_att, file = attsummarytable_file)

#female attitudesummary and save to file location
summary_attf <- skimr::skim(tb_att_f)
print(summary_attf)
attfsummarytable_file <- here("results", "summary tables", "attfsummarytable.rds")
saveRDS(summary_attf, file = attfsummarytable_file)

#male attitude summary and save to file location
summary_attm <- skimr::skim(tb_att_m)
print(summary_attm)
attmsummarytable_file <- here("results", "summary tables", "attmsummarytable.rds")
saveRDS(summary_attm, file = attmsummarytable_file)

## ---- exploratorytables5 --------
#spread through cough summary and save to file location
summary_cough <- skimr::skim(tb_cough)
print(summary_cough)
coughsummarytable_file <- here("results", "summary tables", "coughsummarytable.rds")
saveRDS(summary_cough, file = coughsummarytable_file)

#female spread through cough summary and save to file location
summary_coughf <- skimr::skim(tb_cough_f)
print(summary_coughf)
coughfsummarytable_file <- here("results", "summary tables", "coughfsummarytable.rds")
saveRDS(summary_coughf, file = coughfsummarytable_file)

#male spread through cough summary and save to file location
summary_coughm <- skimr::skim(tb_cough_m)
print(summary_coughm)
coughmsummarytable_file <- here("results", "summary tables", "coughmsummarytable.rds")
saveRDS(summary_coughm, file = coughmsummarytable_file)



## ---- exploratoryfigures1 --------
p1 <- bcg %>% ggplot(aes(x= dimension,
                         y = `BCG immunization coverage among one-year-olds (%)`,
                         fill = subgroup)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_fill_brewer(palette = "Spectral")
plot(p1) # plots bcg coverage by dimension and subgroup
figure_file = here("results", "figures", "bcgexplore.png")
ggsave(filename = figure_file, plot=p1) 

p2 <- catacost %>% ggplot(aes(x=dimension,
                              y = `Families affected by TB facing catastrophic costs due to TB (%)`,
                              fill = subgroup)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_fill_brewer(palette = "Spectral")
plot(p2) # plots catastrophic costs by dimension and subgroup
figure_file = here("results", "figures", "catacostexplore.png")
ggsave(filename = figure_file, plot=p2) 

## ---- exploratoryfigures2 --------
p3 <- cdr %>% ggplot(aes(x=dimension, y = `Case detection rate (%)`,
                         fill = subgroup)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_fill_brewer(palette = "Spectral")
plot(p3) # plots case detection rate by dimensions and subgroup
figure_file = here("results", "figures", "cdrexplore.png")
ggsave(filename = figure_file, plot=p3) 

p4 <- drug_resistance %>% ggplot(aes(x=subgroup,
                                     y = `People with MDR/RR-TB (%)`,
                                     fill = subgroup)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_fill_brewer(palette = "Spectral")
plot(p4) # plots percentage of drug-resistant TB by sex
figure_file = here("results", "figures", "drugexplore.png")
ggsave(filename = figure_file, plot=p4) 

## ---- exploratoryfigures3 --------

p5 <- incidence %>% ggplot(aes(x=fct_reorder(
  setting,`TB incidence (new infections per 100 000 population)`),
                               y = `TB incidence (new infections per 100 000 population)`,
                               color = subgroup)) +
  geom_point()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_fill_brewer(palette = "Spectral")+
  xlab("setting")
plot(p5) #plots incidence by country from lowest incidence to highest, colored by subgroup
figure_file = here("results", "figures", "incexplore.png")
ggsave(filename = figure_file, plot=p5) 

colnames(mortality)
p6 <- mortality %>% ggplot(aes(x=fct_reorder(
  setting, `TB mortality (deaths per 100 000 population)`),
  y = `TB mortality (deaths per 100 000 population)`,
  color = subgroup)) +
    geom_point()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_fill_brewer(palette = "Spectral")+
  xlab("setting")
plot(p6) #plots mortality by country from lowest mortality to highest, colored by subgroup
figure_file = here("results", "figures", "mortexplore.png")
ggsave(filename = figure_file, plot=p6) 

p7 <- pn %>% ggplot(aes(x=subgroup, y = `Prevalence to notification ratio (years)`, fill = subgroup)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Spectral")
plot(p7) #plots prevalence to notification ratio by sex
figure_file = here("results", "figures", "pnexplore.png")
ggsave(filename = figure_file, plot=p7) 


p8 <- prevalence_place %>% ggplot(aes(x=fct_reorder(
  setting, `TB prevalence (cases per 100 000 population)`),
  y = `TB prevalence (cases per 100 000 population)`,
  color = subgroup)) +
  geom_point()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_fill_brewer(palette = "Spectral")+
  xlab("setting")
plot(p8) #plots TB prevalence by country from lowest prevalence to highest, colored by subgroup
figure_file = here("results", "figures", "prevexplore.png")
ggsave(filename = figure_file, plot=p8) 
## ---- exploratoryfigures4 --------
p9 <- tb_att %>% ggplot(aes(x=subgroup,
                            y = `People who would want a family member's TB kept secret (%)`, fill = subgroup)) +
  geom_boxplot()+
  scale_fill_brewer(palette = "Spectral")
plot(p9) # plots attitudes toward TB by sex
figure_file = here("results", "figures", "attexplore.png")
ggsave(filename = figure_file, plot=p9) 


p10 <- tb_att_f %>% ggplot(aes(x=dimension,
                               y = `People who would want a family member's TB kept secret - Female (%)`,
                               fill = subgroup)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))
plot(p10) # plots attitudes toward TB by dimension and subgroup for females
figure_file = here("results", "figures", "attfexplore.png")
ggsave(filename = figure_file, plot=p10) 

p11 <- tb_att_m %>% ggplot(aes(x=dimension,
                               y = `People who would want a family member's TB kept secret - Male (%)`,
                               fill = subgroup)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90)) 
plot(p11) # plots attitudes toward TB by dimension and subgroup for males
figure_file = here("results", "figures", "attmexplore.png")
ggsave(filename = figure_file, plot=p11) 

## ---- exploratoryfigures5 --------
p12 <- tb_cough %>% ggplot(aes(x=subgroup,
                               y = `People who report TB is spread through coughing (%)`,
                               fill = subgroup)) +
  geom_boxplot()+
  scale_fill_brewer(palette = "Spectral")
plot(p12) # plots knowledge about TB by sex
figure_file = here("results", "figures", "coughexplore.png")
ggsave(filename = figure_file, plot=p12) 

p13 <- tb_cough_f %>% ggplot(aes(x=dimension,
                                 y = `People who report TB is spread through coughing - Female (%)`,
                                 fill = subgroup)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))   
plot(p13) # plots knowledge about TB by dimension and subgroup for females
figure_file = here("results", "figures", "coughfexplore.png")
ggsave(filename = figure_file, plot=p13) 

p14 <- tb_cough_m %>% ggplot(aes(x=dimension,
                                 y = `People who report TB is spread through coughing - Male (%)`,
                                 fill = subgroup)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))  
plot(p14)#plots knowledge about TB by dimension and subgroup for males
figure_file = here("results", "figures", "coughmexplore.png")
ggsave(filename = figure_file, plot=p14) 

