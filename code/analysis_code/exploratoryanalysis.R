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
library(scales)


## ---- loaddata --------
#Path to data.
data_location <- here::here("data","processed_data","processeddata.rda")
#load data
load(data_location)

## ---- exploratorytables1 --------
#bcg coverage summary and save to file location
summary_bcg <- skimr::skim(wide_data$`BCG immunization coverage among one-year-olds (%)`)
print(summary_bcg)
bcgsummarytable_file <- here("results", "exploratory_analysis", "summary tables", "bcgsummarytable.rds")
saveRDS(summary_bcg, file = bcgsummarytable_file)

#catastrophic cost summary and save to file location
summary_catacost <- skimr::skim(wide_data$`Families affected by TB facing catastrophic costs due to TB (%)`)
print(summary_catacost)
catacostsummarytable_file <- here("results", "exploratory_analysis", "summary tables", "catacostsummarytable.rds")
saveRDS(summary_catacost, file = catacostsummarytable_file)

#case detection rate summary and save to file location
summary_cdr <- skimr::skim(wide_data$`Case detection rate (%)`)
print(summary_cdr)
cdrsummarytable_file <- here("results", "exploratory_analysis", "summary tables", "cdrsummarytable.rds")
saveRDS(summary_cdr, file = cdrsummarytable_file)

#drug resistance summary and save to file location
summary_drug <- skimr::skim(wide_data$`People with MDR/RR-TB (%)`)
print(summary_drug)
drugsummarytable_file <- here("results", "exploratory_analysis", "summary tables", "drugsummarytable.rds")
saveRDS(summary_drug, file = drugsummarytable_file)

## ---- exploratorytables2 --------
#incidence summary and save to file location
summary_inc <- skimr::skim(wide_data$`TB incidence (new infections per 100 000 population)`)
print(summary_inc)
incsummarytable_file <- here("results", "exploratory_analysis", "summary tables", "incsummarytable.rds")
saveRDS(summary_inc, file = incsummarytable_file)

#mortality summary and save to file location
summary_mort <- skimr::skim(wide_data$`TB mortality (deaths per 100 000 population)`)
print(summary_mort)
mortsummarytable_file <- here("results", "exploratory_analysis", "summary tables", "mortsummarytable.rds")
saveRDS(summary_mort, file = mortsummarytable_file)

## ---- exploratorytables3 --------
#prevalence to notification summary and save to file location
summary_pn <- skimr::skim(wide_data$`Prevalence to notification ratio (years)`)
print(summary_pn)
pnsummarytable_file <- here("results", "exploratory_analysis", "summary tables", "pnsummarytable.rds")
saveRDS(summary_pn, file = pnsummarytable_file)

#prevalence summary and save to file location
summary_prev <- skimr::skim(wide_data$`TB prevalence (cases per 100 000 population)`)
print(summary_prev)
prevsummarytable_file <- here("results", "exploratory_analysis", "summary tables", "prevsummarytable.rds")
saveRDS(summary_prev, file = prevsummarytable_file)

## ---- exploratorytables4 --------
#attitude summary and save to file location
summary_att <- skimr::skim(wide_data$`People who would want a family member's TB kept secret (%)`)
print(summary_att)
attsummarytable_file <- here("results", "exploratory_analysis", "summary tables", "attsummarytable.rds")
saveRDS(summary_att, file = attsummarytable_file)

#female attitudesummary and save to file location
summary_attf <- skimr::skim(wide_data$`People who would want a family member's TB kept secret - Female (%)`)
print(summary_attf)
attfsummarytable_file <- here("results", "exploratory_analysis", "summary tables", "attfsummarytable.rds")
saveRDS(summary_attf, file = attfsummarytable_file)

#male attitude summary and save to file location
summary_attm <- skimr::skim(wide_data$`People who would want a family member's TB kept secret - Male (%)`)
print(summary_attm)
attmsummarytable_file <- here("results", "exploratory_analysis", "summary tables", "attmsummarytable.rds")
saveRDS(summary_attm, file = attmsummarytable_file)

## ---- exploratorytables5 --------
#spread through cough summary and save to file location
summary_cough <- skimr::skim(wide_data$`People who report TB is spread through coughing (%)`)
print(summary_cough)
coughsummarytable_file <- here("results", "exploratory_analysis", "summary tables", "coughsummarytable.rds")
saveRDS(summary_cough, file = coughsummarytable_file)

#female spread through cough summary and save to file location
summary_coughf <- skimr::skim(wide_data$`People who report TB is spread through coughing - Female (%)`)
print(summary_coughf)
coughfsummarytable_file <- here("results", "exploratory_analysis", "summary tables", "coughfsummarytable.rds")
saveRDS(summary_coughf, file = coughfsummarytable_file)

#male spread through cough summary and save to file location
summary_coughm <- skimr::skim(wide_data$`People who report TB is spread through coughing - Male (%)`)
print(summary_coughm)
coughmsummarytable_file <- here("results", "exploratory_analysis", "summary tables", "coughmsummarytable.rds")
saveRDS(summary_coughm, file = coughmsummarytable_file)



## ---- exploratoryfigures1 --------
#plot bcg coverage by dimension and subgroup and save
p1 <- highburden %>%
  filter(!dimension %in% c("Age (2 groups) (0-15+)", "TB drug resistance", "Age (3 groups) (15-49)")) %>% 
  ggplot(aes(x= dimension,
             y = `BCG immunization coverage among one-year-olds (%)`,
             fill = subgroup)) + 
  geom_boxplot()+
  scale_x_discrete(labels =label_wrap(12))+ #adjust x axis labels
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5))+ #adjust legend and title
  guides(fill= guide_legend(ncol=1))+ #reorder legend
  scale_fill_brewer(palette = "Spectral")+
  labs(title = "Immunization in high burden countries by subgroup",
       y = "BCG immunization coverage\namong one-year-olds (%)") # double line in y axis title
plot(p1) 
figure_file = here("results", "exploratory_analysis", "figures", "bcgexplore.png")
ggsave(filename = figure_file, plot=p1) 

#plot catastrophic costs by dimension and subgroup and save
p2 <- highburden %>%
  filter(dimension %in% c("TB drug resistance", "Economic status (wealth quintile)")) %>%
  ggplot(aes(x=dimension,
             y = `Families affected by TB facing catastrophic costs due to TB (%)`,
             fill = subgroup)) +
  geom_boxplot()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels =label_wrap(12))+
  scale_fill_brewer(palette = "Spectral")+
  labs(title = "Catastrophic costs due to TB\nin high burden countries by subgroup")
plot(p2) 
figure_file = here("results", "exploratory_analysis", "figures", "catacostexplore.png")
ggsave(filename = figure_file, plot=p2) 

## ---- exploratoryfigures2 --------
#plots case detection rate by dimensions and subgroup and save
p3 <- highburden %>%
  filter(dimension %in% c("Age (2 groups) (0-15+)", "Sex")) %>%
  ggplot(aes(x=dimension,
             y = `Case detection rate (%)`,
             fill = subgroup)) +
  geom_boxplot()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels =label_wrap(20))+
  scale_fill_brewer(palette = "Spectral")+
  labs(title = "Case detection rate in high burden countries by subgroup")
plot(p3) 
figure_file = here("results", "exploratory_analysis", "figures", "cdrexplore.png")
ggsave(filename = figure_file, plot=p3) 

#plots percentage of drug-resistant TB by sex and save
p4 <- highburden %>%
  filter(dimension %in% "Sex") %>%
  ggplot(aes(x=subgroup,
             y = `People with MDR/RR-TB (%)`,
             fill = subgroup)) +
  geom_boxplot()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels =label_wrap(12))+
  scale_fill_brewer(palette = "Spectral")+
  labs(title = "Proportion of drug resistant TB in high burden countries by subgroup")
plot(p4) 
figure_file = here("results", "exploratory_analysis", "figures", "drugexplore.png")
ggsave(filename = figure_file, plot=p4) 

## ---- exploratoryfigures3 --------
#plots incidence by country from lowest incidence to highest, colored by subgroup 
#and saves
p5a <- wide_data %>%
  filter(indicator_abbr == "incidence") %>% 
  select(c(1,2,3,4,5,6,12)) %>% 
  ggplot(aes(x=fct_reorder(
  setting,`TB incidence (new infections per 100 000 population)`),
  y = `TB incidence (new infections per 100 000 population)`,
  color = subgroup)) +
  geom_point()+
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5),
        legend.position = c(0.15,0.75))+
  scale_fill_brewer(palette = "Spectral")+
  labs(title = "Tuberculosis incidence by country", x = "Setting",
       y = "TB incidence\n(new infections per 100 000 population)")
plot(p5a) 
figure_file = here("results", "exploratory_analysis", "figures", "incexplore1.png")
ggsave(filename = figure_file, plot=p5a) 

#plots incidence by country from lowest incidence to highest, colored by subgroup
#and saves
p5b <- highburden %>%
  filter(dimension %in% "Sex") %>%
  filter(indicator_abbr == "incidence") %>% 
  select(c(1,2,3,4,5,6,12)) %>% 
  ggplot(aes(x = fct_reorder(
    setting,`TB incidence (new infections per 100 000 population)`),
             y = `TB incidence (new infections per 100 000 population)`,
             color = subgroup)) +
  geom_point()+
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5),
        legend.position = c(0.15,0.75))+
  scale_fill_brewer(palette = "Spectral")+
  labs(title = "Tuberculosis incidence for high burden countries", x = "Setting",
       y = "TB incidence\n(new infections per 100 000 population)")
plot(p5b) 
figure_file = here("results", "exploratory_analysis", "figures", "incexplore2.png")
ggsave(filename = figure_file, plot=p5b) 

#plots mortality by country from lowest mortality to highest, colored by subgroup
p6 <- highburden %>% 
  filter(dimension %in% "Sex") %>%
  filter(indicator_abbr == "mortality") %>% 
  select(c(1,2,3,4,5,6,13)) %>% 
  ggplot(aes(x=fct_reorder(
  setting, `TB mortality (deaths per 100 000 population)`),
  y = `TB mortality (deaths per 100 000 population)`,
  color = subgroup)) +
    geom_point()+
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5),
        legend.position = c(0.15,0.75))+
  scale_fill_brewer(palette = "Spectral")+
  labs(title = "Tuberculosis mortality in high burden countries", x = "Setting",
       y = "TB mortality\n(deaths per 100 000 population)")
plot(p6) 
figure_file = here("results", "exploratory_analysis", "figures", "mortexplore.png")
ggsave(filename = figure_file, plot=p6) 

#plots prevalence to notification ratio by sex and saves
p7 <- highburden %>%
  filter(dimension %in% "Sex") %>%
  ggplot(aes(x=subgroup, y = `Prevalence to notification ratio (years)`, fill = subgroup)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Spectral")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Time between prevalence to notification\nin high burden countries")
plot(p7) 
figure_file = here("results", "exploratory_analysis", "figures", "pnexplore.png")
ggsave(filename = figure_file, plot=p7) 

#plots TB prevalence by country from lowest prevalence to highest, colored by subgroup
#and saves
p8a <- wide_data %>%
  filter(indicator_abbr == "prevalence_place") %>% 
  select(c(1,2,3,4,5,6,17)) %>% 
  ggplot(aes(x=fct_reorder(
  setting, `TB prevalence (cases per 100 000 population)`),
  y = `TB prevalence (cases per 100 000 population)`,
  color = subgroup)) +
  geom_point()+
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5),
        legend.position = c(0.15,0.75))+
  scale_fill_brewer(palette = "Spectral")+
  labs(title = "Tuberculosis prevalence by country", x = "Setting",
       y = "TB prevalence\n(cases per 100 000 population)")
plot(p8a) 
figure_file = here("results", "exploratory_analysis", "figures", "prevexplore1.png")
ggsave(filename = figure_file, plot=p8a) 

#plots TB prevalence by country from lowest prevalence to highest, colored by subgroup
#and saves
p8b <- highburden %>%
  filter(indicator_abbr == "prevalence_place") %>% 
  select(c(1,2,3,4,5,6,17)) %>% 
  ggplot(aes(x=fct_reorder(
  setting, `TB prevalence (cases per 100 000 population)`),
  y = `TB prevalence (cases per 100 000 population)`,
  color = subgroup)) +
  geom_point()+
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5),
        legend.position = c(0.15,0.75))+
  scale_fill_brewer(palette = "Spectral")+
  labs(title = "Tuberculosis prevalence in high burden countries", x = "Setting",
       y = "TB prevalence\n(cases per 100 000 population)")
plot(p8b) 
figure_file = here("results", "exploratory_analysis", "figures", "prevexplore2.png")
ggsave(filename = figure_file, plot=p8b) 
## ---- exploratoryfigures4 --------
#plots attitudes toward TB by sex and save
p9 <- highburden %>% 
  filter(dimension %in% "Sex") %>%
  ggplot(aes(x=subgroup,
             y = `People who would want a family member's TB kept secret (%)`,
             fill = subgroup)) +
  geom_boxplot()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels =label_wrap(12))+
  scale_fill_brewer(palette = "Spectral")+
  labs(title = "General attitudes toward TB in high burden countries by subgroup")
plot(p9) 
figure_file = here("results", "exploratory_analysis", "figures", "attexplore.png")
ggsave(filename = figure_file, plot=p9) 

#plots attitudes toward TB by dimension and subgroup for females and saves
p10 <- highburden %>%
  filter(!dimension %in% c("Age (2 groups) (0-15+)", "TB drug resistance", "Sex")) %>%
  ggplot(aes(x=dimension,
             y = `People who would want a family member's TB kept secret - Female (%)`,
             fill = subgroup)) +
  geom_boxplot()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels =label_wrap(12))+
  labs(title = "Female attitudes toward TB in high burden countries by subgroup",
       y = "People who would want a\nfamily member's TB kept secret - Female (%)")
plot(p10) 
figure_file = here("results", "exploratory_analysis", "figures", "attfexplore.png")
ggsave(filename = figure_file, plot=p10) 

#plots attitudes toward TB by dimension and subgroup for males and save
p11 <- highburden %>%
  filter(!dimension %in% c("Age (2 groups) (0-15+)", "TB drug resistance", "Sex")) %>%
  ggplot(aes(x=dimension,
             y = `People who would want a family member's TB kept secret - Male (%)`,
             fill = subgroup)) +
  geom_boxplot()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels =label_wrap(12))+
  labs(title = "Male attitudes toward TB in high burden countries by subgroup",
       y = "People who would want a\nfamily member's TB kept secret - Male (%)") 
plot(p11) 
figure_file = here("results", "exploratory_analysis", "figures", "attmexplore.png")
ggsave(filename = figure_file, plot=p11) 

## ---- exploratoryfigures5 --------
#plots knowledge about TB by sex and save
p12 <- highburden %>%
  filter(dimension %in% "Sex") %>%
  ggplot(aes(x=subgroup,
             y = `People who report TB is spread through coughing (%)`,
             fill = subgroup)) +
  geom_boxplot()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels =label_wrap(12))+
  scale_fill_brewer(palette = "Spectral")+
  labs(title = "General knowledge about TB by subgroup")
plot(p12) 
figure_file = here("results", "exploratory_analysis", "figures", "coughexplore.png")
ggsave(filename = figure_file, plot=p12) 

#plots knowledge about TB by dimension and subgroup for females and save
p13 <- highburden %>%
  filter(!dimension %in% c("Age (2 groups) (0-15+)", "TB drug resistance", "Sex")) %>%
  ggplot(aes(x=dimension,
             y = `People who report TB is spread through coughing - Female (%)`,
             fill = subgroup)) +
  geom_boxplot()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels =label_wrap(12))+
  labs(title = "Female knowledge about TB by subgroup",
       y = "People who report TB is\nspread through coughing - Female (%)")
plot(p13) 
figure_file = here("results", "exploratory_analysis", "figures", "coughfexplore.png")
ggsave(filename = figure_file, plot=p13) 

#plots knowledge about TB by dimension and subgroup for males and save
p14 <- highburden %>%
  filter(!dimension %in% c("Age (2 groups) (0-15+)", "TB drug resistance", "Sex")) %>%
  ggplot(aes(x=dimension,
             y = `People who report TB is spread through coughing - Male (%)`,
             fill = subgroup)) +
  geom_boxplot()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels =label_wrap(12))+
  labs(title = "Male knowledge about TB by subgroup",
       y = "People who report TB is\nspread through coughing - Male (%)") 
plot(p14)
figure_file = here("results", "exploratory_analysis", "figures", "coughmexplore.png")
ggsave(filename = figure_file, plot=p14) 

