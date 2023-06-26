# Lit. Review CI Values
# Load all required data values.
# Pablo Busch 2023. pmbusch@ucdavis.edu

# libraries ----
# Need to install them with install.packages()
library(tidyverse)
library(readxl)
library(flextable)
library(lubridate)
library(officer)
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))


# Load Data-----
fig_name <- "Figures/%s" # Save filte

ci <- read_excel("Data/S2_Articles and CI values collected.xlsx",
                 sheet="Carbon Intensity Values")
names(ci) <- str_replace_all(names(ci)," ","_")

# Data wrangling -----
ci <- ci %>% 
  rename(CI=`Adjusted_CI_[gCO2e/MJ]`)

# factors ----
ci <- ci %>% 
  mutate(Production_family=factor(Production_family,
                                  levels = c("Thermochemical",
                                             "Electrochemical",
                                             "Thermal-electrochemical",
                                             "Biochemical")),
         Production_method=factor(Production_method),
         Feedstock=factor(Feedstock),
         CCS=if_else(is.na(CCS),"",paste0(" + ",CCS)),
         `Production Method`=paste0(Group_Production_Method,CCS),)

## Special pathways ----
# these pathways are the majority of CI values

# NUMBER REPRSENTS THE ORDER
ci <- ci %>% 
  mutate(special_pathway=case_when(
    Group_Production_Method=="Steam Reforming"&Group_Feedstock=="Natural Gas"&CCS==" + CCS" ~ 2,
    Group_Production_Method=="Steam Reforming"&Group_Feedstock=="Natural Gas" ~ 1,
    Group_Production_Method=="Methane cracking"&Group_Feedstock=="Natural Gas" ~ 3,
    Group_Production_Method=="Gasification"&Group_Feedstock=="Coal"&CCS==" + CCS" ~ 5,
    Group_Production_Method=="Gasification"&Group_Feedstock=="Coal" ~ 4,
    Group_Production_Method=="Gasification"&Group_Feedstock=="Biomass"&CCS==" + CCS" ~ 7,
    Group_Production_Method=="Gasification"&Group_Feedstock=="Biomass" ~ 6,
    Group_Production_Method=="Electrolysis"&Group_Feedstock=="Electricity" ~ 8,
    Group_Production_Method=="Electrolysis"&Group_Feedstock=="Renewable (Wind/Solar/Water)" ~ 9,
    Group_Production_Method=="Fermentation"&Group_Feedstock=="Biomass" ~ 10,
    Group_Production_Method=="Thermal water splitting"&Group_Feedstock=="Nuclear" ~ 11,
    Group_Production_Method=="Thermal water splitting"&Group_Feedstock=="Renewable (Wind/Solar/Water)" ~ 12,
    T ~ 0))
# Check
ci %>% filter(special_pathway>0) %>%
  group_by(Group_Production_Method,Group_Feedstock,special_pathway,CCS) %>%
  tally()

# Common labels for figures -----
common_axis_sec <- scale_y_continuous(
  sec.axis = sec_axis(~.*0.12,name=expression("[kg CO"["2"]~"e/kg H"["2"]~"]")))
common_axis <- labs(x="",
                    y=expression("Hydrogen Production Carbon Intenstiy [g CO"["2"]~"e/MJ H"["2"]~"]"))
  
# EoF