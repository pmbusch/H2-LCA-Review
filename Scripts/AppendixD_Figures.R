# Lit. Review CI H2 Pathways
# Figures Appendix D. Analysis on the electricity mix by country
# Pablo Busch 2023. pmbusch@ucdavis.edu
# Data source: https://ember-climate.org/data-catalogue/yearly-electricity-data/

# libraries ----
library(tidyverse)
library(readxl)
library(flextable)
library(lubridate)
library(officer)
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))

# load data ----
fig_name <- "Figures/%s"

# Data is already filtered by countries found in the lit. review.
df <- read_csv("Data/electricity_mix_countries_EMBER.csv")
names(df) <- str_replace_all(names(df)," ","_")

# Figure D1  Energy share of the total electricity generated in 2021 per country -------------

## Prepare data -----

# extract CI
ci_country <- df %>% filter(Variable=="CO2 intensity")
ci_country_year <- ci_country %>% rename(ci=Value) %>% select(Area,Year,ci)
ci_country <- ci_country %>% filter(Year==2021)
ci_country <- ci_country %>% rename(ci=Value) %>% select(Area,ci)

# year
df <- df %>% filter(Year==2021)

# Fuel type to sum 100% and Unit
fuel_type <- c("Other Renewables","Wind","Solar","Hydro",
               "Nuclear","Bioenergy","Other Fossil","Gas","Coal")

fuel_type_color <- c(
  "Wind"="#588970",
  "Solar"="#8B8000", 
  "Other Fossil"="#5A5A5A",
  "Nuclear"="#cab2d6", 
  "Coal"="#8c2d04", 
  "Gas"="#252525", 
  "Bioenergy"="#964B00",
  "Other Renewables"="#005a32",
  "Hydro"="#1f78b4")

df <- df %>% filter(Variable %in% fuel_type, Unit=="%") %>% 
  mutate(Variable=factor(Variable,levels=fuel_type))
df %>% group_by(Area) %>% summarise(value=sum(Value)) # check that they sum 100

# Join CI
df <- df %>% left_join(ci_country,by="Area")

# Add ci to label
df <- df %>% mutate(xlabel=paste0(
  Area," (CI: ",round(ci,0),")"
))

# Labels?
df <- df %>% mutate(ci_label=if_else(Value>10,paste0(round(Value,0),"%"),""))

## Figure -----
df %>% 
  ggplot(aes(reorder(xlabel,ci),Value,fill=Variable))+
  geom_col()+
  geom_text(aes(label=ci_label),family="sans",size=11*5/14 * 0.8,
            col="white",position = position_stack(vjust = 0.5))+
  coord_flip(expand=F)+
  scale_fill_manual(values=fuel_type_color)+
  guides(fill = guide_legend("Energy",reverse = T,byrow = T))+
  labs(y="Share %",x="")+
  theme_bw(11)+
  theme(legend.position = "bottom",
        text = element_text(family="sans"), # arial is mapped as sans in windowsFonts()
        axis.title=element_text(size=11))

# Save figure in different formats
ggsave(sprintf(fig_name,"FigureD1.png"),plot=last_plot(),units = "mm" ,
       width = 190, # Full width
       height = 190*600/980) # Ratio W-H is 980-600
ggsave(sprintf(fig_name,"FigureD1.svg"),plot=last_plot(),units = "mm" ,
       width = 190, # Full width
       height = 190*600/980) # Ratio W-H is 980-600
# save in high res
ggsave(sprintf(fig_name,"FigureD1.tiff"),last_plot(),units = "mm", dpi=500,
       width = 190, # Full width
       height = 190*600/980) # Ratio W-H is 980-600


# Figure D.2 Hydrogen production pathway CI for electrolysis with grid electricity vs Electricity carbon intensity ------
# Comparison to Electrolysis CI

## prepare data -----
# load data from the electroylsis figure (Figure 5) script
source("Scripts/Figure5.R",encoding = "UTF-8")

## add year of publication
articles <- read_excel("Data/S2_Articles and CI values collected.xlsx",
           sheet="Articles Found") %>% select(Key,Year)

# Filter to CI values of electrolysis with country grid and article info
ci_elect2 <- ci_elect %>% 
  filter(Group_Production_Method=="Electrolysis",xlabel=="Electricity Country Mix") %>% 
  left_join(articles, by=c("Key_Article"="Key"))
rm(articles)


ci_elect2 <- ci_elect2 %>% 
  filter(country!="") %>% 
  mutate(country=country %>% 
           str_replace_all("US","United States of America") %>% 
           str_replace_all("UK","United Kingdom"))
# ci_elect2$country %>% unique()

# Estimate Year for electricity mix
ci_elect2 <- ci_elect2 %>% 
  mutate(year_mix=if_else(!is.na(Electricity_Mix_Year), Electricity_Mix_Year,Year-1), # previous year
         year_mix=if_else(year_mix>2021,2021,year_mix),
         year_mix_cat=case_when(
           Electricity_Mix_Year>2022 ~ paste0("Projection: ",Electricity_Mix_Year),
           Article_N==128 & Electricity_Mix_Year==2020 ~"Projection: 2020",
           !is.na(Electricity_Mix_Year) ~ "Published by article",
           T ~ "Estimated by publication year")) %>% 
  mutate(year_mix_cat=if_else(str_detect(year_mix_cat,"Projection"),
                          "Projected electricity mix",
                          year_mix_cat) %>% as.factor())

# join
ci_join <- ci_elect2 %>% left_join(ci_country_year,by=c("country"="Area","year_mix"="Year"))

color_year <- c("Estimated by publication year"="#5A5A5A",
                # "Projection: 2020"="#cab2d6",
                # "Projection: 2030"="#e31a1c",
                # "Projection: 2040"="#1f78b4",
                # "Projection: 2050"="#005a32",
                "Projected electricity mix"="#005a32",
                "Published by article"="#252525")

## figure -----
ci_join %>% 
  mutate(ci=if_else(Article_N==124,174,ci)) %>%  # CI for electricity reported on article, in gCO2e/kWh
  mutate(ci=if_else(Article_N==91,400,ci)) %>%  # CI for electricity reported on article
  mutate(ci=if_else(Article_N==54,550,ci)) %>%  # CI for electricity reported on article
  mutate(ci=if_else(Article_N==12,834.5,ci)) %>%  # CI for electricity reported on article
  mutate(ci=if_else(Article_N==126 & is.na(Electricity_Mix_Year),514,ci)) %>%  # CI for electricity reported on article
  mutate(ci=if_else(Article_N==126 & Electricity_Mix_Year==2030,283,ci)) %>%  # CI for electricity reported on article
  mutate(ci=if_else(Article_N==126 & Electricity_Mix_Year==2050,151,ci)) %>%  # CI for electricity reported on article
  mutate(ci=ci/3.6) %>% # per kWh to per MJ
  filter(country_flag!="") %>%
  ggplot(aes(ci,CI))+
  geom_point(size=8,shape=16,aes(col=year_mix_cat))+
  # geom_point(size=8,shape=1,aes(col=year_mix_cat),stroke=3)+
  geom_flag(aes(country=country_flag))+
  scale_country()+
  geom_abline(slope=1, linetype="dashed")+
  scale_color_manual(values=color_year)+
  labs(y=expression("Hydrogen Production Carbon Intensity [g CO"["2"]~"e/MJ H"["2"]~"]"),
       x=expression("Electricity Carbon Intensity [g CO"["2"]~"/MJ"~"]"),
       col="Year Electricity Mix")+
  guides(country=F)+
  theme_bw(11)+
  theme(legend.position = c(0.2, 0.7),
        text = element_text(family="sans"), # arial is mapped as sans in windowsFonts()
        )

# Save figure in different formats
ggsave(sprintf(fig_name,"FigureD2.png"),plot=last_plot(),units = "mm" ,
       width = 190, # Full width
       height = 190*600/980) # Ratio W-H is 980-600
ggsave(sprintf(fig_name,"FigureD2.svg"),plot=last_plot(),units = "mm" ,
       width = 190, # Full width
       height = 190*600/980) # Ratio W-H is 980-600
ggsave(sprintf(fig_name,"FigureD2.tiff"),last_plot(),units = "mm", dpi=500,
       width = 190, # Full width
       height = 190*600/980) # Ratio W-H is 980-600

# EoF