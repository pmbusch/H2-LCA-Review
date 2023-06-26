# Lit. Review CI Values Figure
# Figure 5 CI for Electrolysis production pathways under different energy sources for electricity
# Also code for Figure E1 of Appendix
# Pablo Busch 2023. pmbusch@ucdavis.edu

# Additional libraries
library(ggrepel)
# devtools::install_github('rensa/ggflags') # Need to Install the Library from devtools
library(ggflags) ## to add country flags

# Load Data-----
source("Scripts/00-Load_CI_data.R",encoding = "UTF-8")

# Grouping for figure ----

xlabel_order <- c("Nuclear","Water","Wind","Solar","Renewable","Electricity Country Mix",
                  "Biomass","Fossil","Natural Gas","Coal")

ci_elect <- ci %>% 
  filter(Group_Production_Method=="Electrolysis") %>% 
  mutate(country=Electricity_Mix) %>% 
  mutate(Electricity_Mix=case_when(
           Electricity_Mix=="Rice husk" ~ "Biomass",
           Electricity_Mix %in% c("Coal","Natural Gas","Biomass",
                                  "Nuclear","Fossil") ~ Electricity_Mix,
           is.na(Electricity_Mix) ~ "Electricity Country Mix",
           T ~ "Electricity Country Mix"),
         xlabel=if_else(Feedstock=="Electricity",
                        Electricity_Mix,
                        Feedstock) %>% 
           factor(levels=xlabel_order)) %>% 
  mutate(pathway=if_else(Production_method=="Electrolysis","Electrolysis Alkaline",Production_method))

# Country label
ci_elect <- ci_elect %>% 
  mutate(country=if_else(xlabel=="Electricity Country Mix",
                         # as.character(Feedstock) %>% str_remove_all("Electricity") %>% 
                         country %>% 
                           str_remove_all("Renewable \\(Wind/Solar/Water\\)") %>% str_trim(),
         ""))
ci_elect$country %>% unique()

# Country codes ----
ci_elect <- ci_elect %>% 
  mutate(country_flag=case_when(
    country=="Germany"~"de",
    country=="Italy"~"it",
    country=="Switzerland"~"ch",
    country=="Denmark"~"dk",
    country=="Finland"~"fi",
    country=="France"~"fr",
    country=="Netherlands"~"nl",
    country=="Norway"~"no",
    country=="Poland"~"pl",
    country=="Sweden"~"se",
    country=="Colombia"~"co",
    country=="Spain"~"ea",
    country=="Europe"~"eu",
    country=="Australia"~"au",
    country=="China"~"cn",
    country=="South Korea"~"kr",
    country=="UK"~"gb",
    country=="Argentina"~"ar",
    T ~ str_to_lower(country)))


# Figure 5----
cols <- c("Electricity Country Mix"="#737373", # light grey
          "Wind"="#005a32", #dark green
          "Solar"="#005a32", #dark green
          "Renewable"="#005a32", #dark green
          "Nuclear"="#7a0177", #pruple
          "Coal"="#8c2d04", # brown
          "Natural Gas"="#252525", #dark grey
          "Biomass"="#41ab5d",#green
          "Water"="#005a32", #dark green
          "Fossil"="#8c2d04")  # brown

# same jitter in all plots
# jitterer <- position_jitter(w = 0.75, h = 0,seed=2013)
jitterer <- position_jitterdodge(jitter.width = 0.5,jitter.height = 0,
                                 seed = 2018,dodge.width = 1)

ci_elect %>% 
  ggplot(aes(xlabel,CI))+
  geom_jitter(data=filter(ci_elect,country==""),
              aes(col=xlabel),alpha=.3,size=5,width=0.2)+
  geom_hline(yintercept=0,linetype="dashed")+
  # geom_text_repel(aes(label=country),max.overlaps = 50)+
  scale_color_manual(values=cols)+
  coord_flip()+
  geom_point(data=filter(ci_elect,country!=""&country_flag!="co"),aes(fill=country_flag),
             size=6,shape=16,position=jitterer)+ # add black border to points
  geom_flag(data=filter(ci_elect,country!=""&country_flag!="co"),
            aes(country=country_flag,fill=country_flag),
            position = jitterer)+
            # position=position_dodge(width=1))+
  geom_point(data=filter(ci_elect,country_flag=="co"),# make colombia visible
             size=6,shape=16)+
  geom_flag(data=filter(ci_elect,country_flag=="co"), 
            aes(country=country_flag))+
  scale_country()+
  scale_x_discrete(limits = xlabel_order)+
  common_axis_sec+common_axis+
  labs(x="Main Energy Source")+
  guides(color = guide_legend("Electricity Source"))+
  theme_bw(11)+
  theme(legend.position = "none",
        text = element_text(family="sans"), # arial is mapped as sans in windowsFonts()
        axis.title=element_text(size=11),
        axis.title.y=element_text(angle=0,margin=margin(r=-70)),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())

# Save figure in different formats
ggsave(sprintf(fig_name,"Figure5.png"),plot=last_plot(),units = "mm" ,
       width = 190, # Full width
       height = 190*600/980) # Ratio W-H is 980-600
ggsave(sprintf(fig_name,"Figure5.svg"),plot=last_plot(),units = "mm" ,
       width = 190, # Full width
       height = 190*600/980) # Ratio W-H is 980-600

# save in high res
ggsave(sprintf(fig_name,"Figure5.tiff"),last_plot(),units = "mm", dpi=500,
       width = 190, # Full width
       height = 190*600/980) # Ratio W-H is 980-600

# Figure E.1 ------
# Analysis by technology
last_plot()+
  theme(axis.title.y=element_text(angle=0,margin=margin(r=-60)))+
  facet_wrap(~pathway)

ggsave(sprintf(fig_name,"FigureE1.png"),plot=last_plot(),units = "mm" ,
       width = 190, # Full width
       height = 190*600/980) # Ratio W-H is 980-600
# save in high res
ggsave(sprintf(fig_name,"FigureE1.tiff"),last_plot(),units = "mm", dpi=500,
       width = 190, # Full width
       height = 190*600/980) # Ratio W-H is 980-600

# EoF