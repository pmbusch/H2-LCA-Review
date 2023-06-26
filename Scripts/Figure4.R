# Lit. Review CI Values Figure
# Figure 4 CI for Gasification production pathways
# Pablo Busch 2023. pmbusch@ucdavis.edu


# Load Data-----
source("Scripts/00-Load_CI_data.R",encoding = "UTF-8")

# Prepare data ----

# Filter only Gasification Pathways
ci_gas <- ci %>% 
  filter(Group_Production_Method=="Gasification") %>% 
  mutate(feed=if_else(is.na(Group_Feedstock_detail),"",paste0(" - ",Group_Feedstock_detail)),
         xlabel=paste0(Group_Feedstock,CCS)) %>% 
  filter(!(Group_Feedstock %in% c("Other fossil fuels","Nuclear",
                                  "Electricity","Natural Gas"))) %>%
  mutate(xlabel=factor(xlabel,levels=c(
    "Biomass + CCS","Biomass",
    "Coal + CCS","Coal")),
    Group_Feedstock2=str_remove_all(Group_Feedstock_detail," Biomass")) %>% 
  group_by(Group_Feedstock2) %>% # n unique article
  mutate(n_unique=n_distinct(Key_Article)) %>% ungroup() %>% 
  replace_na(list(Group_Feedstock2="No biomass detail")) %>% 
  mutate(Group_Feedstock2=paste0(Group_Feedstock2,"\n (n articles = ",n_unique,")") %>% 
           str_replace("27","12")) # biomass no detail are 12 articles

cols <- ci_gas$Group_Feedstock2 %>% unique()
cols <- c(cols[2:6],cols[1]) # reorder

# Figure ----

set.seed(2023) # for jitter

p_gas <- ci_gas %>% 
  ggplot(aes(xlabel,CI))+
  geom_point(aes(col=Group_Feedstock2,group=Group_Feedstock2),
             position=position_jitterdodge(jitter.width=0.2),
             size=5,alpha=.4)+
  geom_hline(yintercept=0,linetype="dashed")+
  coord_flip(ylim = c(-200,840))+
  scale_color_manual(breaks = cols,
                     values = c("#e7298a","#ec7014","#005a32","#719d06","#4a1486","#252525"))+ #https://www.statology.org/ggplot-default-colors/
  theme_bw(11)+
  theme(legend.position = "right",
        text = element_text(family="sans"), # arial is mapped as sans in windowsFonts()
        legend.text = element_text(size=11),
        legend.spacing.x = unit(0.2, 'cm'),
        axis.title=element_text(size=11),
        axis.title.y=element_text(angle=0,margin=margin(r=-60)),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())+
  common_axis_sec+common_axis+
  labs(x="Feedstock and \n Main Energy Source")+
  guides(color = guide_legend("Biomass Detail",reverse=TRUE))
p_gas

# Save figure in different formats
ggsave(sprintf(fig_name,"Figure4.png"),plot=p_gas,units = "mm" ,
       width = 190, # Full width
       height = 190*600/980) # Ratio W-H is 980-600
ggsave(sprintf(fig_name,"Figure4.svg"),plot=p_gas,units = "mm" ,
       width = 190, # Full width
       height = 190*600/980) # Ratio W-H is 980-600

# save in high res
ggsave(sprintf(fig_name,"Figure4.tiff"),p_gas,units = "mm", dpi=500,
       width = 190, # Full width
       height = 190*600/980) # Ratio W-H is 980-600

# EoF