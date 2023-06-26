# Lit. Review CI Values Figure
# Figure 3 CI for Steam Reforming production pathways
# Pablo Busch 2023. pmbusch@ucdavis.edu


# Load Data-----
source("Scripts/00-Load_CI_data.R",encoding = "UTF-8")

# Prepare data ----

# Filter only Steam Reforming Pathways
ci_steam <- ci %>% 
  filter(Group_Production_Method=="Steam Reforming") %>% 
  mutate(feed=if_else(is.na(Group_Feedstock_detail),"",paste0(" - ",Group_Feedstock_detail)),
         xlabel=paste0(Group_Feedstock,CCS)) %>% 
  filter(Group_Feedstock!="Other",Group_Feedstock!="Nuclear") %>% 
  mutate(xlabel=factor(xlabel,levels=c(
    "Other fossil fuels",
    "Biomass",
    "Natural Gas + CCS",
    "Natural Gas")),
    Group_Feedstock2=str_remove_all(Group_Feedstock_detail," Biomass")) %>% 
  group_by(Group_Feedstock2) %>% # n unique article
  mutate(n_unique=n_distinct(Key_Article)) %>% ungroup() %>%
  replace_na(list(Group_Feedstock2="No biomass detail")) %>% 
  mutate(Group_Feedstock2=paste0(Group_Feedstock2," (n articles = ",n_unique,")") %>% 
           str_replace("42","1")) # biomass no detail are 12 articles

cols <- ci_steam$Group_Feedstock2 %>% unique()
cols <- c(cols[2:4],cols[1])

# Figure ----

set.seed(2023) # for jitter
p_steam <- ci_steam %>% 
  ggplot(aes(xlabel,CI))+
  geom_point(aes(col=Group_Feedstock2,group=Group_Feedstock2),
             position=position_jitterdodge(jitter.width=0.2),
             size=5,alpha=.4)+
  coord_flip()+
  scale_color_manual(breaks=cols,
                     values = c("#e7298a","#ec7014","#005a32","#252525"))+ #https://www.statology.org/ggplot-default-colors/
  theme_bw(11)+
  theme(legend.position = c(0.75, 0.55),
        text = element_text(family="sans"), # arial is mapped as sans in windowsFonts()
        axis.title=element_text(size=11),
        axis.title.y=element_text(angle=0,margin=margin(r=-70)),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())+
  common_axis_sec+common_axis+
  labs(x="Feedstock and \n Main Energy Source")+
  guides(color = guide_legend("Biomass Detail",reverse=TRUE))
p_steam


# Save figure in different formats
ggsave(sprintf(fig_name,"Figure3.png"),plot=p_steam,units = "mm" ,
       width = 190, # Full width
       height = 190*600/980) # Ratio W-H is 980-600
ggsave(sprintf(fig_name,"Figure3.svg"),plot=p_steam,units = "mm" ,
       width = 190, # Full width
       height = 190*600/980) # Ratio W-H is 980-600
# save in high res
ggsave(sprintf(fig_name,"Figure3.tiff"),p_steam,units = "mm", dpi=500,
       width = 190, # Full width
       height = 190*600/980) # Ratio W-H is 980-600

# EoF