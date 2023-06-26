# Lit. Review CI Values
# Appendix - Figure C.1 CI for the all the Hydrogen production pathways
# Pablo Busch 2023. pmbusch@ucdavis.edu


# Load Data-----
source("Scripts/00-Load_CI_data.R",encoding = "UTF-8")
source("Scripts/function_TitleTop.R",encoding = "UTF-8")



# Figure ----

# see https://aeclinic.org/aec-blog/2021/6/24/the-colors-of-hydrogen
# https://colorbrewer2.org/#type=sequential&scheme=Greys&n=8
ci$Group_Feedstock %>% unique()
color_scale_feedstock <- cols <- c("Electricity" = "#737373", # light grey
                                   "Biomass" = "#41ab5d", #green
                                   "Coal" = "#8c2d04",  # brown
                                   "Natural Gas" = "#252525", #dark grey
                                   "Other fossil fuels"="black",
                                   "Renewable (Wind/Solar/Water)"="#005a32", #dark green
                                   "Fossil Fuel + CCS"= "#084594", #BLUE
                                   "Other"="#FFA07A",
                                   "Ammonia"="#8B8000",  #dark yellow
                                   "Chlor-alkali"="#DDA0DD", 
                                   "Nuclear"="#7a0177") #purple

# add n for order
n_ci <- ci %>% group_by(Production_method) %>% tally()


all_fig <- ci %>% 
  filter(CI<900) %>% 
  mutate(color_label=if_else(str_detect(CCS,"CCS"),"Fossil Fuel + CCS",Group_Feedstock)) %>% 
  left_join(n_ci) %>% 
  mutate(Production_method=Production_method %>% 
           str_replace_all("By product industrial gas stream recovery","BGIGSR") %>% 
           str_replace_all("Electron beam plasma methane pyrolysis", "EBPMP") %>% 
           str_replace_all("Steam Cracker \\(by-product NGLs\\)","Steam Cracker")) %>% 
  ggplot(aes(reorder(Production_method,n),
             CI,col=color_label,fill=color_label))+
  # geom_point(alpha=.5,size=2,shape=21)+
  geom_jitter(alpha=.5,size=2,shape=21,width = .15)+
  geom_hline(yintercept=0,linetype="dashed")+
  scale_fill_manual(values=color_scale_feedstock)+
  scale_color_manual(values=color_scale_feedstock)+
  coord_flip(ylim = c(-150,840))+
  facet_grid(Production_family~.,scales = "free",space="free")+
  common_axis_sec+common_axis+
  labs(col="Main Energy Source",fill="Main Energy Source")+
       # caption="BGIGSR: By product industrial gas stream recovery. EBPMP: Electron beam plasma methane pyrolysis.")+
  # theme(legend.position = "right")
  theme_bw(11)+
  theme(legend.position = "bottom",
        text = element_text(family="sans"), # arial is mapped as sans in windowsFonts()
        legend.title = element_text(size=9), #change legend title font size
        legend.text = element_text(size=8),
        plot.caption=element_text(size=7, vjust=5,lineheight = 1),
        # panel.spacing = unit(1, 'lines'), 
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.y=element_text(angle=0,margin=margin(r=0)),
        strip.text.y = element_text(angle = 0),
        legend.spacing.x = unit(0.005, 'cm'))

gt <- f.TitleTop(all_fig)
grid.newpage();grid.draw(gt)


# values ommited
ci %>% filter(CI < -100 | CI>900) %>% 
  select(Production_method,Group_Feedstock,CI) #5


# Save figure in different formats
ggsave(sprintf(fig_name,"FigureC1.png"),plot=gt,units = "mm" ,
       width = 1187/3.7795275591, # pixel to mm under dpi=300
       height = 867/3.7795275591)
ggsave(sprintf(fig_name,"FigureC1.svg"),plot=gt,units = "mm" ,
       width = 1187/3.7795275591, # pixel to mm under dpi=300
       height = 867/3.7795275591)
# save in high res
ggsave(sprintf(fig_name,"FigureC1.tiff"),plot=gt,units = "mm", dpi=500,
       width = 190, # Full width
       height = 190*700/980) 


# EoF