# Lit. Review CI Values
# Figure 2 Summary of the CI for the main Hydrogen production pathways
# Pablo Busch 2023. pmbusch@ucdavis.edu


# Load Data -----
source("Scripts/00-Load_CI_data.R",encoding = "UTF-8")


# Filter for selected pathways
ci_special <- ci %>% 
  filter(special_pathway>0) %>% 
  mutate(Feedstock=Group_Feedstock)

ci_special <- ci_special %>% 
  mutate(xlabel_key=paste0(`Production Method`," - ",Group_Feedstock) %>% 
           str_remove(" \\(Wind/Solar/Water\\)") %>% 
           str_replace("Electricity","Electricity Grid Mix") %>% 
           str_replace("Electrolysis - Renewable","Electrolysis - Renewable Electricity"))

ci_special$xlabel_key %>% table


# Summary Statistics ----
summ_stats <- ci_special %>% 
  group_by(xlabel_key,special_pathway,`Production Method`,Feedstock) %>% 
  summarise(n=n(),
            Mean=mean(CI),
            SD=sd(CI),
            min=min(CI),
            median=median(CI),
            max=max(CI)) %>% ungroup()
# remove order column
summ_stats$special_pathway <- NULL

# add summary statistics 
ci_special <- ci_special %>% left_join(summ_stats)

# add labels for figure
ci_special <- ci_special %>% 
  mutate(
    xlabel=
      paste0("**",`Production Method`," - ",Group_Feedstock) %>%
      str_remove(" \\(Wind/Solar/Water\\)"),
    color_label=if_else(str_detect(xlabel,"CCS"),"CCS",Group_Feedstock))


## add regression line with confidence intervals
model1 <- lm(CI~xlabel-1,data=ci_special)
conf_intervals <- confint(model1, method="Wald", level=0.95)
conf_intervals <- tibble(xlabel=rownames(conf_intervals) %>% 
                           str_remove_all("xlabel"),
                         ci_low=conf_intervals[,1],
                         ci_high=conf_intervals[,2])
ci_special_join <- ci_special %>% 
  group_by(xlabel_key,xlabel,special_pathway,color_label,Mean) %>% tally()

# Add CI
conf_intervals <- conf_intervals %>% left_join(ci_special_join)
rm(ci_special_join)


conf_intervals <- conf_intervals %>% 
  mutate(ci_text=paste0(round(Mean,1)," (",round(ci_low,1),
                        ", ",round(ci_high,1),")"))
# Figure ----

# see https://aeclinic.org/aec-blog/2021/6/24/the-colors-of-hydrogen
# https://colorbrewer2.org/#type=sequential&scheme=Greys&n=8
color_scale_feedstock <- cols <- c("Electricity" = "#737373", # light grey
                                   "Biomass" = "#41ab5d", #green
                                   "Coal" = "#8c2d04",  # brown
                                   "Natural Gas" = "#252525", #dark grey
                                   "Renewable (Wind/Solar/Water)"="#005a32", #dark green
                                   "CCS"= "#084594", #BLUE
                                   "Nuclear"="#7a0177") #pruple
set.seed(2023) # for jitter

ci_special %>% 
  filter(CI>-200 & CI<900) %>% # Remopve extreme values from Figure (mentioned in the label for the figure)
  ggplot(aes(reorder(xlabel_key,desc(special_pathway)),
             CI,col=color_label,fill=color_label))+
  geom_jitter(alpha=.7,size=2)+
  geom_point(aes(y=Mean),shape=23,size=4,data=conf_intervals)+ # Add mean
  geom_linerange(aes(y=Mean,ymin=ci_low,ymax=ci_high),data=conf_intervals,size=1)+ # Add CI
  geom_hline(yintercept=0,linetype="dashed")+
  scale_fill_manual(values=color_scale_feedstock)+
  scale_color_manual(values=color_scale_feedstock)+
  scale_y_continuous(breaks = c(seq(0,750,250)), 
                     expand = c(0,0),
                     limits = c(-1050,1500),
                     sec.axis = sec_axis(~.*0.12,
                                         breaks=seq(0,90,30),
                                         name=expression("[kg CO"["2"]~"e/kg H"["2"]~"]"))) +
  coord_flip(xlim=c(0,13.5),expand = F)+
  # add text data
  geom_text(fontface=2,family="sans",size=11*5/14 * 0.8,y=-230,x=13,label="n",hjust = 0,col="gray30")+
  geom_text(family="sans",size=11*5/14 * 0.8,y=-230,aes(label=n),col="gray30",data=conf_intervals)+
  geom_text(fontface=2,family="sans",size=11*5/14 * 0.8,y=950,x=13.2,label=expression(bold("Mean CI [g CO"["2"]~"e/MJ H"["2"]~"]")),hjust = 0,col="gray30")+
  geom_text(fontface=2,family="sans",size=11*5/14 * 0.8,y=950,x=12.7,label="(95% confidence interval)",hjust = 0,col="gray30")+
  geom_text(family="sans",size=11*5/14 * 0.8,y=950,aes(label=ci_text),col="gray30",data=conf_intervals,hjust=0)+
  geom_text(fontface=2,family="sans",size=11*5/14 * 0.8,y=-1050,x=13,label="Pathway",hjust = 0,col="gray30")+
  geom_text(family="sans",size=11*5/14 * 0.8,y=-1050,aes(label=xlabel_key),col="gray30",data=conf_intervals,hjust=0)+
  # add bottom and top bar
  geom_segment(x = 0.01, y = -180,xend = 0.01, yend = 900,col="black",linewidth=0.5)+
  geom_segment(x = 13.49, y = -180,xend = 13.49, yend = 900,col="black",linewidth=0.5)+
  # add manually the markers
  geom_hline(yintercept=c(-125,125,375,625),col="gray30",alpha=.2)+
  common_axis+
# Modify theme to look good
  theme_bw(11)+
  theme(legend.position = "none",
        text = element_text(family="sans"), # arial is mapped as sans in windowsFonts()
        axis.title=element_text(size=11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border=element_blank(),
        axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank())

# Save figure in different formats
ggsave(sprintf(fig_name,"Figure2.png"),plot=last_plot(),units = "mm" ,
       # width = 980/3.7795275591, # pixel to mm under dpi=300
       # height = 600/3.7795275591)
       width = 190, # Full width
       height = 190*600/980) # Ratio W-H is 980-600
       
ggsave(sprintf(fig_name,"Figure2.svg"),plot=last_plot(),units = "mm" ,
       width = 190, # Full width
       height = 190*600/980) # Ratio W-H is 980-600
# save in high res
ggsave(sprintf(fig_name,"Figure2.tiff"),last_plot(),
       units = "mm", dpi=500,
       width = 190, # Full width
       height = 190*600/980) # Ratio W-H is 980-600

# EoF