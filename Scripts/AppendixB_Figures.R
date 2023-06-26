## Meta-analysis on Studies found
# Figures for Appendix B. Metrics on Studies found (metadata)
# Pablo Busch 2023. pmbusch@ucdavis.edu

## Libraries ----

library(tidyverse)
library(readxl)
library(zoo)
library(lubridate)

# Load Data -----
fig_name <- "Figures/%s"

studies <- read_excel("Data/S2_Articles and CI values collected.xlsx",
                      sheet="Articles Found")
(names(studies) <- studies %>% names() %>% 
    str_remove_all("\r|\n|\\?") %>% 
    str_replace_all(" ","_") %>% 
    str_replace("%","perc"))

nrow(studies) # 85 studies found

## Feat data -----

# Journal name to lower
studies <- studies %>% 
  mutate(Source_title=Source_title %>% 
           str_to_title() %>% 
           str_replace_all("Of","of") %>% 
           str_replace_all("And","and") %>% 
           str_replace_all("In","in") %>% 
           str_replace_all("The","the") %>% 
           str_replace_all("internation","Internation"))
studies %>% group_by(Source_title) %>% tally(sort = T)


# FIGURES --------
theme_set(theme_bw(11)+theme(panel.grid.major = element_blank(),
                             text = element_text(family="sans"), # arial is mapped as sans in windowsFonts()
                             panel.grid.minor = element_blank()))
color_figures <- "brown"

## Figure B.1 Number of studies per year of publication -----
# interpolate 2022: 
data_extrapolated <- tibble(
  Year=2022,
  count=floor(17*12/7) # Studies were retrieved in August 2022
)


studies %>%
  # mutate(year=as_date(paste0(Year,"0101"))) %>%
  group_by(Year) %>% 
  summarise(count=n()) %>% ungroup() %>% 
  ggplot(aes(x=Year,y=count)) +
  geom_col(data=data_extrapolated,fill=color_figures,alpha=.3)+
  geom_col(fill=color_figures)+
  geom_text(aes(label=count), family="sans",size=14*5/14 * 0.8, vjust=-0.8)+
  labs(x="", y="Number of Studies")+
  coord_cartesian(ylim=c(0,30), expand=F)
  
ggsave(sprintf(fig_name,"FigureB1.png"),
       width = 190, height = 190*600/980, units = "mm")
ggsave(sprintf(fig_name,"FigureB1.svg"),
       width = 190, height = 190*600/980, units = "mm")
ggsave(sprintf(fig_name,"FigureB1.tiff"),last_plot(),units = "mm", dpi=500,
       width = 190, # Full width
       height = 190*600/980) # Ratio W-H is 980-600


## Figure B.2 Number of studies per Journal of publication -----
journal_count <- studies %>%
  group_by(Source_title) %>% 
  summarise(count=n()) %>% ungroup() %>% 
  mutate(count_sort=count)

journal_count %>% group_by(count) %>% tally() # 23 articles in a unique journal

# caption for Journals with only 1 article
cap_journal <- journal_count %>% filter(count==1) %>% 
  pull(Source_title) %>% paste(collapse = ", ") %>% str_wrap(width=130)
cap_journal <- paste0("Unique Journals: ",cap_journal)


unique_count <- journal_count %>% filter(count==1) %>% nrow()
journal_count <- journal_count %>% filter(count>1)
journal_count <-  rbind(journal_count,c("Articles with unique journal (see caption)",unique_count,1)) %>% 
  mutate(count=as.numeric(count),
         count_sort=as.numeric(count_sort))

theme_set(theme_bw(11)+theme(panel.grid.major = element_blank(),
                             text = element_text(family="sans"), # arial is mapped as sans in windowsFonts()
                             panel.grid.minor = element_blank()))

journal_count %>% 
  ggplot(aes(x=reorder(Source_title,count_sort),y=count)) + 
  geom_col(fill=color_figures)+
  geom_text(aes(label=count), family="sans",size=14*5/14 * 0.8, hjust=-0.8)+
  labs(x="", y="Number of Studies",caption = cap_journal)+
  # scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  coord_flip(ylim=c(0,30), expand=F)+
  theme(plot.caption=element_text(size=8, vjust=1,lineheight = 0.8))


ggsave(sprintf(fig_name,"FigureB2.png"),
       width = 190, height = 190*600/980, units = "mm")
ggsave(sprintf(fig_name,"FigureB2.svg"),
       width = 190, height = 190*600/980, units = "mm")
ggsave(sprintf(fig_name,"FigureB2.tiff"),last_plot(),units = "mm", dpi=500,
       width = 190, # Full width
       height = 190*600/980) # Ratio W-H is 980-600

# Others ------------

## VENN Diagram -----
#Source: https://www.r-graph-gallery.com/14-venn-diagramm.html#custom
library(VennDiagram)

# prepare data
studies$Software %>% table()
studies$Database %>% table()
studies$GWP_values %>% table()

studies <- studies %>% 
  mutate(venn_software=Software!="No mention",
         venn_database=Database!="No mention",
         venn_gwp=GWP_values!="No mention")
  
# Vectors of studies containing each type
set1 <- paste(1:85,studies$venn_software,"")
set1 <- subset(set1, str_detect(set1,"TRUE"))
set2 <- paste(1:85,studies$venn_database,"")
set2 <- subset(set2, str_detect(set2,"TRUE"))
set3 <- paste(1:85,studies$venn_gwp,"")
set3 <- subset(set3, str_detect(set3,"TRUE"))

# Chart
venn.diagram(
  x = list(set2, set1, set3),
  category.names = c("Source (63)","Software (64)" , "Carbon Impact Method (60)"),
  filename = 'venn_diagramm.png',
  output=TRUE,
  
  # Numbers
  cex = 1.2,
  fontfamily = "sans",
  cat.cex=1.0,
  cat.fontfamily = "sans",
  col=c("#440154ff", '#21908dff', 'brown'),
  cat.col = c("#440154ff", '#21908dff', 'brown')
)


## World Cloud from Title ------
# source: https://cran.r-project.org/web/packages/ggwordcloud/vignettes/ggwordcloud.html
library(ggwordcloud)
library(glue)
library(tidytext)

fileText <- substr(studies$Title, 1, nchar(studies$Title)) %>% 
  str_trim()
fileText <- gsub("\\$", "", fileText) 
# tokenize
tokens <- tibble(text = fileText) %>% unnest_tokens(word, text)
# Remove unnecessary words
tokens <- tokens %>% filter((!word %in% c("of","for","and","from",
                                          "in","to","the","a","\\d",
                                          "f","e","d","face",
                                          "can","he","by","within",
                                          "whole","over","net",
                                          "contexts","against","implications",
                                          "readily","considerations",
                                          "20","2070","with","through",
                                          "an","annual","mpa","2050","19"
                                          # "china","california","european",
                                          # "united","india","swiss","uk","eu",
                                          # "paris","german","states"
)))
tokens %>% 
  group_by(word) %>% 
  summarise(count=n()) %>% ungroup() %>% 
  # filter(count>1) %>% # filter at least X times appereance
  ggplot(aes(label = word, size=count)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 20) +
  theme_minimal()



ggsave(sprintf(fig_name,"wordCloud.png"),dpi=600,
       width = 14.87, height = 9.30, units = "in")
ggsave(sprintf(fig_name,"wordCloud.svg"),dpi=600,
       width = 14.87, height = 9.30, units = "in")
# top ten words
tokens %>% 
  group_by(word) %>% 
  summarise(count=n()) %>% ungroup() %>% 
  arrange(desc(count)) %>% head(15)


## EoF