# Facet Grid up Title function

library(gtable)
library(grid)
library(gridExtra)

# put title top
# https://stackoverflow.com/questions/70369558/two-column-facet-grid-with-strip-labels-on-top
f.TitleTop <- function(ggObject){
  
  gt <- ggplotGrob(ggObject)
  panels <-c(subset(gt$layout, grepl("panel", gt$layout$name), se=t:r))
  for(i in rev(panels$t-1)) {
    gt = gtable_add_rows(gt, unit(1.2, "lines"), i)
  }
  panels <-c(subset(gt$layout, grepl("panel", gt$layout$name), se=t:r))
  strips <- c(subset(gt$layout, grepl("strip-r", gt$layout$name), se=t:r))
  stripText = gtable_filter(gt, "strip-r")
  for(i in 1:length(strips$t)) {
    gt = gtable_add_grob(gt, stripText$grobs[[i]]$grobs[[1]], t=panels$t[i]-1, l=5)
  }
  gt = gt[,-6]
  # for(i in panels$t) { # remove for the all figure for the article, for some reason it interfer with the sec. axis
  #   gt$heights[i-1] = unit(1.2, "lines")
  #   gt$heights[i-2] = unit(0.2, "lines")
  # }
  return(gt)
}

# to draw it
# grid.newpage()
# grid.draw(gt)
