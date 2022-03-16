#' ---
#' title: "Conservation Optimization"
#' author: "Adapted for the LFR Salmon PTM by Lia Chalifour"
#' date: "14 July 2020"
#' output: github_document
#' ---
#'
#' This code runs the consOpt package, developed by Nicolai Cryer and Laura Kehoe, to perform optimization of management strategies based on their cost, benefit*feasibility, and complementarity. It generates a stepwise plot showing how many species / Conservation Units can be conserved for a given budget. 
#' 
#' Requires **Benefits.csv** from *getBenefitMatrix.R*, and a costs vector from
#' **CostFeas** table of strategy costs and feasibilities
#' 
#' Also requires COMBO_info.csv, which can be adapted from the sample data in consOPT, and is a csv listing strategy #s that are included in each strategy, so that the combined strategies are accounted for directly during optimization.
library(here)
library(ggplot2)
library(ggforce)
library(scales)
library(cowplot)
library(magick)
library(dplyr)
library(consOpt)

#FREdata() #load example data from the Fraser River Estuary PTM

#write.csv(COMBO_info, "data/COMBO_info.csv", row.names = FALSE) #save FRE combo as template, adjust to match the FR salmon strategies. See Notes on consOpt package for further detail, but essentially this lists all strategies, including the strategies that are combined within the combination strategies (S12 and S13), and the full combination strategy (ALL), in order to constrain the Optimize function so that it does not pair duplicate strategies during the optimization. For the final strategy, you must also list its name (i.e. ALL) under it, as the package will check that the length of that strategy matches the # of strategies listed in row 1, to validate that it contains an "all strategies combined" option.

#Generate benefits matrix
benefits.matrix<- read.csv("data/Benefits_*.csv")

rownames(benefits.matrix)<-benefits.matrix$Strategy
benefits.matrix<- subset(benefits.matrix, Strategy != "S15") #remove future baseline scenario
benefits.matrix<- benefits.matrix[,-1] #remove strategy column


#Generate costs vector
costfeas<- read.csv("data/FRCostFeas.csv")
#costfeas<- read.csv("data/FRCostFeas_WWtreat_removed.csv")

bslcost<- costfeas[15,]
costfeas2<- rbind(bslcost, costfeas)
costfeas2<- costfeas2[-16,] #moved baseline to top and removed from bottom to match order of benefits matrix

cost.vector<- as.vector(costfeas2$Annual_Cost) #use annual cost for budget comparison

#Generate strategy combination matrix
COMBO_info<- read.csv("data/COMBO_info2.csv")

#Run optimization function
results<- Optimize(benefits.matrix=benefits.matrix, cost.vector=cost.vector, combo.strategies=COMBO_info, thresholds = c(50.00, 60.00)) #update threshold to include values below 50.01; no need to include 70% as there are no instances above it in the best guess scenario.

write.csv(results, "data/BG_Solution_consOpt_best.csv", row.names = FALSE)
#write.csv(results, "data/BG_Solution_consOpt_WWtreat_removed.csv", row.names = FALSE)

##### Repeat but for upper benefits estimates to see most optimistic scenario
benefits.matrix_up<- read.csv("data/Upper_*.csv")

rownames(benefits.matrix_up)<- benefits.matrix_up$Strategy #move strategies to row names
benefits.matrix_up<- subset(benefits.matrix_up, Strategy != "S15") #remove future baseline scenario
benefits.matrix_up<- benefits.matrix_up[,-1] #remove strategy column

results_upper<- Optimize(benefits.matrix=benefits.matrix_up, cost.vector=cost.vector, combo.strategies=COMBO_info, thresholds = c(50.00, 60.00, 70.00))

write.csv(results_upper, "data/BG_Solution_consOpt_upper.csv")
#write.csv(results_upper, "data/BG_Solution_consOpt_upper_WWRemoved.csv")

##### Repeat but for lower benefits estimates to see most pessimistic scenario
benefits.matrix_low<- read.csv("data/Lower_*.csv")

rownames(benefits.matrix_low)<- benefits.matrix_low$Strategy #move strategies to row names
benefits.matrix_low<-subset(benefits.matrix_low, Strategy != "S15") #remove future baseline scenario
benefits.matrix_low<- benefits.matrix_low[,-1] #remove strategy column

results_lower<- Optimize(benefits.matrix=benefits.matrix_low, cost.vector=cost.vector, combo.strategies=COMBO_info, thresholds = c(40.00, 50.00))

write.csv(results_lower, "data/BG_Solution_consOpt_lower.csv")
#write.csv(results_lower, "data/BG_Solution_consOpt_lower_annual_WWRemoved.csv")

######################################################################
#Create stepwise plot; note this function is altered slightly from "plotting.R" in the consOpt package code to adjust aesthetics
#####################
## Create custom colour scale to match to persistence threshold:  
threshold_col<- c('#44AA99','#117733','#332288','#AA4499') #turquoise (aka dark moderate cyan), forest green (aka dark cyan-lime green), dark blue, dark moderate pink -- colour blind friendly scale
names(threshold_col)<- c(40,50,60,70)

colours_best<- c('#117733','#332288')
names(colours_best)<- c(50,60)

colours_up<-c('#117733','#332288','#AA4499')
names(colours_up)<- c(50,60,70)

colours_lo<- c('#44AA99','#117733')
names(colours_lo)<-c(40,50) 

#define colours for each plot & run PlotResults before plotting
colours<- scale_color_manual(values = colours_best, name= "Conservation threshold (%)", guide = guide_legend(direction="horizontal", title.position = "top"))
#colours<- scale_color_manual(values = colours_up, name= "Conservation threshold (%)", guide = guide_legend(direction="horizontal", title.position = "top"))
#colours<- scale_color_manual(values = colours_lo, name= "Conservation threshold (%)", guide = guide_legend(direction="horizontal", title.position = "top"))

PlotResults <- function(summary.results, draw.labels=TRUE){
  # Create a plot object from the neat results table
  tmp <- summary.results
  # scale
  tmp$total_cost <- (tmp$total_cost / 10^6)
  tmp$threshold <- round(tmp$threshold) # removing decimal points
  levels(tmp$strategies)[1]<- "B" #shorten baseline label
  
  # Create plot object - add linetype variation by threshold; reduce point size 
  this.plot <- ggplot(tmp, aes(x=total_cost, y=number_of_species, group=threshold, shape=factor(threshold), linetype = factor(threshold), color = factor(threshold), label=strategies)) +
    theme_cowplot() +
    geom_step() +
    scale_linetype(guide = guide_legend(direction="horizontal", title.position = "top")) +
    geom_point(size=2.7) +
    colours +
    scale_shape(guide = guide_legend(direction="horizontal", title.position = "top")) +
    scale_y_continuous(breaks = seq(0,20,2), limits = c(0,19)) +
      #breaks=min(tmp$number_of_species):length(benefits.matrix), limits = c(0, length(benefits.matrix))) + #add limit & adjust breaks to show total CUs (length(benefits.matrix)) rather than max secured. Suppress labels and define below.
    labs(x = "Mean annual cost (million CAD)", 
             y = "Number of salmon CUs secured"
             , linetype = "Conservation threshold (%)"
             , shape = "Conservation threshold (%)",
         color = "Conservation threshold (%)"
    )
  
  if(draw.labels){
    this.plot <- this.plot + geom_text(hjust = -0.15, vjust = 0, nudge_y = -1, size=2.7, check_overlap = TRUE) #adjust labels to be above points and not duplicate (i.e. for Baseline) 
  }
  
  plot(this.plot)
  this.plot
}

#note just set draw.labels=FALSE when calling PlotResults if you don't want strategies showing

#bring in symbols for each strategy
S01<- image_read("figures/PTM_Strategies_Clipart/S01_Fish.png")
S02<- image_read("figures/PTM_Strategies_Clipart/S02_Hydrology.png")
S03<- image_read("figures/PTM_Strategies_Clipart/S03_Protect.png")
S04<- image_read("figures/PTM_Strategies_Clipart/S04_FreshwaterRest.png")
S05<- image_read("figures/PTM_Strategies_Clipart/S05_EstuaryRest.png")
S06<- image_read("figures/PTM_Strategies_Clipart/S06_Barriers.png")
S07<- image_read("figures/PTM_Strategies_Clipart/S07_InvasiveSpp.png")
S08<- image_read("figures/PTM_Strategies_Clipart/S08_Pollution.png")
S09<- image_read("figures/PTM_Strategies_Clipart/S09_Aquaculture.png")
S10<- image_read("figures/PTM_Strategies_Clipart/S10_Hatchery.png")
S11<- image_read("figures/PTM_Strategies_Clipart/S11_Predator.png")
S12<- image_read("figures/PTM_Strategies_Clipart/S12_Fish-Aqua-Hatch.png")
S13<- image_read("figures/PTM_Strategies_Clipart/S13_All-Habitat.png")

#Plot results for best guess scenario - Figure 4
p<- PlotResults(results) 
p<- p + facet_zoom(xlim=c(0,20), zoom.size = 1.75) +
scale_x_continuous(breaks = function (x) pretty(x, n=10)) 

p<- p + theme(
        legend.position = c(0.35,0.97),
        legend.key.height = unit(0.4,"cm"),
        legend.key.width = unit(1, "cm"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.background = element_rect(fill = "white"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        axis.text = element_text(size = 10)
  ) 
ggdraw(p) + 
  draw_image(S07, x=-0.369, y=-0.31, scale = 0.045) +
  draw_image(S05, x=-0.258, y=-0.295, scale = 0.06) +
  draw_image(S04, x=-0.35, y=-0.2, scale = 0.06) + 
  draw_image(S01, x=-0.24, y=-0.18, scale = 0.06) + 
  draw_image(S13, x=0.39, y=-0.03, scale = 0.07) +
  draw_image(S13, x=0.39, y=-0.28, scale = 0.07)

ggsave(filename="Figure_4.pdf", width = 160, height = 160, units = "mm", path = "figures/")

### Figure S7 WW Treatment facility costs removed 
p1<- PlotResults(results) 

p1<- p1 + theme(
  legend.position = c(0.35,0.97),
  legend.key.height = unit(0.4,"cm"),
  legend.key.width = unit(1, "cm"),
  legend.title = element_text(size = 12),
  legend.text = element_text(size = 11),
  legend.background = element_rect(fill = "white"),
  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
  axis.text = element_text(size = 10)
) 

ggsave(filename="FigS7_noWW.pdf", width = 160, height = 100, units = "mm", path = "figures/")

###############################################################

########## Plot optimistic scenario

# Figure S3 
p2<- PlotResults(results_upper) 
p2<- p2 + facet_zoom(xlim=c(0,20), zoom.size = 1.1) +
  scale_x_continuous(breaks = function (x) pretty(x, n=10))

p2<- p2 + theme(
    legend.position = c(0.35,0.99),
    legend.key.height = unit(0.4,"cm"),
    legend.key.width = unit(1, "cm"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.background = element_rect(fill = "white"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.text = element_text(size = 10)
  ) 

ggsave(p2, filename="FigS3_Upper.pdf", width = 160, height = 160, units = "mm", path = "figures/")
###############################################################

########## Plot pessimistic scenario

#Figure S4 - note there are 0 CUs conserved
p3<- PlotResults(results_lower)+
  scale_x_continuous(breaks = function (x) pretty(x, n=10))

p3<- p3 + theme(
    legend.position = c(0.35,0.9),
    legend.key.height = unit(0.4,"cm"),
    legend.key.width = unit(1, "cm"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.background = element_rect(fill = "white"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.text = element_text(size = 10)
  ) 

p3<- ggdraw(p3) + 
  draw_image(S13, x=-0.19, y=-0.17, scale = 0.1)

  
ggsave(p3, filename="FigS4_Lower.pdf", width = 160, height = 100, units = "mm", path = "figures/")



