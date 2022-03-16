#' ---
#' title: "UA plots"
#' author: "Lia Chalifour"
#' date: "31 Aug 2020"
#' output: github_document
#' ---


#' This code is used to create a combined plot of the uncertainty analyses for costs, feasibility, and benefits estimates. Requires .csv files generated in 'calculateCEscore.R', 'Sens_Analysis_Feasibility.R', and 'Sens_Analysis.R'; written to run after running each of those codes and printing the temp plots (i.e., 'cost.plot') without clearing workspace.
#' 

library(ggpubr)
ggarrange(cost.plot, NULL, feas.plot, NULL, ben.plot, nrow=1, labels = c("A", "", "B","", "C"), widths = c(1,-0.1,1,-0.1,1), label.x = 0.8, align = "h")
ggsave("figures/Figure_S5.pdf", width = 13, height = 4, units = "in")

ggarrange(cost.plot.r, NULL, feas.plot.r, NULL, ben.plot.r, nrow=1, labels = c("A", "", "B","", "C"), widths = c(1,-0.15,1,-0.15,1), label.x = 0.8, align = "h")
ggsave("figures/Figure_S6.pdf", width = 12, height = 4, units = "in")



library(tidyverse)
library(mc2d)
library(cowplot)
library(ggridges)
library(here)

library(gridExtra)
par(mfrow=c(2,2))
cost.plot
feas.plot
ben.plot
