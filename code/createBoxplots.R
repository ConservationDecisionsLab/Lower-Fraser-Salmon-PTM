#' ---
#' title: "Plot Benefit Estimates"
#' author: "Adapted for the Lower Fraser Salmon PTM by Lia Chalifour"
#' date: "04 Feb 2020"
#' output: github_document
#' ---

#' Based on *Boxplot_script.R* from FRE PTM project [Found in Dropbox folder "\Fraser_River_Resilience\09_Plot_scripts\Final data scripts"]
#' This script creates two plots for each CU Group:  
#' 1) boxplots of the best guess, lower, and upper estimates for each Strategy from all Experts;  
#' 2) pointrange plots showing the best guess, lower and upper estimates of each Expert for each Strategy.  
#' NOTE: need to create 'figures' folder and 'benefits01' subfolder within it where plots will be saved.  
#'
#' 
#' Load packages
#+ message = FALSE, warning = FALSE
library(tidyverse)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(here)

#' Read in data from benefits aggregation
#+ warning = FALSE, message = FALSE
rlong <- read_csv("data/Results_tidy.csv") # use read_csv to make sure factors read in as character
  
#' Prepare data for plotting 
strat.levels <- unique(rlong$Strategy)
grp.levels <- unique(rlong$CU_group)
est.levels <- c("Lower", "Best.Guess", "Upper")
expcode <- unique(rlong$code) 

# Order the strategies to plot in the desired order
rlong$Strategy <- factor(rlong$Strategy, levels = strat.levels)
rlong$Est.Type <- factor(rlong$Est.Type, levels = est.levels)

#' ## Boxplots
#' Plot group estimates as boxplots and save as .pdf
for (j in seq_along(expcode)) {
  grp.list <- list()
  for (i in seq_along(grp.levels)) {
    temp.grpdata <- subset(rlong, CU_group == grp.levels[i])
    temp.plot <-
      ggplot(temp.grpdata, aes(x = Est.Type, # for each CU group, plot Estimate Type on x-axis
                               y = Value, # and Value on y-axis
                               fill = Est.Type) #and colour by estimate type
             ) + 
      geom_boxplot() + # tell R to display data in boxplot form
      geom_point(data = subset(temp.grpdata, code == expcode[j]), # plot indiv. expert's estimates as blue datapoints on top of the boxplots
                 aes(x = Est.Type, y = Value), 
                 color = 'blue'
                 ) +
      theme_cowplot() +  # use the theme "cowplot" for the plots, which is a nice minimalist theme
      theme(plot.margin = unit(c(1.5, 1, 1.5, 1), "cm"), # adjust margins around the outside of the plot (top, right, bottom, left)
            panel.spacing = unit(1, "lines"), # adjust margins and between panels of the plot (spacing of 1)
            axis.title.y = element_text(margin = margin(t = 0, 
                                                        r = 10,
                                                        b = 0,
                                                        l = 0) # adjust space between y-axis numbers and y-axis label
                                        ),
            plot.caption = element_text(size = 10, hjust = 0)
            ) + 
      scale_x_discrete(name = "",
                       breaks = c("Lower", "Best.Guess", "Upper"),
                       labels = c("L", "B", "U") # Give the x-axis variables shortened labels
      ) + 
      facet_wrap( ~ Strategy, nrow = 3) +  # create a separate panel of estimates for each management strategy
      scale_fill_manual(values = c("white", "gray80", "white"), # Assign colours to each type of estimate and don't show a legend
                        guide = FALSE 
      ) +
      labs(x = "", 
           y = "Probability of Green Status (%)", 
           title = paste(grp.levels[i]),
           caption = paste0(
             "Figure ", i, ". Boxplots summarizing the distribution of the lower (L), best guess (B), and upper (Upper) expert estimates of the probability of persistence 
of ", grp.levels[i], " under the Baseline scenario and each of the management strategies (S1 - S15).  The thick horizontal lines indicate  
the median estimate, while the surrounding box shows the interquartile range. Any outliers are shown as points beyond the plot whiskers. Your 
 individual estimates are shown in blue.")
           ) +  
      ylim(0, 100) # set the y-axis limits from 0-100
grp.list[[i]] <- temp.plot
    
  }
  # To save all plots as a .pdf: 
  plot1 <- marrangeGrob(grp.list, nrow = 1, ncol = 1, top = NULL) # arranges plots for saving to pdf file, one plot per page
  ggsave(filename = paste0("Exp", expcode[j], ".pdf", sep=''), 
         plot1, 
         path = "figures/benefits01", 
         width = 11, height = 8.5, units = "in")
  
}

print(temp.plot)


#' ## Pointrange plots
#' Plots each expert estimate separately (x-axis = Expert, y-axis point = Best guess, range = lower->upper)
#' Note removed apostrophe in "expert's" to avoid glitch in printing  in the captions

# Rearrange table so estimates for each group * strategy are on the same row
rlong.sub2 <- rlong[,c("code","CU_group","Value","Strategy","Est.Type")]
rlong.wide <- spread(rlong.sub2,key=Est.Type,value=Value)
rlong.wide$code<-as.factor(rlong.wide$code)

# Create plots
for (j in seq_along(expcode)) {
  grp.list <- list()
  for (i in seq_along(grp.levels)) {
  
    temp.expdata <- subset(rlong.wide, CU_group == grp.levels[i]) %>%
      mutate(expi = ifelse(code == expcode[j], T, F)) # this column allows for highlighting individual expert estimates
    
    temp.plot2 <-
      ggplot(temp.expdata, aes(x = code, # using the data CU group, plot Experts on X-axis
                               y = Best.Guess, # and corresponding standardized estimates on y-axis
                               color = expi # use this if highlighting individual expert responses
                               # color = code # use this if plotting all experts together (not highlighted)
                               )
             ) +  
      geom_pointrange(aes(ymin = Lower, ymax = Upper)) +
      theme_cowplot() +  # use the theme "cowplot" for the plots, which is a nice minimalist theme
      theme(plot.margin = unit(c(1.5, 1, 1.5, 1), "cm"), # adjust margins around the outside of the plot
            panel.spacing = unit(1, "lines"), # adjust margins and between panels of the plot (spacing of 1)
            axis.title.y = element_text(face = "bold", margin = margin(t = 0,
                                                        r = 5,
                                                        b = 0,
                                                        l = 0)), # adjust space between y-axis numbers and y-axis label
            axis.title.x = element_text(face = "bold", margin = margin(t = 10,
                                                        r = 0,
                                                        b = 0,
                                                        l = 0)), # adjust space between x-axis and label)
            axis.text.x = element_blank(),
            legend.justification=c(1,0), legend.position=c(0.98,-0.05), # repositions legend box (for final plot)
            plot.caption = element_text(size = 12, hjust = 0)
            ) +  
      scale_color_manual(values = c("grey", "blue"), guide = FALSE) + # turn this off if ploting all experts together
      # scale_color_brewer(palette='Paired') + # changes color palette
      facet_wrap( ~ Strategy, nrow = 3) +  # create a separate panel of estimates for each management strategy
      labs(x = "Experts",
           y = "Probability of Green Status (%)",
           title = paste(grp.levels[i]),
           caption = str_wrap(paste0("Figure ", i, ". Plots of each expert estimate of the probability that ", grp.levels[i], " will achieve green status under the Baseline scenario and each of the management strategies (S1 - S15). Each point indicates the best guess of one expert, with the lines corresponding to that experts lower and upper estimates. Your individual estimates are plotted in blue."), width = 130)
           ) +
      ylim(0, 100) # set the y-axis limits from 0-100

    grp.list[[i]] <- temp.plot2
    # To save plots as a .pdf, one file per CU group:
    # ggsave(temp.plot2, file=paste0(grp.levels[i],"_byExp.pdf", sep=''), path = "figures/", width = 10, height = 8, units = "in")
  
  }
  # To save all plots as a single .pdf: 
  plot2 <- marrangeGrob(grp.list, nrow = 1, ncol = 1, top = NULL) # arranges plots for saving to single pdf file, one plot per page
  ggsave(
    # filename = "IndivEstimates.pdf", # if plotting all estimates without highlighting
    filename = paste0("Indiv_Exp", expcode[j], ".pdf", sep=''), # if higlighting individual expert estimates
    plot2, 
    path = "figures/benefits01", 
    width = 11, height = 8.5, units = "in"
    )
}

print(temp.plot2)




