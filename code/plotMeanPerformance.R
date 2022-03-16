#' ---
#' title: "Plot Averaged Performance"
#' author: "Adapted for the Lower Fraser Salmon PTM by Lia Chalifour"
#' date: "18 Feb 2020"
#' output: github_document
#' ---
#' 
#'Based on *plotMeanPerformance.R* by Abbey Camaclang in Martin Lab Github "PTM_analysis"
#' 
#' Creates pointrange plots of the standardized mean estimates of probability of persistence (y-axis) 
#' for each strategy (x-axis) and for each Conservation Unit (subplots).  
#' 
#' It can be used to plot mean estimates that are either unweighted (**Aggregated_Performance.csv** from *aggregateEstimates.r*)
#' or weighted by feasibility (**Expected_Performance.csv** from *getBenefitMatrix.r*)
#'   
#' Load packages
#+ warning = FALSE, message = FALSE
library(tidyverse)
library(cowplot)
library(gridExtra)

#' Prepare data for plotting
#+ warning = FALSE, message = FALSE

#unweighted versions
#exp.pop <- read_csv("data/Aggregated_Performance_*.csv")

#weighted by feasibility (i.e. likely outcome)
exp.pop<- read_csv("data/Expected_Performance_*.csv") #outlier expert removed
#exp.pop<- read_csv("data/Exp_Perf_*CoGov.csv") #V2 with cogovernance feasibilities

exp.pop$Conservation_Unit <- as.factor(exp.pop$Conservation_Unit)

# Organize data into correct format for plotting
exp.pop.long <-
  gather(exp.pop,
         key = Estimate,
         value = Value,
         #BSL_lower:ALL_best_guess) #' <!-- AC: update with the data column names -->
         BSL_lower:S15_Fut_Bas_best_guess) #' if want to include Future Baselines
head(exp.pop.long)

exp.pop.long <- na.omit(exp.pop.long) #note no NAs in _* versions

#' Create new columns in rlong to specify Estimate Type and Strategy separately
# Find the "_" character and use the first 3 digits before it as the Strategy name. Note see stringi-search-regex {stringi} for list of ICU Regex Operators that can be used to search.
exp.pop.long$Strategy <- "NA"
exp.pop.long$Strategy[which(str_detect(exp.pop.long$Estimate, "(?<=_)[^A:Z]")==1)] <-  paste0(substr(exp.pop.long$Estimate[which(str_detect(exp.pop.long$Estimate, "(?<=_)[^A:Z]")==1)], 1,3)) #searches for _ and pattern of at least one capital letter at the beginning of the sequence preceding _; if that is true, then it pastes the 1st three letters of that sequence, which in this csv matches the strategy #, e.g. S01 = strategy 1.

#exp.pop.long$Strategy<- factor(exp.pop.long$Strategy, levels=c("BSL", "S01", "S02", "S03", "S04", "S05", "S06", "S07", "S08", "S09", "S10", "S11", "S12", "S13", "ALL"))

exp.pop.long$Strategy<- factor(exp.pop.long$Strategy, levels=c("BSL", "S01", "S02", "S03", "S04", "S05", "S06", "S07", "S08", "S09", "S10", "S11", "S12", "S13", "ALL", "S15"))


# Create a new column for type of estimate
exp.pop.long$Est.Type <- "NA" # creates a new column in the table #' 
exp.pop.long$Est.Type[grep("best_guess", exp.pop.long$Estimate)] <- "Best.Guess"
exp.pop.long$Est.Type[grep("lower", exp.pop.long$Estimate)] <- "Lower"
exp.pop.long$Est.Type[grep("upper", exp.pop.long$Estimate)] <- "Upper"
exp.pop.long$Est.Type<- factor(exp.pop.long$Est.Type, levels = c("Lower", "Best.Guess", "Upper"))

plot.data <- select(exp.pop.long, -Estimate) %>%
  spread(., Est.Type, Value)

strat.levels <- levels(plot.data$Strategy)


#write_csv(plot.data, "data/Aggregated_Performance_*tidy.csv")
write_csv(plot.data, "data/ExpPerformance_*tidy.csv")

base.data <- plot.data[which(plot.data$Strategy=="BSL"),]
plot.data.nobase <- plot.data[which(plot.data$Strategy!="BSL"),]

#' Plot mean estimates by strategy for all groups
temp.plot2 <- 
  ggplot(plot.data.nobase, aes(x = Strategy, y = Best.Guess)) +
  geom_hline(aes(yintercept = Best.Guess), base.data, colour = "grey") +
  geom_hline(aes(yintercept = Lower), base.data, colour = "grey", lty = "dashed") +
  geom_hline(aes(yintercept = Upper), base.data, colour = "grey", lty = "dashed") +
  #geom_hline(aes(yintercept = 50), colour = "red", lty = "longdash") +
  geom_hline(aes(yintercept = 50), colour = "green") +
  geom_hline(aes(yintercept = 60), colour = "blue") +
  geom_hline(aes(yintercept = 70), colour = "pink") +
  geom_pointrange(aes(ymin = Lower, ymax = Upper)) +
  theme_cowplot() +  # minimalist theme from cowplot package
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"), # top, right, bottom and left margins around the plot area
        panel.spacing = unit(0.5, "lines"), # adjust margins and between panels of the plot (spacing of 1)
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)), # adjust space between y-axis numbers and y-axis label
        axis.text.x = element_text(size = 10, angle = 60, hjust = 0.5, vjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 0)
  ) +
  facet_wrap( ~ Conservation_Unit, nrow = 5, ncol = 4) +  # create a separate panel for each CU
  theme(strip.text.x = element_text(size = 8)) + 
  scale_x_discrete(breaks = strat.levels, labels = strat.levels ) +
  labs(x = "Strategies",
       y = "Probability of Green Status (%)"
        ,
      # caption = str_wrap("Figure 1. Estimated probability of each Conservation Unit achieving Green Status under the Baseline scenario (blue lines), each of the management strategies (1 - 11), the combined strategies (12, 13, ALL), and the Future Baseline scenario (15). Values are based on expert best guess and lower and upper estimates, averaged over number of experts who provided estimates for the strategy and Conservation Unit. The red dashed line indicates a 50-50 chance of achieving Green Status.", width = 140)
 # caption = str_wrap("Figure 1. Expected probability of each Conservation Unit achieving Green Status under the Baseline scenario (grey lines), each of the management strategies (1 - 11), the combined strategies (12, 13, ALL), and the Future Baseline scenario (15). Values are based on expert best guess and lower and upper estimates, averaged over number of experts who provided estimates for the strategy and Conservation Unit, and multiplied by the feasibility of each strategy being successful. The green line indicates a 50-50 chance, blue indicates 60% and pink line indicates 70% chance of achieving Green Status. ", width = 155)
 caption = str_wrap("Figure 2. Expected probability of each Conservation Unit achieving Green Status when Enabling Strategy 2 Governance Solutions is implemented. Each of the management strategies (1 - 11), the combined strategies (12, 13, ALL), and the Future Baseline scenario (15) are shown as points and ranges. Values are based on expert best guess and lower and upper estimates, averaged over number of experts who provided estimates for the strategy and Conservation Unit, and multiplied by the feasibility of each strategy being successful. The grey lines show the business as usual estimates lower lower, best guess, and upper estimates given a baseline of no new management; the green line indicates a 50-50 chance, blue indicates 60% and pink line indicates 70% chance of achieving Green Status. ", width = 155)
) +
  ylim(0, 100) 

print(temp.plot2)

#' Save plot as pdf file

#ggsave(filename = "figures/Aggregated_Performance.pdf", temp.plot2, width = 11, height = 8.5, units = "in")
ggsave(filename = "figures/Expected_Performance.pdf", temp.plot2, width = 11, height = 8.5, units = "in")

#V2 with cogovernance
ggsave(filename = "figures/Expected_Performance_CoGov.pdf", temp.plot2, width = 11, height = 8.5, units = "in")

