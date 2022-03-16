#' ---
#' title: "Calculate Cost-effectiveness"
#' author: "Adapted for the SJR PTM by Abbey Camaclang; further adapted for LFR Salmon PTM by Lia Chalifour"
#' date: "3 July 2019; 1 June 2020"
#' output: github_document
#' ---

#' This code calculates cost-effectiveness scores and ranks strategies by Benefit, Cost, and CE
#' Based on algorithm from Step 2 section of *1_Cost-Effectiveness.R* code from FRE PTM project, but using a different way to implement.  
#' 
#' Also performs uncertainty analysis for costs.  
#' 
#' Requires **Aggregated_Benefits.csv** from *aggregateEstimates.R*, and a **CostFeas.csv** table of strategy cost and feasibility.
#' 
#' Load packages
#+ warning = FALSE, message = FALSE
library(tidyverse)
library(mc2d)
library(cowplot)
library(ggridges)
library(here)

#' Set parameters
a <- 1000000 # scaling to get cost and benefits in same order of magnitude
uncrtn.anal <- 1 # 1 if running cost uncertainty analysis, 0 if not (to save time)
MC <-  10000 # number of iterations for uncertainty analysis
u <- 0.3 # prop. variation from the best estimate

#' Read in benefit values from *aggregateEstimates.R* and cost/feasibility table
#' Note that these are benefits (i.e. % probability with strategy x - % probability at baseline), not performance 
#+ warning = FALSE, message = FALSE
ben.mat.agg <- read_csv("data/Aggregated_Benefits_*.csv")
ben.mat.agg <- ben.mat.agg[, -c(44:46)] # Remove benefit estimates for Future Baseline Scenario

costfeas <- read_csv("data/FRCostFeas.csv")
#costfeas<- read_csv("data/FRCostFeas_WWtreat_removed.csv")
costfeas <- costfeas[costfeas$Strategy != "BSL",]  # Remove baseline values
costfeas$Strategy <- as_factor(as.character(costfeas$Strategy))

#' ## Calculate cost-effectiveness scores
#' CE = (Benefit * Feasibility)/Cost
# Get Best Guess estimates and transpose so that Strategies are in rows and Groups are in columns
strat.ben <- data.frame(t(select(ben.mat.agg, contains("best_guess"))))
names(strat.ben) <- ben.mat.agg$Conservation_Unit

# Create vector of strategy names
strat.names<- vector()
strat.names[which(str_detect(rownames(strat.ben), "(?<=_)[^A:Z]")==1)] <- 
  paste0(substr(rownames(strat.ben)[which(str_detect(rownames(strat.ben), "(?<=_)[^A:Z]")==1)], 1,3)) #searches for _ and pattern of at least one capital letter at the beginning of the sequence preceding _; if that is true, then it pastes the 1st three letters of that sequence, which in this csv matches the strategy #, e.g. S01 = strategy 1.

# Add up aggregated benefits of each strategy across Conservation Units
sum.ben <- data.frame(strat.names, rowSums(strat.ben))
names(sum.ben) <- c("Strategy", "Benefit")
sum.ben$Strategy <- as_factor(as.character(sum.ben$Strategy))

# Join with cost/feasibility table and calculate cost-effectiveness
strat.est <- full_join(sum.ben, costfeas, by="Strategy") %>%
  mutate(., Sc.Cost = Total_Cost/a, # scale costs to get reasonable values
         Exp.Benefit = Benefit * Feasibility, # weight benefits by feasibility
         CE = (Benefit * Feasibility)/Sc.Cost) # divide by scaled costs
  
# Rank strategies by Expected Benefit, Cost, and CE score and save results
CE_Score <- select(strat.est, c("Strategy_Name","Strategy", "Benefit", "Total_Cost", "Annual_Cost", "Feasibility", "Exp.Benefit","CE")) %>%
  mutate(., CE_rank = rank(-CE), 
         ExpBenefit_rank = rank(-Exp.Benefit), 
         Cost_rank = rank(Total_Cost))

 print(CE_Score)
  
write_csv(CE_Score, "data/Cost_Effectiveness_*.csv")
#write_csv(CE_Score, "data/Cost_Effectiveness_WWtreat_removed.csv")

#' ## Uncertainty analysis for cost uncertainty

if (uncrtn.anal == 1) {
  
  samples <- matrix(nrow = nrow(costfeas),ncol = MC)
  MC.CE_Score <- list()

  # get min and max (+/- 30%)
  min.Cost <- costfeas$Total_Cost * (1-u)
  max.Cost <- costfeas$Total_Cost * (1+u)
  best.Cost <- costfeas$Total_Cost
  
  for (it in 1:MC) {
    
    rnd <- runif(1,1,999999)
    set.seed(rnd)
    # sample cost values using rpert from mc2d
    samples[,it] <- rpert(nrow(costfeas),
                          min = min.Cost,
                          mode =best.Cost,
                          max = max.Cost, shape=4)
    costfeas$Total_Cost <- samples[,it]

    # Join with cost/feasibility table and calculate cost-effectiveness
    strat.est <- full_join(sum.ben, costfeas, by="Strategy") %>%
      mutate(., Sc.Cost = Total_Cost/a, # scale costs to get reasonable values
             Exp.Benefit = Benefit * Feasibility, # weight B by F
             CE = (Benefit * Feasibility)/Sc.Cost) # calculate cost-effectiveness scores

    # Rank strategies by (weighted)Benefit, Cost, and CE score
    CE_Score <- select(strat.est, c("Strategy", "Benefit", "Total_Cost", "Annual_Cost", "Feasibility", "Exp.Benefit","CE")) %>%
      mutate(., CE_rank = rank(-CE),
             ExpBenefit_rank = rank(-Exp.Benefit),
             Cost_rank = rank(Total_Cost))
    
    MC.CE_Score[[it]] <- CE_Score
    
    }
  
  # Get results and save as .csv files
  MC.CE_Table <- lapply(MC.CE_Score, "[", 1:length(strat.names), "CE")
  MC.Results <- matrix(unlist(MC.CE_Table), ncol = MC, byrow = FALSE)
  MC.Results <- data.frame(costfeas$Strategy, MC.Results)
  names(MC.Results)[1] <- "Strategy"
  
  MC.CE_Rank <- lapply(MC.CE_Score, "[", 1:length(strat.names), "CE_rank")
  MC.Ranks <- matrix(unlist(MC.CE_Rank), ncol = MC, byrow = FALSE)
  MC.Ranks <- data.frame(costfeas$Strategy, MC.Ranks)
  names(MC.Ranks)[1] <- "Strategy"

  write_csv(MC.Results, "data/MC_CEScores_cost_*.csv")
  
  MC_Samples <- data.frame(costfeas$Strategy, samples)
  write_csv(MC_Samples, "data/MC_PerfSamples_cost_*.csv")

  # Boxplot of CE scores across all MC iterations
  MC.CE <- gather(MC.Results, key = MC.Iter, value = CE, 2:ncol(MC.Results))
  MC.CE$Strategy <- as_factor(MC.CE$Strategy)

  cost.plot <-
    ggplot(MC.CE, aes(x = Strategy # Strategy on x-axis
                      , y = CE # and CE range on y-axis
                      )
           ) +
    geom_boxplot(
      lwd = 0.3 #lwd changes line width
      , fatten = 1 # thickness of median line; default is 2
      , outlier.size = 1 # size of outlier point
      ) + # tell R to display data in boxplot form
    theme_cowplot() +  # use the theme "cowplot" for the plots, which is a nice minimalist theme
    theme(
      axis.text = element_text(size = 10)
      , axis.line = element_line(size = 1)
      ) +
    scale_x_discrete(name = "Management strategies"
                     , breaks = MC.CE$Strategy
                     , labels = MC.CE$Strategy# Give the x-axis variables labels
                     ) +
    labs(x = "Management strategies"
         , y = "Cost-effectiveness score"
         ) +
    ylim(0, 25) # set the y-axis limits to fit scale of CE values
  
  ggsave(filename=paste0("Uncrtn_Cost_", MC, "R_Scores_*.pdf", sep = ""), cost.plot, width = 180, height = 115, units = "mm", path = "figures/")

  
  # Histogram of CE rank frequency
  # need to generalize number of bins/strategies
  MC.CE_r <- gather(MC.Ranks, key = MC.Iter, value = CE_rank, 2:ncol(MC.Ranks))
  MC.CE_r$Strategy <- as_factor(MC.CE_r$Strategy)
  
  count_ranks <- xyTable(MC.CE_r$Strategy, MC.CE_r$CE_rank)
  rank_table <- data.frame(count_ranks$x, count_ranks$y, count_ranks$number)
  rank_table_sort <- as_tibble(rank_table)
  names(rank_table_sort) <- c("Strategy", "CE_rank", "Count")
  # rank_table_sort <- group_by(rank_table_sort, Strategy) %>% # version used in SJR PTM; for each Strategy, finds the most frequent CE_rank
    # filter(Count == max(Count)) %>%
    # arrange(desc(CE_rank), Count)
  rank_table_sort <- group_by(rank_table_sort, CE_rank) %>% # for each CE_rank, finds the most frequent Strategy
    filter(Count == max(Count)) %>%
    arrange(desc(CE_rank), Count)
  
  strat.order <- rank_table_sort$Strategy
  new.names <- sprintf("S%02d", strat.order) #use sprintf to add leading 0 for single digit names, to match other df names (i.e. S08 vs S8). S = "S", %02 = 2-digit width with leading 0 when fewer, d = integer.
  new.names[2]<- "ALL" #re-name S14 as ALL to match previous df names
  
  cost.plot.r <-
    ggplot(MC.CE_r, aes(y = factor(Strategy, levels = new.names)
                        , x = CE_rank
                        , fill = factor(Strategy, levels = new.names)
                        )
           ) +
    geom_density_ridges(stat = "binline", bins = length(new.names), scale = 0.9, draw_baseline = FALSE) +
    theme_ridges(grid = TRUE, center_axis_labels = TRUE) +
    theme(
      legend.position = "none"
      , panel.spacing = unit(0.1, "lines")
      , strip.text = element_text(size = 8)
      , axis.ticks = element_blank()
      , axis.line = element_line(size = 0.3)
      , panel.grid = element_line(size = 0.3)
      ) +
    labs(x = "Cost-effectiveness rank"
         , y = "Management strategies"
         ) +
    scale_x_continuous(breaks = c(1:length(new.names))
                       , labels = c(1:length(new.names)) # Give the x-axis variables labels
                       # , limits = c(0, max(rank_table$count_ranks.x)+1)
                       )
  
  ggsave(filename=paste0("Uncrtn_Cost_", MC, "R_Ranks_*.pdf", sep = ""), cost.plot.r, width = 180, height = 180, units = "mm", path = "figures/")

 
  }

print(cost.plot)
print(cost.plot.r)
