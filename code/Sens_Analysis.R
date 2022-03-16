#' ---
#' title: "Uncertainty Analysis (Benefit estimates)"
#' author: "Abbey Camaclang; adapted for LFR Salmon PTM by Lia Chalifour"
#' date: "27 Aug 2019; 8 July 2020"
#' output: github_document
#' ---


#' This code is used to run the uncertainty analysis on experts' benefit estimates.  
#' 
#' Based on algorithm of R code written by Danial Stratford (danial.stratford@csiro.au), found in PTM Handbook Supporting Info.   
#' For each MC iteration,  
#' 1. Sample performance value for each expert - Conservation Unit - strategy using rpert from mc2d package, shape = 4 (default)  
#' 2. Calculate benefit Bi = pi - p0  
#' 3. Average across experts  
#' 4. Sum over all CUs to get total benefit value for each strategy  
#' 5. Calculate cost-effectiveness CEi = (Bi * Fi)/Ci  
#' 
#' Code not copied directly as data frames for this project are structured differently and wanted to avoid additional rearranging.  
#' 
#' Note, since the FR project 1) did not group CUs, but assessed each separately, we do not need to conduct group weighting by the # species in each group -- deleted this section. 2) "We decided not to elicit confidence bounds because the questions were about degrees of belief for a single event Ie. the probability of salmon in being classified as green in 20 years (rather than a frequency estimate). Eliciting the bounds for these types of questions is mostly used for counter factual thinking to improving thinking about the best estimate."-V Hemming. Given this, we have run the sensitivity analysis using the lower, best guess, and upper estimates as a sort of bounds for the truth - i.e. the experts believe that the truth lies somewhere within this range, and so a sampled estimate from within the distribution of that range across experts is unlikely to surprise them. Since this is a single event and not a frequency, the distribution is nonsensical, however it allows us to test the robustness of CE ranks given a "reasonable" shift in the benefit estimate. Another option would be to calculate a 95% CI, however this is similarly somewhat nonsensicle.
#' 
#' 
#' Load packages
#+ message = FALSE, warning = FALSE
library(mc2d)
library(tidyverse)
library(cowplot)
library(ggridges)

MC <-  10000 # number of iterations
a <- 1000000 # scaling for CE scores

#' ## Prepare data for sampling
#+ warning = FALSE, message = FALSE
costfeas <- read_csv("data/FRCostFeas.csv") 
#costfeas <- read_csv("data/FRCostFeas_WWtreat_removed.csv") 
costfeas$Strategy <- as_factor(costfeas$Strategy)
costfeas <- costfeas[-15,] # Remove baseline values

# use read_csv to make sure factors read in as character - note Abbey used Standardized_Estimates_Long.csv, but we are using Results_tidy.csv, as we did not take separate Confidence Measures so didn't need to standardize benefits. 
rlong.std <- read_csv("data/Results_tidy_*.csv")

rlong.std<- subset(rlong.std, Strategy != "S15") #remove Future Baseline Scenario

names(rlong.std)[3]<- "Conservation_Unit"

# Specify factor levels
grp.levels <- unique(rlong.std$Conservation_Unit)

newstrat.levels <- levels(costfeas$Strategy)
rlong.std$Strategy <- factor(rlong.std$Strategy, levels = newstrat.levels)

est.levels <- c("Lower", "Best.Guess", "Upper")
rlong.std$Est.Type <- factor(rlong.std$Est.Type, levels = est.levels)

# Rearrange table so estimates for each group * strategy are on the same row
rlong.sub2 <- rlong.std[,c(1,3,5:7)]
rlong.std.wide <- spread(rlong.sub2, key = Est.Type, value = Value) 
rlong.std.wide$Conservation_Unit <- factor(rlong.std.wide$Conservation_Unit, levels = grp.levels)

#' ## Uncertainty analysis for benefit uncertainty
#+ warning = FALSE, message = FALSE
samples <- matrix(nrow = nrow(rlong.std.wide),ncol = MC)
MC.CE_Score <- list()
Benefits_uncrtn <- list()

for(it in 1:MC){
  rnd <- runif(1,1,999999)
  set.seed(rnd)
  

  # Sample benefit values across the range of estimates given
  samples[,it] <- rpert(nrow(rlong.std.wide),
                        min = rlong.std.wide$Lower,
                        mode = rlong.std.wide$Best.Guess,
                        max = rlong.std.wide$Upper, shape=4)
  
  temp <- cbind(rlong.std.wide[,1:3], samples[,it]) #cols 1:3 are expert, CU, Strategy
  names(temp)[4] <- "MC.Value"
  
  temp.wide <- spread(temp, key = Strategy, value = MC.Value)
  temp.strat <- select(temp.wide, as.character(newstrat.levels[1]):as.character(newstrat.levels[14])) #select strategies 1:ALL, not baseline
  
  temp.ben <- temp.strat - temp.wide$BSL #subtract Baseline
  temp.ben[temp.ben<0] <- 0 # replaces negative values with 0 (assume same as baseline)
  temp.ben <- cbind(temp.wide[,1:2], temp.ben) 
  
  temp.long <- gather(temp.ben, key = Strategy, value = MCValue, -c(1:2))
  temp.long$Strategy <- factor(temp.long$Strategy, levels = newstrat.levels)
  
  # Calculate total benefit of each strategy across Conservation Units
  # First rearrange so strategies are in rows and CUs are in columns - basically need to end up with col vector of total benefit
  sum.ben <- aggregate(temp.long$MCValue, 
                       by = list(Strategy = temp.long$Strategy),
                       FUN = sum, na.rm = TRUE)
  names(sum.ben) <- c("Strategy", "Benefit")
  sum.ben$Strategy <- factor(sum.ben$Strategy, levels = newstrat.levels)
  
  # Join with cost/feasibility table and calculate cost-effectiveness
  strat.est <- left_join(sum.ben, costfeas, by="Strategy") %>%
    mutate(., Sc.Cost = Total_Cost/a, # scale costs to get reasonable values
           Exp.Benefit = Benefit * Feasibility, # weight benefits
           CE = (Benefit * Feasibility)/Sc.Cost) # calculate cost-effectiveness scores
  
  # Rank strategies by (weighted)Benefit, Cost, and CE score
  CE_Score <- select(strat.est, c("Strategy", "Benefit", "Total_Cost", "Annual_Cost", "Feasibility", "Exp.Benefit","CE")) %>%
    mutate(., CE_rank = rank(-CE), 
           ExpBenefit_rank = rank(-Exp.Benefit), 
           Cost_rank = rank(Total_Cost))
  
  MC.CE_Score[[it]] <- CE_Score
  
  # Aggregate baseline - note CUs back in
  temp.base <- temp.wide[,c(1,2,17)]
  temp.base$Strategy <- "BSL"
  temp.base$Strategy <- factor(temp.base$Strategy, levels = newstrat.levels)
  names(temp.base)[3] <- "MCValue"
  
  # Calculate performance and save in a list - edited to match FR data not weighted, not grouped. 
  perf <- temp.ben[,3:ncol(temp.ben)] + as.matrix(temp.base[,3])
  Benefits_uncrtn[[it]] <- perf %>%
    add_column(Conservation_Unit = temp.ben$Conservation_Unit, .before = "S01")
  
}  

#' Get results and save as .csv files
MC.CE_Table <- lapply(MC.CE_Score, "[", 1:length(strat.est$Strategy), "CE")
MC.Results <- matrix(unlist(MC.CE_Table), ncol = MC, byrow = FALSE)
MC.Results <- data.frame(strat.est$Strategy[1:length(strat.est$Strategy)], MC.Results)
names(MC.Results)[1] <- "Strategy"

MC.CE_Rank <- lapply(MC.CE_Score, "[", 1:length(strat.est$Strategy), "CE_rank")
MC.Ranks <- matrix(unlist(MC.CE_Rank), ncol = MC, byrow = FALSE)
MC.Ranks <- data.frame(strat.est$Strategy[1:length(strat.est$Strategy)], MC.Ranks)
names(MC.Ranks)[1] <- "Strategy"

write_csv(MC.Results, "data/MC_CEScores_benefits_constr_*.csv")
#write_csv(MC.Results, "data/MC_CEScores_benefits_constr_*WWR.csv")

MC_Samples <- data.frame(rlong.std.wide[,1:3], samples)

write_csv(MC_Samples, "data/MC_PerfSamples_benefits_constr_*.csv")
#write_csv(MC_Samples, "data/MC_PerfSamples_benefits_constr_*WWR.csv")

#' Box plots of CE scores
MC.CE <- gather(MC.Results, key = MC.Iter, value = CE, 2:ncol(MC.Results))

ben.plot <-
  ggplot(MC.CE, aes(x = Strategy, # for each Strategy, plot Estimate Type on x-axis 
                    y = CE # and St.Value on y-axis, 
                    ) # and colour the boxplots by estimate type
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
                   , breaks = newstrat.levels[1:14] #note 15 is BSL
                   , labels = newstrat.levels[1:14]# Give the x-axis variables labels
                   ) + 
  labs(x = "Management strategies"
       , y = ""
       )+
  ylim(0, 400)

print(ben.plot)

ggsave(filename=paste0("Uncrtn_Benefit_", MC, "R_scores_constr_*.tiff", sep = ""), ben.plot, width = 180, height = 115, units = "mm", dpi = 600, path = "figures/")
#ggsave(filename=paste0("Uncrtn_Benefit_", MC, "R_scores_constr_*WWR.tiff", sep = ""), ben.plot, width = 180, height = 115, units = "mm", dpi = 600, path = "figures/")

#' Histogram of CE ranks
MC.CE_r <- gather(MC.Ranks, key = MC.Iter, value = CE_rank, 2:ncol(MC.Ranks))

count_ranks <- xyTable(MC.CE_r$Strategy, MC.CE_r$CE_rank)
rank_table <- data.frame(levels(MC.CE_r$Strategy)[count_ranks$x], count_ranks$y, count_ranks$number)
rank_table_sort <- as_tibble(rank_table)
names(rank_table_sort) <- c("Strategy", "CE_rank", "Count")
rank_table_sort <- group_by(rank_table_sort, Strategy) %>% # version used in SJR PTM; for each Strategy, finds the most frequent CE_rank
  filter(Count == max(Count)) %>%
  arrange(desc(CE_rank))

strat.order <- rank_table_sort$Strategy
new.names<-paste0(strat.order)

ben.plot.r <- 
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
                     )

print(ben.plot.r)

ggsave(filename=paste0("Uncrtn_Benefit_", MC, "R_Ranks_constr_*.tiff", sep = ""), ben.plot.r, width = 180, height = 180, units = "mm", dpi = 600, path = "figures/")
#ggsave(filename=paste0("Uncrtn_Benefit_", MC, "R_Ranks_constr_WWR*.tiff", sep = ""), ben.plot.r, width = 180, height = 180, units = "mm", dpi = 600, path = "figures/")

###################################################################################
#Alternate method for testing Benefits Uncertainty; not included in SI due to benefit values > 100%

#' Set additional parameters
uncrtn.anal <- 1 # 1 if running cost uncertainty analysis, 0 if not (to save time)
u <- 0.3 # prop. variation from the best estimate

#' Read in benefit values from *aggregateEstimates.R* and cost/feasibility table
#' Note that these are benefits (i.e. % probability with strategy x - % probability at baseline), not performance 
#+ warning = FALSE, message = FALSE

# Select aggregated benefits for Best Guess only
ben.mat.agg <- rlong.std.wide[,c(1:3,5)] #expert, CU, Strategy, Best Guess

#' ## Uncertainty analysis for benefit uncertainty following methods for cost, feasibility

if (uncrtn.anal == 1) {
  
  samples <- matrix(nrow = nrow(ben.mat.agg),ncol = MC)
  MC.CE_Score <- list()
  
  # get min and max (+/- 30%)
  min.Ben <- ben.mat.agg$Best.Guess * (1-u)
  max.Ben <- ben.mat.agg$Best.Guess * (1+u)
  best.Ben <- ben.mat.agg$Best.Guess
  
  for (it in 1:MC) {
    
    rnd <- runif(1,1,999999)
    set.seed(rnd)
    # sample cost values using rpert from mc2d
    samples[,it] <- rpert(nrow(ben.mat.agg),
                          min = min.Ben,
                          mode =best.Ben,
                          max = max.Ben, shape=4)
    ben.ben<- cbind(ben.mat.agg[1:3],samples[,it])
    names(ben.ben)[4] <- "MC.Value"
    
    #error below because CUs repeat
    ben.mat.wide <- spread(ben.ben, key = Strategy, value = MC.Value)
    ben.mat.strat <- select(ben.mat.wide, as.character(newstrat.levels[1]):as.character(newstrat.levels[14])) #select strategies 1:ALL, not baseline
    
    ben.mat <- ben.mat.strat - ben.mat.wide$BSL #subtract Baseline
    ben.mat[ben.mat<0] <- 0 # replaces negative values with 0 (assume same as baseline)
    ben.mat <- cbind(ben.mat.wide[,1:2], ben.mat) 
    
    ben.mat.long <- gather(ben.mat, key = Strategy, value = MCValue, -c(1:2))
    ben.mat.long$Strategy <- factor(ben.mat.long$Strategy, levels = newstrat.levels)
    
    # Calculate total benefit of each strategy across Conservation Units
    # First rearrange so strategies are in rows and CUs are in columns - basically need to end up with col vector of total benefit
    sum.ben <- aggregate(ben.mat.long$MCValue, 
                         by = list(Strategy = ben.mat.long$Strategy),
                         FUN = sum, na.rm = TRUE)
    names(sum.ben) <- c("Strategy", "Benefit")
    
    # Join with cost/feasibility table and calculate cost-effectiveness
    strat.est <- left_join(sum.ben, costfeas, by="Strategy") %>%
      mutate(., Sc.Cost = Total_Cost/a, # scale costs to get reasonable values
             Exp.Benefit = Benefit * Feasibility, # weight benefits
             CE = (Benefit * Feasibility)/Sc.Cost) # calculate cost-effectiveness scores
    
    # Rank strategies by (weighted)Benefit, Cost, and CE score
    CE_Score <- select(strat.est, c("Strategy", "Benefit", "Total_Cost", "Annual_Cost", "Feasibility", "Exp.Benefit","CE")) %>%
      mutate(., CE_rank = rank(-CE),
             ExpBenefit_rank = rank(-Exp.Benefit),
             Cost_rank = rank(Total_Cost))
    
    MC.CE_Score[[it]] <- CE_Score
    
  }
  
  # Get results and save as .csv files
  MC.CE_Table <- lapply(MC.CE_Score, "[", 1:length(strat.est$Strategy), "CE")
  MC.Results <- matrix(unlist(MC.CE_Table), ncol = MC, byrow = FALSE)
  MC.Results <- data.frame(strat.est$Strategy[1:length(strat.est$Strategy)], MC.Results)
  names(MC.Results)[1] <- "Strategy"
  
  MC.CE_Rank <- lapply(MC.CE_Score, "[", 1:length(strat.est$Strategy), "CE_rank")
  MC.Ranks <- matrix(unlist(MC.CE_Rank), ncol = MC, byrow = FALSE)
  MC.Ranks <- data.frame(strat.est$Strategy, MC.Ranks)
  names(MC.Ranks)[1] <- "Strategy"

  write_csv(MC.Results, "data/MC_CEScores_Ben30_*.csv")
  
  #note that this method results in benefit values over 100% -- max 127.85
  MC_Samples <- data.frame(rlong.std.wide[,1:3], samples)
  write_csv(MC_Samples, "data/MC_PerfSamples_Ben30_*.csv")
  
  # Boxplot of CE scores across all MC iterations
  MC.CE <- gather(MC.Results, key = MC.Iter, value = CE, 2:ncol(MC.Results))
  MC.CE$Strategy <- as_factor(MC.CE$Strategy)
  
  ben.plot <-
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
    ) 
  
  ggsave(filename=paste0("Uncrtn_Ben30_", MC, "R_Scores_*.tiff", sep = ""), ben.plot, width = 180, height = 115, units = "mm", dpi = 600, path = "figures/")
  
  
  # Histogram of CE rank frequency
  # need to generalize number of bins/strategies
  MC.CE_r <- gather(MC.Ranks, key = MC.Iter, value = CE_rank, 2:ncol(MC.Ranks))
  MC.CE_r$Strategy <- as_factor(MC.CE_r$Strategy)
  
  count_ranks <- xyTable(MC.CE_r$Strategy, MC.CE_r$CE_rank)
  rank_table <- data.frame(count_ranks$x, count_ranks$y, count_ranks$number)
  rank_table_sort <- as_tibble(rank_table)
  names(rank_table_sort) <- c("Strategy", "CE_rank", "Count")
  rank_table_sort <- group_by(rank_table_sort, CE_rank) %>% # for each CE_rank, finds the most frequent Strategy
    filter(Count == max(Count)) %>%
    arrange(desc(CE_rank), Count)
  
  strat.order <- rank_table_sort$Strategy
  new.names <- sprintf("S%02d", strat.order) #use sprintf to add leading 0 for single digit names, to match other df names (i.e. S08 vs S8). S = "S", %02 = 2-digit width with leading 0 when fewer, d = integer.
  new.names[1]<- "ALL" #re-name S14 as ALL to match previous df names
  
  ben.plot.r <-
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
         , y = ""
    ) +
    scale_x_continuous(breaks = c(1:length(new.names))
                       , labels = c(1:length(new.names)) # Give the x-axis variables labels
                       # , limits = c(0, max(rank_table$count_ranks.x)+1)
    )

  ggsave(filename=paste0("Uncrtn_Ben30_", MC, "R_Ranks_*.tiff", sep = ""), ben.plot.r, width = 180, height = 180, units = "mm", dpi = 600, path = "figures/")
  
}

print(ben.plot)
print(ben.plot.r)

