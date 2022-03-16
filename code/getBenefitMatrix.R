#' ---
#' title: "Generate Benefits Matrix for Complementarity Analysis"
#' author: "Adapted for the SJR PTM by Abbey Camaclang; further adapted for LFR Salmon PTM by Lia Chalifour"
#' date: "3 July 2019; 8 July 2020"
#' output: github_document
#' ---
#'
#' This code weights benefits by feasibility, recalculates expected performance based on weighted benefit estimates, 
#' and generates the benefit matrix for use in the complementarity analysis. Based on 1_Cost-Effectiveness.R code 
#' from FRE PTM project, but using a different (shorter) way to implement
#' 
#' Requires **Aggregated_Benefits.csv** and **Aggregated_Baseline.csv** from *aggregateEstimates.R*, and a
#' **CostFeas** table of strategy costs and feasibilities
#' 
#' Note that these benefits estimates are the raw estimates from the FR PTM and have not been standardized to 80% confidence, as no separate confidence estimates were taken due to estimates already being a probability rather than a value such as 'abundance'. 
#'
#' Load package
#+ warning = FALSE, message = FALSE
library(tidyverse)

#' Read in data tables
#+ warning = FALSE, message = FALSE
ben.mat.agg <- read_csv("data/Aggregated_Benefits_*.csv") #version with outlier removed
base.mat.agg <- read_csv("data/Aggregated_Baseline_*.csv")

#Pull out future baseline scenario to add at end.
Futbase<- ben.mat.agg[,c(1,44:46)]
  
#Remove Future Baseline Scenario from benefits matrix to enable merging with costfeas
ben.mat.agg <- ben.mat.agg[,1:43]

costfeas <- read_csv("data/FRCostFeas.csv")
costfeas <- costfeas[-15,] # Remove baseline values
costfeas$Strategy <- as_factor(costfeas$Strategy)

## Second version with cogovernance strategy & ww treatment costs removed
#costfeas <- read_csv("data/FRCostFeas_WWtreat_removed.csv")
#costfeas<- costfeas[-15,]
#costfeas$Strategy <- as_factor(costfeas$Strategy)

#' ## Calculate the expected benefits
# Tidy data
rlong.ben <- gather(ben.mat.agg, key = Estimate, value = Value, 
                    colnames(ben.mat.agg)[2]:colnames(ben.mat.agg)[ncol(ben.mat.agg)]) %>%
  separate(., Estimate, c("Strategy", "Est.Type"), sep = "[_]", remove = FALSE) #note gives warning about discarding additional pieces -- this is longer names that have multiple _, i.e. all the best_guess estimates. Doesn't affect values.
rlong.ben$Strategy <- as_factor(rlong.ben$Strategy)

# Join with cost & feasibility table then weight benefits by feasibility
joined.data <- left_join(rlong.ben, costfeas, by = "Strategy") %>%
  mutate(., Wt.Value = Value * Feasibility)

# VERSION 2 Join with cost & feasibility table then weight benefits by Feas_with_cogov
#joined.data <- left_join(rlong.ben, costfeas, by = "Strategy") %>%
  mutate(., Wt.Value = Value * Feas_with_Cogov)

# Spread table and output results, now with Benefit as weighted value by Feas
joined.wide <- joined.data %>%
  select(., c(Conservation_Unit, Estimate, Wt.Value)) %>%
  spread(key = Estimate, value = Wt.Value)
est.levels <- unique(joined.data$Estimate)
joined.wide <- joined.wide[, c("Conservation_Unit", est.levels)] # rearranges columns so strategies are in the correct order


write_csv(joined.wide, "data/Expected_Benefits_*.csv")
#write_csv(joined.wide, "data/Exp_Ben_*CoGov.csv")


#' ## Calculate expected performance
# Join with baseline estimates to make sure the observations (Conservation Units) line up correctly
# then split again to add weighted benefits to (averaged) baseline and get the expected performance
joinedbase.wide <- left_join(base.mat.agg, joined.wide, by = "Conservation_Unit") 
joinedbase.wide <- left_join(joinedbase.wide, Futbase, by = "Conservation_Unit") 

base.mat <- joinedbase.wide[,2:4]
perf.mat <- joinedbase.wide[,5:ncol(joinedbase.wide)] + as.matrix(base.mat)

perf.mat <- cbind(joinedbase.wide$Conservation_Unit,base.mat,perf.mat)
names(perf.mat)[1] <- "Conservation_Unit"

write_csv(perf.mat, "data/Expected_Performance_*.csv")
#write_csv(perf.mat, "data/Exp_Perf_*CoGov.csv")

#' Create expected performance matrix for complementarity analysis (optimization) and uncertainty analysis
# Best guess (most likely) estimates
wt.ben <- perf.mat %>%
  select(., c(Conservation_Unit, contains("best_guess"))) 
wt.ben.t <- data.frame(t(wt.ben[,-1]))
names(wt.ben.t) <- wt.ben$Conservation_Unit 

# Lower (most pessimistic)
wt.ben.low <- perf.mat %>%
  select(., c(Conservation_Unit, contains("lower"))) 
wt.ben.low.t <- data.frame(t(wt.ben.low[,-1]))
names(wt.ben.low.t) <- wt.ben.low$Conservation_Unit 

# Upper (most optimistic)
wt.ben.hi <- perf.mat %>%
  select(., c(Conservation_Unit, contains("upper"))) 
wt.ben.hi.t <- data.frame(t(wt.ben.hi[,-1]))
names(wt.ben.hi.t) <- wt.ben.hi$Conservation_Unit 

# Create vector of strategy names to add to the tables
strat.names <- vector()
strat.names[which(str_detect(rownames(wt.ben.t), "(?<=_)[^A:Z]")==1)] <- 
  paste0(substr(rownames(wt.ben.t)[which(str_detect(rownames(wt.ben.t), "(?<=_)[^A:Z]")==1)], 1,3)) #searches for _ and pattern of at least one capital letter at the beginning of the sequence preceding _; if that is true, then it pastes the 1st three letters of that sequence, which in this csv matches the strategy #, e.g. S01 = strategy 1.

wt.ben.t <- cbind(strat.names,wt.ben.t)
names(wt.ben.t)[1] <- "Strategy"

wt.ben.low.t <- cbind(strat.names, wt.ben.low.t)
names(wt.ben.low.t)[1] <- "Strategy"

wt.ben.hi.t <- cbind(strat.names, wt.ben.hi.t)
names(wt.ben.hi.t)[1] <- "Strategy"


write_csv(wt.ben.t, "data/Benefits_*.csv") # use this table for the complementarity analysis
write_csv(wt.ben.low.t, "data/Lower_*.csv")
write_csv(wt.ben.hi.t, "data/Upper_*.csv")

#write_csv(wt.ben.t, "data/Benefits_*CoGov.csv") # use this table for the complementarity analysis

##save cogov & benefits only in long format for threshold.R plot
ben.mat.gov<- joinedbase.wide
names(ben.mat.gov)[1]<- "Conservation_Unit"
wt.ben.gov <- ben.mat.gov %>%
  select(., c(Conservation_Unit, contains("best_guess"))) 
wt.ben.gov.t <- data.frame(t(wt.ben.gov[,-1]))
names(wt.ben.gov.t) <- wt.ben.gov$Conservation_Unit
wt.ben.gov.t <- cbind(strat.names,wt.ben.gov.t)
names(wt.ben.gov.t)[1] <- "Strategy"

#write_csv(wt.ben.gov.t, "data/Benefits_CoGov*.csv")

ben.mat2<- joinedbase.wide
names(ben.mat2)[1]<- "Conservation_Unit"
wt.ben2 <- ben.mat2 %>%
  select(., c(Conservation_Unit, contains("best_guess"))) 
wt.ben.2t <- data.frame(t(wt.ben2[,-1]))
names(wt.ben.2t) <- wt.ben2$Conservation_Unit
wt.ben.2t <- cbind(strat.names,wt.ben.2t)
names(wt.ben.2t)[1] <- "Strategy"

write_csv(wt.ben.2t, "data/Benefits_long*.csv")

