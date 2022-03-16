#' ---
#' title: "Standardize Benefit Estimates"
#' author: "Adapted for the Fraser Salmon PTM by Lia Chalifour"
#' date: "04 Feb 2020"
#' output: github_document
#' ---

#'  Modified from original script 'standardizeConfidence.R' by Abbey Camaclang: https://github.com/ConservationDecisionsLab/PTM_analysis  
#'  This script standardizes the benefit estimates and saves results tables as .csv files:  
#' 1) **Results_tidy.csv** - Tidy (long) version of the original **benefits_all_experts_LFR.csv** file  
#' 2) **Estimates_per_group.csv** - Number of expert estimates for each salmon Conservation Unit  
#' 3) **Estimates_by_strategy.csv** - Number of expert estimates there are for each strategy x CU  
#' 
#'
#' Load packages
#+ warning = FALSE, message = FALSE
library(tidyverse)
library(stringr)
library(here)  

#' ## Read in and tidy data
#+ warning = FALSE, message = FALSE
results <- read.csv("data/benefits_all_experts_LFR.csv")
# head(results)
# randomly generate letter code for each expert.
set.seed(23)
code<-unique(stringi::stri_rand_strings(26, 2))
expert<- seq(1,26)
ex_code<- data.frame(expert,code)
write.csv(ex_code, "data/ex_code.csv", row.names = FALSE)

#add code to results
results<- merge(ex_code, results, by="expert")

#remove outlier from results
results<- results[results$expert!="5",]

#' Use tidyr package to transform data to tidy version, with single columns for Estimate (e.g., best guess, lower, upper) and Value (value of estimate, 0-100)
rlong <-
  gather(results,
         key = Estimate,
         value = Value,
         BSL_lower:S15_Fut_Bas_best_guess) #' <!-- AC: update with the data column names -->
head(rlong)

rlong <- na.omit(rlong)
rlong$Value <- as.numeric((rlong$Value))
# str(rlong) # Check data type


#' ## Summarize the number of expert estimates
#' Tabulate how many expert estimates there are for each ecological group
exp.table <- table(results$CU_group)
#write.csv(exp.table, "data/Estimates_per_group.csv", row.names=FALSE)
write.csv(exp.table, "data/Estimates_per_group_*.csv", row.names=FALSE) #_* label denotes files with outlier expert estimates removed
exp.table
  
#' Create new columns in rlong to specify Estimate Type and Strategy separately
# Find the "_" character and use the first 3 digits before it as the Strategy name. Note see stringi-search-regex {stringi} for list of ICU Regex Operators that can be used to search.
rlong$Strategy <- "NA"
rlong$Strategy[which(str_detect(rlong$Estimate, "(?<=_)[^A:Z]")==1)] <-  paste0(substr(rlong$Estimate[which(str_detect(rlong$Estimate, "(?<=_)[^A:Z]")==1)], 1,3)) #searches for _ and pattern of at least one capital letter at the beginning of the sequence preceding _; if that is true, then it pastes the 1st three letters of that sequence, which in this csv matches the strategy #, e.g. S01 = strategy 1.

# Create a new column for type of estimate
rlong$Est.Type <- "NA" # creates a new column in the table #' 
rlong$Est.Type[grep("best_guess", rlong$Estimate)] <- "Best.Guess"
rlong$Est.Type[grep("lower", rlong$Estimate)] <- "Lower"
rlong$Est.Type[grep("upper", rlong$Estimate)] <- "Upper"
  
#write_csv(rlong, "data/Results_tidy.csv") #original was used for figures in 'benefits01' folder following a Delphi approach; outlier expert declined to adjust estimates following review of group estimates and discussion
write_csv(rlong, "data/Results_tidy_*.csv")

#' Tabulate how many estimates there are for each strategy
table.subset2 <- rlong[(grep("best_guess", rlong$Estimate)),] # Subset to count how many experts provided estimates for each group and strategy
strat.levels <- unique(table.subset2$Strategy)
table.subset2$Strategy <- factor(table.subset2$Strategy,levels = strat.levels) #' <!-- AC: Note that using as.factor() changes the order to alphabetical -->
st.table2 <- table(table.subset2$CU_group, table.subset2$Strategy)
#write.csv(st.table2, "data/Estimates_by_strategy.csv")
write.csv(st.table2, "data/Estimates_by_strategy_*.csv")
st.table2


