#' ---
#' title: "Aggregate Standardized Benefit Estimates"
#' author: "Adapted for the Lower Fraser Salmon PTM by Lia Chalifour"
#' date: "18 Feb 2020"
#' output: github_document
#' ---
#' 
#' #'Based on *aggregateEstimates.R* by Abbey Camaclang in Conservation Decisions Lab Github "PTM_analysis"
#' This code  
#' a) calculates benefits of each strategy (strategy performance - baseline performance) for each Conservation Unit,  
#' b) aggregates (averages) across experts, and  
#' c) calculates expected performance under each strategy based on the aggregated benefit estimate.  
#'   
#' Based on first part of Step 2 section of 1_Cost-Effectiveness.R code from FRE PTM project.  
#'   
#' If some of the expert estimates need to be weighted differently, must provide a table listing the species in each 
#' ecological group *EcolGroupsList.csv* and a table *SpecialCases.csv* indicating which expert estimates for which 
#' ecological groups and strategies require different weights, and the number of species scored for that estimate. Note here I have deleted code related to weighting as we did not do this for the Lower Fraser Salmon PTM.
#' 
#+ warning = FALSE, message = FALSE
library(tidyverse)

#' Specify how estimates should be aggregated
# (1) if weighting each expert estimate based on the number of species in each group that they scored,  
# (0) if assuming all species in the group were considered in the estimate. In the context of the FR Salmon PTM, all Conservation Units were considered and estimated individually, so no grouping or weighting is needed. 
wt.by.numspp <- 0

#' Read in and prepare data
#+ warning = FALSE, message = FALSE
results <- read.csv("data/benefits_all_experts_LFR.csv")

# head(results)
# randomly generate letter code for each expert. 
set.seed(23)
Expert<-unique(stringi::stri_rand_strings(26, 2))
expert<- seq(1,26)
ex_code<- data.frame(expert,Expert)

#add code to results
results<- merge(ex_code, results, by="expert")

#remove outlier expert from results
results<- results[results$expert!="5",]

#' Calculate benefit: subtract baseline performance from strategy performance for each expert
base.mat <- results[4:6]
strat.mat <- results[7:ncol(results)]

ben.mat <- strat.mat - as.matrix(base.mat)
FB<- ben.mat[43:ncol(ben.mat)]
noFB<- ben.mat[1:42]
noFB[noFB < 0] <- 0 
ben.mat<- cbind(noFB, FB) #rejoin standardized benefit estimates with Future Baseline estimates

#add Expert code & CU identifiers
ben.mat <- cbind("Conservation_Unit"=results[,3], ben.mat)
ben.mat <- cbind("Expert"=results[,2], ben.mat) 

base.mat <- cbind("Conservation_Unit"=results[,3], base.mat)
base.mat <- cbind("Expert"=results[,2], base.mat)


#' Aggregate benefit estimates: average benefit estimates for each species group + strategy across experts
#+ warning = FALSE, message = FALSE
if (wt.by.numspp == 0) {
      
      # Calculate the simple average
      ben.mat.agg <- aggregate(ben.mat[,3:ncol(ben.mat)], by=list(ben.mat$Conservation_Unit), FUN = mean, na.rm = TRUE) 
      base.mat.agg <- aggregate(base.mat[,3:ncol(base.mat)], by=list(base.mat$Conservation_Unit), FUN = mean, na.rm = TRUE)
      
      names(ben.mat.agg)[1] <- "Conservation_Unit"
      names(base.mat.agg)[1] <- "Conservation_Unit"
}

#' Calculate averaged performance: add averaged benefit estimates to the (averaged) baseline
exp.pop <- ben.mat.agg[,2:ncol(ben.mat.agg)] + as.matrix(base.mat.agg[,2:ncol(base.mat.agg)])
exp.pop <- cbind(base.mat.agg, exp.pop)

# print(exp.pop)

#' Output results
write_csv(ben.mat.agg, "data/Aggregated_Benefits_*.csv")
write_csv(base.mat.agg, "data/Aggregated_Baseline_*.csv")
write_csv(exp.pop, "data/Aggregated_Performance_*.csv")
