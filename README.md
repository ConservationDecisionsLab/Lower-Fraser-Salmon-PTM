# Lower-Fraser-Salmon-PTM
Code and data accompanying the manuscript: "Identifying a pathway toward recovery for depleted wild Pacific salmon populations in a large watershed under multiple stressors."

Authors: Lia Chalifour, Cassandra Holt, Abbey E. Camaclang, Mike Bradford, Ross Dixon, Riley J.R. Finn, Victoria Hemming, Scott G. Hinch, Colin D. Levings, Misty MacDuffee, Derek Nishimura, Michael Pearson, John D. Reynolds, David C. Scott, Uwe Spremberg, Steven Stark, John Stevens, Julia K. Baum, Tara G. Martin

Submitted to the Journal of Applied Ecology

# PTM_analysis
Adapted from https://github.com/ConservationDecisionsLab/PTM_analysis by Abbey Camaclang.

Collection of R scripts for standardizing and aggregating expert estimates and calculating cost-effectiveness. Starting data files include:

* benefits_ all_experts _LFR.csv
* FRCostFeas.csv
* COMBO_info2.csv

Also included is the BG_ Solution_table.xlsx with manual validation of the complementarity analysis.

For additional example datasets and original code see https://github.com/ConservationDecisionsLab/PTM_analysis

Data provided to demonstrate use of code and validation of methods. Do not share or reproduce any data without permission from the lead or final authors (Lia Chalifour - lia.chalifour@gmail.com and/or Tara Martin - tara.martin@ubc.ca).  

### standardizeBenefits.R 

* This script standardizes the benefit estimates (benefits_ all_experts _LFR.csv) and saves results tables as .csv files:  
* 1) **Results_tidy.csv** - Tidy (long) version of the original **benefits_ all_experts _LFR.csv** file  
* 2) **Estimates_ per_group.csv** - Number of expert estimates for each salmon Conservation Unit  
* 3) **Estimates_ by_strategy.csv** - Number of expert estimates there are for each strategy x CU  
  
### createBoxplots.R 
* uses Results_tidy.csv to create plots for expert review & feedback. This script creates two plots for each CU Group:
  + boxplots of the best guess, lower, and upper estimates for each Strategy from all Experts;
  + pointrange plots showing the best guess, lower and upper estimates of each Expert for each Strategy.

### aggregateEstimates.R 
* uses benefits_ all_experts _LFR.csv to
  + calculate the average performance (probability of persistence) under the Baseline scenario (Aggregated_Baseline.csv)
  + calculate benefits of each strategy (strategy performance - baseline performance) for each Conservation Unit (Aggregated_Benefits.csv) 
  + calculate expected performance under each strategy based on the aggregated benefit estimate = Aggregated_ Benefits + Aggregated_ Baseline (Aggregated_Performance.csv).
  
### getBenefitMatrix.R
* uses Aggregated_ Baseline.csv and Aggregated_ Benefits.csv, and a table of strategy Cost and Feasibility (FRcostfeas.csv) to
  + calculate the expected benefit of each strategy for each CU = _Benefit * Feasibility_ (Expected_Performance.csv)
  + create a Benefit matrix for use in the optimization using 'best guess' estimates from the Expected_Performance table (Benefits.csv)
  + create versions of the above as well as long format benefit matrix with changes to feasibility due to implementation of an Indigenous led co-governance framework for plotting in threshold.R (Benefits _CoGov* .csv and Benefits _long*.csv)

### plotMeanPerformance.R
* can use Aggregated_Performance.csv or Expected_Performance.csv (weighted by feasibility) to 
  + create pointrange plots of (unweighted or weighted) standardized mean estimates of probability of persistence (y-axis) for each strategy (x-axis) and for each CU (subplots)

### calculateCEscore.R
* uses Aggregated_Benefits.csv and a table of strategy Cost and Feasibility (FRCostFeas.csv) to calculate a cost-effectiveness (CE) score  
_CE = (Benefit*Feasibility)/Cost_ and rank strategies by Benefit, Cost, and CE. Results are saved as Cost_Effectiveness.csv
* conducts uncertainty analysis for cost estimates
 
### Sens_Analysis.R
* conducts uncertainty analysis for benefit estimates

### Sens_Analysis_Feasibility.R
* conducts uncertainty analysis for feasibility estimates

### Cons_optimization.R
This code runs the consOpt package, developed by Nicolai Cryer and Laura Kehoe, to perform optimization of management strategies based on their cost, benefit*feasibility, and complementarity. It generates a stepwise plot showing how many species / Conservation Units can be conserved for a given budget. 

Requires **Benefits.csv** from *getBenefitMatrix.R*, and a costs vector from **CostFeas** table of strategy costs and feasibilities. Also requires COMBO_info.csv, which can be adapted from the sample data in consOPT, and is a csv listing strategy #s that are included in each strategy, so that the combined strategies are accounted for directly during optimization.

* conducts preliminary complementarity analysis
* creates primary results figures for ms (Figure 4) and SI

### threshold.R 

Requires **Benefits _CoGov.csv** and **Benefits _ long*.csv** from *getBenefitMatrix.R*.

* creates Figure 5 for the ms, showing the relative predicted performance (and how many surpass the conservation threshold) for each CU under baseline, all management strategies, and with and Indigenous led co-governance framework. 

### UAplots.R 

* creates Figures S5 and S6 for the ms supplement by combining the uncertainty analysis plots generated for each of cost-effectiveness (*calculateCEscore.R*), benefits (*Sens_Analysis.R*), and feasibility (*Sens_Analysis_Feasibility.R*) estimates. 