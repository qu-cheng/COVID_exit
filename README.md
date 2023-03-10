## Code
* **Figures.R** contains code to regenerate the figures
* **Model_Parameters.R** contains code for specifying parameters of the model
* **Model_EstimateBeta.R** contains code for estimating beta according to the specified R0
* **Model_SampleNext.R** contains code for sampling the next state and time to the next state
* **Model_Main.R** contains code for the individual-based model
* * **Simulation_best_worst_scenarios_with_sens.R** contains code for running the simulations under the best- and worst-case intervention scenarios for China as an example. Simulations for other places can be simply adapted from this file by changing the input file for the parameter sets, age structure and vaccine coverage. Results generated by this file were used to make Figure 2 in the manuscript.
* **Simulation_Random100.R** contains example code for running the simulations for China with the 100 sets of intervention parameters randomly sampled by the Latin hypercube sampling method from the full parameter space. Simulations for other places and for the reduced parameter space can be simply adapted from this file by changing the input file for the parameter sets, age structure and vaccine coverage
* **Save_model_for_shiny.R** contains code for saving the GP models fitted in the reduced intervention parameter space

## Data
* **ContactMat_ZJJ.csv** contains the age-mixing pattern from [a survey](https://www.nature.com/articles/s41598-019-51609-8) conducted in Shanghai in 2017
* **Age_structure.csv** contains the proportion of population in each age group for each location. See more details from the caption of Figure S1
* **China_VacCoverage.csv**, **Shanghai_VacCoverage.csv**, **Shenzhen_VacCoverage.csv**, and **Shiyan_VacCoverage.csv** contain the vaccine coverage by age and dose for these four locations
* **Vaccine_time_distribution.csv** contains the probability distribution of the timing of the latest shot by age group and dose from [Cai et al.](https://www.nature.com/articles/s41591-022-01855-7#Sec14)
* **LHS_sample_for_local_sen_original.csv** contains 100 sets of intervention parameters sampled randomly by the Latin hypercube method from the full parameter space, while **LHS_sample_for_local_sen_China.csv**, **LHS_sample_for_local_sen_SH.csv**, **LHS_sample_for_local_sen_SZ.csv**, and **LHS_sample_for_local_sen_SY.csv** contain the samples after adjusting for the number of hospital and ICU beds per capita for each location

## Results
LOCATIONX represents China, SH (Shanghai), SZ (Shenzhen), and SY (Shiyan)
* **LOCATIONX_best_worst_Sens.rda** contains results from **Simulation_best_worst_scenarios_with_sens.R**. They were used to make Figure 2. 
* **Local_Sens_LOCATIONX_baseline.rda** contains results from **Simulation_Random100.R**. They were used to make Figure 3 and S5.
* **Local_Sens_China_baseline_1m.rda**, **Local_Sens_China_baseline_2m.rda** and **Local_Sens_China_baseline_1m.rda** contains the results when the size of synthetic population was set to 1, 2, or 5 million for China under the baseline sceanrio. They were used to make Figure S3.
* **Local_Sens_LOCATIONX_VacOpt.rda** is the results of 100 random samples from the full parameter space for LOCATIONX. They were used to generate Figures 3 and S5. 
* **Local_Sens_LOCATIONX_VacOpt_cut.rda** and **Local_Sens_LOCATIONX_baseline_cut.rda** are the results of the 100 random sample from the reduced parameter space for LOCATIONX. They were used to generate Figures 4 and S7-10.
* **GP_model_cut.rda** contains the fitted GP models for the reduced parameter space.

## Figures
Contains pdf files for the figures in the manuscript
