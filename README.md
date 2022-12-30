## Code
* **Figures.R** contains code to regenerate the figures
* **Model_Parameters.R** contains code for specifying parameters of the model
* **Model_EstimateBeta.R** contains code for estimating beta according to the specified R0
* **Model_SampleNext.R** contains code for sampling the next state and time to the next state
* **Model_Main.R** contains code for the individual-based model
* **Model_Simulation.R** contains example code for running the simulations for China with the 100 sets of intervention parameters randomly sampled by the Latin hypercube sampling method. Simulations for other places can be simply adapted from this file.

## Data
* **ContactMat_ZJJ.csv** contains the age-mixing pattern from [a survey](https://www.nature.com/articles/s41598-019-51609-8) conducted in Shanghai in 2017
* **Age_structure.csv** contains the proportion of population in each age group for each location. See more details from the caption of Figure S1
* **China_VacCoverage.csv**, **Shanghai_VacCoverage.csv**, **Shenzhen_VacCoverage.csv**, and **Shiyan_VacCoverage.csv** contain the vaccine coverage by age and dose for these four locations
* **Vaccine_time_distribution.csv** contains the probability distribution of the timing of the latest shot by age group and dose from [Cai et al.](https://www.nature.com/articles/s41591-022-01855-7#Sec14)
* **LHS_sample_for_local_sen_original.csv** contains 100 sets of intervention parameters sampled randomly by the Latin hypercube method from the full parameter space, while **LHS_sample_for_local_sen_China.csv**, **LHS_sample_for_local_sen_SH.csv**, **LHS_sample_for_local_sen_SZ.csv**, and **LHS_sample_for_local_sen_SY.csv** contain the samples after adjusting for the number of hospital and ICU beds per capita for each location

## Results
## Figures
