#=============== time ================
t = 0          # start time
dt = 1/6       # time step for simulation
tmax = 365     # Simulation time

#==============Age groups================
AgeGroup <- c("[0,5)", "[5,10)", "[10,15)", "[15,20)", "[20,25)", "[25,30)", "[30,35)", "[35,40)", "[40,45)", "[45,50)", "[50,55)",
              "[55,60)", "[60,65)", "[65,70)", "[70,75)", "[75,Inf)") 

surv.rate.AP <- 0.015  # percent of asymptomatic/presymptomatic cases being detected

drug.eff <- 0.7   # reduce hospitalizations and deaths by 0.7
mask.eff <- 0.75  # the R0 can be reduced by 25% when 100% population self reported wearing mask; https://www.pnas.org/doi/10.1073/pnas.2119266119#supplementary-materials

pA_V0 = c(0.9284,   # proportion of asymptomatic cases when not vaccinated by age groups
          0.9284,
          0.9284,
          0.92156,
          0.9113,
          0.9113,
          0.9113,
          0.9113,
          0.8791,
          0.8791,
          0.8791,
          0.8791,
          0.8411,
          0.8411,
          0.8411,
          0.8411) 
pMH_V0 <- c(0.0796,    # probability of M to H by age group when not vaccinated
            0.06267443,
            0.09667,
            0.096,
            0.07010601,
            0.07010601,
            0.08819604,
            0.08819604,
            0.06844268,
            0.06844268,
            0.13735006,
            0.13735006,
            0.14382485,
            0.14382485,
            0.3396768,
            0.3396768)
pMC_V0 <- c(0.0004,   # probability of M to C by age group when not vaccinated
            0.00102557,
            0.00333,
            0.004,
            0.00399399,
            0.00399399,
            0.00960396,
            0.00960396,
            0.01175732,
            0.01175732,
            0.03324994,
            0.03324994,
            0.06067515,
            0.06067515,
            0.3163232,
            0.3163232)
pHD_V0 <- c(0.004,   # probability of H to D by age group when not vaccinated
            0.0052,
            0.0075,
            0.0075,
            0.0096,
            0.0096,
            0.008,
            0.008,
            0.0323,
            0.0323,
            0.0575,
            0.0575,
            0.0868,
            0.0868,
            0.2498,
            0.2498)
pCD_V0 <- c(0.034,   # probability of C to D by age group when not vaccinated
            0.05,
            0.0674,
            0.1,
            0.1248,
            0.1248,
            0.1045,
            0.1045,
            0.2066,
            0.2066,
            0.3332,
            0.3332,
            0.3645,
            0.3645,
            0.3747,
            0.3747
)






