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


pA_V0 = c(0.973,    # proportion of asymptomatic cases when not vaccinated by age groups
          0.973,
          0.973,
          0.973,
          0.966,
          0.966,
          0.966,
          0.966,
          0.954,
          0.954,
          0.954,
          0.954,
          0.947,
          0.947,
          0.947,
          0.903)
pMH_V0 <- c(0.0796,    # probability of M to H by age group when not vaccinated
            0.05703373,
            0.02223410,
            0.02208000,
            0.03575407,
            0.03575407,
            0.04586194,
            0.04586194,
            0.03216806,
            0.03216806,
            0.07004853,
            0.07004853,
            0.07910367,
            0.07910367,
            0.19361578,
            0.25136083)
pMC_V0 <- c(0.0004,   # probability of M to C by age group when not vaccinated
            0.0009332687,
            0.0007659000,
            0.0009200000,
            0.0020369349,
            0.0020369349,
            0.0049940592,
            0.0049940592,
            0.0055259404,
            0.0055259404,
            0.0169574694,
            0.0169574694,
            0.0333713325,
            0.0333713325,
            0.1803042240,
            0.2340791680)
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






