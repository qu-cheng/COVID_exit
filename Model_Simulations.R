library(tidyverse)
library(data.table)
library(doSNOW)
library(doParallel)
library(truncnorm)

Total.popsize <- 500000   # population size

source("Model_Parameters.R")   # read in parameters
source("Model_SampleNext.R")   # functions for sampling the next state and time to the next state
source("Model_Main.R")         # the function for running the individual-based model
source("Model_EstimateBeta.R") # the function for estimating beta to get the desired R0


par.sample <- read.csv("Data/LHS_sample_for_local_sen_China.csv")  # read in the 100 latin hypercube samples
Age.structure <- read.csv("Data/China_example_age_structure.csv")  # age structure for different locations
vaccine.time <- read.csv("Data/Vaccine_time_distribution.csv")  # read in the time distribution of the vaccination time


nc = parallel::detectCores()
print(nc)
options('mc.cores' = nc)

result.all <- NULL
for(rep.i in 1:100)
{
  # assign paramters from the latin hypercube samples
  drug.coverage <- par.sample$Antiviral[rep.i]
  bed.hospital <- round(par.sample$Hospital[rep.i]*Total.popsize)
  bed.ICU <- round(par.sample$ICU[rep.i]*Total.popsize)
  bed.remain.original <- data.table(hospital = bed.hospital, ICU = bed.ICU)
  mask.coverage <- par.sample$Mask[rep.i]
  
  # read in the vaccine coverage for China
  Vaccine.cov <- read.csv("Data/China_VacCoverage.csv")
  
  # adjust the vaccine coverage rate based on the LHS samples
  # For 5-15, V0 and V1 are available for one more shot; for above 20, V0, V1, and V2 are all available for one more shot
  Vaccine.cov[c(2:3, 5:16), "V1"] <- Vaccine.cov[c(2:3, 5:16), "V1"] + Vaccine.cov[c(2:3, 5:16), "V0"]*c(rep(par.sample$Vac0_19[rep.i], 2), rep(par.sample$Vac20_59[rep.i], 8), rep(par.sample$Vac60_69[rep.i], 2), rep(par.sample$Vac_70above[rep.i], 2))
  
  Vaccine.cov[c(2:3, 5:16), "V0"] <- Vaccine.cov[c(2:3, 5:16), "V0"]*(1-c(rep(par.sample$Vac0_19[rep.i], 2), rep(par.sample$Vac20_59[rep.i], 8), rep(par.sample$Vac60_69[rep.i], 2), rep(par.sample$Vac_70above[rep.i], 2)))
  
  Vaccine.cov[c(2:3, 5:16), "V2"] <- Vaccine.cov[c(2:3, 5:16), "V2"] + Vaccine.cov[c(2:3, 5:16), "V1"]*c(rep(par.sample$Vac0_19[rep.i], 2), rep(par.sample$Vac20_59[rep.i], 8), rep(par.sample$Vac60_69[rep.i], 2), rep(par.sample$Vac_70above[rep.i], 2))
  Vaccine.cov[c(2:3, 5:16), "V1"] <- Vaccine.cov[c(2:3, 5:16), "V1"]*(1-c(rep(par.sample$Vac0_19[rep.i], 2), rep(par.sample$Vac20_59[rep.i], 8), rep(par.sample$Vac60_69[rep.i], 2), rep(par.sample$Vac_70above[rep.i], 2)))
  
  Vaccine.cov[c(5:16), "V3"] <- Vaccine.cov[c(5:16), "V3"] + Vaccine.cov[c(5:16), "V2"]*c( rep(par.sample$Vac20_59[rep.i], 8), rep(par.sample$Vac60_69[rep.i], 2), rep(par.sample$Vac_70above[rep.i], 2))
  Vaccine.cov[c(5:16), "V2"] <- Vaccine.cov[c(5:16), "V2"]*(1-c(rep(par.sample$Vac20_59[rep.i], 8), rep(par.sample$Vac60_69[rep.i], 2), rep(par.sample$Vac_70above[rep.i], 2)))
  
  # nothing can be done for 15-19, since only above 18 are eligible for booster and they already had the booster
  
  # for [0,5), V0 and V1 for those above 3 are available for one shot
  Vaccine.cov[1, "V1"] <- Vaccine.cov[1, "V1"] + (Vaccine.cov[1, "V0"]-0.6)*par.sample$Vac0_19[rep.i]
  Vaccine.cov[1, "V0"] <- (Vaccine.cov[1, "V0"]-0.6)*(1-par.sample$Vac0_19[rep.i]) + 0.6
  
  Vaccine.cov[1, "V2"] <- Vaccine.cov[1, "V2"] + Vaccine.cov[1, "V1"]*par.sample$Vac0_19[rep.i]
  Vaccine.cov[1, "V1"] <- Vaccine.cov[1, "V1"]*(1-par.sample$Vac0_19[rep.i])
  
  
  Vaccine.data <- Age.structure %>%
    gather("City","Pop", Shenzhen2010:China2020) %>%
    filter(City == "China2020") %>%
    mutate(Pop = round(Pop*Total.popsize)) %>%
    mutate(V0 = round(Pop*Vaccine.cov$V0), V1 = round(Pop*Vaccine.cov$V1), V2 = round(Pop*Vaccine.cov$V2), V3 = round(Pop*Vaccine.cov$V3))
  
  Vaccine.data <- Vaccine.data %>%
    select(Age, V0, V1, V2, V3) %>%
    gather("VaccStatus", "Number", V0:V3) %>%
    data.table()
  
  Agents <- Vaccine.data[rep(1:.N, Number)]
  Agents[, Number := NULL]
  Agents[, ID := 1:.N]
  Agents[, Inf.Stat := "S"]   # set all infection status to S at time 0
  
  #========== sample vaccination time ==========
  #==== Above 60 ====
  Agents[, Vaccine.Time := -Inf]
  current.vaccine.time <- vaccine.time %>%
    filter(Age == "60Above", Dose == "Dose3")
  Agents[Age %in% c("[60,65)", "[65,70)", "[70,75)", "[75,Inf)") & VaccStatus == "V3", Vaccine.Time := sample(current.vaccine.time$day.to.ddl, .N, replace = TRUE, prob = current.vaccine.time$diff.y)]
  
  current.vaccine.time <- vaccine.time %>%
    filter(Age == "60Above", Dose == "Dose2")
  Agents[Age %in% c("[60,65)", "[65,70)", "[70,75)", "[75,Inf)") & VaccStatus == "V2", Vaccine.Time := sample(current.vaccine.time$day.to.ddl, .N, replace = TRUE, prob = current.vaccine.time$diff.y)]
  
  current.vaccine.time <- vaccine.time %>%
    filter(Age == "60Above", Dose == "Dose1")
  Agents[Age %in% c("[60,65)", "[65,70)", "[70,75)", "[75,Inf)") & VaccStatus == "V1", Vaccine.Time := sample(current.vaccine.time$day.to.ddl, .N, replace = TRUE, prob = current.vaccine.time$diff.y)]
  
  #==== 18 to 59 ====
  current.vaccine.time <- vaccine.time %>%
    filter(Age == "18to59", Dose == "Dose3")
  Agents[Age %in% c("[20,25)", "[25,30)", "[30,35)", "[35,40)", "[40,45)", "[45,50)", "[50,55)", "[55,60)") & VaccStatus == "V3", Vaccine.Time := sample(current.vaccine.time$day.to.ddl, .N, replace = TRUE, prob = current.vaccine.time$diff.y)]
  
  current.vaccine.time <- vaccine.time %>%
    filter(Age == "18to59", Dose == "Dose2")
  Agents[Age %in% c("[20,25)", "[25,30)", "[30,35)", "[35,40)", "[40,45)", "[45,50)", "[50,55)", "[55,60)") & VaccStatus == "V2", Vaccine.Time := sample(current.vaccine.time$day.to.ddl, .N, replace = TRUE, prob = current.vaccine.time$diff.y)]
  
  current.vaccine.time <- vaccine.time %>%
    filter(Age == "18to59", Dose == "Dose1")
  Agents[Age %in% c("[20,25)", "[25,30)", "[30,35)", "[35,40)", "[40,45)", "[45,50)", "[50,55)", "[55,60)") & VaccStatus == "V1", Vaccine.Time := sample(current.vaccine.time$day.to.ddl, .N, replace = TRUE, prob = current.vaccine.time$diff.y)]
  
  #==== 15 to 20 ====
  current.vaccine.time <- vaccine.time %>%
    filter(Age == "18to59", Dose == "Dose3")
  Agents[Age %in% c("[15,20)") & VaccStatus == "V3", Vaccine.Time := sample(current.vaccine.time$day.to.ddl, .N, replace = TRUE, prob = current.vaccine.time$diff.y)]
  
  current.vaccine.time1 <- vaccine.time %>%
    filter(Age == "18to59", Dose == "Dose2")
  current.vaccine.time2 <- vaccine.time %>%
    filter(Age == "12to17", Dose == "Dose2")
  Agents[Age %in% c("[15,20)") & VaccStatus == "V2", Vaccine.Time := ifelse(runif(.N) <= 0.6, 1, 0)]
  Agents[Age %in% c("[15,20)") & VaccStatus == "V2" & Vaccine.Time == 1, Vaccine.Time := sample(current.vaccine.time2$day.to.ddl, .N, replace = TRUE, prob = current.vaccine.time2$diff.y)]
  Agents[Age %in% c("[15,20)") & VaccStatus == "V2" & Vaccine.Time == 0, Vaccine.Time := sample(current.vaccine.time1$day.to.ddl, .N, replace = TRUE, prob = current.vaccine.time1$diff.y)]
  
  current.vaccine.time1 <- vaccine.time %>%
    filter(Age == "18to59", Dose == "Dose1")
  current.vaccine.time2 <- vaccine.time %>%
    filter(Age == "12to17", Dose == "Dose1")
  Agents[Age %in% c("[15,20)") & VaccStatus == "V1", Vaccine.Time := ifelse(runif(.N) <= 0.6, 1, 0)]
  Agents[Age %in% c("[15,20)") & VaccStatus == "V1" & Vaccine.Time == 1, Vaccine.Time := sample(current.vaccine.time2$day.to.ddl, .N, replace = TRUE, prob = current.vaccine.time2$diff.y)]
  Agents[Age %in% c("[15,20)") & VaccStatus == "V1" & Vaccine.Time == 0, Vaccine.Time := sample(current.vaccine.time1$day.to.ddl, .N, replace = TRUE, prob = current.vaccine.time1$diff.y)]
  
  
  #==== 10 to 15 ====
  current.vaccine.time1 <- vaccine.time %>%
    filter(Age == "3to11", Dose == "Dose2")
  current.vaccine.time2 <- vaccine.time %>%
    filter(Age == "12to17", Dose == "Dose2")
  Agents[Age %in% c("[10,15)") & VaccStatus == "V2", Vaccine.Time := ifelse(runif(.N) <= 0.4, 1, 0)]
  Agents[Age %in% c("[10,15)") & VaccStatus == "V2" & Vaccine.Time == 1, Vaccine.Time := sample(current.vaccine.time1$day.to.ddl, .N, replace = TRUE, prob = current.vaccine.time1$diff.y)]
  Agents[Age %in% c("[10,15)") & VaccStatus == "V2" & Vaccine.Time == 0, Vaccine.Time := sample(current.vaccine.time2$day.to.ddl, .N, replace = TRUE, prob = current.vaccine.time2$diff.y)]
  
  current.vaccine.time1 <- vaccine.time %>%
    filter(Age == "3to11", Dose == "Dose1")
  current.vaccine.time2 <- vaccine.time %>%
    filter(Age == "12to17", Dose == "Dose1")
  Agents[Age %in% c("[10,15)") & VaccStatus == "V1", Vaccine.Time := ifelse(runif(.N) <= 0.4, 1, 0)]
  Agents[Age %in% c("[10,15)") & VaccStatus == "V1" & Vaccine.Time == 1, Vaccine.Time := sample(current.vaccine.time1$day.to.ddl, .N, replace = TRUE, prob = current.vaccine.time1$diff.y)]
  Agents[Age %in% c("[10,15)") & VaccStatus == "V1" & Vaccine.Time == 0, Vaccine.Time := sample(current.vaccine.time2$day.to.ddl, .N, replace = TRUE, prob = current.vaccine.time2$diff.y)]
  
  
  #==== 5 to 10 ====
  current.vaccine.time <- vaccine.time %>%
    filter(Age == "3to11", Dose == "Dose2")
  Agents[Age %in% c("[5,10)", "[0,5)") & VaccStatus == "V2", Vaccine.Time := sample(current.vaccine.time$day.to.ddl, .N, replace = TRUE, prob = current.vaccine.time$diff.y)]
  
  current.vaccine.time <- vaccine.time %>%
    filter(Age == "3to11", Dose == "Dose1")
  Agents[Age %in% c("[5,10)", "[0,5)") & VaccStatus == "V1", Vaccine.Time := sample(current.vaccine.time$day.to.ddl, .N, replace = TRUE, prob = current.vaccine.time$diff.y)]
  
  
  pop.table <- Agents[, .(Pop = length(ID)), by = .(Age)]
  setkey(pop.table, Age)
  
  
  results <- mclapply(1:(nc), function(i){
    system(paste("echo 'now processing:", i,"'"))
    
    bed.remain <- copy(bed.remain.original)
    Agents.current <- copy(Agents)
    model_run(Agents.current, bed.remain, 
                       R0 = 7, 
                       rho = 1, 
                       vac.ve = "between",
                       import.rate = 70/57752557*Total.popsize, 
                       surv.rate.M = 0.75,
                       home.mild.reduced = 0.8,
                       child.sus = c(1, 1),
                       home.nocare.h = 8,
                       home.nocare.c = 10,
                       hospital.nocare.c = 5)
  }, mc.cores = nc)
  
  result.current<- do.call(rbind, results) %>%
    data.frame() %>%
    mutate(repi = rep.i)
  
  result.all <- rbind(result.all, result.current)
  
  save(result.all, file = 'Results/Local_Sens_China_baseline.rda')
}



