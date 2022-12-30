model_run <- function(Agents,   # data.table for the agents
                      bed.remain.input,   # data.table for recording the availability of hospital/ICU beds
                      R0 = 7,   # R0 for calibrating beta
                      rho = 1,  # relative infectiousness of asymptomatic cases to presymptomatic/symptomatic cases
                      vac.ve = c("between","optimistic", "pessimistic"),   # vaccine effectiveness scenarios
                      import.rate = 70/57752557*Total.popsize,    # importation rate, corresponding to 70 imported cases per day to Hubei
                      child.sus = c(1,1),    # relative infectiousness of children (<10) and adolescents (10-19)
                      home.mild.reduced = 0.8,   # reduction in contact rate when self-isolated
                      surv.rate.M = 0.75,     # proportion of mild cases being detected
                      home.nocare.h = 8,      # When not get hospital beds, the chances of severe cases to process to critical and death will increase by 8 times
                      home.nocare.c = 10,    # When not get ICU bed and staying at home, the chance of critical cases to process to death will increase by 10 times
                      hospital.nocare.c = 5 # When not get ICU bed and staying at home, the chance of critical cases to process to death will increase by 5 times
)
{
  Agents[, ":="(Next.Inf.Stat = "NA", Next.Time = 0, Drug = -999, Identified = -999, Place = "Home")]
  source("Model_Parameters.R")
  
  #=========== data process for the sensitivity analysis =========
  home.mild.infectious <- 1 - home.mild.reduced
  
  # set vaccine effectivess according to the scenarios
  if(vac.ve == "between")
  {
    # vaccine effectiveness; mean of optimistic and pessimistic
    pA_VE = c(0.06527832, 0.16671232, 0.24651337)
    pM_VE = c(0.3918381, 0.6272221, 0.8072430)
    pHC_VE = c(0.1532388, 0.1532388, 0.2165780)
    Sus_VE <- c(0.0310, 0.0695, 0.1310)
    Trans_VE <- c(0, 0, 0.053)
  } else if(vac.ve == "optimistic")
  {
    pA_VE = c(0.1154661, 0.1958196, 0.3554217)
    pM_VE = c(0.3918381, 0.7099863, 0.9644860)
    pHC_VE = c(0.1877323, 0.2075472, 0.216578)
    Sus_VE <- c(0.056, 0.091, 0.170)
    Trans_VE <- c(0, 0, 0.106)
  } else if(vac.ve == "pessimistic"){
    pA_VE = c(0.01509054, 0.13760504, 0.13760504)
    pM_VE = c(0.3918381, 0.5444580, 0.6500000)
    pHC_VE = c(0.16071429, 0.16071429, 0.216578)
    Sus_VE <- c(0.006, 0.048, 0.092)
    Trans_VE <- c(0, 0, 0)
  }
  
  
  #===========================================================
  #                  Data processing
  #===========================================================
  # probability of being asymptomatic
  pA <- data.table(Age = rep(AgeGroup, 4), 
                   VaccStatus = rep(c("V0", "V1", "V2", "V3"), each = length(AgeGroup)),
                   pA.baseline = pA_V0,
                   VE = rep(c(0, pA_VE), each = length(AgeGroup)),   # vaccine effectiveness before waning
                   Inf.Stat = "E",
                   Next.Inf.Stat = "NA",
                   Next.Time = 0)
  setkey(pA, Age, VaccStatus, Inf.Stat, Next.Inf.Stat)
  
  # estimate beta according to the specified R0
  beta <- Estimate_beta(rho = rho, 
                        pA = pA_V0, 
                        pMH = pMH_V0, 
                        pMC = pMC_V0, 
                        pop.size = Agents[,.N/nrow(Agents), by = .(Age)][,V1],
                        child.sus = child.sus,
                        R0 = R0)
  
  # probability to different next state from M
  pM <- data.table(Age = rep(AgeGroup, 4), 
                   VaccStatus = rep(c("V0", "V1", "V2", "V3"), each = length(AgeGroup)),
                   pMH.baseline = pMH_V0,
                   pMH.VE = rep(c(0, pM_VE), each = length(AgeGroup)),
                   pMC.baseline = pMC_V0,
                   pMC.VE = rep(c(0, pM_VE), each = length(AgeGroup)),
                   Inf.Stat = "M",
                   Next.Inf.Stat = "NA",
                   Next.Time = 0)
  setkey(pM, Age, VaccStatus, Inf.Stat, Next.Inf.Stat)
  
  # probability to different next state from H
  pH <- data.table(Age = rep(AgeGroup, 4), 
                   VaccStatus = rep(c("V0", "V1", "V2", "V3"), each = length(AgeGroup)),
                   pHD.baseline = pHD_V0,
                   VE = rep(c(0, pHC_VE), each = length(AgeGroup)),
                   Inf.Stat = "H",
                   Next.Inf.Stat = "NA",
                   Next.Time = 0)
  setkey(pH, Age, VaccStatus, Inf.Stat, Next.Inf.Stat)
  
  # probability to different next state from C
  pC <- data.table(Age = rep(AgeGroup, 4), 
                   VaccStatus = rep(c("V0", "V1", "V2", "V3"), each = length(AgeGroup)),
                   pCD.baseline = pCD_V0,
                   VE = rep(c(0, pHC_VE), each = length(AgeGroup)),
                   Inf.Stat = "C",
                   Next.Inf.Stat = "NA",
                   Next.Time = 0)
  setkey(pC, Age, VaccStatus, Inf.Stat, Next.Inf.Stat)
  
  
  
  # contact mat 
  Contact.Mat <- read_csv("Data/ContactMat_ZJJ.csv", show_col_types = FALSE) %>%
    as.data.table()
  setkey(Contact.Mat, Age_Contact, Age_Individual)
  
  # Vaccine-reduced transmissibility
  vac.trans <- data.table(VaccStatus = c("V0", "V1", "V2", "V3"),
                          vac.trans = c(1, 1-Trans_VE)) %>%
    setkey(VaccStatus)
  
  # Vaccine-reduced susceptibility
  vac.sus <- data.table(Age_Individual = rep(AgeGroup, each = 4),
                        VaccStatus_Individual = rep(c("V0", "V1", "V2", "V3"), length(AgeGroup)),
                        vac.sus = rep(c(1, 1-Sus_VE), length(AgeGroup)),
                        k = 1) %>%   # for allowing Cartesian product
    setkey(VaccStatus_Individual, Age_Individual)
  
  
  # estimate the probability of proceeding to D from C by locations
  # assign place
  pC[, Inf.Stat := "C"]
  pC[, Next.Inf.Stat := "NA"]
  pC[, Next.Time := 0]
  pC[, Place := "ICU"]
  
  # processing data for prob while hospitalized
  pC.Hosp <- copy(pC)
  pC.Hosp[, Place := "Hospital"]
  pC.Hosp[, pCD.baseline := pCD.baseline*(1 + hospital.nocare.c)]
  
  # processing data for prob while hospitalized
  pC.Home <- copy(pC)
  pC.Home[, Place := "Home"]
  pC.Home[, pCD.baseline := pCD.baseline*(1 + home.nocare.c)]
  
  pC <- rbindlist(list(pC, pC.Hosp, pC.Home))
  setkey(pC, Age, VaccStatus, Inf.Stat, Next.Inf.Stat, Place)
  
  
  
  
  # estimate the probability of proceeding to D from H by locations
  pH[, Inf.Stat := "H"]
  pH[, Next.Inf.Stat := "NA"]
  pH[, Next.Time := 0]
  pH[, Place := "Hospital"]
  
  # processing data for prob while hospitalized
  pH.Home <- copy(pH)
  pH.Home[, Place := "Home"]
  pH.Home[, ":="(pHD.baseline = pHD.baseline*(1 + home.nocare.h))]
  
  pH <- rbindlist(list(pH, pH.Home))
  setkey(pH, Age, VaccStatus, Inf.Stat, Next.Inf.Stat, Place)
  
  
  
  # estimate the probability of proceeding to H and C from M by locations and antiviral treatment ususage
  pM[, Inf.Stat := "M"]
  pM[, Next.Inf.Stat := "NA"]
  pM[, Next.Time := 0]
  pM[, Drug := 0]
  pM[, Place := "Home"]
  
  
  pM.Drug <- copy(pM)
  pM.Drug[, Drug := 1]
  pM.Drug[, ":="(pMH.baseline = pMH.baseline*(1 - drug.eff), pMC.baseline = pMC.baseline*(1 - drug.eff))]
  
  pM <- rbindlist(list(pM, pM.Drug))
  setkey(pM, Age, VaccStatus, Inf.Stat, Next.Inf.Stat, Drug, Place)
  
  
  total.import <- 0   # record the total number of imported cases to subtract from the final results
  
  
  # start the simulation
  while(t <= tmax)
  {
    #========= simulate case importation ========
    NewCase.No <- rpois(1, import.rate*dt)
    total.import <- total.import + NewCase.No
    
    if(NewCase.No > 0)
    {
      S.ID <- Agents[Inf.Stat == "S", which = TRUE]
      Agents[sample(S.ID, NewCase.No), Inf.Stat := "E"]  # importation at predefined rate; ideally, we should add persons to the agents, but given the large amount of the agents, for simplicity, we change the sampled number of agents to E 
    }
    
    
    #========= estimate force of infection ========
    FOI.rowID <- Agents[(Inf.Stat %in% c("A", "P", "M"))|(Inf.Stat %in% c("H", "C") & Place == "Home"), which = TRUE]  # get the count of infectious state
    
    if(length(FOI.rowID) > 0) 
    {
      # add A, P, M, H, C when they don't occur in the table
      case.table <- dcast(Agents[FOI.rowID, .(Case = length(ID)), by = .(Age, VaccStatus, Inf.Stat, Identified)], Age + VaccStatus + Identified ~ Inf.Stat, value.var = "Case")
      if(! "A" %in% colnames(case.table)) case.table[, A := 0]
      if(! "P" %in% colnames(case.table)) case.table[, P := 0]
      if(! "M" %in% colnames(case.table)) case.table[, M := 0]
      if(! "H" %in% colnames(case.table)) case.table[, H := 0]
      if(! "C" %in% colnames(case.table)) case.table[, C := 0]
      setkey(case.table, Age, VaccStatus)
      
      case.table[is.na(case.table)] <- 0   # replace NA with 0
      case.table[vac.trans, vac.trans := vac.trans, on = .(VaccStatus)]
      case.table[, case.equivalent := (rho*A + M + P + H + C)*vac.trans]   # rho is used to represent the relative infectiousness of asymptomatic cases to presymptomatic/symptomatic cases
      
      case.table[pop.table, pop.size := Pop, on = .(Age)]
      colnames(case.table)[1] <- "Age_Contact"
      case.table[, k := 1]    # added a column for making cartesian join
      
      FOI.cal <- merge(case.table, vac.sus, by = "k", allow.cartesian = TRUE)
      setkey(FOI.cal, Age_Contact, Age_Individual)
      
      FOI.cal[Contact.Mat, contact.rate := ifelse(Identified == 0, beta*Contact*dt, beta*home.mild.infectious*Contact.Home*dt)]  # assumed that cases will be self-isolated at home once detected, so they only have home contacts and at a reduced rate
      FOI.cal[, FOI.comp := contact.rate*case.equivalent/pop.size*(mask.eff)^mask.coverage]   # equation for mask efficacy came from PNAS, all people report wearing mask reduces the R0 by 25%
      
      FOI.table <- FOI.cal[,.(FOI = sum(FOI.comp), vac.sus = mean(vac.sus)), by = .(Age_Individual, VaccStatus_Individual)]  # aggregate across age groups of the cases and vaccine status
      
      FOI.table[, Inf.Stat := "S"]
      FOI.table[, Next.Inf.Stat := "NA"]
      setkey(FOI.table, Age_Individual, VaccStatus_Individual)
      
      # print(FOI.table)
      
      Agents[FOI.table, ":="(FOI = FOI, vac.sus = vac.sus), on = c(Age = "Age_Individual", VaccStatus = "VaccStatus_Individual", Inf.Stat ="Inf.Stat", Next.Inf.Stat = "Next.Inf.Stat")]
      
      Agents[VaccStatus == "V0" & Inf.Stat == "S" & Next.Inf.Stat == "NA", vac.sus.new:= 1] 
      Agents[VaccStatus %in%  c("V1", "V2") & Inf.Stat == "S" & Next.Inf.Stat == "NA", 
             vac.sus.new:= 1 - pmax((1-vac.sus) - 0.076617* (t-Vaccine.Time)/30, (1-vac.sus)*0.13)]   # waned to 0.13 of the original level according to the meta-analysis
      Agents[VaccStatus == "V3" & Inf.Stat == "S" & Next.Inf.Stat == "NA", 
             vac.sus.new := 1 - pmax((1-vac.sus) - 0.079355* (t-Vaccine.Time)/30, (1-vac.sus)*0.62)] # waned to 0.62 of the original level according to the meta-analysis
      
      Agents[!is.na(vac.sus), FOI := FOI*vac.sus.new]
      
      Agents[Age %in% c("[0,5)", "[5,10)"), FOI := FOI*child.sus[1]]   # lower susceptibility for children
      Agents[Age %in% c("[10,15)", "[15,20)"), FOI := FOI*child.sus[2]]  # lower susceptibility for adolescents 
      
      Agents[Inf.Stat == "S" & Next.Inf.Stat == "NA" & runif(.N) <= FOI, ":="(Next.Inf.Stat = "E", Next.Time = t + dt*runif(.N))]  # S to E in the time interval
      
      # Agents[, U := NULL]
      Agents[, FOI := NULL]
      Agents[, vac.sus := NULL]
      Agents[, vac.sus.new := NULL]
    }
    
    Agents[Next.Time <= t + dt & Next.Time > t & Next.Inf.Stat %in% c("D", "R"), Place := "Home"] # for those recovered or died, recycle hospital beds
    Agents[Next.Time <= t + dt & Next.Time > t, ":="(Inf.Stat = Next.Inf.Stat, Next.Inf.Stat = "NA", Next.Time = 0)]
    
    bed.remain.input[, ICU := bed.ICU - sum(Agents$Place == "ICU")]
    bed.remain.input[, hospital := bed.hospital - sum(Agents$Place == "Hospital")]
    
    # sample next state and time to transit after the update
    CNext.sample(Agents, t, bed.remain.input, pC)
    bed.remain.input[, ICU := bed.ICU - sum(Agents$Place == "ICU")]
    bed.remain.input[, hospital := bed.hospital - sum(Agents$Place == "Hospital")]
    
    HNext.sample(Agents, t, bed.remain.input, pH)
    bed.remain.input[, ICU := bed.ICU - sum(Agents$Place == "ICU")]
    bed.remain.input[, hospital := bed.hospital - sum(Agents$Place == "Hospital")]
    
    MNext.sample(Agents, t, bed.remain.input, surv.rate.M, pM)
    bed.remain.input[, ICU := bed.ICU - sum(Agents$Place == "ICU")]
    bed.remain.input[, hospital := bed.hospital - sum(Agents$Place == "Hospital")]
    
    PNext.sample(Agents, t, known.case, bed.remain.input)
    bed.remain.input[, ICU := bed.ICU - sum(Agents$Place == "ICU")]
    bed.remain.input[, hospital := bed.hospital - sum(Agents$Place == "Hospital")]
    
    ANext.sample(Agents, t, known.case, bed.remain.input)
    bed.remain.input[, ICU := bed.ICU - sum(Agents$Place == "ICU")]
    bed.remain.input[, hospital := bed.hospital - sum(Agents$Place == "Hospital")]
    
    ENext.sample(Agents, t, pA)
    bed.remain.input[, ICU := bed.ICU - sum(Agents$Place == "ICU")]
    bed.remain.input[, hospital := bed.hospital - sum(Agents$Place == "Hospital")]
    
    Agents[Next.Time <= t + dt & Next.Time > t, ":="(Inf.Stat = Next.Inf.Stat, Next.Inf.Stat = "NA", Next.Time = 0)]
    
    t = t + dt
  }
  
  # record cases and deaths
  c(TotalCases = nrow(Agents[!Inf.Stat %in% c("S", "E")]) - total.import, TotalDeaths = nrow(Agents[Inf.Stat == "D"]))
}