# function for reparameterize the lognormal distribution
lognorm.repar <- function(meanOriginal, sdOriginal)
{
  meanlog <- log(meanOriginal^2/sqrt(sdOriginal^2 + meanOriginal ^2))
  sdlog <- sqrt(log(1 + (sdOriginal^2/meanOriginal^2)))
  
  c(meanlog = meanlog, sdlog = sdlog)
}

# rewrite the default sample function, since for the default function, sample(10, 1) will randomly sample one value between 1 and 10, instead of output 10
sample.Q <- function(x, size, replace = F, prob = NULL) {
  if (length(x) == 1 & size > 0) return(x)
  if(length(x) == 1 & size == 0) return(integer(0))
  base::sample(x, size = size, replace = replace, prob = prob)
}


# Function for sampling the next state from E
ENext.sample <- function(dat, t, pA)
{
  if(nrow(dat[Inf.Stat == "E" & Next.Inf.Stat == "NA"]) > 0)
  {
    # Sample next status and time to transit to the next status
    dat[pA, ":=" (pA.baseline = pA.baseline, VE = VE), 
        on = .(Age, VaccStatus, Inf.Stat, Next.Inf.Stat)]
    
    
    dat[!is.na(pA.baseline) & VaccStatus %in% c("V0"), 
        pA := pA.baseline]
    
    dat[!is.na(pA.baseline) & VaccStatus %in% c("V1", "V2"), 
        pA := 1-(1-pA.baseline)*(1 - pmax(VE - 0.076617* (t-Vaccine.Time)/30, VE*0.13))]  # reestimate the vaccine effectiveness after waning
    
    dat[!is.na(pA.baseline) & VaccStatus %in% c("V3"), 
        pA := 1-(1-pA.baseline)*(1 - pmax(VE - 0.079355* (t-Vaccine.Time)/30, VE*0.62))] # reestimate the vaccine effectiveness after waning
    
    dat[!is.na(pA.baseline), ":=" (Next.Inf.Stat = ifelse(runif(.N) <= pA, "A", "P"),   # sample the next state
                                   Next.Time = t + rgamma(.N, shape = 3.26, scale = 1.227))]   # sample time to the next state
    
    dat[, ":="(pA.baseline = NULL, pA = NULL, VE = NULL)]
  }
}




# Function for sampling the next state from A
ANext.sample <- function(dat, t, known.case, bed.remain)
{
  no.row <- nrow(dat[Inf.Stat == "A" & Next.Inf.Stat == "NA"])
  
  if(no.row > 0)
  {
    # sample if it will be identified
    dat[Inf.Stat == "A" & Next.Inf.Stat == "NA", Identified := rbinom(.N, 1, surv.rate.AP)]
    
    # Next status, A can only transit to R
    dat[Inf.Stat == "A" & Next.Inf.Stat == "NA", Next.Inf.Stat := "R"]  
    
    # sample time to transit to the next status
    dat[Inf.Stat == "A" & Next.Time == 0, Next.Time := t + rgamma(.N, shape = 3.175, scale = 1.977)]   
  }
}




# Function for sampling the next state from P
PNext.sample <- function(dat, t, known.case, bed.remain)
{
  no.row <- nrow(dat[Inf.Stat == "P" & Next.Inf.Stat == "NA"])
  
  if(no.row > 0)
  {
    # sample if it will be identified
    dat[Inf.Stat == "P" & Next.Inf.Stat == "NA", Identified := rbinom(.N, 1, surv.rate.AP)]
    
    # Next status
    dat[Inf.Stat == "P" & Next.Inf.Stat == "NA", ":="(Next.Inf.Stat = "M")]  
    
    # sample time to transit to the next status
    dat[Inf.Stat == "P" & Next.Time == 0, Next.Time := t + rgamma(.N, shape = 0.7676, scale = 2.345)]  
  }
}




# Function for sampling the next state from M
MNext.sample <- function(dat, t, bed.remain, surv.rate.M, pM)
{
  rowID <- dat[Inf.Stat == "M" & Next.Inf.Stat == "NA", which = TRUE]
  
  if(length(rowID) > 0)
  {
    dat[rowID, Identified := ifelse(Identified == 0, rbinom(.N, 1, surv.rate.M), Identified)]
    
    # assign place; staying in hospital does not affect anything, except taking hospital beds
    M.alt.rows <- dat[Inf.Stat == "M" & Next.Inf.Stat == "NA" & Identified == 1 & Place == "Home", which = TRUE]
    
    # sample drug use
    dat[Inf.Stat == "M" & Next.Inf.Stat == "NA" & Identified == 1, Drug := rbinom(.N, 1, drug.coverage)]
    dat[Inf.Stat == "M" & Next.Inf.Stat == "NA" & Identified == 0, Drug := 0]
    dat[Drug == 1 & Age %in% c("[0,5)","[5,10)"), Drug := 0]   # under 10 not eligible
    dat[Drug == 1 & Age == "[10,15)", Drug := rbinom(.N, 1, 0.6)]  # 10-14, assume only 0.6 eligible (that is 12-14 year-old)
    
    dat[pM, 
        on = .(Age, VaccStatus, Inf.Stat, Next.Inf.Stat, Drug, Place), 
        ":=" (pMH.baseline = pMH.baseline, 
              pMH.VE = pMH.VE, 
              pMC.baseline = pMC.baseline,
              pMC.VE = pMC.VE)]
    
    dat[Inf.Stat == "M" & Next.Inf.Stat == "NA" & VaccStatus %in% c("V0"),
        ":=" (pMH.VE = 0,
              pMC.VE = 0)]
    
    
    dat[Inf.Stat == "M" & Next.Inf.Stat == "NA" & VaccStatus %in% c("V1", "V2"),  # reestimate the vaccine effectiveness after waning
        ":=" (pMH.VE = pmax(pMH.VE - 0.025559*(t-Vaccine.Time)/30, pMH.VE*0.88),
              pMC.VE = pmax(pMC.VE - 0.025559*(t-Vaccine.Time)/30, pMC.VE*0.88))]
    
    dat[Inf.Stat == "M" & Next.Inf.Stat == "NA" & VaccStatus %in% c("V3"),   # reestimate the vaccine effectiveness after waning
        ":=" (pMH.VE = pmax(pMH.VE -0.057485*(t-Vaccine.Time)/30, pMH.VE*0.95),
              pMC.VE = pmax(pMC.VE -0.057485*(t-Vaccine.Time)/30, pMC.VE*0.95))]
    
    dat[Inf.Stat == "M" & Next.Inf.Stat == "NA",
        ":="(pMH = pmin(pMH.baseline*(1-pMH.VE), 1),
             pMC = pmin(pMC.baseline*(1-pMC.VE), 1))]
    dat[Inf.Stat == "M" & Next.Inf.Stat == "NA", pMR := pmax(1-pMH-pMC, 0)]
    
    # Sample next status
    dat[Inf.Stat == "M" & Next.Inf.Stat == "NA", Next.Inf.Stat := c("R","H","C")[min(which(cumsum(unlist(.SD))>runif(.N)))], .SDcols=c("pMR","pMH","pMC"), by = ID]
    
    
    # Sample time to transit to the next status
    dat[Inf.Stat == "M" & Next.Inf.Stat == "R" & Next.Time == 0, Next.Time := t + rgamma(.N, shape = 2.47, scale = 1.82)]   
    dat[Inf.Stat == "M" & Next.Inf.Stat == "H" & Next.Time == 0, ":="(Next.Time = t + rgamma(.N, shape = 4.8400000, scale = 0.4545455))]  
    dat[Inf.Stat == "M" & Next.Inf.Stat == "C" & Next.Time == 0, Next.Time := t + rgamma(.N, shape = 4.8400000, scale = 0.4545455)] 
    
    dat[, ":="(pMR = NULL, 
               pMH = NULL, 
               pMC = NULL, 
               pMH.VE = NULL, 
               pMC.VE = NULL, 
               pMH.baseline = NULL, 
               pMC.baseline = NULL)]
  }
}







# Function for sampling the next state from H
HNext.sample <- function(dat, t, bed.remain, pH)
{
  hospital.row <- dat[Inf.Stat == "H" & Next.Inf.Stat == "NA", which = TRUE]
  no.row <- length(hospital.row)
  
  if(no.row  > 0)
  {
    dat[hospital.row, Identified := 1]
    
    # assign place; staying in hospital does not affect anything, except taking hospital beds
    dat[sample.Q(hospital.row, min(no.row, bed.remain$hospital)), Place := "Hospital"]
    
    # Sample next status
    dat[pH, on = .(Age, VaccStatus, Inf.Stat, Next.Inf.Stat, Place), 
        ":="(pHD.baseline = pHD.baseline, 
             VE = VE)]
    
    dat[Inf.Stat == "H" & Next.Inf.Stat == "NA" & VaccStatus %in% c("V0"), 
        VE := 0]
    dat[Inf.Stat == "H" & Next.Inf.Stat == "NA" & VaccStatus %in% c("V1", "V2"), 
        VE := pmax(VE - 0.025559*(t-Vaccine.Time)/30, VE*0.9)]   # reestimate the vaccine effectiveness after waning
    dat[Inf.Stat == "H" & Next.Inf.Stat == "NA" & VaccStatus %in% c("V3"), 
        VE := pmax(VE -0.057485*(t-Vaccine.Time)/30, VE*0.966)]  # reestimate the vaccine effectiveness after waning
    
    dat[Inf.Stat == "H" & Next.Inf.Stat == "NA", 
        pHD := pmin(pHD.baseline*(1-VE), 1)]
    dat[Inf.Stat == "H" & Next.Inf.Stat == "NA", 
        pHR := 1-pHD]
    
    # sample next state
    dat[Inf.Stat == "H" & Next.Inf.Stat == "NA", Next.Inf.Stat := c("R","D")[min(which(cumsum(unlist(.SD))>runif(.N)))], .SDcols=c("pHR","pHD"), by = ID]
    
    # Sample time to transit to the next status
    dat[Inf.Stat == "H" & Next.Inf.Stat == "R" & Next.Time == 0, Next.Time := t + rgamma(.N, shape = 8.25, scale = 2.19)]   
    dat[Inf.Stat == "H" & Next.Inf.Stat == "D" & Next.Time == 0, Next.Time := t + rgamma(.N, shape = 16, scale = 0.75)]   
    
    dat[, ":="(pHR = NULL, 
               pHD = NULL, 
               VE = NULL,
               pHD.baseline = NULL)]
  } 
}




# Function for sampling the next state from C
CNext.sample <- function(dat, t, bed.remain, pC)
{
  no.row = nrow(dat[Inf.Stat == "C" & Next.Inf.Stat == "NA"])
  
  if(no.row > 0)
  {
    dat[Inf.Stat == "C" & Next.Inf.Stat == "NA", Identified := 1]
    
    dat[sample.Q(dat[Inf.Stat == "C" & Next.Inf.Stat == "NA", which = TRUE], min(bed.remain$ICU, no.row)), Place := "ICU"]  # all people can get ICU bed when available, when only part available, only part get ICU bed
    
    # update number of beds
    bed.remain[, ICU := bed.ICU - sum(Agents$Place == "ICU")]
    
    # for those not get ICU beds and staying at home, assign hospital beds
    no.row.home <- dat[Inf.Stat == "C" & Next.Inf.Stat == "NA" & Place == "Home", which = TRUE]   # for those at home, assign at least a hospital bed
    if(length(no.row.home) > 0) {dat[sample.Q(no.row.home, min(bed.remain$hospital, length(no.row.home))), Place := "Hospital"]}
    bed.remain[, hospital := bed.hospital - sum(Agents$Place == "Hospital")]
    
    # Sample next status
    dat[pC, on = .(Age, VaccStatus, Inf.Stat, Next.Inf.Stat, Place), 
        ":="(pCD.baseline = pCD.baseline, 
             VE = VE)]
    
    dat[Inf.Stat == "C" & Next.Inf.Stat == "NA" & VaccStatus =="V0", 
        VE := 0]
    dat[Inf.Stat == "C" & Next.Inf.Stat == "NA" & VaccStatus %in% c("V1", "V2"),   # reestimate the vaccine effectiveness after waning
        VE := pmax(VE - 0.025559*(t-Vaccine.Time)/30, VE*0.9)]
    dat[Inf.Stat == "C" & Next.Inf.Stat == "NA" & VaccStatus %in% c("V3"),    # reestimate the vaccine effectiveness after waning
        VE := pmax(VE - 0.057485*(t-Vaccine.Time)/30, VE*0.966)]
    
    dat[Inf.Stat == "C" & Next.Inf.Stat == "NA", 
        pCD := pmin(pCD.baseline*(1-VE), 1)]
    dat[Inf.Stat == "C" & Next.Inf.Stat == "NA", 
        pCR := 1-pCD]
    
    
    # sample the next state
    dat[pC, on = .(Age, VaccStatus, Inf.Stat, Next.Inf.Stat, Place), ":="(pCD = pCD, pCR = pCR)]
    dat[Inf.Stat == "C" & Next.Inf.Stat == "NA", Next.Inf.Stat := c("R","D")[min(which(cumsum(unlist(.SD))>runif(.N)))], .SDcols=c("pCR","pCD"), by = ID]
    
    # Sample time to transit to the next status
    dat[Inf.Stat == "C" & Next.Inf.Stat == "R" & Next.Time == 0, Next.Time := t + rgamma(.N, shape = 8.25, scale = 2.19)] 
    dat[Inf.Stat == "C" & Next.Inf.Stat == "D" & Next.Time == 0, Next.Time := t + rgamma(.N, shape = 4.97, scale = 2.15)]   
    
    
    dat[, ":="(pCR = NULL, 
               pCD = NULL, 
               VE = NULL,
               pCD.baseline = NULL)]
    
  }
}



