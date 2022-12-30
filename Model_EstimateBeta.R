Estimate_beta <- function(rho, pA, pMH, pMC, pop.size, child.sus, R0)
{
  pA <- rep(pA, length(pMH))
  pMR <- 1 - pMH - pMC
  
  sus.age <- rep(1, length(pMH))
  sus.age[1:2] <- child.sus[1]
  sus.age[3:4] <- child.sus[2]
  
  # get the time to event through taking the mean from random samples
  tAR.mean <- mean(rgamma(100000, shape = 3.175, scale = 1.977))
  tPM.mean <- mean(rgamma(100000, shape = 0.7676, scale = 2.345))
  tMR.mean <- mean(rgamma(100000, shape = 2.47, scale = 1.82))
  tMH.mean <- mean(rgamma(100000, shape = 4.8400000, scale = 0.4545455))
  tMC.mean <- mean(rgamma(100000, shape = 4.8400000, scale = 0.4545455))
  
  n.agegroup <- length(pMH)
  
  Contact.Mat <- read_csv("Data/ContactMat_ZJJ.csv", show_col_types = FALSE) 
  Contact.Mat <- matrix(Contact.Mat$Contact, ncol = length(unique(Contact.Mat$Age_Individual)))
  
  
  NGM <- matrix(NA, n.agegroup, n.agegroup)
  
  # estimate elements for the next generation matrix
  for(i in 1:n.agegroup)
    for(j in 1:n.agegroup){
      NGM[i,j] <- Contact.Mat[i,j] * pop.size[i]/pop.size[j] * sus.age[i] *(rho*pA[j]*tAR.mean + (1 - pA[j])*tPM.mean + (1-pA[j])*(pMR[j]*tMR.mean + pMH[j]*tMH.mean + pMC[j]*tMC.mean))
    }
  
  R0/eigen(NGM)$values[1]  
}