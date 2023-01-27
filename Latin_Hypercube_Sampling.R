library(lhs)

#========================================================================
#                           full space
#========================================================================
set.seed(20221214)
lhs.sample.original <- maximinLHS(100, 8)
colnames(lhs.sample.original) <- c("Antiviral", "Hospital","ICU", "Mask","Vac0_19", "Vac20_59", "Vac60_69", "Vac_70above")

write.csv(lhs.sample.original, "Data/LHS_sample_for_local_sen_original.csv", row.names = FALSE)



# For China
lhs.sample <- lhs.sample.original 
lhs.sample[, 2] <- lhs.sample[, 2]*(14.4/1000-713.12/140982.5) + 713.12/140982.5  # hospital beds  [current.china = 713.12/140982.5, max.world = 14.4/1000]   https://data.worldbank.org/indicator/SH.MED.BEDS.ZS?most_recent_value_desc=true maximum of the world value
lhs.sample[, 3] <- lhs.sample[, 3]*(48/100000 - 4.37/100000) + 4.37/100000 # ICU beds  [4.37/100000, 48/100000]    47.74 per 100,000 https://ourworldindata.org/grapher/intensive-care-beds-per-100000?tab=table&time=latest
apply(lhs.sample, 2, range)
write.csv(lhs.sample, "Data/LHS_sample_for_local_sen_China.csv", row.names = FALSE)


# For Shanghai
lhs.sample <- lhs.sample.original 
# antiviral  [0, 1]
lhs.sample[, 2] <- lhs.sample[, 2]*(14.4/1000 - 5.78/1000) + 5.78/1000  # hospital beds  
lhs.sample[, 3] <- lhs.sample[, 3]*(48/100000 - 6.14/100000) + 6.14/100000 # ICU beds  
apply(lhs.sample, 2, range)
write.csv(lhs.sample, "Data/LHS_sample_for_local_sen_SH.csv", row.names = FALSE)




# For Shenzhen
lhs.sample <- lhs.sample.original 
lhs.sample[, 2] <- lhs.sample[, 2]*(14.4/1000 - 3.27/1000) + 3.27/1000  # hospital beds  
lhs.sample[, 3] <- lhs.sample[, 3]*(48/100000 - 3.42/100000) + 3.42/100000 # ICU beds  
apply(lhs.sample, 2, range)
write.csv(lhs.sample, "Data/LHS_sample_for_local_sen_SZ.csv", row.names = FALSE)



# For Shiyan
lhs.sample <- lhs.sample.original 
lhs.sample[, 2] <- lhs.sample[, 2]*(14.4/1000 - 6.82/1000) + 6.82/1000  # hospital beds 
lhs.sample[, 3] <- lhs.sample[, 3]*(48/100000 - 5.13/100000) + 5.13/100000 # ICU beds 
apply(lhs.sample, 2, range)
write.csv(lhs.sample, "Data/LHS_sample_for_local_sen_SY.csv", row.names = FALSE)









#========================================================================
#                          Reduced space
#========================================================================
library(lhs)
set.seed(20221215)
lhs.sample.original <- maximinLHS(100, 8)
colnames(lhs.sample.original) <- c("Antiviral", "Hospital","ICU", "Mask","Vac0_19", "Vac20_59", "Vac60_69", "Vac_70above")
write.csv(lhs.sample.original, "Data/LHS_sample_for_local_sen_original_cut.csv", row.names = FALSE)


# For China
lhs.sample <- lhs.sample.original %>% as.data.frame()
lhs.sample$Antiviral <- lhs.sample$Antiviral*(1-0.577) + 0.577
lhs.sample$Hospital <- lhs.sample$Hospital*(14.4/1000-713.12/140982.5) + 713.12/140982.5 
lhs.sample$ICU <- lhs.sample$ICU*(48/100000 - 8.333636e-05) + 8.333636e-05
lhs.sample$Vac_70above <- lhs.sample$Vac_70above*(1-0.760) + 0.760
apply(lhs.sample, 2, range)
write.csv(lhs.sample, "Data/LHS_China_cut_baseline.csv", row.names = FALSE)


lhs.sample <- lhs.sample.original %>% as.data.frame()
lhs.sample$Hospital <- lhs.sample$Hospital*(14.4/1000-713.12/140982.5) + 713.12/140982.5 
lhs.sample$ICU <- lhs.sample$ICU*(48/100000 - 4.37e-05) +4.37e-05
lhs.sample$Vac_70above <- lhs.sample$Vac_70above*(1-0.33) + 0.33
apply(lhs.sample, 2, range)
write.csv(lhs.sample, "Data/LHS_China_cut_VacOpt.csv", row.names = FALSE)


# For SH
lhs.sample <- lhs.sample.original %>% as.data.frame()
lhs.sample$Antiviral <- lhs.sample$Antiviral*(1-0.777) + 0.777
lhs.sample$Hospital <- lhs.sample$Hospital*(14.4/1000-5.78/1000) + 5.78/1000
lhs.sample$ICU <- lhs.sample$ICU*(48/100000 - 0.0001250606) + 0.0001250606
lhs.sample$Vac_70above <- lhs.sample$Vac_70above*(1-0.979) + 0.979
apply(lhs.sample, 2, range)
write.csv(lhs.sample, "Data/LHS_SH_cut_baseline.csv", row.names = FALSE)



lhs.sample <- lhs.sample.original %>% as.data.frame()
lhs.sample$Hospital <- lhs.sample$Hospital*(14.4/1000-5.78/1000) + 5.78/1000
lhs.sample$ICU <- lhs.sample$ICU*(48/100000 - 6.14e-05) +6.14e-05
lhs.sample$Vac_70above <- lhs.sample$Vac_70above*(1-0.793) + 0.793
apply(lhs.sample, 2, range)
write.csv(lhs.sample, "Data/LHS_SH_cut_VacOpt.csv", row.names = FALSE)



# For Shiyan
lhs.sample <- lhs.sample.original %>% as.data.frame()
lhs.sample$Antiviral <- lhs.sample$Antiviral*(1-0.572) + 0.572
lhs.sample$Hospital <- lhs.sample$Hospital*(14.4/1000-6.82/1000) + 6.82/1000
lhs.sample$ICU <- lhs.sample$ICU*(48/100000 - 7.383333e-05) + 7.383333e-05
apply(lhs.sample, 2, range)
write.csv(lhs.sample, "Data/LHS_SY_cut_baseline.csv", row.names = FALSE)

