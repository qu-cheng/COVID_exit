library(lhs)

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
