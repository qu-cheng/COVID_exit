par.sample.original <- read_csv("Data/LHS_sample_for_local_sen_original_cut.csv")
par.sample.nocut <- read_csv("Data/LHS_sample_for_local_sen_original.csv")

# combine samples
par.sample.China1 <- read_csv("Data/LHS_China_cut_baseline.csv") %>%
  mutate(repi = 1:100, Location = "China, baseline")
par.sample.China2 <- read_csv("Data/LHS_China_cut_VacOpt.csv") %>%
  mutate(repi = 1:100, Location = "China, optimistic VE")
par.sample.SH1 <- read_csv("Data/LHS_SH_cut_baseline.csv") %>%
  mutate(repi = 1:100, Location = "Shanghai, baseline")
par.sample.SH2 <- read_csv("Data/LHS_SH_cut_VacOpt.csv") %>%
  mutate(repi = 1:100, Location = "Shanghai, optimistic VE")
par.sample.SZ1 <- read_csv("Data/LHS_sample_for_local_sen_SZ.csv") %>%
  mutate(repi = 1:100, Location = "Shenzhen, baseline")
par.sample.SZ2 <- read_csv("Data/LHS_sample_for_local_sen_SZ.csv") %>%
  mutate(repi = 1:100, Location = "Shenzhen, optimistic VE")
par.sample.SZ3 <- read_csv("Data/LHS_sample_for_local_sen_SZ.csv") %>%
  mutate(repi = 1:100, Location = "Shenzhen, pessimistic VE")
par.sample.SY1 <- read_csv("Data/LHS_SY_cut_baseline.csv") %>%
  mutate(repi = 1:100, Location = "Shiyan, baseline")
par.sample.SY2 <- read_csv("Data/LHS_sample_for_local_sen_SY.csv") %>%
  mutate(repi = 1:100, Location = "Shiyan, optimistic VE")

par.sample.cut <- rbind(par.sample.China1, par.sample.China2,
                        par.sample.SH1, par.sample.SH2,
                        par.sample.SZ1, par.sample.SZ2, par.sample.SZ3,
                        par.sample.SY1, par.sample.SY2)


# combine data
load("Results/China_cut100_baseline.rda")
result.China1 <- result.all %>%
  mutate(Location = "China, baseline")

load("Results/China_cut100_VacOpt.rda")
result.China2 <- result.all %>%
  mutate(Location = "China, optimistic VE")

load("Results/SH_cut100_baseline.rda")
result.SH1 <- result.all %>%
  mutate(Location = "Shanghai, baseline")

load("Results/SH_cut100_VacOpt.rda")
result.SH2<- result.all %>%
  mutate(Location = "Shanghai, optimistic VE")

load("Results/SZ_first100_baseline.rda")
result.SZ1 <- result.all %>%
  mutate(Location = "Shenzhen, baseline")

load("Results/SZ_first100_VacOpt.rda")
result.SZ2 <- result.all %>%
  mutate(Location = "Shenzhen, optimistic VE")

load("Results/SZ_first100_VacPes.rda")
result.SZ3 <- result.all %>%
  mutate(Location = "Shenzhen, pessimistic VE")

load("Results/SY_cut100_baseline.rda")
result.SY1 <- result.all %>%
  mutate(Location = "Shiyan, baseline")

load("Results/SY_first100_VacOpt.rda")
result.SY2 <- result.all %>%
  mutate(Location = "Shiyan, optimistic VE")

result.cut <- rbind(result.China1, result.China2,
                    result.SH1, result.SH2,
                    result.SZ1, result.SZ2, result.SZ3,
                    result.SY1, result.SY2)

result.cut.agg <- result.cut %>%
  mutate(IFR = TotalDeaths/TotalCases*1000, Mort.rate = TotalDeaths/500000*100000,
         Location = factor(Location, levels = unique(Location))) %>%
  group_by(repi, Location) %>%
  summarize(Case.median = median(TotalCases), 
            death.median = median(TotalDeaths),
            IFR.prob = mean(IFR <= 1),
            Mort.rate.prob = mean(Mort.rate <= 14.3),
            mort.rate.median = median(Mort.rate)) %>%
  ungroup() %>%
  left_join(par.sample.cut) %>%
  mutate(Location = factor(Location, levels = unique(Location))) 

Places <- unique(result.cut.agg$Location)


set.seed(20230107)
GP.model <- list()
for(i in 1:9)
{
  print(i)
  current.dat <- result.cut.agg %>% 
    filter(Location == Places[i])
  
    # current.model <- GauPro::GauPro(as.matrix(par.sample.original), current.dat$mort.rate.median)
  if(!i %in% c(5,6,7,9))
  {
    current.model <- GP_fit(as.matrix(par.sample.original), current.dat$mort.rate.median)
    GP.model[[i]] <- current.model
  }
  
  if(i %in% c(5,6,7,9))
  {
    current.model <- GP_fit(as.matrix(par.sample.nocut), current.dat$mort.rate.median)
    GP.model[[i]] <- current.model
  }
}

save(GP.model, file = "GP_model_all.rda")
