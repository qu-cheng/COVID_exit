library(tidyverse)
library(cowplot)
library(ggsci)
library(scales)
library(data.table)
library(GPfit)
library(GauPro)
library(vip)
library(showtext)
library(metR)
showtext_auto()





#========================================================================
#                         Figure 2
#========================================================================
AgeGroup <- c("[0,5)", "[5,10)", "[10,15)", "[15,20)", "[20,25)", "[25,30)", "[30,35)", "[35,40)", "[40,45)", "[45,50)", "[50,55)",
              "[55,60)", "[60,65)", "[65,70)", "[70,75)", "[75,Inf)") 

Chen.data <- data.frame(Age.numeric = c(9.1, 29.9, 49.3, 67.3, 85.5), 
                      IFR.median = c(0, 0, 0.01, 0.13, 1.99)/100,
                      IFR.low = NA,
                      IFR.high = NA,
                      type = "Observed in Shanghai")

load("Results/SH_validation.rda")
result.current.agg <- result.current %>%
  mutate(IFR = Death/Case) %>%
  mutate(Age = factor(Age, levels = AgeGroup)) %>%
  group_by(Age) %>%
  summarise(IFR.median = median(IFR), 
            IFR.low = quantile(IFR, 0.025),
            IFR.high = quantile(IFR, 0.975)) %>%
  mutate(Age.numeric = c(2.2,6.9,11.9,17.3,22.2, 27.1, 31.9, 37.0, 42.0, 47.1, 51.9, 56.9, 62.1, 66.8, 71.8, 82.0)) %>%
  mutate(type = "Simulated for Shanghai") %>%
  select(Age.numeric, IFR.median, type, IFR.low, IFR.high)

result.validation <- rbind(Chen.data, result.current.agg)

result.validation %>%
  ggplot(aes(x = Age.numeric, y = IFR.median*100, col = type, shape = type)) +
  geom_point() +
  theme_cowplot() +
  labs(col = "", shape = "") +
  theme(legend.position = c(0.05, 0.9)) +
  xlab("Mean age in each age group") +
  ylab("Infection fatality rate (%)") +
  scale_color_nejm()
# ggsave("Figures/Fig2_SH_validation.pdf", width = 8, height = 4)





#========================================================================
#                         Figure 3
#========================================================================
location_names <- list(
  "China, Best-case" = "(A) China, best-case", 
  "Shanghai, Best-case" = "(B) Shanghai, best-case",
  "Shenzhen, Best-case" = "(C) Shenzhen, best-case",
  "Shiyan, Best-case" = "(D) Shiyan, best-case", 
  "China, Worst-case" = "(E) China, worst-case", 
  "Shanghai, Worst-case" = "(F) Shanghai, worst-case", 
  "Shenzhen, Worst-case" = "(G) Shenzhen, worst-case", 
  "Shiyan, Worst-case" = "(H) Shiyan, worst-case"
)

location_labeller <- function(variable,value){
  return(location_names[value])
}

Var.scenario <- data.frame(repi = c(1,2), scenario = c("Best-case", "Worst-case"))

load("Results/China_best_worst_Sens.rda")
result.China <- result.all %>%
  mutate(location = "China")

load("Results/SH_best_worst_Sens.rda")
result.SH <- result.all %>%
  mutate(location = "Shanghai")

load("Results/SZ_best_worst_Sens.rda")
result.SZ <- result.all %>%
  mutate(location = "Shenzhen")

load("Results/SY_best_worst_Sens.rda")
result.SY <- result.all %>%
  mutate(location = "Shiyan")

result.all <- rbind(result.China, result.SH, result.SZ, result.SY) %>%
  mutate(Scenario = factor(Scenario, levels = c("baseline",
                                                "mild0.5625",
                                                "R05",
                                                "R010",
                                                "VacOptimistic",
                                                "VacPessimistic",
                                                "ChildSus", 
                                                "AsympInfectivity",
                                                "SelfIsolation", 
                                                "home.h5",
                                                "home.h2"
  )))


result.all.best.worst <- result.all %>%
  mutate(IFR = TotalDeaths/TotalCases*1000, Mort.rate = TotalDeaths/500000*100000) %>%
  group_by(repi, Scenario, location) %>%
  summarize(Case.median = median(TotalCases), 
            death.median = median(TotalDeaths),
            IFR.prob = mean(IFR <= 1),
            Mort.rate.prob = mean(Mort.rate <= 14.3),
            mort.rate.median = median(Mort.rate),
            mort.rate.low = quantile(Mort.rate, 0.025),
            mort.rate.high = quantile(Mort.rate, 0.975)) %>%
  left_join(Var.scenario) 

result.all %>%
  mutate(IFR = TotalDeaths/TotalCases*1000, Mort.rate = TotalDeaths/500000*100000) %>%
  group_by(repi, Scenario, location) %>%
  summarize(Case.median = median(TotalCases), 
            death.median = median(TotalDeaths),
            IFR.prob = mean(IFR <= 1),
            Mort.rate.prob = mean(Mort.rate <= 14.3),
            mort.rate.median = median(Mort.rate),
            mort.rate.low = quantile(Mort.rate, 0.025),
            mort.rate.high = quantile(Mort.rate, 0.975)) %>%
  left_join(Var.scenario) %>%
  mutate(panel = paste(location, scenario, sep = ", ")) %>%
  mutate(panel = factor(panel, levels = c("China, Best-case", "Shanghai, Best-case","Shenzhen, Best-case","Shiyan, Best-case", "China, Worst-case", "Shanghai, Worst-case", "Shenzhen, Worst-case", "Shiyan, Worst-case"))) %>%
  ggplot(aes(x = Scenario, y = mort.rate.median, col = scenario, shape = Scenario)) +
  geom_point() +
  geom_errorbar(aes(ymin = mort.rate.low, ymax = mort.rate.high), width = 0.3) +
  facet_wrap(~panel, scales = "free", labeller = as_labeller(location_labeller), ncol = 4) +
  theme_cowplot() +
  xlab("Scenarios") +
  ylab("Mortality rate (1/100,000)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0)) +
  guides(col = "none", shape = "none") +
  scale_shape_manual(values=1:11) +
  scale_x_discrete(labels=c("baseline" = "Baseline",
                            "mild0.5625" = "75% testing",
                            "home.h5" = "5*Hosp. Mort.",
                            "home.h2" = "2*Hosp. Mort.",
                            "VacOptimistic" = "Optimistic VE",
                            "VacPessimistic" = "Pessimistic VE",
                            "ChildSus" = "Lower Child. Sus.", 
                            "AsympInfectivity" = "Lower Asymp. Inf.",
                            "SelfIsolation" = "No Self-Isolation",
                            "R05" = expression("R"["0"]~"= 5"),
                            "R010" = expression("R"["0"]~"= 10"))) +
  geom_hline(yintercept = 14.3, col = "gray70", linetype = "dashed") +
  scale_color_aaas()
# ggsave("Figures/Fig3_best_worst_sensitivity.pdf", width = 11, height = 8)




#========================================================================
#                           Figure 4
#========================================================================
par.sample.original <- read_csv("Data/LHS_sample_for_local_sen_original.csv") %>%
  mutate(repi = 1:100)
par.sample.China <- read_csv("Data/LHS_sample_for_local_sen_China.csv") %>%
  mutate(repi = 1:100, Location = "China")
par.sample.SH <- read_csv("Data/LHS_sample_for_local_sen_SH.csv") %>%
  mutate(repi = 1:100, Location = "Shanghai")
par.sample.SZ <- read_csv("Data/LHS_sample_for_local_sen_SZ.csv") %>%
  mutate(repi = 1:100, Location = "Shenzhen")
par.sample.SY <- read_csv("Data/LHS_sample_for_local_sen_SY.csv") %>%
  mutate(repi = 1:100, Location = "Shiyan")
par.sample.baseline <- rbind(par.sample.China, par.sample.SH, par.sample.SZ, par.sample.SY)

load("Results/China_first100_baseline.rda")
result.China <- result.all %>%
  mutate(Location = "China")

load("Results/SH_first100_baseline.rda")
result.SH <- result.all %>%
  mutate(Location = "Shanghai")

load("Results/SZ_first100_baseline.rda")
result.SZ <- result.all %>%
  mutate(Location = "Shenzhen")

load("Results/SY_first100_baseline.rda")
result.SY <- result.all %>%
  mutate(Location = "Shiyan")

result.baseline <- rbind(result.China, result.SH, result.SZ, result.SY)
result.baseline.agg <- result.baseline %>%
  mutate(IFR = TotalDeaths/TotalCases*1000, Mort.rate = TotalDeaths/500000*100000,
         Location = factor(Location, levels = unique(Location))) %>%
  group_by(repi, Location) %>%
  summarize(Case.median = median(TotalCases), 
            death.median = median(TotalDeaths),
            IFR.prob = mean(IFR <= 1),
            Mort.rate.prob = mean(Mort.rate <= 14.3),
            mort.rate.median = median(Mort.rate)) %>%
  ungroup() %>%
  left_join(par.sample.baseline) %>%
  mutate(Location = factor(Location, levels = unique(Location))) 

result.baseline.agg %>%
  group_by(Location) %>%
  summarise(pass.n = sum(mort.rate.median <= 14.3),
            mean.mort.rate = mean(mort.rate.median))



# y values for the dots on the top of the figure
df.jitter <- data.frame(Location = unique(result.baseline.agg$Location), jitter = c(1.12, 1.14, 1.08, 1.1))

result.all.best.worst.part <- result.all.best.worst %>%
  filter(Scenario %in% c("baseline")) %>%
  select(Scenario, location, mort.rate.median, scenario) %>%
  ungroup() %>%
  mutate(Location = rep(unique(result.baseline.agg$Location), 2)) %>%
  left_join(df.jitter)

Fig4A <- result.baseline.agg %>%
  left_join(df.jitter) %>%
  ggplot(aes(x = mort.rate.median, col = Location)) +
  geom_density() +
  geom_point(aes(x = mort.rate.median, y = jitter)) +
  geom_point(data = result.all.best.worst.part, aes(x = mort.rate.median, y = jitter), col = "black") +
  scale_color_d3() +
  xlab("Mortality rate (1/100,000)") +
  ylab("Density") +
  scale_x_sqrt(expand = c(0,0)) +
  theme_cowplot() +
  theme(legend.position = c(0.7, 0.8)) +
  ggtitle("(A)") +
  #scale_y_continuous(expand = c(0,0)) +
  labs(col = "") +
  geom_vline(xintercept = 14.3, linetype = "dashed") +
  guides(col = "none")





#======== variable importance ======
Places <- unique(result.baseline.agg$Location)

imp_fun <- function(object, newdata) { # for permutation-based VI scores
  predict(object, newdata)$Y_hat
}

baseline.importance <- NULL
for(i in 1:4)
{
  current.dat <- result.baseline.agg %>% 
    filter(Location == Places[i])
  
  current.model <- GP_fit(as.matrix(par.sample.original[,1:8]), current.dat$mort.rate.median)
  p1 <- vip(current.model, method = "permute", train =  as.matrix(par.sample.original[,1:8]), target = current.dat$mort.rate.median, metric = "rmse", pred_wrapper = imp_fun, nsim = 10)
  current.importance <- p1$data
  current.importance$Location = Places[i]
  
  baseline.importance <- rbind(baseline.importance, current.importance)
}


Fig4B <- baseline.importance %>%
  mutate(Variable = factor(Variable, levels = rev(c("Vac_70above","Antiviral",  "ICU", "Vac60_69", "Vac20_59", "Mask", "Vac0_19", "Hospital")))) %>%
  mutate(Location = factor(Location, levels = rev(Places))) %>%
  ggplot(aes(x = Variable, y = Importance, fill = Location)) +
  geom_bar(stat = "identity", width = 0.8, position = "dodge") +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 14),
        legend.position = c(0.6, 0.2)) +
  coord_flip() +
  ylab("Permultation importance") +
  xlab("") +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = rev(pal_d3()(4))) +
  labs(fill = "") +
  ggtitle("(B)") +
  scale_x_discrete(labels = rev(c("ΔVac. 70above","Antiviral",  "ICU", "ΔVac. 60-69", "ΔVac. 20-59", "Mask", "ΔVac. 0-19", "Hospital")))

# save_plot("Figures/Fig4_full_space.pdf", plot_grid(Fig4A, Fig4B), base_height = 5, base_width = 10)




#========================================================================
#                           Figure 5
#========================================================================
par.sample.original <- read_csv("Data/LHS_sample_for_local_sen_original_cut.csv")
par.sample.nocut <- read_csv("Data/LHS_sample_for_local_sen_original.csv")

par.sample.China <- read_csv("Data/LHS_China_cut_baseline.csv") %>%
  mutate(repi = 1:100, Location = "China, baseline")
par.sample.SH <- read_csv("Data/LHS_SH_cut_baseline.csv") %>%
  mutate(repi = 1:100, Location = "Shanghai, baseline")
par.sample.SZ <- read_csv("Data/LHS_sample_for_local_sen_SZ.csv") %>%
  mutate(repi = 1:100, Location = "Shenzhen, baseline")
par.sample.SY <- read_csv("Data/LHS_SY_cut_baseline.csv") %>%
  mutate(repi = 1:100, Location = "Shiyan, baseline")
par.sample.cut <- rbind(par.sample.China, par.sample.SH,  par.sample.SZ,  par.sample.SY)

load("Results/China_cut100_baseline.rda")
result.China <- result.all %>%
  mutate(Location = "China, baseline")

load("Results/SH_cut100_baseline.rda")
result.SH <- result.all %>%
  mutate(Location = "Shanghai, baseline")

load("Results/SZ_first100_baseline.rda")
result.SZ <- result.all %>%
  mutate(Location = "Shenzhen, baseline")

load("Results/SY_cut100_baseline.rda")
result.SY <- result.all %>%
  mutate(Location = "Shiyan, baseline")

result.cut <- rbind(result.China, result.SH, result.SZ, result.SY)
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
 # left_join(par.sample.cut) %>%
  mutate(Location = factor(Location, levels = unique(Location))) 

Places <- unique(result.cut.agg$Location)

Health.resources <- data.frame(Location = Places,
                               Antiviral_low = c(0.577, 0.777, 0, 0.572),
                               # Hospital_low = c(3.27, 5.06, 5.78, 3.27, 6.82)/1000,
                               ICU_low = c(8.33, 12.5, 3.42, 7.38)/100000,
                               Vac_70above_low = c(0.76, 0.979, 0, 0))

imp_fun <- function(object, newdata) { # for permutation-based VI scores
  # predict(object, newdata)$Y_hat
  predict(object, newdata)
}



set.seed(20230107)
cut.importance <- NULL
for(i in 1:4)
{
  current.dat <- result.cut.agg %>% 
    filter(Location == Places[i])
  
  if(i != 3)
  {
    current.model <- GauPro::GauPro(as.matrix(par.sample.original), current.dat$mort.rate.median)
    p1 <- vip(current.model, method = "permute", train =  as.matrix(par.sample.original), target = current.dat$mort.rate.median, metric = "rmse", pred_wrapper = imp_fun, nsim = 10)
    current.importance <- p1$data
    current.importance$Location = Places[i]
  }
  
  if(i == 3)
  {
    current.model <- GauPro::GauPro(as.matrix(par.sample.nocut), current.dat$mort.rate.median)
    p1 <- vip(current.model, method = "permute", train =  as.matrix(par.sample.nocut), target = current.dat$mort.rate.median, metric = "rmse", pred_wrapper = imp_fun, nsim = 10)
    current.importance <- p1$data
    current.importance$Location = Places[i]
  }
  
  
  cut.importance <- rbind(cut.importance, current.importance)
}

cut.importance.top3 <- cut.importance %>% 
  arrange(Location,desc(Importance)) %>% 
  group_by(Location) %>% 
  filter(row_number() <= 3) %>%
  ungroup()

cut.importance.top3



GP.prediction.cut <- list()
for(i in 1:4)
{
  current.dat <- result.cut.agg %>% 
    ungroup() %>%
    filter(Location == Places[i])
  
  cat(i,  "\n")
  
  importance.var <- cut.importance.top3 %>%
    filter(Location == Places[i]) %>%
    pull(Variable)
  
  prediction <- expand.grid(A = seq(0, 1, length.out = 100), 
                            B = seq(0, 1, length.out = 100), 
                            C = seq(0, 1, length.out = 100))
  colnames(prediction) <- importance.var
  
  if(i != 3)
  {
    current.model <- GauPro::GauPro(as.matrix(par.sample.original)[, importance.var], current.dat$mort.rate.median)
  }
  
  if(i == 3)
  {
    current.model <- GauPro::GauPro(as.matrix(par.sample.nocut)[, importance.var], current.dat$mort.rate.median)
  }
  
  current.prediction <- predict(current.model, as.matrix(prediction))
  
  current.prediction.dt <- prediction %>%
    mutate(Predicted = current.prediction,
           Location = Places[i])  %>%
    left_join(Health.resources) %>%
    mutate(ICU = ICU*(48/100000 - ICU_low) + ICU_low)
  
  if("Antiviral" %in% importance.var) 
  {
    current.prediction.dt <- current.prediction.dt %>%
      mutate(Antiviral = Antiviral*(1-Antiviral_low) + Antiviral_low)
  }
  
  if("Vac_70above" %in% importance.var) 
  {
    current.prediction.dt <- current.prediction.dt %>%
      mutate(Vac_70above = Vac_70above*(1- Vac_70above_low) +  Vac_70above_low)
  }
  


  
  GP.prediction.cut[[i]] <- current.prediction.dt
}




Fig5A <- GP.prediction.cut[[1]] %>%
  mutate(ICU = ICU*100000) %>%
  group_by(Antiviral, Vac_70above) %>%
  summarize(minICU = min(ICU[Predicted <= 14.3])) %>%
  ggplot(aes(x = Vac_70above, y = Antiviral, fill = minICU)) +
  geom_tile() +
  #  facet_wrap(~Location, scales = "free") +
  geom_contour(aes(z = minICU), col = "black", breaks = seq(4, 50, 2)) + 
  geom_label_contour(aes(z = minICU), stroke = 0.2, breaks = seq(4, 50, 2), skip = 1, label.placer = label_placement_random(seed = 1)) +
  theme_cowplot() +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 14, face = "bold")) +
  scale_fill_material("blue") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0.0)) +
  guides(fill = "none")+
  xlab("ΔVac. 70+") +
  ylab("Antiviral coverage") +
  labs(fill = "ICU") +
  ggtitle("(A) China, reduced")





Fig5B <- GP.prediction.cut[[2]] %>%
  mutate(ICU = ICU*100000) %>%
  group_by(Antiviral, Vac60_69) %>%
  summarize(minICU = min(ICU[Predicted <= 14.3])) %>%
  ggplot(aes(x = Vac60_69, y = Antiviral, fill = minICU)) +
  geom_tile() +
  #  facet_wrap(~Location, scales = "free") +
  geom_contour(aes(z = minICU), col = "black", breaks = seq(4, 50, 2)) + 
  geom_label_contour(aes(z = minICU), stroke = 0.2, breaks = seq(4, 50, 2), skip = 1, label.placer = label_placement_random(seed = 24)) +
  theme_cowplot() +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 14, face = "bold")) +
  scale_fill_material("blue") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0.0)) +
  guides(fill = "none")+
  xlab("ΔVac. 60-69") +
  ylab("Antiviral coverage") +
  labs(fill = "ICU") +
  ggtitle("(B) Shanghai, reduced")



Fig5C <- GP.prediction.cut[[3]] %>%
  mutate(ICU = ICU*100000) %>%
  group_by(Antiviral, Vac_70above) %>%
  summarize(minICU = min(ICU[Predicted <= 14.3])) %>%
  ggplot(aes(x = Vac_70above, y = Antiviral, fill = minICU)) +
  geom_tile() +
  #  facet_wrap(~Location, scales = "free") +
  geom_contour(aes(z = minICU), col = "black", breaks = seq(4, 50, 2)) + 
  geom_label_contour(aes(z = minICU), stroke = 0.2, breaks = seq(4, 50, 2), skip = 1) +
  theme_cowplot() +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 14, face = "bold")) +
  scale_fill_material("blue") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0.0)) +
  guides(fill = "none")+
  xlab("ΔVac. 70+") +
  ylab("Antiviral coverage") +
  labs(fill = "ICU") +
  ggtitle("(C) Shenzhen")



Fig5D <- GP.prediction.cut[[4]] %>%
  mutate(ICU = ICU*100000) %>%
  group_by(Antiviral, Vac_70above) %>%
  summarize(minICU = min(ICU[Predicted <= 14.3])) %>%
  ggplot(aes(x = Vac_70above, y = Antiviral, fill = minICU)) +
  geom_tile() +
  #  facet_wrap(~Location, scales = "free") +
  geom_contour(aes(z = minICU), col = "black", breaks = seq(4, 50, 2)) + 
  geom_label_contour(aes(z = minICU), stroke = 0.2, breaks = seq(4, 50, 2), skip = 1) +
  theme_cowplot() +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 14, face = "bold")) +
  scale_fill_material("blue") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0.0)) +
  guides(fill = "none")+
  xlab("ΔVac. 70+") +
  ylab("Antiviral coverage") +
  labs(fill = "ICU") +
  ggtitle("(D) Shiyan, reduced")

# save_plot("Figures/Fig5_heatmaps.pdf", plot_grid(Fig4A, Fig4B, Fig4C, Fig4D, ncol = 2), base_width = 8, base_height = 8)









#========================================================================
#                           Figure S1
#========================================================================
Age.structure <- read.csv("Data/Age_structure.csv")

City_names <- c(
  `China2020` = "(A) China",
  `Shanghai2020` = "(B) Shanghai",
  `Shenzhen2020` = "(C) Shenzhen",
  `Shiyan2020` = "(D) Shiyan"
)

Age.structure %>%
  select(Age, Shenzhen2020, Shanghai2020, Shiyan2020, China2020) %>%
  gather(Location, Prop, Shenzhen2020:China2020) %>%
  mutate(Age = factor(Age, levels = Age.structure$Age)) %>%
  ggplot(aes(x = Age, y = Prop, fill = Location)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Location, labeller = as_labeller(City_names)) +
  guides(fill = FALSE) +
  theme_cowplot() +
  xlab("Age group") +
  ylab("Proportion") +
  theme(strip.background = element_rect(fill = NA), strip.text = element_text(hjust = 0, size = 14), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_npg()

# ggsave("Figures/FigS1_age_structure.pdf", width = 8, height = 8)







#========================================================================
#                           Figure S2
#========================================================================
China.v <- read.csv("Data/China_VacCoverage.csv") %>%
  gather(Dose, Prop, V0:V3) %>% 
  mutate(Location = "China")

Shenzhen.v <- read.csv("Data/Shenzhen_VacCoverage.csv") %>%
  gather(Dose, Prop, V0:V3) %>% 
  mutate(Location = "Shenzhen")

Shiyan.v <- read.csv("Data/Shiyan_VacCoverage.csv") %>%
  gather(Dose, Prop, V0:V3) %>% 
  mutate(Location = "Shiyan")

Shanghai.v <- read.csv("Data/Shanghai_VacCoverage.csv") %>%
  gather(Dose, Prop, V0:V3) %>% 
  mutate(Location = "Shanghai")

vac.data <- rbind(China.v, Shenzhen.v, Shiyan.v, Shanghai.v)

City_names <- c(
  `China` = "(A) China",
  `Shanghai` = "(B) Shanghai",
  `Shenzhen` = "(C) Shenzhen",
  `Shiyan` = "(D) Shiyan"
)

vac.data %>%
  mutate(Age = factor(Age, levels = Age.structure$Age)) %>%
  ggplot(aes(x = Age, y = Prop, fill = Dose)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Location, labeller = as_labeller(City_names)) +
  theme_cowplot() + 
  theme(strip.background = element_rect(fill = NA), strip.text = element_text(hjust = 0, size = 14), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  xlab("Age group") +
  ylab("Proportion") +
  scale_fill_manual(breaks = c("V0", "V1", "V2", "V3"), labels = c("None", "One dose", "Two doses", "Booster"), values = brewer_pal(type = "div")(4)) +
  labs(fill = "")

# ggsave("Figures/FigS2_vac_coverage.pdf", width = 8, height = 8)







#========================================================================
#                           Figure S3
#========================================================================
load('Results/China_first100_baseline.rda')
result.500k <- result.all %>%
  mutate(pop.size = 500000) %>%
  filter(repi <= 3)

load("Results/China_first100_baseline_1m.rda")
result.1m <- result.all%>%
  mutate(pop.size = 1000000)

load("Results/China_first100_baseline_2m.rda")
result.2m <- result.all%>%
  mutate(pop.size = 2000000)

load("Results/China_first100_baseline_5m.rda")
result.5m <- result.all%>%
  mutate(pop.size = 5000000)

result.all <- rbind(result.500k, result.1m, result.2m, result.5m) %>%
  mutate(mort.rate = TotalDeaths/pop.size*100000)

result.all %>%
  group_by(repi, pop.size) %>%
  summarize(mort.rate.median = median(mort.rate), 
            mort.rate.low = quantile(mort.rate, 0.025),
            mort.rate.high = quantile(mort.rate, 0.975)) %>%
  ggplot(aes(x = repi, y = mort.rate.median, fill = as.factor(pop.size))) +
  geom_bar(stat = "identity", position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = mort.rate.low, ymax = mort.rate.high), width = 0.2, position = position_dodge(width=0.9)) +
  scale_fill_d3(labels = c("500k", "1 million", "2 million", "5 million"))+
  theme_cowplot() +
  xlab("Random intervention parameter set") +
  ylab("Mortality rate (1/100,000)") +
  labs(fill = "No. of agents")
# ggsave("Figures/FigS3_number_of_agents.pdf", width = 8, height = 4.5)






#========================================================================
#                           Figure S4
#========================================================================
ZJJ.contact <- read_csv("./Data/ContactMat_ZJJ.csv") %>%
  as.data.table()

FigS4A <- ZJJ.contact %>%
  mutate(Age_Individual = factor(Age_Individual, levels = unique(Age_Individual)),
         Age_Contact = factor(Age_Contact, levels = unique(Age_Individual))) %>%
  ggplot(aes(x = Age_Contact, y = Age_Individual, fill = Contact)) +
  geom_tile()+
  scale_fill_distiller(direction = 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("Age of contacts") +
  ylab("Age of individual") +
  labs(fill = "Number of\ncontacts") +
  coord_equal() +
  ggtitle("(A) All settings") +
  theme(legend.position = "bottom")

FigS4B <- ZJJ.contact %>%
  mutate(Age_Individual = factor(Age_Individual, levels = unique(Age_Individual)),
         Age_Contact = factor(Age_Contact, levels = unique(Age_Individual))) %>%
  ggplot(aes(x = Age_Contact, y = Age_Individual, fill = Contact.Home)) +
  geom_tile()+
  scale_fill_distiller(direction = 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("Age of contacts") +
  ylab("Age of individual") +
  labs(fill = "Number of\ncontacts") +
  coord_equal() +
  ggtitle("(B) Home settings") +
  theme(legend.position = "bottom")

# save_plot("Figures/FigS4_contact_mat.pdf", plot_grid(FigS4A, FigS4B), base_width = 8, base_height = 5)





#========================================================================
#                           Figure S5
#========================================================================
par.sample.original <- read_csv("Data/LHS_sample_for_local_sen_original.csv")
Places <- c( "China", "Shanghai", "Shenzhen",  "Shiyan")


#======== prediction ==========
GP.prediction <- NULL
Fold <- rep(1:10, each = 10)
for(i in 1:length(Places))
{
  current.dat <- result.baseline.agg %>%     # result.baseline.agg is from the code for Figure 3
    ungroup() %>%
    filter(Location == Places[i])
  
  for(j in 1:10)
  {
    cat(i, j, "\n")
    current.model <- GauPro::GauPro(as.matrix(par.sample.original)[Fold != j,], current.dat$mort.rate.median[Fold != j])
    
    current.predict <- data.frame(Observed = current.dat$mort.rate.median[Fold == j],
                                  Predicted = predict(current.model, as.matrix(par.sample.original)[Fold == j,]),
                                  Location = Places[i], 
                                  Fold = j)
    GP.prediction <- rbind(GP.prediction, current.predict)
  }
}

GP.prediction %>%
  mutate(Location = rep(c("(A) China", "(B) Shanghai", "(C) Shenzhen", "(D) Shiyan"), each = 100)) %>%
  ggplot(aes(x = Observed, y = Predicted, col = as.factor(Fold))) +
  geom_point() +
  geom_abline(slope = 1, col = "gray70") +
  facet_wrap(~Location, scales = "free")+
  theme_cowplot() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 14)) +
  guides(col = FALSE) +
  xlab("Observed median mortality rate (1/100,000)") +
  ylab("Predicted median mortality rate (1/100,000)")
# ggsave("Figures/FigS5_GP_validation_full_space.pdf", width = 6, height = 6)








#========================================================================
#                           Figure S6
#========================================================================
load("Results/China_cut100_baseline.rda")
result.China1 <- result.all %>%
  mutate(Location = "China, baseline, cut")

load("Results/China_cut100_VacOpt.rda")
result.China2 <- result.all%>%
  mutate(Location = "China, optimistic VE, cut")

load("Results/SH_cut100_baseline.rda")
result.SH1 <- result.all%>%
  mutate(Location = "Shanghai, baseline, cut")

load("Results/SH_cut100_VacOpt.rda")
result.SH2 <- result.all%>%
  mutate(Location = "Shanghai, optimistic VE, cut")

load("Results/SY_cut100_baseline.rda")
result.SY <- result.all%>%
  mutate(Location = "Shiyan, baseline, cut")

result.cut <- rbind(result.China1, result.China2, result.SH1, result.SH2, result.SY)
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
#  left_join(par.sample.cut) %>%
  mutate(Location = factor(Location, levels = unique(Location))) 


result.cut.agg %>%
  group_by(Location) %>%
  summarise(mean(mort.rate.median <= 14.3))


 df.jitter <- data.frame(Location = unique(result.cut.agg$Location), jitter = seq(2.2, 2.05, length.out = 5))


result.cut.agg %>%
  left_join(df.jitter) %>%
  ggplot(aes(x = mort.rate.median, col = Location)) +
  geom_density() +
  geom_point(aes(x = mort.rate.median, y = jitter), size = 0.5) +
  #  scale_color_d3() +
  xlab("Mortality rate (1/100,000)") +
  ylab("Density") +
  scale_x_sqrt() +
  theme_cowplot() +
  scale_color_d3() +
  #  theme(legend.position = c(0.7, 0.8)) +
  #scale_y_continuous(expand = c(0,0)) +
  labs(col = "") +
  geom_vline(xintercept = 14.3, linetype = "dashed") 
ggsave("Figures/FigS6_density_plot_cut.pdf", width = 8, height = 4)




#========================================================================
#                           Figure S7
#========================================================================
par.sample.original <- read_csv("Data/LHS_sample_for_local_sen_original_cut.csv") %>%
  mutate(repi = 1:100)

Places <- unique(result.cut.agg$Location)
GP.prediction <- NULL
Fold <- rep(1:10, each = 10)
for(i in 1:length(Places))
{
  current.dat <- result.cut.agg %>% 
    ungroup() %>%
    filter(Location == Places[i])
  
  for(j in 1:10)
  {
    cat(i, j, "\n")
    # current.model <- GP_fit(as.matrix(par.sample.original)[Fold != j,], current.dat$mort.rate.median[Fold != j])
      current.model <- GauPro::GauPro(as.matrix(par.sample.original[,1:8])[Fold != j, ], current.dat$mort.rate.median[Fold != j])
      
      current.predict <- data.frame(Observed = current.dat$mort.rate.median[Fold == j],
                                    Predicted = predict(current.model, as.matrix(par.sample.original[,1:8])[Fold == j, ]),
                                    Location = Places[i], 
                                    Fold = j)
    
    GP.prediction <- rbind(GP.prediction, current.predict)
  }
}

GP.prediction %>%
  mutate(Location = rep(paste("(", LETTERS[1:5], ") ", Places, sep = ""), each = 100)) %>%
  ggplot(aes(x = Observed, y = Predicted, col = as.factor(Fold))) +
  geom_point() +
  geom_abline(slope = 1, col = "gray70") +
  facet_wrap(~Location, scales = "free")+
  theme_cowplot() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 14)) +
  guides(col = FALSE) +
  xlab("Observed median mortality rate (1/100,000)") +
  ylab("Predicted median mortality rate (1/100,000)")
# ggsave("Figures/FigS7_Cut_GPprediction.pdf", width = 10, height = 7.5)




#========================================================================
#                           Figure S8
#========================================================================
par.sample.original.cut <- read_csv("Data/LHS_sample_for_local_sen_original_cut.csv")

par.sample.China1 <- read_csv("Data/LHS_China_cut_baseline.csv") %>%
  mutate(repi = 1:100, Location = "China, baseline, cut")
par.sample.China2 <- read_csv("Data/LHS_China_cut_VacOpt.csv") %>%
  mutate(repi = 1:100, Location = "China, optimistic VE, cut")
par.sample.SH1 <- read_csv("Data/LHS_SH_cut_baseline.csv") %>%
  mutate(repi = 1:100, Location = "Shanghai, baseline, cut")
par.sample.SH2 <- read_csv("Data/LHS_SH_cut_VacOpt.csv") %>%
  mutate(repi = 1:100, Location = "Shanghai, optimistic VE, cut")
par.sample.SY1 <- read_csv("Data/LHS_SY_cut_baseline.csv") %>%
  mutate(repi = 1:100, Location = "Shiyan, baseline, cut")
par.sample.cut <- rbind(par.sample.China1, par.sample.China2, par.sample.SH1, par.sample.SH2, par.sample.SY1)

result.cut.agg <- result.cut.agg %>%
  left_join(par.sample.cut) %>%
  mutate(Location = factor(Location, levels = unique(Location))) 

Places <- unique(result.cut.agg$Location)

imp_fun <- function(object, newdata) { # for permutation-based VI scores
  # predict(object, newdata)$Y_hat
  predict(object, newdata)
}



set.seed(20230107)
cut.importance <- NULL
for(i in 1:5)
{
  current.dat <- result.cut.agg %>% 
    filter(Location == Places[i])
  
  current.model <- GauPro::GauPro(as.matrix(par.sample.original.cut), current.dat$mort.rate.median)
  p1 <- vip(current.model, method = "permute", train =  as.matrix(par.sample.original.cut), target = current.dat$mort.rate.median, metric = "rmse", pred_wrapper = imp_fun, nsim = 10)
  current.importance <- p1$data
  current.importance$Location = Places[i]
  
  cut.importance <- rbind(cut.importance, current.importance)
}



cut.importance %>%
  mutate(Variable = factor(Variable, levels = rev(c("Vac_70above","Antiviral",  "ICU", "Vac60_69", "Vac20_59", "Mask", "Vac0_19", "Hospital")))) %>%
  mutate(Location = factor(Location, levels = rev(Places))) %>%
  ggplot(aes(x = Variable, y = Importance, fill = Location)) +
  geom_bar(stat = "identity", width = 0.8, position = "dodge") +
  #  facet_wrap(~Location, scales = "free", labeller = location_labeller)+
  theme_cowplot() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 14),
        legend.position = c(0.6, 0.3)) +
  coord_flip() +
  ylab("Permultation importance") +
  xlab("") +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = rev(pal_d3()(5))) +
  labs(fill = "") +
  # guides(fill = "none") +
  scale_x_discrete(labels = rev(c("ΔVac. 70above","Antiviral",  "ICU", "ΔVac. 60-69", "ΔVac. 20-59", "Mask", "ΔVac. 0-19", "Hospital")))
# ggsave("Figures/FigS8_variable_importance_cut.pdf", width = 8, height = 4)







#========================================================================
#                           Figure S9
#========================================================================
load("Results/China_cut100_baseline.rda")
result.China1 <- result.all %>%
  mutate(Location = "China, baseline, cut")

load("Results/China_cut100_VacOpt.rda")
result.China2 <- result.all%>%
  mutate(Location = "China, optimistic VE, cut")

load("Results/SH_cut100_baseline.rda")
result.SH1 <- result.all%>%
  mutate(Location = "Shanghai, baseline, cut")

load("Results/SH_cut100_VacOpt.rda")
result.SH2 <- result.all%>%
  mutate(Location = "Shanghai, optimistic VE, cut")

load("Results/SZ_first100_baseline.rda")
result.SZ1 <- result.all%>%
  mutate(Location = "Shenzhen, baseline")

load("Results/SZ_first100_VacOpt.rda")
result.SZ2 <- result.all%>%
  mutate(Location = "Shenzhen, optimistic VE")

load("Results/SZ_first100_VacPes.rda")
result.SZ3 <- result.all%>%
  mutate(Location = "Shenzhen, pessimistic VE")

load("Results/SY_cut100_baseline.rda")
result.SY1 <- result.all%>%
  mutate(Location = "Shiyan, baseline, cut")

load("Results/SY_first100_VacOpt.rda")
result.SY2 <- result.all%>%
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
  #  left_join(par.sample.cut) %>%
  mutate(Location = factor(Location, levels = unique(Location))) 

par.sample.original <- read_csv("Data/LHS_sample_for_local_sen_original_cut.csv")
par.sample.nocut <- read_csv("Data/LHS_sample_for_local_sen_original.csv")






imp_fun <- function(object, newdata) { # for permutation-based VI scores
  # predict(object, newdata)$Y_hat
  predict(object, newdata)
}



set.seed(20230107)
cut.importance <- NULL
Places <- unique(result.cut.agg$Location)
for(i in 1:9)
{
  print(i)
  current.dat <- result.cut.agg %>% 
    filter(Location == Places[i])
  
  if(!i %in% c(5,6,7,9))
  {
    current.model <- GauPro::GauPro(as.matrix(par.sample.original), current.dat$mort.rate.median)
    p1 <- vip(current.model, method = "permute", train =  as.matrix(par.sample.original), target = current.dat$mort.rate.median, metric = "rmse", pred_wrapper = imp_fun, nsim = 10)
    current.importance <- p1$data
    current.importance$Location = Places[i]
  }
  
  if(i %in% c(5,6,7,9))
  {
    current.model <- GauPro::GauPro(as.matrix(par.sample.nocut), current.dat$mort.rate.median)
    p1 <- vip(current.model, method = "permute", train =  as.matrix(par.sample.nocut), target = current.dat$mort.rate.median, metric = "rmse", pred_wrapper = imp_fun, nsim = 10)
    current.importance <- p1$data
    current.importance$Location = Places[i]
  }
  
  
  cut.importance <- rbind(cut.importance, current.importance)
}

cut.importance.top3 <- cut.importance %>% 
  arrange(Location,desc(Importance)) %>% 
  group_by(Location) %>% 
  filter(row_number() <= 3) %>%
  ungroup()

cut.importance.top3


cut.importance.top3
#======== prediction ==========
GP.prediction <- NULL
Fold <- rep(1:10, each = 10)
for(i in 1:9)
{
  current.dat <- result.cut.agg %>% 
    ungroup() %>%
    filter(Location == Places[i])
  
  importance.var <- cut.importance.top3 %>%
    filter(Location == Places[i]) %>%
    pull(Variable)
  
  for(j in 1:10)
  {
    cat(i, j, "\n")
    # current.model <- GP_fit(as.matrix(par.sample.original)[Fold != j,], current.dat$mort.rate.median[Fold != j])
    
    if(!i %in% c(5,6,7,9))
    {
      current.model <- GauPro::GauPro(as.matrix(par.sample.original)[Fold != j, importance.var], current.dat$mort.rate.median[Fold != j])
      
      current.predict <- data.frame(Observed = current.dat$mort.rate.median[Fold == j],
                                    Predicted = predict(current.model, as.matrix(par.sample.original)[Fold == j, importance.var]),
                                    Location = Places[i], 
                                    Fold = j)
    }
    if(i %in% c(5,6,7,9))
    {
      current.model <- GauPro::GauPro(as.matrix(par.sample.nocut)[Fold != j, importance.var], current.dat$mort.rate.median[Fold != j])
      
      current.predict <- data.frame(Observed = current.dat$mort.rate.median[Fold == j],
                                    Predicted = predict(current.model, as.matrix(par.sample.nocut)[Fold == j, importance.var]),
                                    Location = Places[i], 
                                    Fold = j)
    }
    
    
    
    GP.prediction <- rbind(GP.prediction, current.predict)
  }
}


# save(GP.prediction, file = 'GP_prediction_10foldcv_cut.rda')

GP.prediction %>%
  group_by(Location) %>%
  summarize(rho = cor(Observed, Predicted))


GP.prediction %>%
  mutate(Location = rep(paste("(", LETTERS[1:9], ") ", Places, sep = ""), each = 100)) %>%
  ggplot(aes(x = Observed, y = Predicted, col = as.factor(Fold))) +
  geom_point() +
  geom_abline(slope = 1, col = "gray70") +
  facet_wrap(~Location, scales = "free")+
  theme_cowplot() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 14)) +
  guides(col = FALSE) +
  xlab("Observed median mortality rate (1/100,000)") +
  ylab("Predicted median mortality rate (1/100,000)")
ggsave("Figures/FigS9_Cut_GPprediction_top3.pdf", width = 10, height = 10)











#========================================================================
#                           Figure ST1.1
#========================================================================
par_names <- list(
  'Mask'="(A) Mask",
  'Hospital'="(B) Hospital",
  'ICU'="(C) ICU",
  'Vac0_19'="(D) ΔVac. 0-19",
  'Vac20_59'="(E) ΔVac. 20-59",
  'Vac60_69'="(F) ΔVac. 60-69",
  'Vac_70above'="(G) ΔVac. 70above",
  'Antiviral'="(H) Antiviral"
)

par_labeller <- function(variable,value){
  return(par_names[value])
}

par.list <- data.frame(repi = 1:8, par.name = colnames(par.sample.original)[1:8])

# read best-worst change
best.worst.change <- NULL

load("Results/China_best_worst_change.rda")
result.all <- result.all %>%
  mutate(Scenario = rep(rep(c("baseline", "optimistic VE"), each = 48), 8), Location = "China") 
best.worst.change <- rbind(best.worst.change, result.all)


load("Results/SH_best_worst_change.rda")
result.all <- result.all %>%
  mutate(Scenario = rep(rep(c("baseline", "optimistic VE"), each = 48), 8), Location = "Shanghai")
best.worst.change <- rbind(best.worst.change, result.all)

load("Results/SZ_best_worst_change.rda")
result.all <- result.all %>%
  mutate(Scenario = rep(rep(c("baseline", "optimistic VE", "pessimistic VE"), each = 48), 8), Location = "Shenzhen") 
best.worst.change <- rbind(best.worst.change, result.all)


load("Results/SY_best_worst_change.rda")
result.all <- result.all %>%
  mutate(Scenario = rep(rep(c("baseline", "optimistic VE"), each = 48), 8), Location = "Shiyan")
best.worst.change <- rbind(best.worst.change, result.all)



best.worst.change.agg <- best.worst.change %>%
  left_join(par.list) %>%
  mutate(IFR = TotalDeaths/TotalCases*1000, Mort.rate = TotalDeaths/500000*100000) %>%
  group_by(Location, Scenario, par.name) %>%
  summarize(Case.median = median(TotalCases), 
            death.median = median(TotalDeaths),
            IFR.prob = mean(IFR <= 1),
            Mort.rate.prob = mean(Mort.rate <= 14.3)) %>%
  ungroup() %>%
  mutate(par.name = factor(par.name, levels = colnames(par.sample.original)[1:8])) %>%
  arrange(par.name)



finer.result <- NULL
load("Results/China_finer_baseline.rda")
par.sample <- read.csv("Data/par_best_worst_finer.csv") %>%
  filter(Location == "China", Scenario == "baseline") %>%
  mutate(repi = 1:n())
current.par <- unique(par.sample$Par)

all.result <- data.frame(Par = rep(colnames(par.sample)[1:8], each = 11),
                         value = rep(seq(0, 1, 0.1), 8),
                         p.safe.exit = NA)
# all.result$p.safe.exit[all.result$value == 0 & !all.result$Par %in% current.par] <- 1
all.result$p.safe.exit[all.result$value == 0] <- best.worst.change.agg$Mort.rate.prob[best.worst.change.agg$Location == "China"&best.worst.change.agg$Scenario == "baseline"]
all.result$p.safe.exit[all.result$value == 1] <- 1
all.result$p.safe.exit[!all.result$Par %in% current.par] <- 1
all.result$value[all.result$Par == "Hospital"] <- seq(5.06/1000, 14.4/1000, length.out = 11)
all.result$value[all.result$Par == "ICU"] <- seq(4.37/100000, 48/100000, length.out = 11)

result.agg <- result.all %>%
  mutate(IFR = TotalDeaths/TotalCases*1000, Mort.rate = TotalDeaths/500000*100000) %>%
  group_by(repi)%>%
  summarize(Mort.rate.prob = mean(Mort.rate <= 14.3)) %>%
  left_join(par.sample) %>%
  mutate(Par = factor(Par, levels = colnames(par.sample)[1:8])) %>%
  arrange(Par)

all.result$p.safe.exit[is.na(all.result$p.safe.exit)] <- result.agg$Mort.rate.prob
all.result$Location <- "China"
all.result$Scenario <- "baseline"
finer.result <- rbind(finer.result, all.result)





load("Results/China_finer_VacOpt.rda")
par.sample <- read.csv("Data/par_best_worst_finer.csv") %>%
  filter(Location == "China", Scenario == "VacOpt") %>%
  mutate(repi = 1:n())
current.par <- unique(par.sample$Par)

all.result <- data.frame(Par = rep(colnames(par.sample)[1:8], each = 11),
                         value = rep(seq(0, 1, 0.1), 8),
                         p.safe.exit = NA)
# all.result$p.safe.exit[all.result$value == 0 & !all.result$Par %in% current.par] <- 1
all.result$p.safe.exit[all.result$value == 0] <- best.worst.change.agg$Mort.rate.prob[best.worst.change.agg$Location == "China"&best.worst.change.agg$Scenario == "optimistic VE"]
all.result$p.safe.exit[all.result$value == 1] <- 1
all.result$p.safe.exit[!all.result$Par %in% current.par] <- 1
all.result$value[all.result$Par == "Hospital"] <- seq(5.06/1000, 14.4/1000, length.out = 11)
all.result$value[all.result$Par == "ICU"] <- seq(4.37/100000, 48/100000, length.out = 11)

result.agg <- result.all %>%
  mutate(IFR = TotalDeaths/TotalCases*1000, Mort.rate = TotalDeaths/500000*100000) %>%
  group_by(repi)%>%
  summarize(Mort.rate.prob = mean(Mort.rate <= 14.3)) %>%
  left_join(par.sample) %>%
  mutate(Par = factor(Par, levels = colnames(par.sample)[1:8])) %>%
  arrange(Par)

all.result$p.safe.exit[is.na(all.result$p.safe.exit)] <- result.agg$Mort.rate.prob
all.result$Location <- "China"
all.result$Scenario <- "optimistic VE"
finer.result <- rbind(finer.result, all.result)








load("Results/SH_finer_baseline.rda")
par.sample <- read.csv("Data/par_best_worst_finer.csv") %>%
  filter(Location == "Shanghai", Scenario == "baseline") %>%
  mutate(repi = 1:n())
current.par <- unique(par.sample$Par)

all.result <- data.frame(Par = rep(colnames(par.sample)[1:8], each = 11),
                         value = rep(seq(0, 1, 0.1), 8),
                         p.safe.exit = NA)
# all.result$p.safe.exit[all.result$value == 0 & !all.result$Par %in% current.par] <- 1
all.result$p.safe.exit[all.result$value == 0] <- best.worst.change.agg$Mort.rate.prob[best.worst.change.agg$Location == "Shanghai"&best.worst.change.agg$Scenario == "baseline"]
all.result$p.safe.exit[all.result$value == 1] <- 1
all.result$p.safe.exit[!all.result$Par %in% current.par] <- 1
all.result$value[all.result$Par == "Hospital"] <- seq(5.78/1000, 14.4/1000, length.out = 11)
all.result$value[all.result$Par == "ICU"] <- seq(6.14/100000, 48/100000, length.out = 11)

result.agg <- result.all %>%
  mutate(IFR = TotalDeaths/TotalCases*1000, Mort.rate = TotalDeaths/500000*100000) %>%
  group_by(repi)%>%
  summarize(Mort.rate.prob = mean(Mort.rate <= 14.3)) %>%
  left_join(par.sample) %>%
  mutate(Par = factor(Par, levels = colnames(par.sample)[1:8])) %>%
  arrange(Par)

all.result$p.safe.exit[is.na(all.result$p.safe.exit)] <- result.agg$Mort.rate.prob
all.result$Location <- "Shanghai"
all.result$Scenario <- "baseline"
finer.result <- rbind(finer.result, all.result)





load("Results/SH_finer_VacOpt.rda")
par.sample <- read.csv("Data/par_best_worst_finer.csv") %>%
  filter(Location == "Shanghai", Scenario == "VacOpt") %>%
  mutate(repi = 1:n())
current.par <- unique(par.sample$Par)

all.result <- data.frame(Par = rep(colnames(par.sample)[1:8], each = 11),
                         value = rep(seq(0, 1, 0.1), 8),
                         p.safe.exit = NA)
# all.result$p.safe.exit[all.result$value == 0 & !all.result$Par %in% current.par] <- 1
all.result$p.safe.exit[all.result$value == 0] <- best.worst.change.agg$Mort.rate.prob[best.worst.change.agg$Location == "Shanghai"&best.worst.change.agg$Scenario == "optimistic VE"]
all.result$p.safe.exit[all.result$value == 1] <- 1
all.result$p.safe.exit[!all.result$Par %in% current.par] <- 1
all.result$value[all.result$Par == "Hospital"] <- seq(5.78/1000, 14.4/1000, length.out = 11)
all.result$value[all.result$Par == "ICU"] <- seq(6.14/100000, 48/100000, length.out = 11)

result.agg <- result.all %>%
  mutate(IFR = TotalDeaths/TotalCases*1000, Mort.rate = TotalDeaths/500000*100000) %>%
  group_by(repi)%>%
  summarize(Mort.rate.prob = mean(Mort.rate <= 14.3)) %>%
  left_join(par.sample) %>%
  mutate(Par = factor(Par, levels = colnames(par.sample)[1:8])) %>%
  arrange(Par)

all.result$p.safe.exit[is.na(all.result$p.safe.exit)] <- result.agg$Mort.rate.prob
all.result$Location <- "Shanghai"
all.result$Scenario <- "optimistic VE"
finer.result <- rbind(finer.result, all.result)




all.result <- data.frame(Par = rep(colnames(par.sample)[1:8], each = 11),
                         value = rep(seq(0, 1, 0.1), 8),
                         p.safe.exit = 1)
all.result$value[all.result$Par == "Hospital"] <- seq(3.27/1000, 14.4/1000, length.out = 11)
all.result$value[all.result$Par == "ICU"] <- seq(3.42/100000, 48/100000, length.out = 11)
all.result$Location <- "Shenzhen"
all.result$Scenario <- "baseline"
finer.result <- rbind(finer.result, all.result)


all.result <- data.frame(Par = rep(colnames(par.sample)[1:8], each = 11),
                         value = rep(seq(0, 1, 0.1), 8),
                         p.safe.exit = 1)
all.result$value[all.result$Par == "Hospital"] <- seq(3.27/1000, 14.4/1000, length.out = 11)
all.result$value[all.result$Par == "ICU"] <- seq(3.42/100000, 48/100000, length.out = 11)
all.result$Location <- "Shenzhen"
all.result$Scenario <- "optimistic VE"
finer.result <- rbind(finer.result, all.result)


all.result <- data.frame(Par = rep(colnames(par.sample)[1:8], each = 11),
                         value = rep(seq(0, 1, 0.1), 8),
                         p.safe.exit = 1)
all.result$value[all.result$Par == "Hospital"] <- seq(3.27/1000, 14.4/1000, length.out = 11)
all.result$value[all.result$Par == "ICU"] <- seq(3.42/100000, 48/100000, length.out = 11)
all.result$Location <- "Shenzhen"
all.result$Scenario <- "pessimistic VE"
finer.result <- rbind(finer.result, all.result)





load("Results/SY_finer_baseline.rda")
par.sample <- read.csv("Data/par_best_worst_finer.csv") %>%
  filter(Location == "Shiyan", Scenario == "baseline") %>%
  mutate(repi = 1:n())
current.par <- unique(par.sample$Par)

all.result <- data.frame(Par = rep(colnames(par.sample)[1:8], each = 11),
                         value = rep(seq(0, 1, 0.1), 8),
                         p.safe.exit = NA)
# all.result$p.safe.exit[all.result$value == 0 & !all.result$Par %in% current.par] <- 1
all.result$p.safe.exit[all.result$value == 0] <- best.worst.change.agg$Mort.rate.prob[best.worst.change.agg$Location == "Shiyan"&best.worst.change.agg$Scenario == "baseline"]
all.result$p.safe.exit[all.result$value == 1] <- 1
all.result$p.safe.exit[!all.result$Par %in% current.par] <- 1
all.result$value[all.result$Par == "Hospital"] <- seq(6.82/1000, 14.4/1000, length.out = 11)
all.result$value[all.result$Par == "ICU"] <- seq(5.13/100000, 48/100000, length.out = 11)

result.agg <- result.all %>%
  mutate(IFR = TotalDeaths/TotalCases*1000, Mort.rate = TotalDeaths/500000*100000) %>%
  group_by(repi)%>%
  summarize(Mort.rate.prob = mean(Mort.rate <= 14.3)) %>%
  left_join(par.sample) %>%
  mutate(Par = factor(Par, levels = colnames(par.sample)[1:8])) %>%
  arrange(Par)

all.result$p.safe.exit[is.na(all.result$p.safe.exit)] <- result.agg$Mort.rate.prob
all.result$Location <- "Shiyan"
all.result$Scenario <- "baseline"
finer.result <- rbind(finer.result, all.result)






all.result <- data.frame(Par = rep(colnames(par.sample)[1:8], each = 11),
                         value = rep(seq(0, 1, 0.1), 8),
                         p.safe.exit = 1)
all.result$value[all.result$Par == "Hospital"] <- seq(6.82/1000, 14.4/1000, length.out = 11)
all.result$value[all.result$Par == "ICU"] <- seq(5.13/100000, 48/100000, length.out = 11)
all.result$Location <- "Shiyan"
all.result$Scenario <- "optimistic VE"
finer.result <- rbind(finer.result, all.result)





finer.result %>%
  mutate(Par = factor(Par, levels = c("Mask", "Hospital", "ICU", "Vac0_19",  "Vac20_59", "Vac60_69", "Vac_70above", "Antiviral")),
         Location = factor(Location, levels = unique(finer.result$Location))) %>%
  ggplot(aes(x = value, y = p.safe.exit, col = Scenario, linetype = Scenario)) +
  geom_line() +
  facet_grid(rows = vars(Location), cols = vars(Par), scales = "free_x", labeller = labeller(.cols = par_labeller)) +
  xlab("") +
  ylab("Probability of safe exit") +
  theme_cowplot() +
  geom_hline(yintercept = 0.95, linetype = "dashed", col = "gray") +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 11.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom")
# ggsave("Figures/FigST1.1_change_one_best_to_worst.pdf", width = 13, height = 9)










#========================================================================
#                           ST2
#========================================================================
load("Results/China_first100_VacOpt.rda")
result.China <- result.all %>%
  mutate(Location = "China", Scenario = "VacOptimistic")

load("Results/SH_first100_VacOpt.rda")
result.SH <- result.all %>%
  mutate(Location = "Shanghai", Scenario = "VacOptimistic")

load("Results/SZ_first100_VacOpt.rda")
result.SZ1 <- result.all %>%
  mutate(Location = "Shenzhen", Scenario = "VacOptimistic")

load("Results/SZ_first100_VacPes.rda")
result.SZ2 <- result.all %>%
  mutate(Location = "Shenzhen", Scenario = "VacPessimistic")

load("Results/SY_first100_VacOpt.rda")
result.SY <- result.all %>%
  mutate(Location = "Shiyan", Scenario = "VacOptimistic")
result.sens <- rbind(result.China, result.SH, result.SZ1, result.SZ2, result.SY)

par.sample.original <- read_csv("Data/LHS_sample_for_local_sen_original.csv") %>%
  mutate(repi = 1:100)
par.sample.China <- read_csv("Data/LHS_sample_for_local_sen_China.csv") %>%
  mutate(repi = 1:100, Location = "China")
par.sample.SH <- read_csv("Data/LHS_sample_for_local_sen_SH.csv") %>%
  mutate(repi = 1:100, Location = "Shanghai")
par.sample.SZ <- read_csv("Data/LHS_sample_for_local_sen_SZ.csv") %>%
  mutate(repi = 1:100, Location = "Shenzhen")
par.sample.SY <- read_csv("Data/LHS_sample_for_local_sen_SY.csv") %>%
  mutate(repi = 1:100, Location = "Shiyan")
par.sample.sens <- rbind(par.sample.China, par.sample.SH, par.sample.SZ, par.sample.SY)

result.sens.agg <- result.sens %>%
  mutate(IFR = TotalDeaths/TotalCases*1000, Mort.rate = TotalDeaths/500000*100000,
         Location = factor(Location, levels = unique(Location))) %>%
  group_by(repi, Location, Scenario) %>%
  summarize(Case.median = median(TotalCases), 
            death.median = median(TotalDeaths),
            IFR.prob = mean(IFR <= 1),
            Mort.rate.prob = mean(Mort.rate <= 14.3),
            mort.rate.median = median(Mort.rate)) %>%
  ungroup() %>%
  left_join(par.sample.sens) %>%
  mutate(Location = factor(Location, levels = unique(Location))) 

result.sens.agg %>%
  group_by(Location, Scenario) %>%
  summarise(pass.n = sum(mort.rate.median <= 14.3),
            mean.rate = mean(mort.rate.median))

# y values for the dots on the top of the figure
df.jitter <- data.frame(Location = c("China", "Shanghai", "Shenzhen", "Shiyan", "Shenzhen"), 
                        Scenario = c(rep("VacOptimistic", 4), "VacPessimistic"),
                        jitter = c(1.14, 1.16, 1.08, 1.1, 1.12)+0.05)

result.all.best.worst.part.sens <- result.all.best.worst %>%
  filter(Scenario == "VacOptimistic"|(Scenario == "VacPessimistic"&location == "Shenzhen")) %>%
  select(Scenario, location, mort.rate.median, scenario) %>%
  ungroup() %>%
  mutate(Location = location) %>%
  left_join(df.jitter)


FigST2A <- result.sens.agg %>%
  left_join(df.jitter) %>%
  ggplot(aes(x = mort.rate.median, col = interaction(Location, Scenario))) +
  geom_density() +
  geom_point(aes(x = mort.rate.median, y = jitter), size = 0.5) +
  geom_point(data = result.all.best.worst.part.sens, aes(x = mort.rate.median, y = jitter), col = "black") +
  scale_color_d3() +
  xlab("Mortality rate (1/100,000)") +
  ylab("Density") +
  scale_x_sqrt(expand = c(0,0)) +
  theme_cowplot() +
  theme(legend.position = c(0.7, 0.8)) +
  ggtitle("(A)") +
  #scale_y_continuous(expand = c(0,0)) +
  labs(col = "") +
  geom_vline(xintercept = 14.3, linetype = "dashed") +
  guides(col = "none")





#======== variable importance ======
Places <- unique(result.sens.agg[, c("Location", "Scenario")])

imp_fun <- function(object, newdata) { # for permutation-based VI scores
  predict(object, newdata)$Y_hat
  # predict(object, newdata)[,1]
}

sens.importance <- NULL
for(i in 1:nrow(Places))
{
  current.dat <- result.sens.agg %>% 
    filter(Location == Places$Location[i], Scenario == Places$Scenario[i])
  
  current.model <- GP_fit(as.matrix(par.sample.original[,1:8]), current.dat$mort.rate.median)
  p1 <- vip(current.model, method = "permute", train =  as.matrix(par.sample.original[,1:8]), target = current.dat$mort.rate.median, metric = "rmse", pred_wrapper = imp_fun, nsim = 10)
  current.importance <- p1$data
  current.importance$Location = Places$Location[i]
  current.importance$Scenario = Places$Scenario[i]
  
  sens.importance <- rbind(sens.importance, current.importance)
}



FigST2B <- sens.importance %>%
  mutate(type = paste(Location, Scenario, sep = ", "),
         Variable = factor(Variable, levels = rev(c("Vac_70above","Antiviral",  "ICU", "Vac60_69", "Vac20_59", "Mask", "Vac0_19", "Hospital"))),
         type = factor(type, levels =rev( unique(type)))) %>%
  ggplot(aes(x = Variable, y = Importance, fill = type)) +
  geom_bar(stat = "identity", width = 0.8, position = "dodge") +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 14),
        legend.position = c(0.3, 0.2)) +
  coord_flip() +
  ylab("Permultation importance") +
  xlab("") +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = rev(pal_d3()(5))) +
  labs(fill = "") +
  ggtitle("(B)") +
  scale_x_discrete(labels = rev(c("ΔVac. 70above","Antiviral",  "ICU", "ΔVac. 60-69", "ΔVac. 20-59", "Mask", "ΔVac. 0-19", "Hospital")))

# save_plot("Figures/FigST2.1_full_space_sens.pdf", plot_grid(FigST2A, FigST2B), base_height = 5, base_width = 10)






par.sample.original <- read_csv("Data/LHS_sample_for_local_sen_original.csv")

#======== prediction ==========
GP.prediction <- NULL
Fold <- rep(1:10, each = 10)
for(i in 1:nrow(Places))
{
  current.dat <- result.sens.agg %>% 
    filter(Location == Places$Location[i], Scenario == Places$Scenario[i])
  
  for(j in 1:10)
  {
    cat(i, j, "\n")
    current.model <- GauPro::GauPro(as.matrix(par.sample.original)[Fold != j,], current.dat$mort.rate.median[Fold != j])
    
    current.predict <- data.frame(Observed = current.dat$mort.rate.median[Fold == j],
                                  Predicted = predict(current.model, as.matrix(par.sample.original)[Fold == j,]),
                                  Location = Places$Location[i], 
                                  Scenario = Places$Scenario[i],
                                  Fold = j)
    GP.prediction <- rbind(GP.prediction, current.predict)
  }
}

GP.prediction %>%
  mutate(Location = rep(paste("(", LETTERS[1:5], ") ", Places, sep = ""), each = 100)) %>%
  mutate(Location = rep(c("(A) China, optimistic VE", "(B) Shanghai, optimistic VE", "(C) Shenzhen, optimistic VE", "(D) Shenzhen, pessimistic VE", "(E) Shiyan, optimistic VE"), each = 100)) %>%
  ggplot(aes(x = Observed, y = Predicted, col = as.factor(Fold))) +
  geom_point() +
  geom_abline(slope = 1, col = "gray70") +
  facet_wrap(~Location, scales = "free")+
  theme_cowplot() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 14)) +
  guides(col = FALSE) +
  xlab("Observed median mortality rate (1/100,000)") +
  ylab("Predicted median mortality rate (1/100,000)")
# ggsave("Figures/FigST2.2_GP_validation_full_space.pdf", width = 10, height = 6.5)









par.sample.original <- read_csv("Data/LHS_sample_for_local_sen_original_cut.csv")
par.sample.nocut <- read_csv("Data/LHS_sample_for_local_sen_original.csv")

par.sample.China <- read_csv("Data/LHS_China_cut_VacOpt.csv") %>%
  mutate(repi = 1:100, Location = "China, optimistic VE")
par.sample.SH <- read_csv("Data/LHS_SH_cut_VacOpt.csv") %>%
  mutate(repi = 1:100, Location = "Shanghai, optimistic VE")
par.sample.SZ1 <- read_csv("Data/LHS_sample_for_local_sen_SZ.csv") %>%
  mutate(repi = 1:100, Location = "Shenzhen, optimistic VE")
par.sample.SZ2 <- read_csv("Data/LHS_sample_for_local_sen_SZ.csv") %>%
  mutate(repi = 1:100, Location = "Shenzhen, pessimistic VE")
par.sample.SY <- read_csv("Data/LHS_sample_for_local_sen_SY.csv") %>%
  mutate(repi = 1:100, Location = "Shiyan, optimistic VE")
par.sample.cut <- rbind(par.sample.China, par.sample.SH,  par.sample.SZ1, par.sample.SZ2,  par.sample.SY)

load("Results/China_cut100_VacOpt.rda")
result.China <- result.all %>%
  mutate(Location = "China, optimistic VE")

load("Results/SH_cut100_VacOpt.rda")
result.SH <- result.all %>%
  mutate(Location = "Shanghai, optimistic VE")

load("Results/SZ_first100_VacOpt.rda")
result.SZ1 <- result.all %>%
  mutate(Location = "Shenzhen, optimistic VE")

load("Results/SZ_first100_VacPes.rda")
result.SZ2 <- result.all %>%
  mutate(Location = "Shenzhen, pessimistic VE")

load("Results/SY_first100_VacOpt.rda")
result.SY <- result.all %>%
  mutate(Location = "Shiyan, optimistic VE")

result.cut <- rbind(result.China, result.SH, result.SZ1, result.SZ2, result.SY)
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
  # left_join(par.sample.cut) %>%
  mutate(Location = factor(Location, levels = unique(Location))) 

Places <- unique(result.cut.agg$Location)

Health.resources <- data.frame(Location = Places,
                               Antiviral_low = c(0.577, 0.777, 0, 0, 0.572),
                               ICU_low = c(4.37,	6.14,	3.42,	3.42, 5.13)/100000,
                               Vac_70above_low = c(0.33, 0.793, 0,0, 0))

imp_fun <- function(object, newdata) { # for permutation-based VI scores
  # predict(object, newdata)$Y_hat
  predict(object, newdata)
}



set.seed(20230107)
cut.importance <- NULL
for(i in 1:5)
{
  current.dat <- result.cut.agg %>% 
    filter(Location == Places[i])
  
  if(i < 3)
  {
    current.model <- GauPro::GauPro(as.matrix(par.sample.original), current.dat$mort.rate.median)
    p1 <- vip(current.model, method = "permute", train =  as.matrix(par.sample.original), target = current.dat$mort.rate.median, metric = "rmse", pred_wrapper = imp_fun, nsim = 10)
    current.importance <- p1$data
    current.importance$Location = Places[i]
  }
  
  if(i >= 3)
  {
    current.model <- GauPro::GauPro(as.matrix(par.sample.nocut), current.dat$mort.rate.median)
    p1 <- vip(current.model, method = "permute", train =  as.matrix(par.sample.nocut), target = current.dat$mort.rate.median, metric = "rmse", pred_wrapper = imp_fun, nsim = 10)
    current.importance <- p1$data
    current.importance$Location = Places[i]
  }
  
  
  cut.importance <- rbind(cut.importance, current.importance)
}

cut.importance.top3 <- cut.importance %>% 
  arrange(Location,desc(Importance)) %>% 
  group_by(Location) %>% 
  filter(row_number() <= 3) %>%
  ungroup()

cut.importance.top3



GP.prediction.cut <- list()
for(i in 1:5)
{
  current.dat <- result.cut.agg %>% 
    ungroup() %>%
    filter(Location == Places[i])
  
  cat(i,  "\n")
  
  importance.var <- cut.importance.top3 %>%
    filter(Location == Places[i]) %>%
    pull(Variable)
  
  prediction <- expand.grid(A = seq(0, 1, length.out = 100), 
                            B = seq(0, 1, length.out = 100), 
                            C = seq(0, 1, length.out = 100))
  colnames(prediction) <- importance.var
  
  if(i < 3)
  {
    current.model <- GauPro::GauPro(as.matrix(par.sample.original)[, importance.var], current.dat$mort.rate.median)
  }
  
  if(i >= 3)
  {
    current.model <- GauPro::GauPro(as.matrix(par.sample.nocut)[, importance.var], current.dat$mort.rate.median)
  }
  
  current.prediction <- predict(current.model, as.matrix(prediction))
  
  current.prediction.dt <- prediction %>%
    mutate(Predicted = current.prediction,
           Location = Places[i])  %>%
    left_join(Health.resources) %>%
    mutate(ICU = ICU*(48/100000 - ICU_low) + ICU_low)
  
  if("Antiviral" %in% importance.var) 
  {
    current.prediction.dt <- current.prediction.dt %>%
      mutate(Antiviral = Antiviral*(1-Antiviral_low) + Antiviral_low)
  }
  
  if("Vac_70above" %in% importance.var) 
  {
    current.prediction.dt <- current.prediction.dt %>%
      mutate(Vac_70above = Vac_70above*(1- Vac_70above_low) +  Vac_70above_low)
  }
  
  GP.prediction.cut[[i]] <- current.prediction.dt
}




FigST2.3A <- GP.prediction.cut[[1]] %>%
  mutate(ICU = ICU*100000) %>%
  group_by(Antiviral, Vac_70above) %>%
  summarize(minICU = min(ICU[Predicted <= 14.3])) %>%
  ggplot(aes(x = Vac_70above, y = Antiviral, fill = minICU)) +
  geom_tile() +
  #  facet_wrap(~Location, scales = "free") +
  geom_contour(aes(z = minICU), col = "black", breaks = seq(4, 50, 2)) + 
  geom_label_contour(aes(z = minICU), stroke = 0.2, breaks = seq(4, 50, 2), skip = 1) +
  theme_cowplot() +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 14, face = "bold")) +
  scale_fill_material("blue") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0.0)) +
  guides(fill = "none")+
  xlab("ΔVac. 70+") +
  ylab("Antiviral coverage") +
  labs(fill = "ICU") +
  ggtitle("(A) China, optimistic, reduced")





FigST2.3B <- GP.prediction.cut[[2]] %>%
  mutate(ICU = ICU*100000) %>%
  group_by(Antiviral, Vac_70above) %>%
  summarize(minICU = min(ICU[Predicted <= 14.3])) %>%
  ggplot(aes(x = Vac_70above, y = Antiviral, fill = minICU)) +
  geom_tile() +
  #  facet_wrap(~Location, scales = "free") +
  geom_contour(aes(z = minICU), col = "black", breaks = seq(4, 50, 2)) + 
  geom_label_contour(aes(z = minICU), stroke = 0.2, breaks = seq(4, 50, 2), skip = 1) +
  theme_cowplot() +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 14, face = "bold")) +
  scale_fill_material("blue") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0.0)) +
  guides(fill = "none")+
  xlab("ΔVac. 70+") +
  ylab("Antiviral coverage") +
  labs(fill = "ICU") +
  ggtitle("(B) Shanghai, optimistic, reduced")




FigST2.3C  <- GP.prediction.cut[[3]] %>%
  mutate(ICU = ICU*100000) %>%
  group_by(Antiviral, Vac_70above) %>%
  summarize(minICU = min(ICU[Predicted <= 14.3])) %>%
  ggplot(aes(x = Vac_70above, y = Antiviral, fill = minICU)) +
  geom_tile() +
  #  facet_wrap(~Location, scales = "free") +
  geom_contour(aes(z = minICU), col = "black", breaks = seq(4, 50, 2)) + 
  geom_label_contour(aes(z = minICU), stroke = 0.2, breaks = seq(4, 50, 2), skip = 1) +
  theme_cowplot() +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 14, face = "bold")) +
  scale_fill_material("blue") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0.0)) +
  guides(fill = "none")+
  xlab("ΔVac. 70+") +
  ylab("Antiviral coverage") +
  labs(fill = "ICU") +
  ggtitle("(C) Shenzhen, optimistic")



FigST2.3D  <- GP.prediction.cut[[4]] %>%
  mutate(ICU = ICU*100000) %>%
  group_by(Antiviral, Vac_70above) %>%
  summarize(minICU = min(ICU[Predicted <= 14.3])) %>%
  ggplot(aes(x = Vac_70above, y = Antiviral, fill = minICU)) +
  geom_tile() +
  #  facet_wrap(~Location, scales = "free") +
  geom_contour(aes(z = minICU), col = "black", breaks = seq(4, 50, 2)) + 
  geom_label_contour(aes(z = minICU), stroke = 0.2, breaks = seq(4, 50, 2), skip = 1) +
  theme_cowplot() +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 14, face = "bold")) +
  scale_fill_material("blue") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0.0)) +
  guides(fill = "none")+
  xlab("ΔVac. 70+") +
  ylab("Antiviral coverage") +
  labs(fill = "ICU") +
  ggtitle("(C) Shenzhen, pessimistic")



FigST2.3E  <- GP.prediction.cut[[5]] %>%
  mutate(ICU = ICU*100000) %>%
  group_by(Antiviral, Vac_70above) %>%
  summarize(minICU = min(ICU[Predicted <= 14.3])) %>%
  ggplot(aes(x = Vac_70above, y = Antiviral, fill = minICU)) +
  geom_tile() +
  #  facet_wrap(~Location, scales = "free") +
  geom_contour(aes(z = minICU), col = "black", breaks = seq(4, 50, 2)) + 
  geom_label_contour(aes(z = minICU), stroke = 0.2, breaks = seq(4, 50, 2), skip = 1) +
  theme_cowplot() +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 14, face = "bold")) +
  scale_fill_material("blue") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0.0)) +
  guides(fill = "none")+
  xlab("ΔVac. 70+") +
  ylab("Antiviral coverage") +
  labs(fill = "ICU") +
  ggtitle("(E) Shiyan, optimistic")



# save_plot("Figures/FigST2.3_heatmaps.pdf", plot_grid(FigST2.3A, FigST2.3B, FigST2.3C, FigST2.3D, FigST2.3E, ncol = 3), base_width = 12, base_height = 8)