################################################################################
########  Discrete-event simulation of a cancer natural history model   ########
################################################################################

# Fernando Alarid-Escudero, PhD (1) 
# Stavroula Chrysanthopoulou, PhD (2)
# Yuliia Sereda, PhD (3)
# Tom Trikalinos, MD (2, 3)

# Affiliations: 		
# 1 Department of Health Policy, School of Medicine, and Stanford Health Policy, 
#   and Freeman-Spogli Institute for International Studies, Stanford University, 
#   Stanford, California, USA
# 2 Department of Biostatistics, School of Public Health, Brown University, 
#   Providence, Rhode Island, USA
# 3 Department of Health Services, Policy and Practice, School of Public Health, Brown University, 
#   Providence, Rhode Island, USA
################################################################################

rm(list = ls()) # clear memory (removes all the variables from the workspace)

# 01 Load packages & functions ----
library(nhppp)

set.seed(1)                      # set the seed 

# Model structure
n_i <- 100000 # number of individuals

# Event hazards
# Loosely based on https://www.frontiersin.org/journals/physiology/articles/10.3389/fphys.2022.780917/full
r_Dx <- 0.235 # rate to get diagnosed
r_cancer_death <- 0.1 # rate to die from cancer
param_weibull_shape <- 2.78
param_weibull_scale_PH <- 6.24e-06 # To use with proportional hazards (PH) parameterization
param_weibull_scale_AFT <- param_weibull_scale_PH^(-1/param_weibull_shape) # To use with accelerated failure time (AFT) parameterization

# hist(rweibull(n = 1000, scale = param_weibull_scale_AFT, shape = param_weibull_shape))
# hist(rweibullPH(n = 1000, scale = param_weibull_scale_PH, shape = param_weibull_shape))
# 
# curve(dweibull(x, scale = param_weibull_scale_AFT, shape = param_weibull_shape), 
#       from = 0, to = 10, col = "blue", lwd = 2, ylab = "Density", xlab = "Time", 
#       main = "Weibull AFT parametrization")
# curve(dweibullPH(x, scale = param_weibull_scale_PH, shape = param_weibull_shape), 
#       from = 0, to = 10, col = "red", lwd = 2, ylab = "Density", xlab = "Time", 
#       main = "Weibull PH parametrization")

## 02.1 Background mortality ----
# Probabilities of death from healthy state is age-dependent
# load probability of death by age from Human Mortality Database
dt_hmd_usa_2015 <- read.csv("data/HMD_USA_Mx_2015.csv")[, -c(1, 7)] 
max_age <- max(dt_hmd_usa_2015$Age)
dt_hmd_usa_2015_long <- reshape2::melt(dt_hmd_usa_2015, 
                                       id.vars = c("Year", "Age"), 
                                       variable.name = "Sex", 
                                       value.name = "death_rate")
# Generate a wide/long block-diagonal data.frame with age-specific mortality 
# rates conditional on being alive at each year of age
df_p_HD <- obtain_probs_des(dt_hmd_usa_2015_long)

# Convert data frame to a data table for efficiency
dt_p_HD <- data.table(df_p_HD) 
dt_p_HD
setkey(dt_p_HD, Year, Age, Sex)      # set the data table to be indexed by Age and Sex

# 03 Sample individual level characteristics ----
set.seed(2024025)
## 03.1 Static characteristics ----
# Store these static individual characteristics in one data frame
dt_Pop <- data.table(Ind  = 1:n_i, 
                     Year = 2015,
                     Age = 0, 
                     Sex  = "Total")
dt_Pop

# Compute individual- and age-specific probabilities of dying from other causes
m_p_HD <- as.matrix(dt_p_HD[.(dt_Pop$Year, dt_Pop$Age, dt_Pop$Sex), DP0:DP110])

# 04 Discrete-event simulation (DES) ----
## 04.1 Sample age of death as a function of Yea, Age, and Sex ----
dt_Pop[, Age_death_oc := nps_nhppp(m_probs = m_p_HD, correction = "uniform")]

## 04.2 Sample time and age of cancer onset ----
dt_Pop[, Time_to_cancer := rweibull(n = .N, 
                                    scale = param_weibull_scale_AFT, 
                                    shape = param_weibull_shape)]
dt_Pop[, Age_to_cancer := Age + Time_to_cancer]

## 04.3 Sample time and age of diagnosis ----
dt_Pop[, Time_to_diagnosis := rexp(n = .N, rate = r_Dx)]
dt_Pop[, Age_to_diagnosis := Age_to_cancer + Time_to_diagnosis]

## 04.4 Sample time and age of cancer death ----
dt_Pop[, Time_to_cancer_death := rexp(n = .N, rate = r_cancer_death)]
dt_Pop[, Age_to_cancer_death := Age_to_diagnosis + Time_to_cancer_death]

## 04.5 Age and cause of death ----
dt_Pop[, Age_death := matrixStats::rowMins(cbind(Age_death_oc, 
                                                 Age_to_cancer_death))]
dt_Pop[, Cause_of_death := fifelse(test = (Age_death == Age_death_oc),
                                   yes  = "Other",
                                   no   = "Cancer")]
## 04.6 Got cancer ----
dt_Pop[, Got_cancer := fifelse(test = (Age_to_cancer < Age_death),
                                   yes  = TRUE,
                                   no   = FALSE)]

# 05 Compute outcomes ----
## 05.1 Age of cancer onset ----
dt_Pop[, Age_cancer_onset := fifelse(test = (Got_cancer),
                                     yes  = Age_to_cancer,
                                     no   = NaN)]
dt_Pop[Age_cancer_onset > 0, 
       .(`Age of cancer onset (years)` = mean(Age_cancer_onset), 
         `% got cancer` = scales::percent(.N/n_i))]

hist(dt_Pop[Age_cancer_onset > 0, Age_cancer_onset])

## 05.2 Sojourn time ----
# Sojourn time is defined as the time spent in a preclinical state.

# Calculate who gets diagnosed with cancer before dying from other causes
dt_Pop[, Cancer_before_death := fifelse(test = (Age_to_diagnosis < Age_death),
                                        yes  = TRUE,
                                        no   = FALSE)]

# Calculate time spent with cancer before getting diagnosed
dt_Pop[, Time_with_preclinical_cancer := fifelse(test = (Cancer_before_death),
                                                 yes  = Time_to_diagnosis,
                                                 no   = NaN)]
# Expected time spent with preclinical cancer on those who got cancer and got
# diagnosed before dying 
dt_Pop[Time_with_preclinical_cancer > 0, 
       .(`Sojourn time (years)` = mean(Time_with_preclinical_cancer), 
         `% got Dx cancer` = scales::percent(.N/n_i))]
