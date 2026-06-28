library(data.table)
annual_mortality_rate_dat <- fread("HMD_USA_Mx_2015.csv")
annual_mortality_rate_dat[, V1:= NULL]
colnames(annual_mortality_rate_dat) <- tolower(colnames(annual_mortality_rate_dat))
long_dat <- melt(annual_mortality_rate_dat, 
                 id.vars = c("year", "age"), 
                 measure.vars = c("female", "male", "total"), 
                 variable.name = "sex", 
                 value.name = "lambda")
annual_mortality_rates <- dcast(long_dat, year + sex ~ age, value.var = "lambda")
setnames(annual_mortality_rates, "year", "birth_cohort")
setnames(annual_mortality_rates, as.character(0:110), c(paste0("age_", 0:109), "age_110+"))
saveRDS(annual_mortality_rates, "annual_mortality_rates.rds")
rm(list =c("annual_mortality_rate_dat", "long_dat"))
