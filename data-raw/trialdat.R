## code to prepare `trialdat` dataset goes here

load(file = "./data-raw/trialdata.rda")
trialdat <- dat_trial
rm(dat_trial)

usethis::use_data(trialdat, overwrite = TRUE)
