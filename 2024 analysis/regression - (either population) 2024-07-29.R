# This file is available at https://github.com/ebmgt/NHS-Religion/
# Author: Zehan Yang, PhD. zehan.yang@uconn.edu. You may also contact rbadgett@kumc.edu
# Permission: GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
# Last edited 2024-07-29

# This file uses the datafile:
# "data_trusts.csv"

# If Rstudio
library(tcltk)
if (Sys.getenv("RSTUDIO") != "1"){
  tk_messageBox(type = "ok", paste('1. ', 'Working directory:\n', getwd(), sepo=''), caption = paste("Hello and good",daypart))
}else{
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  #ScriptsDir <- paste(getwd(),'/Scripts',sep='')
}

# Libraries -----
library(betareg)
library(readr)
library(ggplot2)
library(domir)

# Functions -----
current.date <- function() {
  as.character(strftime(Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE))
}
lm_r2 <- # for dominance analysis
  # https://cran.r-project.org/web/packages/domir/
  # call with:
  # dominance <- domir(as.formula(formula), lm_r2, data = temp)
  # OR
  # dominance <- domir(outcome  ~ predictor1 + predictor2, lm_r2, data = temp)
  function(fml, data) {
    lm_res <- lm(fml, data = data) # data_ammc_byRespondent
    summary(lm_res)[["r.squared"]]
  }
function_general_dominance_values <- function(formula, data) {
  # Perform the dominance analysis
  dominance_result <- domir(as.formula(formula), lm_r2, data = data)
  general_dominance_values <- dominance_result$General
  
  # Combine variable names with their General Dominance Values
  variables <- names(general_dominance_values)
  values <- general_dominance_values
  
  # Create a data frame for sorting
  df <- data.frame(Variable = variables, Value = values)
  
  # Sort the data frame by Value in descending order
  df <- df[order(-df$Value), ]
  
  # Print each variable name and its value on a new line
  for (i in 1:nrow(df)) {
    cat(df$Variable[i], ":", df$Value[i], "\n")
  }
}

# Get data -----
#* Source? -----
data.source <- tk_select.list(c('All','Trusts only'), preselect = 'Trusts only', multiple = FALSE,
                                title =paste0("\n\n",Sys.info()["user"], ":\n\nWho are we studying?\n\n"))

if (data.source == 'Trusts only'){
  regdat <- read_csv("data_trusts.csv")
  #** data_full: Data prep -----
  names(regdat)[names(regdat) == "stress.stress"] <- "stress"
  names(regdat)[names(regdat) == "physician.physician"] <- "physician"
  names(regdat)[names(regdat) == "gender.gender"] <- "female"
}else{
  tk_messageBox(type = c('okcancel'), "data_full does not have for regression:\n\nfemale\nmedical/dental", caption = "Warning")
  regdat <- read_csv("data_full.csv")
  #** data_trusts: Data prep ----- 
  names(regdat)[names(regdat) == "Religion"] <- "religion"
  regdat$trust <- ''
  regdat <- regdat[, c(1, ncol(regdat), 2:(ncol(regdat) - 1))]
  names(regdat)[names(regdat) == "Year"] <- "year"
}
#* Optimize data types -----
# Below does not affect regression results
regdat$religion <- as.factor(regdat$religion)
regdat$religion <- relevel(regdat$religion, ref = "Hindu") 

regdat$religion1 <- NA # edit 2024-04-25
regdat$religion1[which(regdat$religion %in% c("Hindu", "Buddhist", "Sikh"))] <- "Dharmic"
regdat$religion1[which(regdat$religion %in% c("Christian", "Jewish", "Muslim"))] <- "Abraham"

regdat$religion2 <- NA # edit 2024-04-25
regdat$religion2[which(regdat$religion1 %in% c("Dharmic", "Abraham", "Other"))] <- "Has Religion"

regdat$religion <- factor(regdat$religion, 
                          levels = c("Hindu", "Buddhist",
                                     "Sikh", "Christian", "Jewish", 
                                     "Muslim", "Other", "No",
                                     "withheld"))

regdat$religion1 <- factor(regdat$religion1,
                           levels = c("No", "Dharmic", "Abraham", "Other", "withheld"))

regdat$religion2 <- factor(regdat$religion2,
                           levels = c("No", "Has Religion", "withheld"))

regdat$year <- as.numeric(regdat$year)
regdat$trust <- factor(regdat$trust, 
                       labels = c("acute_specialist", "acute",
                                  "ambulance", "CCG", "community",
                                  "me"))

sapply(regdat, class)

# Table 1 -----
base_columns <- grep("base", colnames(regdat), value = TRUE)

# Calculate the sum for each of these columns
(sums <- colSums(regdat[, base_columns]))
(respondents <- sum (regdat$stress.stress.base))
Stress_overall_rate <- sum(regdat$stress*regdat$stress.stress.base/respondents)
cat (paste(format(respondents, big.mark = ","), "surveys contained a response to the ¿first covariate questions?, after the religion question, and thus were included in the analysis. The rate of stress was ", round(100*Stress_overall_rate,1), '%.'))

# Regression -----
#*  All religion FOR TABLE 3 -----
# Testing all covariates for significance
formula <- "stress ~ religion + year +
                  physical.public + physical.manager + physical.colleague +
                  emotional.public + emotional.manager + emotional.colleague +
                  + physician + female"

mod0 <- betareg(as.formula(formula), data = regdat)
summary(mod0)
function_general_dominance_values(formula, regdat)

# Bob edits 2024-07-18. REMOVED insig: female, emotional by colleagues, physical by public
formula <- "stress ~ religion + year +
                  physical.manager + physical.colleague +
                  emotional.public + emotional.manager +
                  + physician"

mod0 <- betareg(as.formula(formula), data = regdat)
summary(mod0)
function_general_dominance_values(formula, regdat)

dominance <- domir(as.formula(formula), lm_r2, data = regdat)
cat("Overall Value: ",dominance$Value)

cat("General dominance for religion: ",dominance$General_Dominance["religion"])

cat("General dominance for emotional harassment by public: ",dominance$General_Dominance["emotional.public"])

cat("General dominance for physical abuse by colleage: ",dominance$General_Dominance["physical.colleague"])

# all religion - not needed per 2024-07-23
mod1 <- betareg(stress ~ religion + trust + year +
                  physical.public + physical.manager + physical.colleague +
                  emotional.public + emotional.manager + emotional.colleague +
                  physician + female,
                data = regdat)
summary(mod1)

# Dharmic vs Abraham vs other - 2024-07-23 - if we keep religion1, should remove female and other insig
mod2 <- betareg(stress ~ religion1 + trust + year +
                  physical.public + physical.manager + physical.colleague +
                  emotional.public + emotional.manager + emotional.colleague +
                  physician + female,
                data = regdat)
summary(mod2)

# religion vs no religion - 2024-07-23 - if we keep religion1, should remove female and other insig
mod3 <- betareg(stress ~ religion2 + trust + year +
                  physical.public + physical.manager + physical.colleague +
                  emotional.public + emotional.manager + emotional.colleague +
                  physician + female,
                data = regdat)
summary(mod3)

# Meta-Analysis via Multivariate/Multilevel Linear (Mixed-Effects) Models -----
# THIS IS STILL BEING CHECKED FOR ERRORS AND CONSISTENCY
## In the meta analysis, I include all variables in the model `mod_beta
dat <- read_csv("sts_has.csv")
dat$Religion <- factor(dat$Religion, levels = c("Hindu", unique(dat$Religion)[-3]))
dat$Year <- as.factor(dat$Year)s

dat$stressvariance <- 1 / dat$stress_base

# Year of 2018
meta_2018 <- rma.mv(yi = stress, V = stressvariance, 
                    mods = ~ Emotional_Public + Emotional_Manager +
                      Emotional_Colleagues + Physical_Manager + Physical_Public +
                      Physical_Colleagues, random = ~ 1 | Religion, 
                    data = dat[which(dat$Year == 2018), ])
summary(meta_2018)
I2(meta_2018)  # Calculate I2
forest(meta_2018)

## Year of 2019
meta_2019 <- rma.mv(yi = stress, V = stressvariance, 
                    mods = ~ Emotional_Public + Emotional_Manager +
                      Emotional_Colleagues + Physical_Manager + Physical_Public +
                      Physical_Colleagues, random = ~ 1 | Religion, 
                    data = dat[which(dat$Year == 2019), ])
summary(meta_2019)
I2(meta_2019)  # Calculate I2
forest(meta_2019)

## Year of 2020
meta_2020 <- rma.mv(yi = stress, V = stressvariance, 
                    mods = ~ Emotional_Public + Emotional_Manager +
                      Emotional_Colleagues + Physical_Manager + Physical_Public +
                      Physical_Colleagues, random = ~ 1 | Religion, 
                    data = dat[which(dat$Year == 2020), ])
summary(meta_2020)
I2(meta_2020) # Calculate I2
forest(meta_2020)

# All years ==================
meta_all_years<- rma.mv(yi = stress, V = stressvariance, 
                        mods = ~ Year + Emotional_Public + Emotional_Manager +
                          Emotional_Colleagues + Physical_Manager + Physical_Public +
                          Physical_Colleagues, random = ~ 1 | Religion, 
                        data = dat)
summary(meta_all_years)
I2(meta_all_years) # Calculate I2


