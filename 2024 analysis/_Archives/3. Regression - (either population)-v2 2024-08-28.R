# This file is available at https://github.com/ebmgt/NHS-Religion/
# Author: Zehan Yang, PhD. zehan.yang@uconn.edu. You may also contact rbadgett@kumc.edu
# Permission: GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
# Last edited 2024-08-29

# This file uses the datafile:
# "data_trusts_burnout.csv"

library(tcltk) # For interactions and troubleshooting, Oart of base so no install needed.

# Global variables -----
Pb <- NULL  # For function_progress

# Set working directory -----
if (Sys.getenv("RSTUDIO") != "1"){
  args <- commandArgs(trailingOnly = FALSE)
  script_path <- sub("--file=", "", args[grep("--file=", args)])  
  script_path <- dirname(script_path)
  setwd(script_path)
}else{
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  #ScriptsDir <- paste(getwd(),'/Scripts',sep='')
}

# Did the script load? -----
tk_messageBox(type = "ok", paste('1. ', 'R has loaded.\n\nWorking directory:\n', getwd(), sepo=''), caption = "Hello")

# Functions -----
function_progress <- function(progress, titletext){
  #if  (!exists("titletext") || is.null(titletext)){titletext <- 'testing'}
  #if  (!exists("progress") || is.null(progress)){progress <- 0}
  if (progress == 0){
    Pb <<- tkProgressBar(titletext, "", 0, 100, 0)
  }
  info <- sprintf("%d%% done", round(progress))
  setTkProgressBar(Pb, value = progress, title = paste(titletext,sprintf("(%s)", info)), label = info)
  if (progress == 100){
    close(Pb)
  }
}
function_libraries_install <- function(packages){
  install.packages(setdiff(packages, rownames(installed.packages())))
  for(package_name in packages)
  {
    #library(package_name,character.only=TRUE,quietly=TRUE);
    library(package_name,character.only=TRUE, quietly = FALSE);
  }
}
current.date <- function() {
  as.character(strftime(Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE))
}
# Function to calculate I2
I2 <- function(res) {
  W <- diag(1/res$vi)
  X <- model.matrix(res)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))
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
# Packages -----
function_progress(0,'Libraries')

packages_essential <- c('stringr','openxlsx','grid','plyr')
function_libraries_install(packages_essential)
function_progress(25,'Libraries')

packages_regression <- (c("betareg", "readr"))
function_libraries_install(packages_regression)
function_progress(50,'Libraries')

packages_meta <- (c("meta", "metafor",'grid'))
function_libraries_install(packages_meta)
function_progress(75,'Libraries')

packages_dominance <- (c("domir"))
function_libraries_install(packages_dominance)

function_progress(100,'Libraries')

# Parameters -----
#* Select data sources ----
data.source <- tk_select.list(c('All',
                      'Trusts and CCGs only'), 
                      preselect = 'Trusts and CCGs only', multiple = FALSE,
                      title =paste0("\n\n",Sys.info()["user"], ":\n\nWho are we studying?\n\n"))
#* Select outcome sources ----
Outcome.source <- tk_select.list(c('Burned_out_rate',
                      'Stress'), 
                      preselect = 'Burned_out_rate', multiple = FALSE,
                      title =paste0("\n\n",Sys.info()["user"], ":\n\nWhat outcome are we studying?\n\n"))
#* Select religion.reference ----
religion.reference <- tk_select.list(c('Hindu','No religion'),
                                     preselect = 'Hindu', multiple = FALSE,
                                     title =paste0("\n\n",Sys.info()["user"], ":\n\nWhich religous group is referent in regression?\n\n"))
#* Year -----
year.handling <- tk_select.list(c('Factor','Numeric'), preselect = 'Factor', multiple = FALSE,
                                title =paste0("\n\n",Sys.info()["user"], ":\n\nHow to handle year?\n\n"))
# Data grab -----
if (data.source == 'Trusts and CCGs only'){
  regdat <- read_csv("data_trusts_burnout.csv")
  #** data_full: Data prep -----
  names(regdat)[names(regdat) == "Year"] <- "year"
  names(regdat)[names(regdat) == "Benchmarking group"] <- "trust"
  names(regdat)[names(regdat) == "Religion"] <- "religion"
  if (Outcome.source == 'Burned_out_rate'){
    names(regdat)[names(regdat) == "Burned_out_rate"] <- "stress"
    names(regdat)[names(regdat) == "Burned_out_base"] <- "stress_base"
  } else{
    names(regdat)[names(regdat) == "Stress_rate"] <- "stress"
    names(regdat)[names(regdat) == "Stress_base"] <- "stress_base"
  }
  names(regdat)[names(regdat) == "Med_Dental_rate"] <- "physician"
  names(regdat)[names(regdat) == "Female_rate"] <- "female"
  names(regdat)[names(regdat) == "Physical_public_base"] <- "physical.public"
  names(regdat)[names(regdat) == "Physical_manager_base"] <- "physical.manager"
  names(regdat)[names(regdat) == "Physical_colleagues_base"] <- "physical.colleague"
  names(regdat)[names(regdat) == "Emotional_public_rate"] <- "emotional.public"
  names(regdat)[names(regdat) == "Emotional_manager_rate"] <- "emotional.manager"
  names(regdat)[names(regdat) == "Emotional_colleagues_rate"] <- "emotional.colleague"
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
regdat$stress <- regdat$stress / 100

if (year.handling == 'Factor'){
  #regdat$year <- factor(regdat$year)
  regdat$year <- factor(regdat$year, level = c(2021, 2022, 2023)) # per Zehan 2024-08-17
  #regdat$year <- relevel(regdat$year, ref = 2021) 
  }else{
  regdat$year <- as.numeric(regdat$year)
}
#** Religion -----
unique(regdat$religion)
regdat$religion <- factor(regdat$religion, 
                          levels = c("Hindu", "Buddhist",
                                     "Sikh", "Christian", "Jewish", 
                                     "Muslim", "Any other religion", "No religion",
                                     "I would prefer not to say"))
regdat$religion <- relevel(regdat$religion, ref = religion.reference)

regdat$religion1 <- NA # edit 2024-04-25
regdat$religion1[which(regdat$religion %in% c("Hindu", "Buddhist", "Sikh"))] <- "Dharmic"
regdat$religion1[which(regdat$religion %in% c("Christian", "Jewish", "Muslim"))] <- "Abraham"

regdat$religion2 <- 'withheld' # edit 2024-08-05 was 'NA
regdat$religion2[which(regdat$religion %in% c("Hindu", "Buddhist",
                                               "Sikh", "Christian", "Jewish", 
                                               "Muslim", "Any other religion"))] <- "Yes"
regdat$religion2[which(regdat$religion %in% c("No"))] <- "No"
regdat$religion2[which(regdat$religion %in% c("withheld"))] <- "withheld"

regdat$religion1 <- factor(regdat$religion1,
                           levels = c("No", "Dharmic", "Abraham", "Other", "withheld"))

regdat$religion2 <- factor(regdat$religion2,
                           levels = c("No", "Yes", "withheld", 'NA'))

regdat$trust <- factor(regdat$trust, 
                       labels = c("acute", "acute_specialist", 
                                  "ambulance", "community",
                                  "mental_health"))

sapply(regdat, class)

# Table 1 -----
base_columns <- grep("base", colnames(regdat), value = TRUE)

# Calculate the sum for each of these columns
(sums <- colSums(regdat[, base_columns]))
(respondents <- sum (regdat$stress_base))
Stress_overall_rate <- sum(regdat$stress*regdat$stress_base/respondents)
cat (paste(format(respondents, big.mark = ","), "surveys contained a response to the ¿first covariate questions?, after the religion question, and thus were included in the analysis. The rate of stress was ", round(100*Stress_overall_rate,1), '%.'))

# Regression -----
#*  TABLE 3 All religion  -----
# Testing all covariates for significance
# Religion
unique(regdat$religion)
mean(regdat$stress)
regdat$Patient_facing_rate
formula <- "stress ~ religion + year +
                  physical.public + physical.manager + physical.colleague +
                  emotional.public + emotional.manager + emotional.colleague +
                  physician + female + Ethnic_minority_rate + Patient_facing_rate"

mod0 <- betareg(as.formula(formula), data = regdat)
cat('Analysis of year: ', year.handling, '. Outcome: ', Outcome.source)
summary(mod0)
function_general_dominance_values(formula, regdat)

#* Parsimonious -----
formula <- "stress ~ religion + year +
                  emotional.colleague +
                  emotional.public + emotional.manager +
                  + physician"

mod0 <- betareg(as.formula(formula), data = regdat)
cat('Analysis of year: ', year.handling)
summary(mod0)
function_general_dominance_values(formula, regdat)

dominance <- domir(as.formula(formula), lm_r2, data = regdat)
cat("Overall Value: ",dominance$Value)
cat("General dominance for religion: ",dominance$General_Dominance["religion"])
cat("General dominance for emotional harassment by public: ",dominance$General_Dominance["emotional.public"])
cat("General dominance for physical abuse by colleage: ",dominance$General_Dominance["physical.colleague"])

# all religion - not needed per 2024-07-23
mod1 <- betareg(stress ~ religion + year + #trust + # 2024-08-05
                  physical.public + physical.manager + physical.colleague +
                  emotional.public + emotional.manager + emotional.colleague +
                  physician + female,
                data = regdat)
summary(mod1)

# Dharmic vs Abraham vs other - 2024-07-23 - if we keep religion1, should remove female and other insig
table(regdat$religion2)
mod2 <- betareg(stress ~ religion2 + year + #trust + # 2024-08-05
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
## In the meta analysis, I include all variables in the model `mod_beta
#dat <- read_csv("sts_has.csv")
dat <- read_csv("data_trusts_burnout.csv") # 2024-08-17
dat$Religion <- factor(dat$Religion, levels = c("Hindu", unique(dat$Religion)[-4])) # was -3
dat$Year <- as.factor(dat$Year)
dat$stress.stress <- dat$Burned_out_rate
dat$stress_base <- dat$Burned_out_base
dat$stressvariance <- 1 / dat$stress_base
dat$emotional.public <- dat$Emotional_public_rate
dat$emotional.manager <- dat$Emotional_manager_rate
dat$emotional.colleague <- dat$Emotional_colleagues_rate
dat$physical.manager <- dat$Physical_manager_rate
dat$physical.public <- dat$Physical_public_rate
dat$physical.colleague <- dat$Physical_colleagues_rate

# All years ==================
meta_all_years <- rma.mv(yi = stress.stress, V = stressvariance, 
                         mods = ~ Year + emotional.public + emotional.manager +
                           emotional.colleague + physical.manager + physical.public +
                           physical.colleague, random = ~ 1 | Religion, 
                         data = dat)
meta_all_years <- rma.mv(yi = stress.stress, V = stressvariance, 
                         mods = ~ Year + emotional.public + emotional.manager +
                           emotional.colleague, random = ~ 1 | Religion, 
                         data = dat)
summary(meta_all_years)
I2(meta_all_years) # Calculate I2

# Forest plot -----
