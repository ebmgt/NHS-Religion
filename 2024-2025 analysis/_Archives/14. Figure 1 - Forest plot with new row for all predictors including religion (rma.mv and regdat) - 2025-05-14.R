# This file is available at https://github.com/ebmgt/NHS-Religion/
# Author:rbadgett@kumc.edu and zyang.uconn@gmail.com
# Permission: GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
# Last edited 2025-06-14

#== Startup ======
library(tcltk) # For interactions and troubleshooting, part of base package so no install needed.

#* Cleanup ======
# Remove all of environment
#rm(list = ls())
# Remove all but functions from the environment
#rm(list = ls(envir = .GlobalEnv)[!sapply(ls(envir = .GlobalEnv), function(x) is.function(get(x, envir = .GlobalEnv)))])
#rm(list = ls(pattern = "^model")) # globals
# remove (data_model)

#* Set working directory -----
if (Sys.getenv("RSTUDIO") != "1"){
  args <- commandArgs(trailingOnly = FALSE)
  script_path <- sub("--file=", "", args[grep("--file=", args)])  
  script_path <- dirname(script_path)
  setwd(script_path)
}else{
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}
getwd()

# Functions ------------
#* Functions for library installs -----
function_progress <- function(progress, titletext = "testing") {
  # Check if Tcl/Tk is available
  if (!capabilities("tcltk")) {
    message("Tcl/Tk not available; cannot display a progress bar.")
    return(invisible(NULL))
  }
  
  # If Pb does not exist or got removed, create a new progress bar at 0
  if (!exists("Pb", envir = .GlobalEnv)) {
    Pb <<- tkProgressBar(title = titletext, label = "", min = 0, max = 100, initial = 0)
  }
  
  info <- sprintf("%d%% done", round(progress))
  
  # Attempt to set the progress bar; if it fails, try recreating it
  tryCatch({
    setTkProgressBar(Pb, value = progress, 
                     title = paste(titletext, sprintf("(%s)", info)), 
                     label = info)
  }, error = function(e) {
    # If there's an error, try to close and remove Pb, then recreate
    if (exists("Pb", envir = .GlobalEnv)) {
      try(close(Pb), silent = TRUE)
      rm(Pb, envir = .GlobalEnv)
    }
    # Recreate the progress bar and update
    Pb <<- tkProgressBar(title = titletext, label = "", min = 0, max = 100, initial = 0)
    setTkProgressBar(Pb, value = progress, 
                     title = paste(titletext, sprintf("(%s)", info)), 
                     label = info)
  })
  
  # If progress reached 100%, close and remove the progress bar
  if (progress == 100) {
    close(Pb)
    rm(Pb, envir = .GlobalEnv)
  }
}

function_libraries_install <- function(packages){
  install.packages(setdiff(packages, rownames(installed.packages())), 
                   repos = "https://cloud.r-project.org/",
                   #type = "binary"
  )
  for(package_name in packages)
  {
    library(package_name, character.only=TRUE, quietly = FALSE);
    cat('Installing package: ', package_name)
  }
  #tk_messageBox(type = "ok", paste(packages, collapse="\n"), title="Packages installed")
}
#* Functions to show data -----
function_display_df_in_viewer <- function(df, caption) {
  dt <- DT::datatable(df, caption = caption, options = list(pageLength = 25), escape = FALSE)
  print(dt)
}
function_display_model_in_viewer <- function(model, caption) {
  temp <- summary(model)
  summary(model)
  odds_ratios_df <- data.frame(Coefficient = round(temp$coefficients$mean[, "Estimate"],3) ,
                               Odds_Ratio  = sprintf(exp(temp$coefficients$mean[, "Estimate"]), fmt='%#.2f'), 
                               P.value     = sprintf(temp$coefficients$mean[, "Pr(>|z|)"], fmt='%#.3f'))
  DT::datatable(odds_ratios_df, caption = caption, options = list(pageLength = 25))
}
function_plot_print <- function (plotname, plotwidth, plotheight, imagetype) {
  
  #plotname <- gsub("[:\\s\n?!']", "", plotname)
  plotname <- gsub(":|\\s|\\n|\\?|\\!|\\'", "", plotname)
  
  current.date <- as.character(strftime(Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE))
  
  rstudioapi::savePlotAsImage(
    paste(plotname, ' -- ', current.date, '.', imagetype, sep=''),
    format = imagetype, width = plotwidth, height = plotheight)
}

#* Functions for I2 -----
function_I2_sigma2_Zehan_CIs <- function(res) {
  
  message(paste0("\033[32m\033[1mCall: ", paste(as.character(res$call), collapse = ", ") ,"\033[0m"))
  message(paste0("\033[32m\033[1mMethod: ", res$method,"\033[0m"))
  message(paste0("\033[32m\033[1mdata rows: ", nrow(as.data.frame(res$data)),"\033[0m"))
  message(paste0("\033[32m\033[1mFormula: ", res$formula.mods,"\033[0m"))
  
  ## 1.  Denominator  D  (same formula you use)
  W <- diag(1 / res$vi)
  X <- model.matrix(res)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  D <- (res$k - res$p) / sum(diag(P))
  
  ## 2.  Point estimate of I²
  tau2_hat <- sum(res$sigma2)
  I2_hat   <- 100 * tau2_hat / (tau2_hat + D)
  
  ## 3.  Profile‑likelihood CI for τ²  (first variance component)
  ci       <- confint(res, sigma2 = 1)$random   # matrix 2 × 3
  tau2_lb  <- ci["sigma^2", "ci.lb"]
  tau2_ub  <- ci["sigma^2", "ci.ub"]
  
  ## 4.  Transform τ² limits → I² limits
  I2_lb <- 100 * tau2_lb / (tau2_lb + D)
  I2_ub <- 100 * tau2_ub / (tau2_ub + D)
  
  ## 5.  Return neatly rounded
  round(c(I2       = I2_hat,
          CI.lower = I2_lb,
          CI.upper = I2_ub), 1)
  
  #CI_text <- sprintf("%#.1f%%", I2)
  #CI_text <- sprintf("%s [%s - %s]", CI_text[1], CI.lower, CI.upper)
  # Wrap-up
  #message(paste0("\012\033[32m\033[1mI2: ", CI_text,"\012\033[0m"))
  #return(CI_text)
  
}
#* Functions - other ----------
function_collapse_and_clean_formula <- function(formula){
  formula_all_final <- formula
  # collapse and clean the formula
  formula_all_final <- paste(formula_all_final, collapse = "")
  formula_all_final <- gsub("<.*>", "", formula_all_final)  # Remove environment object
  formula_all_final <- str_remove(formula_all_final, "Outcome_rate")
  # Remove religion (this is formula with religion)
  formula_all_final <- str_remove(formula_all_final, "Religion \\+ ")
  # HAD TO REMOVE YEAR
  #formula_all_final <- str_remove(formula_all_final, "year \\+ ")
  assign("formula_all_final", formula_all_final, envir=.GlobalEnv)
  message(paste0("\033[32m\033[1mformula_all_final:\n", formula_all_final, "\033[0m"))
  return (formula_all_final)
}

# Libraries and packages -----
function_progress(0,'Libraries')

packages_essential <- c('stringr','grid','dplyr','readr', 'crayon')
function_libraries_install(packages_essential)
function_progress(20,'Libraries')

packages_open_office <- c('openxlsx','htmltools')
function_libraries_install(packages_open_office)
function_progress(30,'Libraries')

packages_regression <- (c("betareg", "MASS","StepBeta"))
function_libraries_install(packages_regression)
function_progress(40,'Libraries')

packages_dominance <- (c("domir"))
function_libraries_install(packages_dominance)
function_progress(50,'Libraries')

packages_dominance <- (c("boot"))
function_libraries_install(packages_dominance)
function_progress(60,'Libraries')

packages_viewer_output <- c('knitr','kableExtra','dplyr','DT')
function_libraries_install(packages_viewer_output)
function_progress(70,'Libraries')

packages_spline<- (c("splines","splines2"))
function_libraries_install(packages_spline)
function_progress(80,'Libraries')

packages_meta <- (c("meta", "metafor",'grid'))
function_libraries_install(packages_meta)
function_progress(90,'Libraries')

function_progress(100,'Libraries')

# _________________________------------
# Parameters ----
Outcome.source <- tk_select.list(
  c('Burned_out_rate', 'Burned_out_sometimes_rate','Burned_out_ever_rate', 'Stress_rate'), 
  preselect = 'Burned_out_rate', multiple = FALSE,
  title =paste0("\n\n",Sys.info()["user"], ":\n\nWhat outcome are we studying?\n\n"))
Outcome.source.label <- gsub("_", " ", Outcome.source)

# Data grab -----
#* religion_summary*.csv ----
  # Outcome.source <- "Burned_out_rate"
  # Outcome.source <- "Stress_rate"
  if (Outcome.source == "Burned_out_rate"){
    file.filter   <- matrix(c("Spreadsheets","religion_summary_Burn*.csv;religion_summary_Burn*.xls;religion_summary_Burn*.xlsx","All","..\\data\\*.*"),byrow=TRUE,ncol=2)
  }else{
    file.filter   <- matrix(c("Spreadsheets","religion_summary_Stress*.csv;religion_summary_Stress*.xls;religion_summary_Stress*.xlsx","All","..\\data\\*.*"),byrow=TRUE,ncol=2)
  }
  filename      <- choose.files(filters = file.filter,caption = "Select data file for religion summary (9 rows)",index = 1, multi=FALSE)
  religion_summary <- read_csv(filename)
  
#* regdat*.csv ----
  # Outcome.source <- "Burned_out_rate"
  # Outcome.source <- "Stress_rate"
  if (Outcome.source == "Burned_out_rate"){
    file.filter   <- matrix(c("Spreadsheets","regdat*.csv;regdat*.xls;regdat*.xlsx","All","..\\data\\*.*"),byrow=TRUE,ncol=2)
  }else{
    file.filter   <- matrix(c("Spreadsheets","regdat*.csv;regdat*.xls;regdat*.xlsx","All","..\\data\\*.*"),byrow=TRUE,ncol=2)
  }
  filename      <- choose.files(filters = file.filter,caption = "Select data file for regdat (135 rows)",index = 1, multi=FALSE)
  file.extension<- substr(filename,regexpr("\\.[^\\.]*$", filename)+1, nchar(filename))
  data.import <- NULL
  if (file.extension == 'csv'){
    data.import   <- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", stringsAsFactors=FALSE, strip.white=TRUE)
  }else{
    wb.temp <- loadWorkbook(filename)
    data.import <- read.xlsx (wb.temp, sheet = 1, startRow = 1, colNames = TRUE, na.strings = "NA", detectDates = TRUE)
  }
regdat <- data.import

#* Data process -----
  #** religion_summary*.* ----
  religion_summary$Outcome_variance <- 1 / religion_summary$Outcome_base
  #religion_summary <- religion_summary[order(religion_summary$Outcome_rate), ]
  
  #** regdat*.* ----
  regdat$Outcome_variance <- 1 / regdat$Outcome_base
  
  ModelNames_list <- list()
  R2_list_no_CI <- list()
  I2_list_no_CI <- list()
  
  #* Display religion_summary in viewer -----
  df_Table1 <- religion_summary[,c(2:19)]
  df_Table1$Outcome_rate <- round(df_Table1$Outcome_rate*100,1)
  names(df_Table1)[2] <- Outcome.source.label
  names(df_Table1)[3] <- "Respondents"
  names(df_Table1)[2:17] <-  gsub("_rate", "", names(df_Table1)[2:17])
  names(df_Table1)[2:17] <-  gsub("_", " ", names(df_Table1)[2:17])
  colnames(df_Table1)
  df_Table1[,4:17] <- round(df_Table1[,4:17],1)
  
  function_display_df_in_viewer(df_Table1,
                                paste0("Table. Demographics for ", Outcome.source.label))
  
# _________________________________________-----  
# dat_temp using escalc to eventually become religion_summary4 for the forest plot -------------
  # With dat_temp- later used in dat_prepared1 and sig testing
  dat_temp<- escalc(measure="PLO", 
                    xi = round(religion_summary$Outcome_rate*religion_summary$Outcome_base,0), # Numerator
                    ni = religion_summary$Outcome_base, # Denominator
                    data = religion_summary)
  dat_temp <- dat_temp[, -c(1, 5:18) ]

# PREDICTIONS dataframes------
  regression.type <- 'backward_StepBeta'

  #* 1) Religion only -----
  independents <- "religion_no_mods"
  independents_label <- "Religion only"
  regression.type <- 'backward_BetaReg'
  # With regdat
  res <- rma.mv (Outcome_rate, Outcome_variance,
                  method = 'ML', #measure = "PLO", # escalc set "PLO"
                  random = ~ 1 | Religion, 
                  data = regdat)
  model_religion_no_mods_metafor <- res # needed later
  confint(res)

  ##* I2 ----------------
  (I2_temp <- function_I2_sigma2_Zehan_CIs(res))
  I2_list_no_CI[[independents]] <- 
    paste0(I2_temp["I2"], "% (", I2_temp["CI.lower"], "% to ", I2_temp["CI.upper"], "%)")
  
  ##* R2 ----------------
  model_filename <- paste0("model_final_", regression.type,'_', independents, "_", Outcome.source, ".rds")
  res <- model_temp <- 
    readRDS(model_filename)
  assign(paste0("model_", independents, "_BetaReg"), res) 
  R2_list_no_CI[[independents]] <- sprintf("%#.1f%%", res$pseudo.r.squared*100)
  
  ###* start new dat_temp for dat_prepared1 to later use in religion_summary3 -------
  dat_temp$Predictors     <- independents_label
  # dat_temp$yi and dat_temp$vi already created with escalc
  dat_temp$ni <- religion_summary$Outcome_base    # base or denominator
  dat_temp$xi <- round(religion_summary$Outcome_rate*dat_temp$ni,0) # outcomes or numerators
  ###* creat dat_prepared1 to later use in religion_summary3 -------
  dat_prepared1 <- dat_temp %>%
    dplyr::select("Religion", "Predictors", "xi", "ni", "yi", "vi") %>%
    mutate("Predictors" = independents_label) %>%
    dplyr::select("Religion", "Predictors", "xi", "ni", "yi", "vi")
  
  #* 2) demographics_sans_Religion-----
  # Get formula_temp from file
  independents <- "demographics_sans_Religion"
  independents_label <- "Demographics only"
  regression.type <- 'backward_StepBeta'
  model_filename <- paste0("model_final_", regression.type,'_', independents, "_", Outcome.source, ".rds")
  model_demographics_sans_Religion_BetaReg <-
    model_temp <- 
    readRDS(model_filename)
  formula_temp <-function_collapse_and_clean_formula(model_temp$formula)
  res <- rma.mv(Outcome_rate, Outcome_variance,
                 method = 'ML', measure = "PLO", # escalc set "PLO"
                 random = ~ 1 | Religion, 
                 mods = as.formula(formula_temp), 
                 data = regdat)
  model_demographics_sans_Religion_metafor <- res
  confint(res)

  ##* I2 ----------------
  (I2_temp <- function_I2_sigma2_Zehan_CIs(res))
  I2_list_no_CI[[independents]] <- 
    paste0(I2_temp["I2"], "% (", I2_temp["CI.lower"], "% to ", I2_temp["CI.upper"], "%)")
  
  ##* R2 ----------------
  model_filename <- paste0("model_final_", regression.type,'_', independents, "_", Outcome.source, ".rds")
  res <- model_temp <- 
    readRDS(model_filename)
  assign(paste0("model_", independents, "_BetaReg"), res) 
  R2_list_no_CI[[independents]] <- sprintf("%#.1f%%", res$pseudo.r.squared*100)
  
  #dat<- marginaleffects::predictions(model_final, type = "link", newdata = religion_summary)
  
  ###* start new dat_temp for dat_prepared2 to later use in religion_summary3 -------
  dat_temp<- data.frame(Religion = religion_summary$Religion)
  dat_temp$yi <- betareg::predict(
    model_temp,
    newdata = religion_summary,
    type = "link"
  )
  dat_temp$Predictors <- independents_label # "Demographics only"
  dat_temp$ni <- religion_summary$Outcome_base    # base or denominator
  dat_temp$xi <- round(religion_summary$Outcome_rate * religion_summary$Outcome_base,0) # outcomes or numerators
  
  ###* create predictions -------
  beta_coef <- coef(model_temp, model = "mean")
  beta_vcov <- vcov(model_temp, model = "mean")
  # (3) Recreate the design matrix for your new data using
  ####* the same formula used to fit betareg model ----
  X_new <- model.matrix(
    formula(model_temp),
    data = religion_summary
  )
  # (4) Compute the linear predictor (eta_hat) on the link (logit) scale:
  eta_hat <- as.vector(X_new %*% beta_coef)
  # (5) Compute the variance of the linear predictor for each row:
  #     var_eta[i] = X_new[i, ] %*% beta_vcov %*% t(X_new[i, ])
  #     The rowSums() trick does that for all rows in one shot.
  var_eta <- rowSums((X_new %*% beta_vcov) * X_new)
  # (6) (Optional) Attach to your dataframe
  religion_summary$predicted_logit <- eta_hat       # the logit-scale mean
  religion_summary$var_predicted_logit <- var_eta   # the logit-scale variance
  dat_temp$yi <- eta_hat
  dat_temp$vi <- var_eta
  
  ###* create dat_prepared2 to later use in religion_summary3 -------
  dat_temp$Predictors     <- independents
  independents_label <- independents_label
  dat_prepared2 <- dat_temp%>%
    dplyr::select("Religion", "Predictors", "xi", "ni", "yi", "vi") %>%
    mutate("Predictors" = independents_label) %>%
    dplyr::select("Religion", "Predictors", "xi", "ni", "yi", "vi")
  
  #* 3) all_sans_religion-----
  independents <- "all_sans_religion"
  independents_label <- "Demographics plus stressors"
  regression.type <- 'backward_StepBeta'
  # Get model_temp from file
  # below removed "_text" 2025-05-07
  model_filename <- paste0("model_final_", regression.type,'_',independents, "_", Outcome.source, ".rds")
  model_all_sans_religion_BetaReg <- 
    model_temp <- 
    readRDS(model_filename)
  formula_temp <-function_collapse_and_clean_formula(model_temp$formula)
  # Warning: this uses religion_summary.csv !!!
  res <- rma.mv(Outcome_rate, Outcome_variance, 
                 method = 'ML', measure = "PLO", # escalc set "PLO"
                 random = ~ 1 | Religion, 
                 mods = as.formula(formula_temp), 
                 data = regdat)
  model_all_sans_Religion_metafor <- res
  confint(res)

  ##* I2 ----------------
  (I2_temp <- function_I2_sigma2_Zehan_CIs(res))
  I2_list_no_CI[[independents]] <- 
    paste0(I2_temp["I2"], "% (", I2_temp["CI.lower"], "% to ", I2_temp["CI.upper"], "%)")
  
  ##* R2 ----------------
  model_filename <- paste0("model_final_", regression.type,'_', independents, "_", Outcome.source, ".rds")
  res <- model_temp <- 
    readRDS(model_filename)
  assign(paste0("model_", independents, "_BetaReg"), res) 
  R2_list_no_CI[[independents]] <- sprintf("%#.1f%%", res$pseudo.r.squared*100)
  
  ###* start new dat_temp for dat_prepared3 to later use in religion_summary3 -------
  dat_temp$Predictors     <- independents
  independents_label <- independents_label
  dat_temp<- data.frame(Religion = religion_summary$Religion)
  dat_temp$yi <- betareg::predict(
    model_temp,
    newdata = religion_summary,
    type = "link"
  )
  dat_temp$Predictors <- independents_label
  dat_temp$ni <- religion_summary$Outcome_base    # base or denominator
  dat_temp$xi <- round(religion_summary$Outcome_rate * religion_summary$Outcome_base,0) # outcomes or numerators
  
  ####* create predictions -------
  beta_coef <- coef(model_temp, model = "mean")
  beta_vcov <- vcov(model_temp, model = "mean")
  # (3) Recreate the design matrix for your new data using
  ###* the same formula used to fit betareg model ----
  X_new <- model.matrix(
    formula(model_temp),
    data = religion_summary
  )
  # (4) Compute the linear predictor (eta_hat) on the link (logit) scale:
  eta_hat <- as.vector(X_new %*% beta_coef)
  # (5) Compute the variance of the linear predictor for each row:
  #     var_eta[i] = X_new[i, ] %*% beta_vcov %*% t(X_new[i, ])
  #     The rowSums() trick does that for all rows in one shot.
  var_eta <- rowSums((X_new %*% beta_vcov) * X_new)
  dat_temp$yi <- eta_hat    # the logit-scale mean
  dat_temp$vi <- var_eta    # the logit-scale variance
  
  ###* create dat_prepared3 to later use in religion_summary3 -------
  dat_prepared3 <- dat_temp %>%
    dplyr::select("Religion", "Predictors", "xi", "ni", "yi", "vi") %>%
    mutate("Predictors" = independents_label) %>%
    dplyr::select("Religion", "Predictors", "xi", "ni", "yi", "vi")
  
  remove(dat_temp)
  
  #* Merge predictions into one database -----
  # Sort dataframe - must be before sig testing revisions religion names
  dat_prepared1$rank <- rank(dat_prepared1$yi, ties.method = "first")
  dat_prepared2$rank <- dat_prepared1$rank[match(dat_prepared2$Religion, dat_prepared1$Religion)]
  dat_prepared3$rank <- dat_prepared1$rank[match(dat_prepared3$Religion, dat_prepared1$Religion)]
  
  religion_summary3 <- rbind(dat_prepared1, dat_prepared2, dat_prepared3)
  
  religion_summary3$Predictors <- ifelse(religion_summary3$Predictors =="Demographics and stressors", "Demographics plus stressors", religion_summary3$Predictors)
  
  religion_summary3 <- religion_summary3[order(religion_summary3$rank, -xtfrm(religion_summary3$Predictors)),]

  #* Delete religion and predictors to allow one column of descriptors
  religion_summary3$Religion   <- ifelse(religion_summary3$Predictors !="Religion only", "", religion_summary3$Religion)
  religion_summary3$Predictors <- ifelse(religion_summary3$Predictors =="Religion only", "", paste0('  ',religion_summary3$Predictors))
  
  function_display_df_in_viewer(religion_summary3,'religion_summary3 - for forest plot')

  # _________________________________________-----  
  # Forest plot  -----
  #dev.off()
  my_transf <- function(x) 100 * transf.ilogit(x)
  
  par(mar=c(5.1 + 3, 4.1, 4.1, 1)) # (bottom, left, top, right)
  
  forest_plot <- forest(
    x = religion_summary3$yi,
    #sei = religion_summary3$vi, 
    sei = sqrt(religion_summary3$vi),
    digits = 1,
    slab = religion_summary3$Religion,
    #order = "Religion",  
    header = c('', paste0(Outcome.source.label, ' rate')),
    ilab = cbind(religion_summary3$Predictor), ilab.pos = 4, ilab.xpos = 0.01,
    xlab="Pooled Proportion (%)",
    col = rep(c(1, 8, 8), times = 9),
    plim=c(0.5,1.5),  # plim is ignored with psize
    #at = (seq(0.2, 0.5, 0.1)),
    at = (seq(20, 50, 10)),
    psize = 1 / religion_summary3$vi^(0.0001),
    #xlim=c(0,0.65), # chosen c(0.12,0.52)
    xlim=c(0, 65), # chosen c(0.12,0.52)
    #transf = transf.ilogit,
    transf  = my_transf,
    shade=religion_summary3$Predictors =='',
    cex = 1, efac = 1.5
  )
  # Remove vertical dotted line on the left
  rect(xleft = -10, ybottom = -1, xright = 0, ytop = 28, col = "white", border = "white")
  
  title("Burnout by religious affiliation, demographics, and stressors at work")
  
  mtext('Rate by religous affiliation\n   (predicted by...)', 
        line = -1, at = 0.002, font=2, adj=0, side=3)
  
  function_plot_print ('Figure 1. forest_plot_-_Burnout v2',700,800,'png')

