# This file is available at https://github.com/ebmgt/NHS-Religion/
# Author:rbadgett@kumc.edu and zyang.uconn@gmail.com
# Permission: GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
# Last edited 2025-06-14

# Get source for Starts up and functions -----
source("00. religion_common_code1_Startup_functions.R") 
function_display_model_in_viewer <- function(model, caption) {
  temp <- summary(model)
  #summary(model_final)
  odds_ratios_df <- data.frame(Coefficient = round(temp$coefficients$mean[, "Estimate"],3) ,
                               Odds_Ratio  = sprintf(exp(temp$coefficients$mean[, "Estimate"]), fmt='%#.3f'), 
                               P.value     = sprintf(temp$coefficients$mean[, "Pr(>|z|)"], fmt='%#.3f'))
  DT::datatable(odds_ratios_df, caption = caption, options = list(pageLength = 25))
}

# Display openxlsx worksheet in the viewer
function_display_xlsx_in_viewer <- function(wb_path_or_object, sheet) {
  # Check if input is a file path or a workbook object
  if (is.character(wb_path_or_object)) {
    # If it's a file path, load the workbook
    wb <- loadWorkbook(wb_path_or_object)
  } else {
    # If it's already a workbook object, use it
    wb <- wb_path_or_object
  }
  
  # Extract data from the specified sheet
  sheet_data <- read.xlsx(wb, sheet = sheet)
  
  # Check if sheet_data is valid
  if (is.null(sheet_data) || nrow(sheet_data) == 0) {
    stop("The sheet is empty or doesn't exist.")
  }
  
  # Convert the data frame to an HTML table
  html_table <- HTML(paste0(
    '<table border="1" style="width:100%; border-collapse:collapse;">',
    paste0(
      '<tr><th>', paste(names(sheet_data), collapse = '</th><th>'), '</th></tr>',
      paste(apply(sheet_data, 1, function(row) {
        paste0('<tr><td>', paste(row, collapse = '</td><td>'), '</td></tr>')
      }), collapse = "")
    ),
    '</table>'
  ))
  
  # Render the HTML table in the RStudio Viewer
  html_print(html_table)
}

function_plot_print <- function (plotname, plotwidth, plotheight){
  plotname <- gsub(":|\\s|\\n|\\?|\\!|\\'", "", plotname)
  (current.date <- as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE)))
  rstudioapi::savePlotAsImage(
    paste(plotname,' -- ',current.date,'.tif',sep=''),
    format = "tiff", width = plotwidth, height = plotheight)
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

#* bootstrapping ----
#  Helper that builds a (data, indices) function for boot::boot()
make_boot_fun <- function(model_fun,          # e.g. betareg, glm, lmer
                          formula,            # formula object
                          stat_fun,           # returns a single number
                          ...) {              # extra args for model_fun
  force(model_fun); force(formula); force(stat_fun); force(list(...))
  
  function(data, indices) {
    d   <- data[indices, , drop = FALSE]      # resampled rows
    fit <- model_fun(formula = formula,
                     data    = d,
                     ...)                     # dots passed through
    stat_fun(fit)                             # the statistic returned
  }
}

function_collapse_and_clean_formula <- function(formula){
  formula_all_final <- formula
  # collapse and clean the formula
  formula_all_final <- paste(formula_all_final, collapse = "")
  formula_all_final <- gsub("<.*>", "", formula_all_final)  # Remove environment object
  #formula_all_final <- gsub('\\"', '', formula_all_final)
  #formula_all_final <- str_remove(formula_all_final, "~")
  formula_all_final <- str_remove(formula_all_final, "Outcome_rate")
  # Remove religion (this is formula with religion)
  formula_all_final <- str_remove(formula_all_final, "Religion \\+ ")
  # Removed year from all models as was not sig in final model
  # Only affects demographics sans religion
  formula_all_final <- str_remove(formula_all_final, "year \\+ ")
  # Start with "~"
  #formula_all_final <- paste0('~ ', formula_all_final)
  assign("formula_all_final", formula_all_final, envir=.GlobalEnv)
  message(paste0("\033[32m\033[1mformula_all_final:\n", formula_all_final, "\033[0m"))
  return (formula_all_final)
}
# Libraries/packages --------------
function_progress(0,'Libraries')

#* Essential -----
packages_essential <- c('stringr','grid','dplyr','readr','tidyverse','crayon')
function_libraries_install(packages_essential)
function_progress(40,'Libraries')

#* openoffice -----
packages_openoffice <- (c("openxlsx")) # ‘officer’ is for word / pptx
function_libraries_install(packages_openoffice)
function_progress(60,'Libraries')

#* Beta-regression -----
packages_betareg <- (c("betareg","MASS","StepBeta")) # ‘officer’ is for word / pptx
function_libraries_install(packages_betareg)
function_progress(70,'Libraries')

#* Meta-analyses -----
packages_meta <- (c("metafor","boot"))
function_libraries_install(packages_meta)
function_progress(90,'Libraries')

#* Viewer output -----
packages_viewer_output <- c('knitr','kableExtra','dplyr','DT','htmltools')
function_libraries_install(packages_viewer_output)
function_progress(100,'Libraries')

# _________________________-------
# Set parameters ------------
# Parameters -----
Outcome.source <- tk_select.list(c('Burned_out_rate', 'Burned_out_sometimes_rate','Burned_out_ever_rate',
                                   'Stress_rate'), 
                                 preselect = 'Burned_out_rate', multiple = FALSE,
                                 title =paste0("\n\n",Sys.info()["user"], ":\n\nWhat outcome are we studying?\n\n"))
Outcome.source.label <- gsub("_", " ", Outcome.source)

# Get data -------
# Open regdat.xlsx which is 135 rows
# Uses Burned out rate rather than Outcome rate
file.filter   <- matrix(c("Spreadsheets","regdat*.csv;regdat*.xls;regdat*.xlsx","All","..\\data\\*.*"),byrow=TRUE,ncol=2)
filename      <- choose.files(filters = file.filter,caption = "Select data file",index = 1, multi=FALSE)
file.extension<- substr(filename,regexpr("\\.[^\\.]*$", filename)+1, nchar(filename))
data.import <- NULL
if (file.extension == 'csv'){
  data.import   <- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", stringsAsFactors=FALSE, strip.white=TRUE)
}else{
  #data.import   <- read.xlsx(filename)
  #data.import<- read.table(filename, header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  # Help https://rdrr.io/cran/openxlsx/man/loadWorkbook.html
  wb.temp <- loadWorkbook(filename)
  data.import <- read.xlsx (wb.temp, sheet = 1, startRow = 1, colNames = TRUE, na.strings = "NA", detectDates = TRUE)
}

regdat <- data.import

if ("year" %in% names(regdat)) {
  names(regdat)[names(regdat) == "year"] <- "Year"
}
regdat$Outcome_variance <- 1 / regdat$Outcome_base

ModelNames_list <- list()
R2_list <- list()
I2_list <- list()

# New 2025-03-07
cat(green$bold("\nChecking for rates between 0 and 1\n\n"))
rate_cols <- grep("rate", names(regdat), ignore.case = TRUE, value = TRUE)
rate_means <- sapply(regdat[, rate_cols, drop = FALSE], mean, na.rm = TRUE)
# Identify columns where mean > 1
cols_to_adjust <- names(rate_means[rate_means > 1])
# Print warning message if any columns have a mean > 1
if (length(cols_to_adjust) > 0) {
  cat(red$bold("These columns have rates > 1:\n"), 
      red$bold(paste(cols_to_adjust, collapse = ", ")), 
      "\n")
  
  # Divide the identified columns by 100
  regdat[, cols_to_adjust] <- regdat[, cols_to_adjust] / 100
  
  # Print confirmation message
  cat(green$bold("Columns adjusted (divided by 100):\n"), 
      green$bold(paste(cols_to_adjust, collapse = ", ")), 
      "\n")
} else {
  cat(green$bold("All rate columns are within (0,1). No adjustments needed.\n"))
}

if (Outcome.source == 'Burned_out_rate'){
  regdat$Outcome_rate <- regdat$Burned_out_rate
  regdat$Outcome_base <- regdat$Burned_out_base
} else if (Outcome.source == 'Burned_out_sometimes_rate'){
  regdat$Outcome_rate <- regdat$Burned_out_sometimes_rate
  regdat$Outcome_base <- regdat$Burned_out_base
} else if (Outcome.source == 'Burned_out_ever_rate'){
  regdat$Outcome_rate <- regdat$Burned_out_ever_rate
  regdat$Outcome_base <- regdat$Burned_out_base
} else{
  regdat$Outcome_rate <- regdat$Stress_rate
  regdat$Outcome_base <- regdat$Stress_base
}

#* 0. religion_no_mods ------
independents <- "Religion_no_mods"
independents_label <- "Religion only"
regression.type <- 'backward_StepBeta'

##** Get model and formula -----
# RDS binary, no text
# Watchout, this has unique filename due to not needing StepBeta
# No good: model_filename <- paste0("model_final_", regression.type,'_',independents, "_", Outcome.source, ".rds")
model_filename <- paste0("model_final_backward_BetaReg_",independents, "_", Outcome.source, ".rds")
model_temp <- readRDS(model_filename)
assign(paste0("model_",independents), model_temp)
function_display_model_in_viewer(model_temp, paste0(independents_label, ": ", regression.type))
cat(crayon::green$bold(model_temp$formula, "\n"))

##** R2 -------------
R2_list[[independents]] <- model_temp$pseudo.r.squared
assign(paste0("R2_",independents), model_temp$pseudo.r.squared)
cat(crayon::green$bold("R2: ",
                       get(paste0("R2_", independents))*100
))

# Get R2 with CIs
tk_messageBox(type = "ok", "Slow code ahead!", caption = "Bootstrapping, first of four bootstaps")
my_stat <- function(fit) unname(fit$pseudo.r.squared)  # one numeric value
boot_fun <- make_boot_fun(betareg, model_temp$formula, my_stat)
set.seed(2025)
B <- 2000
boot_out <- boot(
  data      = regdat,
  statistic = boot_fun,
  R         = B,
  parallel  = "multicore"  # or "snow" / "no"
)
temp <- boot.ci(boot_out, type = c("perc", "bca"))
temp <- paste0("(", round(100*temp$bca[4],1),"% to ", round(100*temp$bca[5],1),"%)")
temp <- paste0(round( get(paste0("R2_", independents))*100,1), "% ",temp)
R2_list[[independents]] <- temp
(R2_religion_no_mods_CIs <- temp)

##** I2 ------------
# Below removes Religion from the betareg formula
formula_temp <- function_collapse_and_clean_formula (model_temp$formula)

res <- rma.mv(Outcome_rate, Outcome_variance, 
              method = 'ML', measure = "PLO", # escalc set "PLO"
              random = ~ 1 | Religion, # 2025-04-30
              # No mods for the religion only model
              #mods = as.formula(formula_temp), 
              data = regdat)
confint(res)
(I2_temp <- function_I2_sigma2_Zehan_CIs(res))

I2_list[[independents]] <- 
  paste0(I2_temp["I2"], "% (", I2_temp["CI.lower"], "% to ", I2_temp["CI.upper"], "%)")

#* 1. Demographics SANS religion -----
independents <- "demographics_sans_Religion"
independents_label <- "Demographics only"
regression.type <- 'backward_StepBeta'

##** Get model and formula -----
# RDS binary, no text
model_filename <- paste0("model_final_", regression.type,'_',independents, "_", Outcome.source, ".rds")
model_temp <- readRDS(model_filename)
assign(paste0("model_",independents), model_temp)
function_display_model_in_viewer(model_temp, paste0(independents_label, ": ", regression.type))
cat(crayon::green$bold(model_temp$formula, "\n"))

##** R2 -------------
R2_list[[independents]] <- model_temp$pseudo.r.squared
assign(paste0("R2_",independents), model_temp$pseudo.r.squared)
cat(crayon::green$bold("R2: ",
                       get(paste0("R2_", independents))*100
))

# Get R2 with CIs
my_stat <- function(fit) unname(fit$pseudo.r.squared)  # one numeric value
boot_fun <- make_boot_fun(betareg, model_temp$formula, my_stat)
set.seed(2025)
B <- 2000
boot_out <- boot(
  data      = regdat,
  statistic = boot_fun,
  R         = B,
  parallel  = "multicore"  # or "snow" / "no"
)
temp <- boot.ci(boot_out, type = c("perc", "bca"))
temp <- paste0("(", round(100*temp$bca[4],1),"% to ", round(100*temp$bca[5],1),"%)")
temp <- paste0(round( get(paste0("R2_", independents))*100,1), "% ",temp)
(R2_list[[independents]] <- temp)

##** I2_demographics_sans_Religion -----
# Below removes Religion from the betareg formula
formula_temp <- function_collapse_and_clean_formula (model_temp$formula)
res <- rma.mv(Outcome_rate, Outcome_variance, 
              method = 'ML', measure = "PLO", # escalc set "PLO"
              random = ~ 1 | Religion, # 2025-04-30
              mods = as.formula(formula_temp), 
              data = regdat)
confint(res)
(I2_temp <- function_I2_sigma2_Zehan_CIs(res))

I2_list[[independents]] <- 
  paste0(I2_temp["I2"], "% (", I2_temp["CI.lower"], "% to ", I2_temp["CI.upper"], "%)")

#2. Demographics and stressors all_sans_religion ------
independents <- "all_sans_Religion"
independents_label <- "Demographics and stressors"
regression.type <- 'backward_StepBeta'

##** Get model and formula -----
# RDS binary, no text
model_filename <- paste0("model_final_", regression.type,'_',independents, "_", Outcome.source, ".rds")
model_temp <- readRDS(model_filename)
assign(paste0("model_",independents), model_temp)
function_display_model_in_viewer(model_temp, paste0(independents_label, ": ", regression.type))
cat(crayon::green$bold(model_temp$formula, "\n"))

##** R2 -------------
R2_list[[independents]] <- model_temp$pseudo.r.squared
assign(paste0("R2_",independents), model_temp$pseudo.r.squared)
cat(crayon::green$bold("R2: ",
                       get(paste0("R2_", independents))*100
))

# Get R2 with CIs
my_stat <- function(fit) unname(fit$pseudo.r.squared)  # one numeric value
boot_fun <- make_boot_fun(betareg, model_temp$formula, my_stat)
set.seed(2025)
B <- 2000
boot_out <- boot(
  data      = regdat,
  statistic = boot_fun,
  R         = B,
  parallel  = "multicore"  # or "snow" / "no"
)
temp <- boot.ci(boot_out, type = c("perc", "bca"))
temp <- paste0("(", round(100*temp$bca[4],1),"% to ", round(100*temp$bca[5],1),"%)")
temp <- paste0(round( get(paste0("R2_", independents))*100,1), "% ",temp)
(R2_list[[independents]] <- temp)

##** I2_all_sans_religions -----
# Below removes Religion from the betareg formula
formula_temp <- function_collapse_and_clean_formula (model_temp$formula)
res <- rma.mv(Outcome_rate, Outcome_variance, 
              method = 'ML', measure = "PLO", # escalc set "PLO"
              random = ~ 1 | Religion, # 2025-04-30
              mods = as.formula(formula_temp), 
              data = regdat)

confint(res)
(I2_temp <- function_I2_sigma2_Zehan_CIs(res))

I2_list[[independents]] <- 
  paste0(I2_temp["I2"], "% (", I2_temp["CI.lower"], "% to ", I2_temp["CI.upper"], "%)")

#3. all_and_religion -----
# IMPORTANT: When all non-religious variables were combined with religion, harassment by colleagues was replace by physical violence by colleagues
independents <- "all_and_Religion"
independents_label <- "Demographics, stressors, and religion"
regression.type <- 'backward_StepBeta'

##** Get model and formula -----
# RDS binary, no text
model_filename <- paste0("model_final_", regression.type,'_',independents, "_", Outcome.source, ".rds")
model_temp <- readRDS(model_filename)
assign(paste0("model_",independents), model_temp)
function_display_model_in_viewer(model_temp, paste0(independents_label, ": ", regression.type))
cat(crayon::green$bold(model_temp$formula, "\n"))

##** R2 -------------
R2_list[[independents]] <- model_temp$pseudo.r.squared
assign(paste0("R2_",independents), model_temp$pseudo.r.squared)
cat(crayon::green$bold("R2: ",
    get(paste0("R2_", independents))*100
))

# Get R2 with CIs
my_stat <- function(fit) unname(fit$pseudo.r.squared)  # one numeric value
boot_fun <- make_boot_fun(betareg, model_temp$formula, my_stat)
set.seed(2025)
B <- 2000
boot_out <- boot(
  data      = regdat,
  statistic = boot_fun,
  R         = B,
  parallel  = "multicore"  # or "snow" / "no"
)
temp <- boot.ci(boot_out, type = c("perc", "bca"))
temp <- paste0("(", round(100*temp$bca[4],1),"% to ", round(100*temp$bca[5],1),"%)")
temp <- paste0(round( get(paste0("R2_", independents))*100,1), "% ",temp)
(R2_list[[independents]] <- temp)

##** I2_all_and_Religion -----
# Below removes Religion from the betareg formula
formula_temp <- function_collapse_and_clean_formula (model_temp$formula)
res <- rma.mv(Outcome_rate, Outcome_variance, 
              method = 'ML', measure = "PLO", # escalc set "PLO"
              random = ~ 1 | Religion, # 2025-04-30
              mods = as.formula(formula_temp), 
              data = regdat)
res
confint(res)
(I2_temp <- function_I2_sigma2_Zehan_CIs(res))

I2_list[[independents]] <- 
  paste0(I2_temp["I2"], "% (", I2_temp["CI.lower"], "% to ", I2_temp["CI.upper"], "%)")

#* Likelihood ratio test (LRT) ----------
# ModelNames_list
model_Religion_no_mods
get(paste0("R2_", independents))
logLik_reduced <- logLik(model_Religion_no_mods)
logLik_full    <- logLik(model_all_and_Religion)

function_Likelihood_ratio_test <- function (model_reduced, model_full){
  logLik_full     <-  logLik(model_full)
  logLik_reduced  <-  logLik(model_reduced)
  # logLik_full     <-  logLik(model_demographics_sans_Religion)
  # logLik_reduced  <-  logLik(model_Religion_no_mods)
  lrt_stat <- 2 * (logLik_full - logLik_reduced)
  df_diff <-abs(attr(logLik_full, "df") - attr(logLik_reduced, "df"))
  p_value <- pchisq(lrt_stat, df = df_diff, lower.tail = FALSE)
  return (p_value <- as.character(sprintf(p_value, fmt='%#.3f')))
}

factor_count <- length(model_all_and_Religion$coefficients$mean) - 1 - 7 # 7 if for the levels of religion other than the reference factor

#* Table 3 display -----------------
# Create a data frame for data_Table1

df_Table3 <- data.frame(
  Predictors = character(),
  R2  = character(),
  I2  = character(),
  'Incremental signficance' = character(),
  stringsAsFactors = FALSE
)

#** Religion only -----
factor_count <- 1
p_value <- function_Likelihood_ratio_test(model_Religion_no_mods, model_demographics_sans_Religion)
df_Table3_temp_row <- data.frame(
  Predictors = paste0("Religion only<br>(", factor_count, " covariates)"),
  R2  = paste0("", R2_list['Religion_no_mods']),
  I2  = paste0("", I2_list['Religion_no_mods']),
  'Incremental signficance' = 'NA',
  stringsAsFactors = FALSE
)
df_Table3 <- rbind(df_Table3,df_Table3_temp_row)

#** demographics_sans_Religion -----
factor_count <- length(model_demographics_sans_Religion$coefficients$mean) - 1
p_value <- function_Likelihood_ratio_test(model_Religion_no_mods, model_demographics_sans_Religion)
df_Table3_temp_row <- data.frame(
  Predictors = paste0("Demographics other than religion<br>(", factor_count, " covariates)"),
  R2  = paste0("", R2_list['demographics_sans_Religion']),
  I2  = paste0("", I2_list['demographics_sans_Religion']),
  'Incremental signficance' = p_value,
  stringsAsFactors = FALSE
)
df_Table3 <- rbind(df_Table3,df_Table3_temp_row)

#** all_sans_religion -----
p_value <- function_Likelihood_ratio_test(model_demographics_sans_Religion, model_all_sans_Religion)
factor_count <- length(model_all_sans_Religion$coefficients$mean) - 1
df_Table3_temp_row <- data.frame(
  Predictors = paste0("Stressors and demographics\nother than religion<br>(", factor_count, " covariates)"),
  R2  = paste0("", R2_list['all_sans_Religion']),
  I2  = paste0("", I2_list['all_sans_Religion']),
  'Incremental signficance' = p_value,
  stringsAsFactors = FALSE
)
df_Table3 <- rbind(df_Table3,df_Table3_temp_row)

#** All factors, including religion -----
p_value <- function_Likelihood_ratio_test(model_all_sans_Religion, model_all_and_Religion)
factor_count <- length(model_all_and_Religion$coefficients$mean) - 1 - 7
df_Table3_temp_row <- data.frame(
  Predictors = paste0("All factors, including religion<br>(", factor_count, " covariates)"),
  R2  = paste0("", R2_list['all_and_Religion']),
  I2  = paste0("", I2_list['all_and_Religion']),
  'Incremental signficance' = p_value,
  stringsAsFactors = FALSE
)
df_Table3 <- rbind(df_Table3,df_Table3_temp_row)

# Create a kable and display it in the Viewer with a title

df_Table3$R2 <- str_replace(df_Table3$R2, "\\(", "<br>(")
df_Table3$I2 <- str_replace(df_Table3$I2, "\\(", "<br>(")

kable_output <- knitr::kable(df_Table3, format = "html", 
  col.names = c("Predictors", 
                "Variance*<br> in burnout<br>explained<br>R²", 
                "Heterogeneity\u2020<br>unexplained in burnout<br>I²", 
                "Incremental improvement<br>(p-value)\u2021"), align='l', escape = FALSE) %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  #add_header_above(c("Table 3.  Comparison of contributors to burnouts" = 4), align = "l", bold = TRUE) %>%  # Adds a title above the table
  column_spec(1, width = "30%") %>%  # Adjust width of 'Predictors' column
  column_spec(2:4, width = "15%") %>%  # Adjust width and center-align other columns
  #row_spec(1:nrow(df_Table3)) %>%  # Center-align content rows
  #scroll_box(width = "100%") %>%
  footnote(general = " ", 
           general_title = "", 
           footnote_as_chunk = TRUE, 
           escape = FALSE, 
           threeparttable = TRUE)

title_html  <- paste0("<div style='text-align: left;'><b>Table 3.</b> Comparison of contributors to burnout across the NHS workforce.</div>",
                      "<hr style='width: 100%; margin-top: 10px; margin-bottom: 10px;'>")
footer_html <- paste0("<hr style='width: 100%;'>",
                      "* Proportion of variance in burnout across reporting groups explained by all predictors including religion (beta regression).<br>",
                      "† Heterogeneity of variance in burnout between reporting groups (mixed effects regression).<br>",
                      "‡ Likelihood ratio test comparing the beta-regression models in the current row with the model in the preceding row.<br>",
                      "§ When all non-religious variables were combined with religion, harassment by colleagues was replaced by physical violence by colleagues."
                      )
html_output <- HTML(title_html,
                    "<style>hr{height:1px}</style>",
                    kable_output,
                    footer_html)
html_print(html_output) 

#function_display_df_in_viewer(df_Table3,"Table 3. Comparison of contributors to burnout.")

