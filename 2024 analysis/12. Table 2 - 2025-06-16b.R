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
  return(df)
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

# Get source for Libraries/package loading -----
source("00. religion_common_code2_Libraries.R")  

function_progress(100,'Libraries')

# _________________________------------
# Parameters -----
#* Select Data sources ----
data.source <- tk_select.list(c('All',
                                'Trusts and CCGs only'), 
                              preselect = 'Trusts and CCGs only', multiple = FALSE,
                              title =paste0("\n\n",Sys.info()["user"], ":\n\nWho are we studying?\n\n"))
#* Select Outcome sources ----
Outcome.source <- tk_select.list(c('Burned_out_rate', 'Burned_out_sometimes_rate','Burned_out_ever_rate',
                                   'Stress_rate'), 
                                 preselect = 'Burned_out_rate', multiple = FALSE,
                                 title =paste0("\n\n",Sys.info()["user"], ":\n\nWhat outcome are we studying?\n\n"))
Outcome.source.label <- gsub("_", " ", Outcome.source)
#* Select Religion.reference ----
Religion.reference <- tk_select.list(c('Hindu','No religion'),
                                     preselect = 'No religion', multiple = FALSE,
                                     title =paste0("\n\n",Sys.info()["user"], ":\n\nWhich religous group is referent in regression?\n\n"))
#Religion_levels <- relevel(Religion_levels, ref = Religion.reference)
#levels(regdat$Religion)

#* Year -----
year.handling <- tk_select.list(c('Factor','Numeric'), preselect = 'Numeric', multiple = FALSE,
                                title =paste0("\n\n",Sys.info()["user"], ":\n\nHow to handle year?\n\n"))

# _________________________------------
# Get data -------
# Open regdat*.xlsx which is 135 rows
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
## Rename “year” → “Year” *only* if the column is present
if ("year" %in% names(regdat)) {
  names(regdat)[names(regdat) == "year"] <- "Year"
}

cat(green$bold("\nFile: '", basename(filename), "', rows: ", nrow(regdat),".",sep = "" ))

# _________________________------------
#* Data process -----
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

# Religion_levels defined in "00. religion_common_code1_Startup_functions.R"
regdat$Religion <- as.factor(regdat$Religion)
regdat$Religion <- relevel(regdat$Religion, ref = Religion.reference)
levels(regdat$Religion)

regdat$Year <- as.numeric(regdat$Year)

# TABLE 2 ----
# Remove year? 2025-01-09:
# Left columns: no
# Right columns: yes, as was insig in multivariate
# ______________________________------
#* LEFT column start, bivariate-----
##** Religion ----
##* Dataframe to hold Betareg SINGLE bivariate
results <- data.frame(
  Variable = character(),
  Coefficient = numeric(),
  Odds_ratio = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

fmla <- as.formula("Outcome_rate ~ Religion")
model_Religion_no_mods <- betareg(fmla, data = regdat)
coefs <- model_Religion_no_mods$coefficients$mean
coef_tab <- summary(model_Religion_no_mods)$coefficients$mean   # columns: Estimate, SE, z, p

invlogit <- function(eta) 1/(1 + exp(-eta))

## reference values 
ref_lp   <- coefs["(Intercept)"]             # "No religion"
mu_ref   <- invlogit(ref_lp)
odds_ref <- exp(ref_lp)

## build table 
# burnout_rate_100 is the estimated rate of burnout if 100% of respondents had the cofactor
results_religion <- data.frame(
  Variable          = sub("^Religion", "", names(coefs)),
  Coefficient       = coefs,
  Odds_ratio        = NA_real_,
  RR                = NA_real_,
  burnout_rate_100  = NA_real_,
  p_value           = coef_tab[, 4],           # raw p-values
  stringsAsFactors  = FALSE
)

for (i in seq_along(coefs)) {
  lp_i <- ref_lp + if (i == 1) 0 else coefs[i]
  mu_i <- invlogit(lp_i)
  
  results_religion$burnout_rate_100[i] <- mu_i
  
  if (i == 1) {                    # reference group
    results_religion$Odds_ratio[i] <- 1
    results_religion$RR[i]         <- 1
  } else {
    results_religion$Odds_ratio[i] <- exp(coefs[i])   # odds_i / odds_ref
    results_religion$RR[i]         <- mu_i / mu_ref   # risk_i / risk_ref
  }
}

results_religion[ , 2:6] <- lapply(results_religion[ , 2:6], 
                            function(x) sprintf('%.3f', as.numeric(x)))

results_religion$Variable <-
  ifelse(str_sub(results_religion$Variable, 1, 5) == "I wou",
         "Withheld",
         results_religion$Variable)

results_religion$Variable <-
  ifelse(str_sub(results_religion$Variable, 1, 5) == "Any o",
         "Other",
         results_religion$Variable)

#rownames(results_religion) <- results_religion$Variable

###* Ordering -----
results_religion <- results_religion %>% 
  arrange(
    case_when(
      Variable == "(Intercept)"                        ~ -1,  # always first
      Variable %in% c("Withheld")                      ~  2,  # always last
      Variable %in% c("Other", "Any other religion")   ~  1,  # next-to-last
      TRUE                                             ~  0   # all others
    ),
    Variable                                           # alphabetical inside group 0
  )

function_display_df_in_viewer(results_religion, "results_religion")

##** Non religion covariates -----
vars_nonreligion <- grep("rate", names(regdat), value = TRUE, ignore.case = TRUE)
vars_nonreligion <- vars_nonreligion[!grepl("burned", vars_nonreligion, ignore.case = TRUE)]
vars_nonreligion <- setdiff(vars_nonreligion, c("Benchmarking_group","Outcome_rate","Stress_rate"))
vars_nonreligion <- c(vars_nonreligion,"Year")

results_nonreligion <- bind_rows(lapply(vars_nonreligion, function(var) {
  
  ## 1) fit bivariate model --------------------------------------------------
  fmla <- reformulate(var, response = "Burned_out_rate")   # Outcome ~ var
  fit  <- betareg(fmla, data = regdat)
  
  ## 2) pull coefficients & p-value -----------------------------------------
  b    <- coef(fit)                       # numeric vector
  b0   <- b["(Intercept)"]
  b1   <- b[var]
  pval <- summary(fit)$coefficients$mean[var, 4]
  
  ## 3) predicted rates & effect measures -----------------------------------
  mu0  <- invlogit(b0)                    # predictor = 0
  mu1  <- invlogit(b0 + b1)               # predictor = 1
  tibble(
    Variable         = var,
    Coefficient      = b1,
    Odds_ratio       = exp(b1),
    RR               = mu1 / mu0,
    burnout_rate_100 = mu1,
    p_value          = pval
  )
}))

results_nonreligion <- as.data.frame(results_nonreligion) # tibble → df

results_nonreligion[ , 2:6] <- lapply(results_nonreligion[ , 2:6], 
                                   function(x) sprintf('%.3f', as.numeric(x)))

###* Ordering the rows-----
###** Read the formula-----
form_text <- readLines(
  "formula_initial_all_and_religion_reduced_Burned_out_rate.txt"
) |> paste(collapse = " ")

form_vars <- form_text |>
  str_extract("(?<=~).*?(?=<environment)") |>      # text between ~ and <environment
  str_replace_all("[\\r\\n]", " ") |>              # drop CR/LF
  str_squish() |>                                  # squeeze whitespace
  str_split("\\+") |>                              # split at '+'
  unlist() |>
  str_trim()

ordering <- setdiff(form_vars,
                    c("Religion", "Outcome_rate", "Burned_out_rate"))

ordering <- ordering[ordering %in% results_nonreligion$Variable]

results_nonreligion <- results_nonreligion |>
  mutate(Variable = factor(Variable, levels = ordering)) |>
  arrange(Variable) |>
  mutate(Variable = as.character(Variable))          # keep it character

print(results_nonreligion)

results <- rbind(results_religion, results_nonreligion)

# Note: 2025-01-11: changed values for: 
# "I would prefer not to say" and 
#  year
function_display_df_in_viewer(results, "<b>Table 2a (left):</b> with religion bivariate P values")

#* RIGHT column, multivariate-----
# Remove year from right column? yes 2025-01-09, as was insig in multivariate

model_filename <- "model_final_backward_StepBeta_all_and_Religion_Burned_out_rate.rds"
model_temp <- readRDS(model_filename)
cat(crayon::green$bold(model_temp$formula, "\n"))

R2 <- model_temp$pseudo.r.squared
message(paste0("\033[32m\033[1mSig all and religion R2: ",
               round(100 * R2, 1), "%\033[0m"))

coef_vec   <- model_temp$coefficients$mean                 # numeric vector
coef_table <- summary(model_temp)$coefficients$mean        # SE, z, p etc.

ref_lp <- coef_vec["(Intercept)"]                          # “baseline” LP
mu_ref <- invlogit(ref_lp)

results_final <- tibble::tibble(
  Variable         = names(coef_vec),
  Coefficient      = coef_vec,
  Odds_ratio       = exp(coef_vec),                        # OR = e^β
  RR               = NA_real_,
  burnout_rate_100 = NA_real_,
  p_value          = coef_table[, 4]                       # raw p-values
) %>%
  ## compute μ̂ at predictor = 1 and RR -------------------------
dplyr::mutate(
  lp_1             = ref_lp + dplyr::if_else(Variable == "(Intercept)",
                                             0, Coefficient),
  burnout_rate_100 = invlogit(lp_1),
  RR               = dplyr::if_else(Variable == "(Intercept)",
                                    1, burnout_rate_100 / mu_ref)
) %>%
  ## tidy formatting -------------------------------------------
dplyr::mutate(
  Coefficient      = sprintf('%.3f', Coefficient),
  Odds_ratio       = sprintf('%.3f', Odds_ratio),
  RR               = sprintf('%.3f', RR),
  burnout_rate_100 = sprintf('%.3f', burnout_rate_100),
  p_value          = sprintf('%.3f', p_value)
) %>%
  dplyr::select(-lp_1)                                     # drop helper column

## move Variable column to row-names and drop the column ------
results_final <- as.data.frame(results_final)

results_final[ , 2:6] <- lapply(results_final[ , 2:6], 
                                   function(x) sprintf('%.3f', as.numeric(x)))

results_final$Variable <-
  ifelse(str_sub(results_final$Variable, 1, 13) == "ReligionI wou",
         "ReligionWithheld",
         results_final$Variable)

results_final$Variable <-
  ifelse(str_sub(results_final$Variable, 1, 13) == "ReligionAny o",
         "ReligionOther",
         results_final$Variable)

idx <- grepl("^Religion", results_final$Variable)

##* results_final_religion -----
results_final_religion <- results_final[idx,  , drop = FALSE]
results_final_religion$Variable <- str_sub(results_final_religion$Variable,9)

###* Ordering -----
results_final_religion <- results_final_religion %>% 
  arrange(
    case_when(
      Variable == "(Intercept)"                        ~ -1,  # always first
      Variable %in% c("Withheld")                      ~  2,  # always last
      Variable %in% c("Other", "Any other religion")   ~  1,  # next-to-last
      TRUE                                             ~  0   # all others
    ),
    Variable                                           # alphabetical inside group 0
  )

##* results_final_other -----
results_final_other    <- results_final[!idx, , drop = FALSE]
results_final_other$Variable
###* Ordering -----
ordering2 <- c("(Intercept)", ordering)          # prepend or append, as you like

results_final_other <- results_final_other %>% 
  mutate(Variable = factor(Variable, levels = ordering2)) %>% 
  arrange(Variable) %>% 
  mutate(Variable = as.character(Variable))

#rownames(results_final) <- results_final$Variable


results_final <- rbind(results_final_religion,results_final_other)

results_final <- results_final[order(results_final$Variable != "(Intercept)"), ]

##  View / export ----------------------------------------------

function_display_df_in_viewer(
  results_final,
  paste0("<b>Table 2b (right):</b> Final multivariable model, R² = ",
         round(R2 * 100, 1), "%")
)

function_general_dominance_values(model_temp, regdat)

