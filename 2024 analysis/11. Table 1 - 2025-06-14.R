# This file is available at https://github.com/ebmgt/NHS-Religion/
# Author:rbadgett@kumc.edu and zyang.uconn@gmail.com
# Permission: GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
# Last edited 2025-06-14

# Get source for Starts up and functions -----
source("00. religion_common_code1_Startup_functions.R")  
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
cat(green$bold("\nFile: '", basename(filename), "', rows: ", nrow(regdat),".",sep = "" ))

# Data process -----
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
Religion_levels <- relevel(Religion_levels, ref = Religion.reference)
regdat$Religion <- factor(regdat$Religion, 
                          levels = Religion_levels)
levels(regdat$Religion)
regdat$Year <- as.numeric(regdat$Year)

# TABLE 1 ----
# Create a data frame for data_Table1
data_Table1 <- data.frame(
  RowName = character(),
  Total  = character(),
  Christian  = character(),
  Jewish  = character(),
  Muslim = character(),
  Buddhist = character(),
  Hindu = character(),
  Sikh = character(),
  Other = character(),
  Withheld = character(),
  No_Religion = character(),
  Significance_bivariate = numeric(),
  Significance_multivariate = numeric(),
  stringsAsFactors = FALSE
)

# Row: respondents_total_estimated (but imperfect estimate)
# regdat$Respondents is the MAX number of responses to any one question and 
# was created in combined_data$Respondents in the 
# R script "2. Datafile creation from org type download file (Benchmarking_Group)- 2024-12-19.R"
(respondents_total_estimated <- sum(regdat$Respondents)) # 1878675

# new as of 2025-01-09
(respondents_total <- sum(regdat$Burned_out_base)) # 1876265

respondents_religion <- sapply(Religion_levels, function(r) {
  round(sum(regdat$Respondents[regdat$Religion == r], na.rm = TRUE))
})

temp_cell_values <- sapply(Religion_levels, function(r) {
  value <- respondents_religion[Religion_levels == r]
  paste(
    formatC(value, digits = 0, format = "f", big.mark = ","),' ',
    round(100 * value / respondents_total, 0),
    '%',
    sep = ""
  )
})

temp_cell_values <- sapply(Religion_levels, function(r) {
  value <- sum(regdat$Burned_out_base[regdat$Religion == r])  # Correct indexing by religion level
  paste(
    formatC(value, digits = 0, format = "f", big.mark = ","),
    " ",
    round(100 * value / respondents_total, 0),
    "%",
    sep = ""
  )
})

respondents_total <- formatC(respondents_total, digits = 0, format = "f", big.mark = ",")

temp_cell_values <- c(paste(respondents_total, '100%'),
                      temp_cell_values)
temp_row_values <- c('Respondents', temp_cell_values, NA, NA) # Significance coluumns are empty
data_Table1[nrow(data_Table1) + 1, ] <- temp_row_values

function_Table1_addrow <- function(factor){
  # Religion_levels
  # factor <- 'Burned_out'
  # r <- 'Hindu'
  # Calculate values for each religion level
  temp_cell_values <- sapply(Religion_levels, function(r) {
    # Filter rows for the current religion
    numerator <- round((regdat[[paste0(factor, '_rate')]][regdat$Religion == r]) * #  / 100) * removed 2025-03-07
                         regdat[[paste0(factor, '_base')]][regdat$Religion == r], 0)
    denominator <- regdat[[paste0(factor, '_base')]][regdat$Religion == r]
    # Calculate rate for the current religion
    round(sum(numerator, na.rm = TRUE) / sum(denominator, na.rm = TRUE) * 100, 1)
  })
  
  # Calculate the total cell value
  total_numerator <- sum(regdat[[paste0(factor, '_rate')]] * regdat[[paste0(factor, '_base')]] / 1, na.rm = TRUE) # 2025-03-07 replace /100 with /1
  cat("\ntotal_numerator: ", total_numerator, "\n")
  total_denominator <- sum(regdat[[paste0(factor, '_base')]], na.rm = TRUE)
  cat("\ntotal_denominator: ", total_denominator, "\n")
  total_value <- 100 * total_numerator / total_denominator
  
  # Add the total cell value to the religion-specific values
  temp_cell_values <- c(total_value, temp_cell_values)
  
  # Create the new row to add to data_Table1
  temp_row_values <- c(
    factor,
    formatC(temp_cell_values, digits = 0, format = "f", big.mark = ""),
    NA, NA  # Placeholder for significance columns
  )
  
  # Append the row to data_Table1
  data_Table1[nrow(data_Table1) + 1, ] <<- temp_row_values
}

# Call the function
function_Table1_addrow('Burned_out')

# Demographics
function_Table1_addrow('Older')
function_Table1_addrow('Female')
function_Table1_addrow('Ethnic_minority')
function_Table1_addrow('Sexual_minority')
function_Table1_addrow('Patient_facing')
function_Table1_addrow('Med_Dental')

# Stressors
function_Table1_addrow('Discrimination_internal')
function_Table1_addrow('Discrimination_public')
function_Table1_addrow('Emotional_public')
function_Table1_addrow('Emotional_manager')
function_Table1_addrow('Emotional_colleagues')
function_Table1_addrow('Physical_public')
function_Table1_addrow('Physical_manager')
function_Table1_addrow('Physical_colleagues')
function_Table1_addrow('Time_demands_meet')

## * Display table -----
sum(is.na(regdat))            # Total number of missing values in the dataset
sum(complete.cases(regdat))   # Number of complete cases (rows without missing values)

# Count NA values in each column
regdat <- regdat[, !names(regdat) %in% "Religion1"]
na_counts <- sapply(regdat, function(col) sum(is.na(col)))
(na_counts[na_counts > 0])
sum(complete.cases(regdat))
nrow(regdat)

function_display_df_in_viewer(data_Table1,"<b>Table 1.</b> Description of respondents. Percentages in the first row sum to 100%. Otherwise, values are percentages of respondents with the column's religious affiliation.")
