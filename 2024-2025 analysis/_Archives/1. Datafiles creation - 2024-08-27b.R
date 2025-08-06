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

# Functions ------
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
  install.packages(setdiff(packages, rownames(installed.packages())), repos = "https://cloud.r-project.org/")
  for(package_name in packages)
  {
    library(package_name, character.only=TRUE, quietly = FALSE);
    cat('Installing package: ', package_name)
  }
}

function_plot_print <- function (plotname, plotheight, plotwidth){
  plotname <- gsub(":|\\s|\\n|\\?|\\!|\\'", "", plotname)
  rstudioapi::savePlotAsImage(
    paste(plotname,' -- ',current.date(),'.png',sep=''),
    format = "png", width = plotwidth, height = plotheight)
}

current.date <- function(){
  return (as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE)))
}
# Function to prepend a single quote if one is found anywhere in the field
prepend_quote_if_exists <- function(x) {
  if (is.character(x) && grepl("'", x)) {
    x <- paste0("'", x)
  }
  return(x)
}

# Function to process column names based on merged cells above
process_column_names <- function(sheet_data) {
  # Initialize the prefix variable
  last_valid_prefix <- NULL
  
  # Loop through each column in row 5
  for (col in 1:ncol(sheet_data)) {
    # Check the first row of the merged cells (row 1)
    cell_value <- sheet_data[1, col]
    
    if (!is.na(cell_value) && startsWith(cell_value, "Q") && grepl("-", cell_value)) {
      # Extract the text before the first '-' and trim it
      last_valid_prefix <- trimws(sub("-.*$", "", cell_value))
    }
    
    # Apply the prefix to the current column if a valid prefix is available
    if (!is.null(last_valid_prefix)) {
      # Prepend the column name with the last valid prefix
      sheet_data[5, col] <- paste(last_valid_prefix, sheet_data[5, col], sep = " ")
    }
  }
  colnames(sheet_data) <- sheet_data[5, ]  # Set the new column names
  sheet_data <- sheet_data[-5, ]  # Remove the row with the headers
  return(sheet_data)
}

# Packages/libraries -----
function_progress(0,'Libraries')
packages_essential <- c("tcltk",'stringr','openxlsx','readr','dplyr')
function_libraries_install(packages_essential)
function_progress(100,'Libraries')

# Parameters -----
#* Aggregation level? -----
grouping <- tk_select.list(c('organisation type results','organisational results (cannot isolate religion)'), 
                       preselect = 'organisation type results', multiple = FALSE,
                       title = "\n\nWhat aggregation or grouping level are we studying?\n\n")
#* Year? -----
Year <- tk_select.list(c(2021,2022,2023), 
                    preselect = '2021', multiple = FALSE,
                    title = "\n\nWhat year are we studying?\n\n")
Year <- substr(as.character(Year), 3, 4)

file_name_root <- paste0('Data-sources/NSS', Year," detailed spreadsheets ", grouping)

# Define the paths to the files
input_file_path <- 
  paste0(file_name_root,".xlsx")
output_file_header_path <- 
  paste0(file_name_root," (header).xlsx")
output_file_path <- 
  paste0(file_name_root," (selected).xlsx")
output_csv_path <- 
  paste0(file_name_root," (selected).csv")

# Check file contents ===================================
# Load the workbook -----
wb <- loadWorkbook(input_file_path)

#* Get the names of all worksheets -----
sheet_names <- getSheetNames(input_file_path)

# Remove the "Notes" worksheet
sheet_names <- sheet_names[sheet_names != "Notes"]

# Create a workbook for the header file -----
new_wb_header <- createWorkbook()


# Process each worksheet for the header file
index <- 0
for (sheet_name in sheet_names) {
  function_progress(100*(index)/length(sheet_names), paste('Processing ',sheet_name))
  if (index+1 == length(sheet_names)){function_progress(100,'Done')}
  # Read the entire sheet to preserve structure and formatting
  sheet_data <- readWorkbook(input_file_path, sheet = sheet_name, colNames = FALSE, skipEmptyRows = FALSE)
  
  # Process the column names in row 5 based on the cells directly above
  sheet_data <- process_column_names(sheet_data)
  
  # Remove rows 1-4 from the data
  sheet_data <- sheet_data[-(1:4), ]
  
  # Keep only the first 5 rows after the column names
  sheet_data <- sheet_data[1:5, ]
  
  # Add a new sheet to the header workbook
  addWorksheet(new_wb_header, sheetName = sheet_name)
  
  # Write the modified data back to the header workbook
  writeData(new_wb_header, sheet = sheet_name, x = sheet_data, startRow = 1, colNames = TRUE)
  
  # Reapply any merged cells from the original workbook
  original_sheet <- wb[[sheet_name]]
  merged_cells <- original_sheet$mergedCells
  if (!is.null(merged_cells)) {
    for (merge_range in merged_cells) {
      mergeCells(new_wb_header, sheet_name, merge_range)
    }
  }
  index <- index + 1
}

# Save the workbook header -----
saveWorkbook(new_wb_header, output_file_header_path, overwrite = TRUE)

print(paste("New workbook header saved at:", output_file_header_path))

# Create a workbook with selected columns -----
new_wb_selected <- createWorkbook()

# Process the specific worksheets for the output file with selected columns
combined_data <- NULL

for (sheet_name in sheet_names) {
  sheet_data <- readWorkbook(input_file_path, sheet = sheet_name, colNames = FALSE, skipEmptyRows = FALSE)
  
  # Process the column names
  sheet_data <- process_column_names(sheet_data)
  
  # Remove rows 1-4 from the columns
  sheet_data <- sheet_data[-(1:4), ]
  
#* EDIT FIELDS TO KEEP HERE -----  
  if (sheet_name == "YOUR JOB Q1-3") {
    # Find the columns through "Weighting"
    end_col_index <- grep("Weighting", colnames(sheet_data))
    if (length(end_col_index) > 0) {
      selected_columns <- sheet_data[, 1:end_col_index]
    } else {
      selected_columns <- NULL
      message("Weighting column not found.")
    }
    Q1_column <- sheet_data[, grep("^Q1 ", colnames(sheet_data))]
    selected_columns <- cbind(selected_columns,Q1_column)
  } else if (sheet_name == "HEALTH WELLBEING SAFETY Q10-11") {
    # Keep only columns that start with "Q11c"
    selected_columns <- sheet_data[, grep("^Q11c", colnames(sheet_data))]
  } else if (sheet_name == "HEALTH WELLBEING SAFETY Q12") {
    # Keep only columns that start with "Q12"
    selected_columns <- sheet_data[, grep("^Q12", colnames(sheet_data))]
  } else if (sheet_name == "HEALTH WELLBEING SAFETY Q13-14") {
    physical_columns <- sheet_data[, grep("^Q13", colnames(sheet_data))]
    # Keep only columns that start with "Q13"
    emotional_columns <- sheet_data[, grep("^Q14", colnames(sheet_data))]
    selected_columns <- cbind(emotional_columns,physical_columns)
  } else if (sheet_name == "BACKGROUND INFORMATION Q24-27") {
    # 2021 and before # str_detect(input_file_path, "21")
      # Gender: Keep only columns that start with "Q24a" 
      gender_columns <- sheet_data[, grep("^Q24a", colnames(sheet_data))]
      # Age: Keep only columns that start with "Q24c" 
      age_columns <- sheet_data[, grep("^Q24c", colnames(sheet_data))]
      selected_columns <- cbind(gender_columns, age_columns)
      # Ethnicity: Keep only columns that start with "Q25" 
      ethnicity_columns <- sheet_data[, grep("^Q25", colnames(sheet_data))]
      selected_columns <- cbind(gender_columns, age_columns, ethnicity_columns)
  } else if (sheet_name == "BACKGROUND INFORMATION Q26-29") {
    # 2022 # str_detect(input_file_path, "22")
    # Gender: Keep only columns that start with "Q26a"
    gender_columns <- sheet_data[, grep("^Q26a", colnames(sheet_data))]
    # Age: keep only columns that start with "Q26c" Age
    age_columns <- sheet_data[, grep("^Q26c", colnames(sheet_data))]
    selected_columns <- cbind(gender_columns, age_columns)
    # Ethnicity: Keep only columns that start with "Q27" 
    ethnicity_columns <- sheet_data[, grep("^Q27", colnames(sheet_data))]
    selected_columns <- cbind(gender_columns, age_columns, ethnicity_columns)
  } else if (sheet_name == "BACKGROUND INFORMATION Q27-30") {
    # 2023 and after
      # Gender: Keep only columns that start with "Q27a"
      gender_columns <- sheet_data[, grep("^Q27a", colnames(sheet_data))]
      # Age: keep only columns that start with "Q27c"
      age_columns <- sheet_data[, grep("^Q27c", colnames(sheet_data))]
      # Ethnicity: Keep only columns that start with "Q28" 
      ethnicity_columns <- sheet_data[, grep("^Q28", colnames(sheet_data))]
      ### Religion Not needed as as aggregated by this **
      #Keep only columns that start with "Q27" Religion
      #religion_columns <- sheet_data[, grep("^Q27", colnames(sheet_data))]
      selected_columns <- cbind(gender_columns, age_columns, ethnicity_columns)
  } else if (sheet_name == "BACKGROUND INFORMATION Q28-31") {
    # Medical-dental staff 2021
    selected_columns <- sheet_data[, grep("^Q31", colnames(sheet_data))]
  } else if (sheet_name == "BACKGROUND INFORMATION Q30-33") {
    # Medical-dental staff 2022
    selected_columns <- sheet_data[, grep("^Q33", colnames(sheet_data))]
  } else if (sheet_name == "BACKGROUND INFORMATION Q31-35") {
    # Medical-dental staff 2023
    selected_columns <- sheet_data[, grep("^Q35", colnames(sheet_data))]
  } else {selected_columns <- NULL}
  
  # Combine the selected columns from different sheets
  if (!is.null(selected_columns)) {
    if (sheet_name == "YOUR JOB Q1-3") {
      combined_data <- selected_columns
    } else {
      combined_data <- cbind(combined_data, selected_columns)
    }
  }
}

# combined_data$`Benchmarking group` 
if (grouping == "organisation type results"){
  colnames(combined_data)[colnames(combined_data) == 'Base'] <- 'Benchmarking group'
}

if (grouping == "organisation type results"){
combined_data <- combined_data[!combined_data$`Benchmarking group` %in% 
                                 c('Community Surgical Services',
                                   'CSUs',
                                   'Social Enterprises - Community', 
                                   'Social Enterprises - Community'), ]
combined_data <- combined_data[combined_data$`Breakdown` %in% 
                                 c('Religion'), ]
}

if (grouping == "organisational results"){
# We included all Trust types and Clinical Commissioning Groups (CCG)
# We excluded Benchmarking group: Commissioning Support Units, Social Enterprises, and Community Surgical Services due to their smaller size.
# Integrated care boards (ICBs) replaced clinical commissioning groups (CCGs) in the NHS in England from 1 July 2022.
# Omit rows where Benchmarking group is excluded
if ("Benchmarking group" %in% colnames(combined_data)) {
  combined_data <- combined_data[!combined_data$`Benchmarking group` %in% 
                    c('Community Surgical Services',
                      'CSUs',
                      'Social Enterprises - Community', 
                      'Social Enterprises - Community'), ]
  }
}

# Convert all columns that start with "Q" to numeric
combined_data[, grep("^Q", colnames(combined_data))] <- 
  lapply(combined_data[, grep("^Q", colnames(combined_data))], as.numeric)

# This is not valid as no Q had high response rate
combined_data$Respondents <- apply(combined_data[, grep("^Q", colnames(combined_data))], 1, max, na.rm = TRUE)

#* Rename fields to friendly (EDIT IF FIELDS CHANGE/ADDed -----
# Burnout Q12b
if ("Q12b % Often" %in% colnames(combined_data) && "Q12b % Always" %in% colnames(combined_data)) {
  combined_data$Burned_out_rate <- combined_data$`Q12b % Often` + combined_data$`Q12b % Always`
  combined_data$Burned_out_ever_rate <- 100 - combined_data$`Q12b % Never`
  colnames(combined_data)[colnames(combined_data) == 'Q12b Base (number of responses)'] <- 'Burned_out_base'
  # NEED DISTRIBUTED!!! HAVE THIS FOR 2019. HERE OK? NO
  combined_data$Burned_out_response_rate <- combined_data$Burned_out_base/combined_data$Respondents
  #lm.out <- lm(Burned_out_rate ~ Burned_out_response_rate, data=combined_data)
  #lm.out.summary <- summary(lm.out)
  #combined_data$Burned_out_rate_adjusted <- predict(lm.out)
  combined_data$Burned_out_rate_adjusted <- NULL
} else {
  message("One or both columns 'Q12b % Yes' and 'Q12b % Always' are not found in the data.")
}

# Work_stress Q11c
if ("Q11c % Yes" %in% colnames(combined_data) && "Q11c % Yes" %in% colnames(combined_data)) {
  combined_data$Stress_rate <- combined_data$`Q11c % Yes`
  colnames(combined_data)[colnames(combined_data) == 'Q11c Base (number of responses)'] <- 'Stress_base'
} else {
  message("One or both columns 'Q12b % Often' and 'Q12b % Always' are not found in the data.")
}

#** Demographics -----
# Patient_facing Q1
if ("Q1 % Yes, frequently" %in% colnames(combined_data) && "Q1 % Yes, occasionally" %in% colnames(combined_data)) {
  combined_data$Patient_facing_rate <- combined_data$`Q1 % Yes, frequently` + combined_data$`Q1 % Yes, occasionally`
  colnames(combined_data)[colnames(combined_data) == 'Q1 Base (number of responses)'] <- 'Patient_facing_base'
} else {
  message("One or both columns 'Q1 % Yes, frequently' and 'Q1 % Yes, occasionally' are not found in the data.")
}
# Gender Q24A (2021) OR Q26A ( 2022) OR Q27A (2023 or more)
if ("Q24a % Female" %in% colnames(combined_data)) {
  combined_data$Female_rate <- combined_data$`Q24a % Female`
  colnames(combined_data)[colnames(combined_data) == 'Q24a Base (number of responses)'] <- 'Female_rate_base'
  }else if ("Q26a % Female" %in% colnames(combined_data)){
    combined_data$Female_rate <- combined_data$`Q26a % Female`
    colnames(combined_data)[colnames(combined_data) == 'Q26a Base (number of responses)'] <- 'Female_rate_base'
  }else if ("Q27a % Female" %in% colnames(combined_data)){
    combined_data$Female_rate <- combined_data$`Q27a % Female`
    colnames(combined_data)[colnames(combined_data) == 'Q27a Base (number of responses)'] <- 'Female_rate_base'
  } else {
  message("One or both columns 'Q24a % Female' OR 'Q27a % Female' are not found in the data.")
}
# Age Q24c (2021) OR Q26c (2022) or Q27c (2023 or more)
if ("Q24c % 41-50" %in% colnames(combined_data)) {
  combined_data$Older_rate <- combined_data$`Q24c % 51-65` + combined_data$`Q24c % 66+`
  colnames(combined_data)[colnames(combined_data) == 'Q24c Base (number of responses)'] <- 'Older_rate_base'
  } else if ("Q26c % 41-50" %in% colnames(combined_data)) {
    combined_data$Older_rate <- combined_data$`Q26c % 51-65` + combined_data$`Q26c % 66+`
    colnames(combined_data)[colnames(combined_data) == 'Q26c Base (number of responses)'] <- 'Older_rate_base'
  } else if ("Q27c % 41-50" %in% colnames(combined_data)) {
    combined_data$Older_rate <- combined_data$`Q27c % 51-65` + combined_data$`Q27c % 66+`
    colnames(combined_data)[colnames(combined_data) == 'Q27c Base (number of responses)'] <- 'Older_rate_base'
  } else {
  message("One or both columns 'Q24c % 51-65' OR 'Q27c % 51-65' are not found in the data.")
  }
# Ethnicity Q25 (2021) OR Q27 (2022) or Q28 (2023 or more)
"Q25 % White"
if ("Q25 % White - Irish" %in% colnames(combined_data)) {
  selected_columns <- combined_data %>% select(matches("Q25 % White"))
  combined_data$Ethnic_minority_rate <- 100 - rowSums(selected_columns, na.rm = TRUE)
  colnames(combined_data)[colnames(combined_data) == 'Q25 Base (number of responses)'] <- 'Ethnicity_rate_base'
} else if ("Q27 % White - Irish" %in% colnames(combined_data)) {
  selected_columns <- combined_data %>% select(matches("Q27 % White"))
  combined_data$Ethnic_minority_rate <- 100 - rowSums(selected_columns, na.rm = TRUE)
  colnames(combined_data)[colnames(combined_data) == 'Q27 Base (number of responses)'] <- 'Ethnicity_rate_base'
} else if ("Q28 % White - Irish" %in% colnames(combined_data)) {
  selected_columns <- combined_data %>% select(matches("Q28 % White"))
  combined_data$Ethnic_minority_rate <- 100 - rowSums(selected_columns, na.rm = TRUE)
  colnames(combined_data)[colnames(combined_data) == 'Q28 Base (number of responses)'] <- 'Ethnicity_rate_base'
} else {
  message("One or both columns 'Q25' OR 'Q27' or 'Q28' are not found in the data.")
}
# MedDental Q31 (2021) OR Q33 (2022) or Q35 (2023 or more)
if ("Q31 % Medical / Dental - Consultant" %in% colnames(combined_data)) {
  combined_data$Med_Dental_rate <- combined_data$`Q31 % Medical / Dental - Consultant` + combined_data$`Q31 % Medical / Dental - In Training` + combined_data$`Q31 % Medical / Dental - Other`
  colnames(combined_data)[colnames(combined_data) == 'Q31 Base (number of responses)'] <- 'Med_Dental_rate_base'
} else if ("Q33 % Medical / Dental - Consultant" %in% colnames(combined_data)) {
  combined_data$Med_Dental_rate <- combined_data$`Q33 % Medical / Dental - Consultant` + combined_data$`Q33 % Medical / Dental - In Training` + combined_data$`Q33 % Medical / Dental - Other`
  colnames(combined_data)[colnames(combined_data) == 'Q33 Base (number of responses)'] <- 'Med_Dental_rate_base'
} else if ("Q35 % Medical / Dental - Consultant" %in% colnames(combined_data)) {
  combined_data$Med_Dental_rate <- combined_data$`Q35 % Medical / Dental - Consultant` + combined_data$`Q35 % Medical / Dental - In Training` + combined_data$`Q35 % Medical / Dental - Other`
  colnames(combined_data)[colnames(combined_data) == 'Q35 Base (number of responses)'] <- 'Med_Dental_rate_base'
} else {
  message("One or both columns 'Q31 % Medical / Dental - Consultant' OR 'Q33 % Medical / Dental - Consultant' OR 'Q35 % Medical / Dental - Consultant' are not found in the data.")
}
#** Physical -----
# Physical.public Q13a
if ("Q13a % Never" %in% colnames(combined_data) && "Q13a % Never" %in% colnames(combined_data)) {
  combined_data$Physical_public_rate <- 100 - combined_data$`Q13a % Never`
  colnames(combined_data)[colnames(combined_data) == 'Q13a Base (number of responses)'] <- 'Physical_public_base'
} else {
  message("One or both columns 'Q13a % Often' and 'Q13a % Always' are not found in the data.")
}
# Physical.manager Q13b
if ("Q13b % Never" %in% colnames(combined_data) && "Q13b % Never" %in% colnames(combined_data)) {
  combined_data$Physical_manager_rate <- 100 - combined_data$`Q13b % Never`
  colnames(combined_data)[colnames(combined_data) == 'Q13b Base (number of responses)'] <- 'Physical_manager_base'
} else {
  message("One or both columns 'Q13b % Often' and 'Q13b % Always' are not found in the data.")
}
# Physical.colleagues Q13c
if ("Q13c % Never" %in% colnames(combined_data) && "Q13c % Never" %in% colnames(combined_data)) {
  combined_data$Physical_colleagues_rate <- 100 - combined_data$`Q13c % Never`
  colnames(combined_data)[colnames(combined_data) == 'Q13c Base (number of responses)'] <- 'Physical_colleagues_base'
} else {
  message("One or both columns 'Q13c % Often' and 'Q13c % Always' are not found in the data.")
}
#** Emotional -----
# Emotional.public Q14a
if ("Q14a % Never" %in% colnames(combined_data) && "Q14a % Never" %in% colnames(combined_data)) {
  combined_data$Emotional_public_rate <- 100 - combined_data$`Q14a % Never`
  colnames(combined_data)[colnames(combined_data) == 'Q14a Base (number of responses)'] <- 'Emotional_public_base'
} else {
  message("One or both columns 'Q14a % Often' and 'Q14a % Always' are not found in the data.")
}
# Emotional.manager Q14b
if ("Q14b % Never" %in% colnames(combined_data) && "Q14b % Never" %in% colnames(combined_data)) {
  combined_data$Emotional_manager_rate <- 100 - combined_data$`Q14b % Never`
  colnames(combined_data)[colnames(combined_data) == 'Q14b Base (number of responses)'] <- 'Emotional_manager_base'
} else {
  message("One or both columns 'Q14b % Often' and 'Q14b % Always' are not found in the data.")
}
# Emotional.colleagues Q14c
if ("Q14c % Never" %in% colnames(combined_data) && "Q14c % Never" %in% colnames(combined_data)) {
  combined_data$Emotional_colleagues_rate <- 100 - combined_data$`Q14c % Never`
  colnames(combined_data)[colnames(combined_data) == 'Q14c Base (number of responses)'] <- 'Emotional_colleagues_base'
} else {
  message("One or both columns 'Q14c % Often' and 'Q14c % Always' are not found in the data.")
}

#* Add the combined data to the new workbook -----
addWorksheet(new_wb_selected, sheetName = "Selected Data")
writeData(new_wb_selected, sheet = "Selected Data", x = combined_data, startRow = 1, colNames = TRUE)

# Save the workbook with selected data -----
saveWorkbook(new_wb_selected, output_file_path, overwrite = TRUE)

print(paste("New workbook with selected fields saved at:", output_file_path))
