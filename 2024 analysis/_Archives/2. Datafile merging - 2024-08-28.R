# This file is available at https://github.com/ebmgt/NHS-Religion/
# Author: Zehan Yang, PhD. zehan.yang@uconn.edu and rbadgett@kumc.edu
# Permission: GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
# Last edited 2024-08-28

### Start =======================================
library(tcltk) # For interactions and troubleshooting

##* Functions -----
`%notin%` <- Negate(`%in%`)
`%!=na%` <- function(e1, e2) (e1 != e2 | (is.na(e1) & !is.na(e2)) | (is.na(e2) & !is.na(e1))) & !(is.na(e1) & is.na(e2))
`%==na%` <- function(e1, e2) (e1 == e2 | (is.na(e1) & is.na(e2)))

function_libraries_install <- function(packages){
  install.packages(setdiff(packages, rownames(installed.packages())), repos = "https://cloud.r-project.org/")
  for(package_name in packages)
  {
    library(package_name, character.only=TRUE, quietly = FALSE);
    cat('Installing package: ', package_name)
  }
  #tk_messageBox(type = "ok", paste(packages, collapse="\n"), title="Packages installed")
}

current.date <- function(){
  return (as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE)))
}  

# Packages install ------
packages_essential <- c("tcltk",'stringr','openxlsx','readr')
function_libraries_install(packages_essential)

packages_other <- c("dplyr",'readxl','readr')
function_libraries_install(packages_other)

# Did the script load? -----
hour <- as.numeric(str_sub(Sys.time(), 12, 13))
daypart <- NULL
if (hour > 17) {
  daypart <- 'evening'
} else if ( hour > 11) {
  daypart <- 'afternoon'
} else if ( hour > 5) {
  daypart <- 'morning'
} else {
  daypart <- 'madrugada'
}

# If Rstudio
if (Sys.getenv("RSTUDIO") != "1"){
  args <- commandArgs(trailingOnly = FALSE)
  script_path <- sub("--file=", "", args[grep("--file=", args)])  
  script_path <- dirname(script_path)
  setwd(script_path)
}else{
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  #ScriptsDir <- paste(getwd(),'/Scripts',sep='')
}
tk_messageBox(type = "ok", paste('1. ', 'Working directory:\n', getwd(), sepo=''), caption = paste("Good",daypart))

# Define the subdirectory path
subdirectory_path <- "Data-sources"

# List all Excel files in the subdirectory whose filenames contain 'selected'
selected_files <- list.files(path = subdirectory_path, pattern = "organisation type results \\(selected\\).*\\.xlsx$", full.names = TRUE)
# Filter out temp files that contain '~' in their names
selected_files <- selected_files[!grepl("~", basename(selected_files))]
# Print the selected files (optional, for verification)

print(selected_files)

# Initialize an empty list to store the data from each file
combined_data_list <- list()

# Loop through each file and read its contents
for (file in selected_files) {
  # Read the first sheet of the Excel file; modify if a specific sheet is needed
  file_data <- read_excel(file)
  
  # Extract the first occurrence of two adjacent digits
  two_digits <- str_extract(file, "\\d{2}")
  # Prepend '20' and convert to a number
  file_data$year <- as.numeric(paste0("20", two_digits))
  
  # Append the data to the list
  combined_data_list <- append(combined_data_list, list(file_data))
}

# Combine all dataframes row-wise
combined_data <- bind_rows(combined_data_list)

combined_data <- combined_data %>% select(-starts_with("Q"))

#* Rename columns-----
if ("year" %in% colnames(combined_data)) {
  colnames(combined_data)[colnames(combined_data) == "year"] <- "Year"
}
if (!"Religion" %in% colnames(combined_data)) {
  
  # Check for "religion" and rename it to "Religion" if found
  if ("religion" %in% colnames(combined_data)) {
    colnames(combined_data)[colnames(combined_data) == "religion"] <- "Religion"
  }
  
  # Check for "Breakdown subgroup" and rename it to "Religion" if found
  if ("Breakdown subgroup" %in% colnames(combined_data)) {
    colnames(combined_data)[colnames(combined_data) == "Breakdown subgroup"] <- "Religion"
  }
}

#* Rearrange columns ------
combined_data <- combined_data %>%
  select(Religion, Year, everything())

# Identify the position of the "Weighting" column
weighting_index <- which(names(combined_data) == "Weighting")

# Split the dataframe into two parts: before and after the "Weighting" column
columns_before <- combined_data[, 1:weighting_index]
columns_after <- combined_data[, (weighting_index + 1):ncol(combined_data)]

# Sort the column names after "Weighting" alphabetically
sorted_columns_after <- columns_after[, order(names(columns_after))]

# Combine the sorted columns with the columns before "Weighting"
combined_data <- cbind(columns_before, sorted_columns_after)

# Wrap-up -----
#* Save the combined data as a CSV ------
output_csv_path <- file.path(subdirectory_path, "data_trusts_burnout.csv")
write_csv(combined_data, output_csv_path)

#* Wrap-up messages -----
msg <- paste("\n\n",Sys.info()["user"], ":\n\nCombined data saved to:", output_csv_path)
# If Rstudio
if (Sys.getenv("RSTUDIO") != "1"){
  tk_messageBox(type = "ok", msg, title = paste("Finished"))
}else{
  cat(msg)
}

