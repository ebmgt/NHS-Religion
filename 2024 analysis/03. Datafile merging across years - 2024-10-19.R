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
  #tk_messageBox(type = "ok", paste(packages, collapse="\n"), title="Packages installed")
}

current.date <- function(){
  return (as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE)))
}  

# Packages install ------
function_progress(0,'Libraries')

packages_essential <- c("tcltk",'stringr','openxlsx','readr')
function_libraries_install(packages_essential)
function_progress(50,'Libraries')

packages_other <- c("dplyr",'readxl','readr')
function_libraries_install(packages_other)
function_progress(100,'Libraries')

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
getwd()

##Parameters  -----
#* Breakdown group  -----
Breakdown_included <- tk_select.list(c('Religion','Ethnic background (summary)',
                                       'Age','Gender',
                                       'Occupation group (summary)',
                                       'Patient facing role',
                                       'All'), 
                                     preselect = 'Religion', multiple = FALSE,
                                     title =paste0("\n\n",Sys.info()["user"], ":\n\nWhat breakdown group are we studying?\n\n"))

subdirectory_path <- "Data-sources"

selected_files <- list.files(path = subdirectory_path, pattern = "organisation type results \\(selected columns\\).*\\.xlsx$", full.names = TRUE)

selected_files <- selected_files[!grepl("~", basename(selected_files))]

selected_files <- selected_files[grepl(substr(Breakdown_included, start = 1, stop = 4), basename(selected_files))]
print(selected_files)

if(length(selected_files)==0){
  tk_messageBox(type = c('ok'), paste0("\n\n", Sys.info()["user"], ":\n\nNo files found for ",Breakdown_included), caption = "Oops!")
  stop()
}
# Data grab -----
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

combined_data <- combined_data %>% dplyr::select(-starts_with("Q"))

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
    colnames(combined_data)[colnames(combined_data) == "Breakdown subgroup"] <- Breakdown_included
  }
}

#* Rearrange columns ------
combined_data <- combined_data %>%
  dplyr::select(names(combined_data)[1], Year, everything())

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
if (Breakdown_included == 'Religion'){
  output_csv_path <- file.path(subdirectory_path, "data_trusts_well-being.csv")
}else{
  output_csv_path <- paste0(subdirectory_path, '/data_trusts_well-being - ', Breakdown_included,".csv")
}
write_csv(combined_data, output_csv_path)

#* Wrap-up messages -----
msg <- paste("\n\n",Sys.info()["user"], ":\n\nCombined data saved to:", output_csv_path)
# If Rstudio
if (Sys.getenv("RSTUDIO") != "1"){
  tk_messageBox(type = "ok", msg, title = paste("Finished"))
}else{
  message(paste0("\033[32m\033[1mBob: ", msg, "\nMove the new file to the main directory!\033[0m"))
  
}

