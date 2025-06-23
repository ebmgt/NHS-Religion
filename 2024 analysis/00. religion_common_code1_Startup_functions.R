# This file is available at https://github.com/ebmgt/NHS-Religion/
# Author:rbadgett@kumc.edu and zyang.uconn@gmail.com
# Permission: GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
# Last edited 2025-06-14

#== Startup ======
library(tcltk) # For interactions and troubleshooting, part of base package so no install needed.

#* Cleanup ======
# Remove all of environment
# rm(list = ls())
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

# Global variables -----
Pb_global          <- NULL  # For function_progress
formula_final      <- NULL
model_final        <- NULL # not used as of 2024-10-19

Religion_levels    <- as.factor(c("Christian","Jewish","Muslim","Buddhist","Hindu","Sikh","Any other religion","I would prefer not to say", "No religion"))
Religion_labels    <- c("Christian","Jewish","Muslim","Buddhist","Hindu","Sikh","Other","Withheld", "No religion")
levels(Religion_levels)
Religion_levels <- relevel(Religion_levels, ref = "No religion")
levels(Religion_levels)

# Functions ------------
`%notin%` <- Negate(`%in%`)
`%!=na%` <- function(e1, e2) (e1 != e2 | (is.na(e1) & !is.na(e2)) | (is.na(e2) & !is.na(e1))) & !(is.na(e1) & is.na(e2))
`%==na%` <- function(e1, e2) (e1 == e2 | (is.na(e1) & is.na(e2)))

function_progress <- function(progress, titletext){
  #if  (!exists("titletext") || is.null(titletext)){titletext <- 'testing'}
  #if  (!exists("progress") || is.null(progress)){progress <- 0}
  if (progress == 0){
    Pb_global <<- tkProgressBar(titletext, "", 0, 100, 0)
  }
  info <- sprintf("%d%% done", round(progress))
  setTkProgressBar(Pb_global, value = progress, title = paste(titletext,sprintf("(%s)", info)), label = info)
  if (progress == 100){
    close(Pb_global)
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

#* Functions to show data -----
# Display the dataframe
function_display_df_in_viewer <- function(df,
                                          caption       = NULL,   # bold title above table
                                          footer        = NULL,   # left-aligned note below
                                          highlight_row = NULL) { # 1-based index
  
  ## ---- 1. Caption (bold, left-aligned)
  caption_html <- if (!is.null(caption) && nzchar(caption)) {
    htmltools::HTML(
      sprintf("<div style='text-align:left;'>%s</div>", caption)
    )
  } else {
    NULL
  }
  
  ## ---- 2. Optional row-highlight JavaScript
  rowCallback <- NULL
  if (!is.null(highlight_row)) {
    rowCallback <- DT::JS(sprintf(
      "function(row, data, displayNum, displayIndex, dataIndex){
         if (displayIndex === %d){
           $(row).css('background-color','chartreuse');
         }
       }",
      highlight_row - 1          # zero-based for JS
    ))
  }
  
  ## ---- 3. DT options
  dt_opts <- list(pageLength = 25)
  if (!is.null(rowCallback)) dt_opts$rowCallback <- rowCallback
  
  ## ---- 4. Build the DT widget
  dt_widget <- DT::datatable(
    df,
    caption = caption_html,
    options = dt_opts,
    escape  = FALSE
  )
  
  ## ---- 5. Footer block
  # left line: user-supplied footer (if any)
  left_line  <- if (!is.null(footer) && nzchar(footer)) {
    sprintf("<div style='text-align:left;'>%s</div>", footer)
  } else ""
  
  # right line: always e-mail + date
  right_line <- sprintf(
    "<div style='text-align:right;'>rbadgett@kumc.edu, %s</div>", Sys.Date()
  )
  
  footer_html <- htmltools::HTML(paste(left_line, right_line, sep = "\n"))
  
  ## ---- 6. Append footer and return widget
  htmlwidgets::appendContent(dt_widget, footer_html)
}


