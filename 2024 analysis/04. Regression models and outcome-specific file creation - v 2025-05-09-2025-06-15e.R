# This file is available at https://github.com/ebmgt/NHS-Religion/
# Authors: Robert Badgett and Zehan Yang, PhD. rbadgett@kumc.edu
# Permission: GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
# Last edited 2025-02-12s

# This file uses the datafile:
# "data_trusts_burnout.csv"

# Startup -----
library(tcltk) # For interactions and troubleshooting, Oart of base so no install needed.

# Troubleshooting
options(error = NULL)   # Default
#options(warn = 2)        # Converts warnings into errors
#options(warn = 2, error = browser)
#options(error = recover) # This will provide a menu showing the call stack, and you can choose which environment to inspect.
# browser()
# Key Commands in browser()
# n: Next
#Executes the next line of code and stays in the debugging mode. This is similar to "step over" in other debuggers.
# s: Step into
# If the next line is a function call, s steps into that function, allowing you to debug inside the function. If it's not a function, it behaves like n.
# c: Continue
# Continues execution until the next breakpoint or until the script completes.
# Q: Quit
# Exits the browser and stops the debugging session.

# Global variables -----
Pb_global          <- NULL  # For function_progress
formula_final      <- NULL
model_final        <- NULL # not used as of 2024-10-19

Religion_levels    <- as.factor(c("Christian","Jewish","Muslim","Buddhist","Hindu","Sikh","Any other religion","I would prefer not to say", "No religion"))
Religion_labels    <- c("Christian","Jewish","Muslim","Buddhist","Hindu","Sikh","Other","Withheld", "No religion")
levels(Religion_levels)
Religion_levels <- relevel(Religion_levels, ref = "No religion")
levels(Religion_levels)

# Create a color palette and map ------
Pallette_RoyGBiv    <- c('red','orange','yellow', 'green', 'blue', '#4B0082', 'violet')
colors_Breakdown    <- c('orange1','orange2','darkorange2','firebrick1','firebrick2','firebrick3','firebrick4', 'gray', 'gray50')
color_map_religions <- setNames(colors_Breakdown, Religion_levels)

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
getwd()

# Did the script load? -----
#tk_messageBox(type = "ok", paste('1. ', 'R has loaded.\n\nWorking directory:\n', getwd(), sepo=''), caption = "Hello")

# Functions -----
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
function_display_df_in_viewer <- function(df, caption, highlight_row = NULL) {
  # convert caption to HTML so it can be displayed with styling
  caption <- htmltools::HTML(
    paste0("<div style='text-align: left;'>", caption, "</div>")
  )
  
  # By default, the rowCallback is empty
  rowCallback <- NULL
  
  # If highlight_row is specified, build the rowCallback JavaScript
  if (!is.null(highlight_row)) {
    rowCallback <- DT::JS(sprintf(
      "function(row, data, displayNum, displayIndex, dataIndex) {
         // displayIndex is zero-based
         if (displayIndex === %d) {
           $(row).css('background-color', 'chartreuse');
         }
       }", 
      highlight_row - 1  # zero-based index in JavaScript
    ))
  }
  
  # Set DT options
  options <- list(
    pageLength = 25
  )
  
  # If we have a rowCallback, add it to our options
  if (!is.null(rowCallback)) {
    options$rowCallback <- rowCallback
  }
  
  # Create the datatable
  DT::datatable(
    df, 
    caption = caption, 
    options = options, 
    escape = FALSE
  )
}

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
# Function to calculate I² from sigma2 (heterogeneity measure)
# Zehan's function
function_calculate_I2_Zehan <- function(res) {
  W <- diag(1/res$vi)
  X <- model.matrix(res)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))
}

function_calculate_I2_v4 <- function (sigma2, total_variance) {
  I2 <- 100 * sigma2 / total_variance
  return(round(I2, 1))
}

function_I2_descriptor <- function(I2){
  I2_descriptor = "might not be important"
  if (I2 > 30){I2_descriptor = "moderate"}
  if (I2 > 50){I2_descriptor = "substantial"}
  if (I2 > 75){I2_descriptor = "considerable"}
  return (I2_descriptor)
}
# Function to calculate residual I² accounting for covariates
function_calculate_I2_residual_v3 <- function(res) {
  # Total heterogeneity (variance components)
  total_variance <- sum(res$sigma2)
  
  # Residual variance (based on degrees of freedom and model structure)
  W <- diag(1 / res$vi)
  X <- model.matrix(res)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  residual_variance <- (res$k - res$p) / sum(diag(P))
  
  # Calculate residual I² based on the total and residual variance
  I2_residual <- 100 * total_variance / (total_variance + residual_variance)
  
  return(round(I2_residual, 1))
}

# Function to calculate I2 residual
function_calculate_I2_residual <- function(res) {
  # Total heterogeneity (before accounting for covariates)
  total_heterogeneity <- sum(res$sigma2)
  
  # Variance-covariance matrix of the weights
  W <- diag(1 / res$vi)
  
  # Design matrix (for moderators/covariates)
  X <- model.matrix(res)
  
  # Projection matrix for covariates
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  
  # Residual heterogeneity (after accounting for covariates)
  residual_heterogeneity <- sum(res$sigma2)
  
  # Calculate the I2 accounting for covariates
  I2_residual <- 100 * residual_heterogeneity / 
    (residual_heterogeneity + (res$k - res$p) / sum(diag(P)))
  
  # Return the residual I2 (after accounting for covariates)
  return(round(I2_residual, 1))
}

#* Regression functions -----
function_stepwise_forward_betareg <- function(data, p_value_threshold = 0.05) {
  # Start with an empty model (intercept only)
  initial_formula <- as.formula("Outcome_rate ~ 1")
  model_final <- betareg(initial_formula, data = data)
  
  # Identify all potential predictors
  all_predictors <- setdiff(names(data), "Outcome_rate")
  factor_vars <- names(data)[sapply(data, is.factor)]
  
  # Initialize variables
  added_predictors <- c()  # Track predictors already in the model
  removed_predictors <- c() # Track predictors that have been removed
  best_formula <- initial_formula
  iteration_count <- 0
  
  # Debug: Initial state
  message("Starting forward stepwise selection with initial formula: ", deparse(best_formula))
  cat("Potential predictors: ", paste(all_predictors, collapse = ", "), "\n")
  
  repeat {
    iteration_count <- iteration_count + 1
    cat('\n\n-----------------------------------\n\nfunction_stepwise_forward_betareg: start of iteration: ', sprintf("%02d", iteration_count), '\n\n')
    
    # Check if maximum iterations are reached
    if (iteration_count > 100) {
      stop("Error: The function is in an infinite loop. Please check the model.")
    }
    
    # Find predictors not yet added or removed
    remaining_predictors <- setdiff(all_predictors, c(added_predictors, removed_predictors))
    
    # Print remaining predictors for debugging
    message('Remaining predictors to consider: ', paste(remaining_predictors, collapse = ", "))
    
    # If no more predictors to evaluate, stop
    if (length(remaining_predictors) == 0) {
      message("No more predictors to evaluate. Stopping forward stepwise selection.")
      break
    }
    
    # Evaluate all potential predictors not yet in the model
    candidate_models <- lapply(remaining_predictors, function(pred) {
      tryCatch({
        new_formula <- as.formula(paste(deparse(best_formula), "+", pred))
        model <- betareg(new_formula, data = data)
        list(formula = new_formula, model = model, predictor = pred)
      }, error = function(e) {
        return(NULL)
      })
    })
    
    # Filter out any failed models (NULL values)
    candidate_models <- Filter(Negate(is.null), candidate_models)
    
    # Initialize tracking variables for best predictor
    best_predictor <- NULL
    best_p_value <- Inf
    
    # Loop through candidate models to find the best predictor
    for (i in seq_along(candidate_models)) {
      cm <- candidate_models[[i]]
      pred_p_values <- summary(cm$model)$coefficients$mean[, "Pr(>|z|)"][-1]  # Exclude intercept
      
      # Find the p-value of the current predictor being added (i.e., the last predictor in the formula)
      current_pred <- names(pred_p_values)[length(pred_p_values)]
      current_p_value <- pred_p_values[length(pred_p_values)]
      
      # Check if this predictor has the best p-value so far
      if (current_p_value < p_value_threshold && current_p_value < best_p_value) {
        best_predictor <- current_pred
        best_formula <- cm$formula
        best_model <- cm$model
        best_p_value <- current_p_value
      }
    }
    
    # If no significant predictors found, terminate
    if (is.null(best_predictor)) {
      message("No more significant predictors to add. Stopping forward stepwise selection.")
      break
    }
    
    # Add the best predictor to the model
    added_predictors <- c(added_predictors, best_predictor)
    model_final <- best_model
    cat("Best predictor added: ", best_predictor, " with p-value: ", best_p_value, "\n")
    message("Updated formula: ", deparse(best_formula))
    
    # Debug: Print model summary
    print(summary(model_final))
    
    # Re-evaluate all existing predictors in the model to ensure none are now non-significant
    current_p_values <- summary(model_final)$coefficients$mean[, "Pr(>|z|)"][-1] # Exclude intercept
    non_significant_predictors <- names(current_p_values)[current_p_values > p_value_threshold]
    
    # Retain all levels of Religion if any level is significant
    if ("Religion" %in% added_predictors) {
      religion_p_values <- summary(model_final)$coefficients$mean[grep("^Religion", rownames(summary(model_final)$coefficients$mean)), "Pr(>|z|)"]
      if (any(religion_p_values < p_value_threshold)) {
        non_significant_predictors <- non_significant_predictors[!grepl("^Religion", non_significant_predictors)]
      }
    }
    
    if (length(non_significant_predictors) > 0) {
      message("Removing non-significant predictors: ", paste(non_significant_predictors, collapse = ", "))
      removed_predictors <- c(removed_predictors, non_significant_predictors)
      added_predictors <- setdiff(added_predictors, non_significant_predictors)
      best_formula <- as.formula(paste("Outcome_rate ~", paste(added_predictors, collapse = " + ")))
      model_final <- betareg(best_formula, data = data)
    }
  }
  
  # Update the global formula with the final formula
  assign("formula_final", best_formula, envir = .GlobalEnv)
  message("\nGlobal formula_final updated to: ", deparse(best_formula))
  
  # Print the summary of the final model
  message("\nSummary of the final model with significant predictors:")
  print(summary(model_final))
  
  # Return the final model with only significant predictors
  return(model_final)
}

function_stepwise_backward_betareg <- function(formula, data, p_value_threshold = 0.05) {
  # Debug: Check the initial formula
  message("Initial formula: ", deparse(formula))
  
  # Fit the initial model
  current_model <- betareg(formula, data = data)
  
  # Extract initial set of predictors from the formula
  terms_obj <- terms(formula)
  response_var <- all.vars(formula)[1]  # Extract the response variable
  remaining_predictors <- attr(terms_obj, "term.labels")  # Extract all predictors from formula
  
  # Identify factor variables in the data
  factor_vars <- names(data)[sapply(data, is.factor)]
  cat("Factor variables identified: ", paste(factor_vars, collapse = ", "), "\n")
  
  # Clean up predictor names
  remaining_predictors <- trimws(gsub("[^[:alnum:]_\\.]", "", remaining_predictors))
  cat("1 Initial set of predictors extracted: ", paste(remaining_predictors, collapse = ", "), "\n")
  
  # Check if predictors were extracted correctly
  if (length(remaining_predictors) == 0) {
    stop("Error: No predictors found in the formula. Please check the formula format.")
  }
  
  model_final <- current_model
  formula_final <- formula # Keep track of the formula separately
  iteration_count <- 0  # To prevent infinite loops
  evaluated_factors <- c() # To track factors that have already been evaluated and retained
  
  repeat {
    iteration_count <- iteration_count + 1
    # Debug: Indicate start of iteration
    cat('\n\n-----------------------------------\n\nfunction_stepwise_backward_betareg: start of iteration: ', iteration_count, '\n\n')
    
    if (iteration_count > 100) {  # Stop after 100 iterations to prevent infinite loops
      stop("Error: The function is in an infinite loop. Please check the model.")
    }
    
    # Debug: Show current model formula
    message("Current model formula: ", deparse(formula_final))  # Use formula_final
    cat("Model formula being used: ", deparse(formula_final), "\n")  # Additional debug
    
    # Get p-values of the current model
    p_values <- summary(model_final)$coefficients$mean[, "Pr(>|z|)"][-1] # Exclude intercept
    message("Current model P-values: ", paste(p_values, collapse = ", "))
    
    # Clean up predictor names from p-values to match remaining predictors
    clean_names <- trimws(gsub("[^[:alnum:]_\\.]", "", names(p_values)))
    
    # Debug: Print the cleaned names and remaining predictors
    cat("Cleaned predictor names: ", paste(clean_names, collapse = ", "), "\n")
    cat("Current predictors being evaluated: ", paste(remaining_predictors, collapse = ", "), "\n")
    
    # Find the most insignificant variables (the highest p-values above the threshold)
    non_significant <- which(p_values > p_value_threshold)
    
    # Filter out levels of any factor that has been evaluated and retained
    non_significant <- non_significant[!sapply(clean_names[non_significant], function(x) {
      any(sapply(evaluated_factors, function(f) grepl(f, x)))
    })]
    
    # If no non-significant variables are found, stop
    if (length(non_significant) == 0) {
      message("All remaining predictors are significant.")
      break
    }
    
    # Loop through the non-significant predictors until we find one that can be removed
    for (i in order(p_values[non_significant], decreasing = TRUE)) {
      remove_pred <- clean_names[non_significant[i]]
      message("Attempting to remove non-significant predictor: ", remove_pred)
      
      # Check if remove_pred is part of a factor
      matched_factor <- NULL
      for (factor in factor_vars) {
        if (grepl(factor, remove_pred)) {
          matched_factor <- factor
          break
        }
      }
      
      if (!is.null(matched_factor)) {
        # Debug: Indicate factor matched
        message("Matched factor: ", matched_factor)
        
        # Get all levels associated with the factor
        related_levels <- grep(paste0("^", matched_factor), clean_names, value = TRUE)
        
        # Debug: Print related levels found
        cat("Related levels found: ", paste(related_levels, collapse = ", "), "\n")
        
        # Check significance of all levels of the factor
        significant_levels <- related_levels[p_values[related_levels] <= p_value_threshold]
        
        if (length(significant_levels) > 0) {
          # Some levels are significant, retain the entire factor
          if (!(matched_factor %in% evaluated_factors)) {
            message("Retaining all levels of factor variable: ", matched_factor, " because some levels are significant.")
            evaluated_factors <- c(evaluated_factors, matched_factor) # Track the factor as evaluated
          } else {
            message("Skipping already evaluated factor: ", matched_factor)
          }
          
          # Skip removal attempt for this factor and continue to the next predictor
          next
        } else {
          # No levels are significant, remove the whole factor, including its base name
          message("Removing all levels of factor variable: ", matched_factor)
          remaining_predictors <- setdiff(remaining_predictors, c(matched_factor, related_levels))
          clean_names <- setdiff(clean_names, c(matched_factor, related_levels))
          break # Exit the loop after removing this factor
        }
        
      } else if (remove_pred %in% clean_names) {
        # Remove non-factor predictor
        remaining_predictors <- setdiff(remaining_predictors, remove_pred)
        clean_names <- setdiff(clean_names, remove_pred)
        break # Exit the loop after removing this predictor
      } else {
        # Error handling if predictor is not found
        message("Error: Predictor to remove not found in remaining predictors. Possible mismatch: ", remove_pred)
        break
      }
    }
    
    cat("3 Remaining predictors after removal: ", paste(remaining_predictors, collapse = ", "), "\n")
    
    # If no predictors are left, stop
    if (length(remaining_predictors) == 0) {
      message("No predictors left to remove.")
      break
    }
    
    # Update the formula by constructing it with the remaining predictors
    formula_final <- as.formula(paste(response_var, "~", paste(remaining_predictors, collapse = " + ")))
    message("Updated formula: ", deparse(formula_final))
    
    
    # Fit the new model with the updated formula
    model_final <- betareg(formula_final, data = data)

    # Debug: Results of this iteration
    cat("\n4 Results of this iteration:
        New formula after: ", deparse(formula_final), 
        "\n\tCovariate removed: ", non_significant,
        "\n\tAIC: ", round(AIC(model_final),1),
        "\n\tBIC: ", round(BIC(model_final),1),
        "\n\t Pseudo R2: ", round(model_final$pseudo.r.squared,1),
        "\n")
  }
  
  # Update the global formula_final with the final formula
  assign("formula_final", formula_final, envir = .GlobalEnv)
  message("\nGlobal formula_final updated to: ", deparse(formula_final))
  
  # Print the summary of the final model
  message("\nSummary of the final model with significant predictors:")
  print(summary(model_final))
  
  # Analyze the Religion factors
  Religion_coeff <- summary(model_final)$coefficients$mean[grep("^Religion", rownames(summary(model_final)$coefficients$mean)), ]
  
  if (length(Religion_coeff) > 0) {
    strongest_Religion <- Religion_coeff[which.min(Religion_coeff[, "Estimate"]), ]
    message("\nStrongest Religion factor: ", rownames(strongest_Religion), 
            " with estimate: ", strongest_Religion["Estimate"], 
            " and p-value: ", strongest_Religion["Pr(>|z|)"])
    
    # Full pairwise comparison of Religion factors
    pairwise_comparisons <- expand.grid(rownames(Religion_coeff), rownames(Religion_coeff))
    pairwise_comparisons <- pairwise_comparisons[pairwise_comparisons[,1] != pairwise_comparisons[,2], ]
    
    results <- apply(pairwise_comparisons, 1, function(pair) {
      diff <- abs(Religion_coeff[pair[1], "Estimate"] - Religion_coeff[pair[2], "Estimate"])
      se_diff <- sqrt(Religion_coeff[pair[1], "Std. Error"]^2 + Religion_coeff[pair[2], "Std. Error"]^2)
      z_value <- diff / se_diff
      p_value <- 2 * (1 - pnorm(abs(z_value)))
      list(pair = pair, diff = diff, p_value = p_value)
    })
    
    # Print all pairwise comparison results
    message("\nPairwise comparisons of Religion factors:")
    for (comparison in results) {
      message("Comparison between ", comparison$pair[1], " and ", comparison$pair[2], 
              ": Difference = ", round(comparison$diff, 4), 
              ", p-value = ", round(comparison$p_value, 4))
    }
    
    # Determine if any comparison shows a significant difference
    if (any(sapply(results, function(x) x$p_value < 0.05))) {
      message("\nThere is a significant difference between at least one pair of Religion factors.")
    } else {
      message("\nNo significant differences found between Religion factors.")
    }
  } else {
    message("\nNo religion factors were retained in the final model.")
  }
  
  # Return the final model with only significant predictors
  return(model_final)
}

function_check_revise_formula <- function(model, p_threshold = 0.05) {
  #model <- model_all_sans_religion_reduced_final
  #p_threshold <- 0.05
  # Ensure the model is a betareg object
  if (!inherits(model, "betareg")) {
    stop("The provided model is not a 'betareg' object.")
  }
  
  # Extract the summary of the model
  model_summary <- summary(model)
  
  # Extract p-values for the mean model coefficients
  p_values <- model_summary$coefficients$mean[, "Pr(>|z|)"]
  
  # Exclude the intercept from p-value consideration
  p_values <- p_values[names(p_values) != "(Intercept)"]
  
  # Identify predictors with p-values greater than the threshold
  predictors_to_remove <- names(p_values)[p_values > p_threshold]
  
  # Extract the current formula from the model
  current_formula <- formula(model)
  
  # Get the current predictors from the formula
  current_terms <- attr(terms(current_formula), "term.labels")
  
  # Remove the insignificant predictors from the list of predictors
  new_terms <- setdiff(current_terms, predictors_to_remove)
  
  # Reconstruct the new formula based on remaining predictors
  if (length(new_terms) == 0) {
    # If no predictors remain, model only includes the intercept
    response <- as.character(current_formula)[2]
    new_formula <- as.formula(paste(response, "~ 1"))
  } else {
    # Reconstruct the formula with the remaining predictors
    response <- as.character(current_formula)[2]
    new_formula <- as.formula(
      paste(response, "~", paste(new_terms, collapse = " + "))
    )
  }
  
  # Return the revised formula
  return(new_formula)
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

#* Viewer output -----
packages_viewer_output <- c('knitr','kableExtra','dplyr','DT')
function_libraries_install(packages_viewer_output)

function_progress(60,'Libraries')

library(splines)
packages_spline<- (c("splines2"))
function_libraries_install(packages_spline)
function_progress(70,'Libraries')

packages_meta <- (c("meta", "metafor",'grid'))
function_libraries_install(packages_meta)
function_progress(80,'Libraries')

function_progress(100,'Libraries')

# CLEAN ENVIRONMENT -----------------------
# Remove all but functions from the environment
#rm(list = ls()[sapply(ls(), function(x) {!is.function(get(x))})])
# OR PRESERVE GLOBAL VALUES:
#rm(list = ls()[sapply(ls(), function(x) {
#  var <- get(x)
#  !is.function(var) && !(is.character(var) || is.numeric(var) || is.factor(var))
#})])

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
# _____________________________-----
# Data grab -----
if (data.source == 'Trusts and CCGs only'){
  #tk_messageBox(type = "ok", paste0('Current: ', data.source), caption = "Troubleshooting")
  file.filter   <- matrix(c("Spreadsheets","data_trusts_well-being*.csv;data_trusts_well-being*.xls;data_trust_well-being*.xlsx","All","..\\data\\*.*"),byrow=TRUE,ncol=2)
  filename      <- choose.files(filters = file.filter,caption = "Select data file",index = 1, multi=FALSE)
  regdat <- read_csv(filename)
  regdat$stress <- NULL  # Creates a new column 'stress' filled with NA
  regdat$Outcome_base <- NULL  # Creates a new column 'Outcome_base' filled with NA
  #** data_full: Data prep -----
  names(regdat)[names(regdat) == "Year"] <- "year"
  cat ('Ready to parse stress from: ', Outcome.source)
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
}else{
  regdat <- read_csv("data_full.csv")
  #** data_trusts: Data prep ----- 
  regdat$trust <- ''
  regdat <- regdat[, c(1, ncol(regdat), 2:(ncol(regdat) - 1))]
  names(regdat)[names(regdat) == "Year"] <- "year"
}

# Added 2025-06-15
if ("year" %in% names(regdat)) {
  names(regdat)[names(regdat) == "year"] <- "Year"
}

# Identify columns with rates < 1 and fix-------
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

#* Optimize data types -----
#**  Outcome_rate -----
message (paste0("\033[1m\033[30m 2. Current Outcome_rate rate: ", round(mean(regdat$Outcome_rate*100),1),'%\033[0m'))

if (year.handling == 'Factor'){
  #regdat$year <- factor(regdat$year)
  regdat$Year <- factor(regdat$Year, level = c(2021, 2022, 2023)) # per Zehan 2024-08-17
  #regdat$year <- relevel(regdat$year, ref = 2021) 
}else{
  regdat$Year <- as.numeric(regdat$Year)
}
#** Religion prep -----
levels(Religion_levels)
regdat$Religion <- factor(regdat$Religion, 
                          levels = Religion_levels)
levels(regdat$Religion)
regdat$Religion <- relevel(regdat$Religion, ref = Religion.reference)
levels(regdat$Religion)

regdat$Religion1 <- NA # edit 2024-04-25
regdat$Religion1[which(regdat$Religion %in% c("Hindu", "Buddhist", "Sikh"))] <- "Dharmic"
regdat$Religion1[which(regdat$Religion %in% c("Christian", "Jewish", "Muslim"))] <- "Abraham"

regdat$Religion2 <- 'withheld' # edit 2024-08-05 was 'NA
regdat$Religion2[which(regdat$Religion %in% c("Hindu", "Buddhist",
                                              "Sikh", "Christian", "Jewish", 
                                              "Muslim", "Any other religion"))] <- "Yes"
regdat$Religion2[which(regdat$Religion %in% c("No"))] <- "No"
regdat$Religion2[which(regdat$Religion %in% c("withheld"))] <- "withheld"

regdat$Religion1 <- factor(regdat$Religion1,
                           levels = c("No", "Dharmic", "Abraham", "Other", "withheld"))

regdat$Religion2 <- factor(regdat$Religion2,
                           levels = c("No", "Yes", "withheld", 'NA'))

regdat$Benchmarking_group <- factor(regdat$Benchmarking_group, 
                                    labels = c("acute", "acute_specialist", 
                                               "ambulance", "community",
                                               "mental_health"))

# WRITE regdat with both outcomes -----
header_style <- createStyle(fgFill = "#4B9CD3", halign = "LEFT", textDecoration = "Bold", fontColour = "white", borderColour = "white", border = "TopBottomLeftRight")
write.xlsx(
  regdat, 
  paste0("regdat - ", Sys.Date(), ".xlsx"), 
  creator = "rbadgett@kumc.edu", 
  headerStyle = header_style, 
  tabColour = "#4B9CD3"
)

# ______________________________------
# Results ------
# Respondents for para 1. See also separate script -----
base_columns <- grep("base", colnames(regdat), value = TRUE)

# Calculate the sum for each of these columns
(sums <- colSums(regdat[, base_columns]))
(Respondents <- max(sums, na.rm = TRUE))
(Outcome_respondents <- sum (regdat$Outcome_base))
Outcome_overall_rate <- sum(regdat$Outcome_rate*regdat$Outcome_base/Outcome_respondents)
message (paste("\033[32m\033[1m",format(Respondents, big.mark = ","), " is the based for burnout and is also the maximum base for any question. The outcome rate (crude) of is ", round(100*Outcome_overall_rate,1), "%.\033[0m"))

# Response rates ------
sum(regdat$Burned_out_base)
# "MH & LD and MH, LD & Community" on the online dashboard is " mental health and learning disability (MH & LD) trusts, and mental health, learning disability, and community trusts"

regdat$Religion <- relevel(regdat$Religion, ref = Religion.reference)
levels(regdat$Religion)

# ______________________________------
# Regression ------
##* Formulas (initial) ------
formula_initial_religion_only <- "Outcome_rate ~ 
            Religion" 
            #+ Year"
formula_initial_all_and_religion <- "Outcome_rate ~ 
            Religion +  
            # Benchmarking_group + 
            Ethnic_minority_rate + Female_rate + Sexual_minority_rate + Older_rate + 
            Patient_facing_rate + Med_Dental_rate +
            Discrimination_internal_rate  + Discrimination_public_rate + 
            Emotional_public_rate + Emotional_manager_rate + Emotional_colleagues_rate + 
            Physical_public_rate + Physical_manager_rate + Physical_colleagues_rate +
            Time_demands_meet_rate +
            Year"
model_all_and_religion <- betareg(formula_initial_all_and_religion, data = regdat)
formula_all_and_religion_reduced <- model_all_and_religion$formula
formula_all_and_religion_reduced_text <- capture.output(print(formula_all_and_religion_reduced))
writeLines(formula_all_and_religion_reduced_text, 
           paste0("formula_initial_all_and_religion_reduced_", Outcome.source, ".txt"))
formula_initial_all_sans_religion <- "Outcome_rate ~ 
            Year + 
            # Benchmarking_group + 
            Ethnic_minority_rate + Female_rate + Sexual_minority_rate + Older_rate + 
            Med_Dental_rate + Patient_facing_rate +
            Discrimination_public_rate + Discrimination_internal_rate  + 
            Physical_public_rate + Physical_manager_rate + Physical_colleagues_rate +
            Emotional_public_rate + Emotional_manager_rate + Emotional_colleagues_rate + 
            Time_demands_meet_rate"
formula_initial_demographics_sans_religion <- "Outcome_rate ~ 
            # Benchmarking_group + 
            # Year + # added 2025-05-09, removed 2025-06-15
            Ethnic_minority_rate + Female_rate + Sexual_minority_rate + Older_rate + 
            Med_Dental_rate + Patient_facing_rate"

##* formula all SANS religion -----
# Step 1: Split by newline
lines <- unlist(strsplit(formula_initial_all_sans_religion, "\n"))
# Step 2: Remove comments from each line
lines <- gsub("#.*", "", lines)
# Step 3: Trim whitespace and remove empty lines
lines <- trimws(lines)
lines <- lines[lines != ""]
# Step 4: Reconstruct the formula into a single string
cleaned_formula_str <- paste(lines, collapse = " ")
# Check the cleaned formula string
cleaned_formula_str
# It should look like:
# "Outcome_rate ~ ..."
# Step 5: Convert the cleaned string to a formula
form <- as.formula(cleaned_formula_str)

# Extract all variables except Outcome_rate and Benchmarking_group
all_vars <- all.vars(form)
vars_of_interest <- setdiff(all_vars, c("Outcome_rate", "Benchmarking_group"))

#** Get final formulas - Multivariate / STEPBETA backward------
#*** Betareg -----
#*** MOD0 for formula first -----
mod0_religion_only <- betareg(as.formula(formula_initial_religion_only), data = regdat)
mod0_all_and_religion <- betareg(as.formula(formula_initial_all_and_religion), data = regdat)
mod0_all_sans_religion <- betareg(as.formula(formula_initial_all_sans_religion), data = regdat)
mod0_demographics_sans_religion <- betareg(as.formula(formula_initial_demographics_sans_religion), data = regdat)
summary(mod0_religion_only)
summary(mod0_all_and_religion) # Year not sig 2025-05-09
summary(mod0_all_sans_religion)# Year not sig 2025-05-09
summary(mod0_demographics_sans_religion) # YEAR IS ONLY SIG HERE, so removed

#**** 0. Religion only -----
model_religion_no_mods <- mod0_religion_only
function_display_model_in_viewer(mod0_religion_only,paste("Table multivariable. Religion only and ", Outcome.source.label))

#**** 1. DEMOGRAPHICS sans religions -----
function_display_model_in_viewer(mod0_demographics_sans_religion,paste("Table 1. Demographics (all) and ", Outcome.source.label))
#****** STEPBETA -----
model_demographics_sans_religion_reduced <- StepBeta(mod0_demographics_sans_religion)
formula_demographics_sans_religion_reduced <- function_check_revise_formula(model_demographics_sans_religion_reduced)
model_demographics_sans_religion_reduced_final <- betareg(as.formula(formula_demographics_sans_religion_reduced), data = regdat)
function_display_model_in_viewer(model_demographics_sans_religion_reduced_final,paste("Table multivariable. Demographics (significant - excluded Patient facing and sexual minority rates) and ", Outcome.source.label))
message(paste0("\033[32m\033[1mSig demographics only R2: ", round(model_demographics_sans_religion_reduced_final$pseudo.r.squared*100,1), "%\033[0m"))

#**** 2. All sans religion (multivariable)-----
function_display_model_in_viewer(mod0_all_sans_religion,paste("Table. All factors SANS burnout and ", Outcome.source.label))
#****** STEPBETA -----
model_all_sans_religion_reduced <- StepBeta(mod0_all_sans_religion)
summary(model_all_sans_religion_reduced)
formula_all_sans_religion_reduced_final <- function_check_revise_formula(model_all_sans_religion_reduced)
model_all_sans_religion_reduced_final <- betareg(as.formula(formula_all_sans_religion_reduced_final), data = regdat)
summary(model_all_sans_religion_reduced_final)
# Check twice on this one!
formula_all_sans_religion_reduced_final <- function_check_revise_formula(model_all_sans_religion_reduced_final)
model_all_sans_religion_reduced_final <- betareg(as.formula(formula_all_sans_religion_reduced_final), data = regdat)
function_display_model_in_viewer(model_all_sans_religion_reduced_final,paste("Table. All independently significant stressors SANS religion and ", Outcome.source.label))
message(paste0("\033[32m\033[1mAll sig stressors R2: ", round(model_all_sans_religion_reduced_final$pseudo.r.squared*100,1), "%\033[0m"))

#**** 3. ALL with religion ----- 
# Remove year? no 2025-01-09. However, was insig in bivariate
#formula_initial_all_and_religion1 <- gsub("\\s*year \\+\\s*", "", formula_initial_all_and_religion)
formula_initial_all_and_religion1 <- as.formula(formula_initial_all_and_religion)
#assign(paste0("formula_final_backward_Religion_", Outcome.source), formula_initial_all_and_religion) 
formula_initial_all_and_religion1 <- as.formula(formula_initial_all_and_religion1)
mod0_all_and_religion <- betareg(formula_initial_all_and_religion1, data = regdat)
model_all_and_religion_reduced <- StepBeta(mod0_all_and_religion)
###
formula_all_and_religion_reduced <- model_all_and_religion_reduced$formula

mean(regdat$Emotional_manager_rate)
# CAREFUL with regdat$Emotional_manager_rate below 2025-05-01 -----
# regdat$Emotional_manager_rate <- regdat$Emotional_manager_rate * 10
formula_all_and_religion_reduced_final <- function_check_revise_formula(model_all_and_religion_reduced)
model_all_and_religion_reduced_final   <- betareg(as.formula(formula_all_and_religion_reduced_final), data = regdat)
summary(model_all_and_religion_reduced_final)
R2 <- model_all_and_religion_reduced_final$pseudo.r.squared
#message(paste0("\033[32m\033[1mSig all and religion R2: ", round(R2_model_all_and_religion_reduced*100,1), "%\033[0m"))
message(paste0("\033[32m\033[1mSig all and religion R2: ", round(100*R2,1), "%\033[0m"))
function_display_model_in_viewer(
  model_all_and_religion_reduced_final,
  paste0("Table 2b (right). Model final for ",
         Outcome.source.label,'. R2 = ', round(R2*100,1),'%.\n', 
         "degrees of freedom: ", model_all_and_religion_reduced_final$df.null)
  ) # , round(R2_model_final_backward_Religion * 100,1)

# Write formulas and models (backward) -------
#**** 0. Religion only -----
saveRDS(model_religion_no_mods,
        paste0("model_final_backward_BetaReg_religion_no_mods_", Outcome.source, ".rds"))

#**** 1. Demographics SANS religion -----
#formula_temp_text <- capture.output(print(formula_demographics_sans_religion_reduced))
#writeLines(formula_temp_text, 
#           paste0("formula_final_backward_StepBeta_demographics_sans_Religion_", Outcome.source, ".txt"))
#model_temp_text   <- capture.output(print(model_demographics_sans_religion_reduced_final))
#writeLines(model_temp_text, 
#           paste0("model_final_backward_StepBeta_demographics_sans_Religion_", Outcome.source, ".txt"))
saveRDS(model_demographics_sans_religion_reduced_final,
        paste0("model_final_backward_StepBeta_demographics_sans_Religion_", Outcome.source, ".rds"))

#**** 2. All SANS religion -----
#formula_temp_text <- capture.output(print(formula_all_sans_religion_reduced_final))
#writeLines(formula_temp_text, 
#           paste0("formula_final_backward_StepBeta_all_sans_Religion_", Outcome.source, ".txt"))
#model_temp_text   <- capture.output(print(model_all_sans_religion_reduced_final))
#writeLines(model_temp_text, 
#           paste0("model_final_backward_StepBeta_all_sans_Religion_", Outcome.source, ".txt"))
saveRDS(model_all_sans_religion_reduced_final,
           paste0("model_final_backward_StepBeta_all_sans_Religion_", Outcome.source, ".rds"))

#**** 3. All WITH religion -----
#formula_temp_text <- capture.output(print(formula_all_and_religion_reduced_final))
#writeLines(formula_temp_text, 
#           paste0("formula_final_backward_StepBeta_all_and_Religion_", Outcome.source, ".txt"))
#model_temp_text   <- capture.output(print(model_all_and_religion_reduced_final))
#writeLines(model_temp_text, 
#           paste0("model_final_backward_StepBeta_all_and_Religion_", Outcome.source, ".txt"))
saveRDS(model_all_and_religion_reduced_final,
        paste0("model_final_backward_StepBeta_all_and_Religion_", Outcome.source, ".rds"))

# _________________________________---------------

#**** Summary fit stats AIC, BIC-------------

(aic_model_all_and_religion_reduced <- AIC(model_all_and_religion_reduced_final)) # Akaike information criterion
(bic_model_all_and_religion_reduced <- BIC(model_all_and_religion_reduced_final)) # Bayesian Information Criterion (BIC)
(R2_model_all_and_religion_reduced  <- model_all_and_religion_reduced_final$pseudo.r.squared)

function_display_model_in_viewer(model_all_and_religion_reduced_final,paste0("Table 2 (NOT USED). Model final for ", Outcome.source.label,' (not including religion). R2 = ', round(R2_model_all_and_religion_reduced * 100,1),'%.\n','. Mean rate: ', 100*round(mean(regdat$Outcome_rate),3),'%'))

#*** Dominance -----
summary(model_demographics_sans_religion_reduced_final)
function_general_dominance_values(model_demographics_sans_religion_reduced_final, regdat)
summary(model_all_and_religion_reduced_final)
(model_all_and_religion_reduced_final$pseudo.r.squared)
# GOOD:
function_general_dominance_values(model_all_and_religion_reduced_final, regdat)
#domir(as.formula(model_final), lm_r2, data = regdat)

# Response to Dr Grady added 2025-03-06
message(paste0("\033[32m \033[1mUsing model_all_and_religion_reduced_final\012\033[0m"))
(Coefficient_female <- model_all_and_religion_reduced_final$coefficients$mean['Female_rate'])
(OR_female <- exp(Coefficient_female))
dominance_temp <- function_general_dominance_values(model_all_and_religion_reduced_final, regdat)
dominance_temp$Value[dominance_temp$Variable == "Female_rate"]

#** Comparison of the models-----
(aic_model_demographics_sans_religion_reduced_final <- AIC(model_demographics_sans_religion_reduced_final))
(aic_model_all_sans_religion_reduced_final          <- AIC(model_all_sans_religion_reduced_final))
(aic_model_all_and_religion_reduced_final           <- AIC(model_all_and_religion_reduced_final))

(bic_model_demographics_sans_religion_reduced_final <- BIC(model_demographics_sans_religion_reduced_final))
(bic_model_all_sans_religion_reduced_final          <- BIC(model_all_sans_religion_reduced_final))
(bic_model_all_and_religion_reduced_final           <- BIC(model_all_and_religion_reduced_final))
   
(R2_model_demographics_sans_religion_reduced_final <- model_demographics_sans_religion_reduced_final$pseudo.r.squared)
(R2_model_all_sans_religion_reduced_final          <- model_all_sans_religion_reduced_final$pseudo.r.squared)
(R2_model_all_and_religion_reduced_final           <- model_all_and_religion_reduced_final$pseudo.r.squared)

#*** Summary dataframeS from regdat  -----
# regdat has 135 rows: 45 units over 3 years
religion_summary <- regdat %>%
  group_by(Religion) %>%
  dplyr::summarize(
    Outcome_rate = mean(Outcome_rate, na.rm = TRUE),
    Outcome_base = sum(Outcome_base, na.rm = TRUE),
    Older_rate = mean(Older_rate, na.rm = TRUE),
    Female_rate = mean(Female_rate, na.rm = TRUE),
    Ethnic_minority_rate = mean(Ethnic_minority_rate, na.rm = TRUE),
    Sexual_minority_rate = mean(Sexual_minority_rate, na.rm = TRUE),
    Patient_facing_rate = mean(Patient_facing_rate, na.rm = TRUE),
    Med_Dental_rate = mean(Med_Dental_rate, na.rm = TRUE),
    Emotional_public_rate = mean(Emotional_public_rate, na.rm = TRUE),
    Emotional_manager_rate = mean(Emotional_manager_rate, na.rm = TRUE),
    Emotional_colleagues_rate = mean(Emotional_colleagues_rate, na.rm = TRUE),
    Physical_manager_rate = mean(Physical_manager_rate, na.rm = TRUE),
    Physical_colleagues_rate = mean(Physical_colleagues_rate, na.rm = TRUE),
    Discrimination_internal_rate= mean(Discrimination_internal_rate, na.rm = TRUE),
    Discrimination_public_rate= mean(Discrimination_public_rate, na.rm = TRUE),
    Time_demands_meet_rate = mean(Time_demands_meet_rate, na.rm = TRUE),
    year = NA # 2025-05-01as.factor(2022) # ASSUMPTION FOR YEAR. IS THIS ASSUMPTION OK?
  )
#**** Write csv, religion_summary_ -----
write.csv(religion_summary,paste0('religion_summary_',Outcome.source,'-', Sys.Date(),'.csv'))
