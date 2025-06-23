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
  
  #regdat <- read.xlsx("regdat - 2025-01-11.xlsx")
