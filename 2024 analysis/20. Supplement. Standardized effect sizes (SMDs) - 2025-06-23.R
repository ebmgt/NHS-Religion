# This file is available at https://github.com/ebmgt/NHS-Religion/
# Author:rbadgett@kumc.edu
# Permissions:
#* Code GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
#* Images CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
# Optimized for coding with R Studio document outline view
# Last edited 2025-06-19

# Get source for Starts up and functions -----
source("00. religion_common_code1_Startup_functions.R")  

# Get source for Libraries/package loading -----
source("00. religion_common_code2_Libraries.R")  

library(esc)
# _________________________------------
# Packages ----
library(betareg); library(mediation); library(lmtest); library(car)
library(betareg); library(dplyr); library(knitr)

#_________________________________-----
# Analyses ------
# Method 1: Chinn, 2000
# beta = log-odds coefficient from a 0/1 predictor
beta  <- 0.63        # ln(OR)
se_b  <- 0.25        # its SE
n_tx  <- 120
n_ct  <- 115
N     <- 1876265 # n_tx + n_ct

#**
d      <- beta * sqrt(3) / pi       # Chinn / H-H
se_d   <- se_b * sqrt(3) / pi
cat(green$bold("SMD: ", round(d,3)))


J      <- 1 - 3/(4*N - 9)           # Hedges correction
g      <- J * d
var_g  <- (J^2) * se_d^2
ci_g   <- g + c(-1,1) * 1.96 * sqrt(var_g)
round(c(g, ci_g), 3)

# Method 2: Campbell calculator and Wilson's package ------------
# https://www.campbellcollaboration.org/calculator/
# https://mason.gmu.edu/~dwilsonb/
esc_g_unstd <- esc_B(
  b       =  2.30,   # raw coefficient (mean shift between groups)
  sdy     =  5.70,   # pooled SD of the outcome
  grp1n   = 48410,        # treated N
  grp2n   = 1876265-48410,# control N
  es.type = "g"      # ask directly for Hedges g
)

esc_g_unstd

