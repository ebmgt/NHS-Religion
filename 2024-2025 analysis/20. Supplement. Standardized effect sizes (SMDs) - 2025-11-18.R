# This file is available at https://github.com/ebmgt/NHS-Religion/
# Author:rbadgett@kumc.edu
# Permissions:
#* Code GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
#* Images CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
# Optimized for coding with R Studio document outline view
# Last edited 2025-11-18

beta <- -0.350 # From Table 2

# OR to RRR -----
#* Zhang, 1998 -----
# http://pubmed.gov/9832001
OR  <- exp(beta)
P0 <- 0.33 # Baseline rate in Table 1

RR <- OR /(1 - P0 + (P0 * OR)) # Equation of Zhang, 1998 PMID 9832001
RR

RRR <- 1 - RR
RRR * 100

(RR*P0)

P0 - (P0 * RRR)

message(paste0("\033[32m \033[1m Relative risk reduction): ", sprintf(RRR, fmt='%#.2f'), "\012\033[0m"))

# OR to SMD -----
#* Murad, 2019 -----
# https://pubmed.gov/30670455/
  # Murad et al
OR  <- exp(beta)
SMD <- log(OR) / (pi/sqrt(3)) # R uses 'log' for ln
message(paste0("\033[32m \033[1m Standardized mean difference (SMD): ", sprintf(SMD, fmt='%#.3f'), "\012(Cohen: Small is 0.2; moderate is 0.5; large is 0.8. PMID 19565683)\012\033[0m"))

# OR to ARR (absolute risk reduction) -----
#* Murad, 2019 -----
# https://pubmed.gov/30670455/
# Murad et al
OR  <- exp(beta)
CER <- PO <- 0.33 # control event rate, "(that is, baseline risk)"
ARR <- CER - OR*CER/(1 - CER + OR * CER)
ARR
PO - ARR
message(paste0("\033[32m \033[1m Absolute risk reduction (ARR): ", sprintf(ARR*100, fmt='%#.1f'), "%\012\033[0m"))
