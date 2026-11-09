# This file is available at https://github.com/ebmgt/NHS-Religion/
# Author:rbadgett@kumc.edu
# Permissions:
#* Code GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
#* Images CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
# Optimized for coding with R Studio document outline view
# Last edited 2025-11-18

beta <- -0.350 # From Table 2

# OR to SMD -----
#* Murad, 2019 -----
# https://pubmed.gov/30670455/
OR  <- exp(beta)
SMD <- log(OR) / (pi/sqrt(3)) # R uses 'log' for ln
message(paste0("\033[32m \033[1m Standardized mean difference (SMD): ", sprintf(SMD, fmt='%#.3f'), "\012(Cohen: Small is 0.2; moderate is 0.5; large is 0.8. PMID 19565683)\012\033[0m"))

# OR to ARR (absolute risk reduction) -----
#* Murad, 2019 -----
# https://pubmed.gov/30670455/
OR  <- exp(beta)
CER <- PO <- 0.33 # control event rate, "(that is, baseline risk)"
ARR <- CER - OR*CER/(1 - CER + OR * CER)
ARR
PO - ARR
message(paste0("\033[32m \033[1m Absolute risk reduction, unadjusted (ARR): ", sprintf(ARR*100, fmt='%#.1f'), "%\012\033[0m"))

# Hinduism via Murad's approach with SE and CIs added
beta  <- -0.349955039 # From "12. Table 2 - 2025-11-25.R" -> "Table 2. MV beta-coefficients-2025-11-25.csv"
se_b  <-  0.165369417 # From "12. Table 2 - 2025-11-25.R" -> "Table 2. MV beta-coefficients-2025-11-25.csv"
# Judaism via Murad's approach with SE and CIs added
beta  <- -0.198988763 # From "12. Table 2 - 2025-11-25.R" -> "Table 2. MV beta-coefficients-2025-11-25.csv"
se_b  <-  0.08076247 # From "12. Table 2 - 2025-11-25.R" -> "Table 2. MV beta-coefficients-2025-11-25.csv"

Odds_ratio = exp(beta)
SMD    <- beta * sqrt(3)/pi
SMD_se <- se_b * sqrt(3)/pi
if (SMD < 0) {
  # Use a temporary variable to perform the swap
  ci_lower <- SMD - (1.96 * SMD_se)
  ci_upper <- SMD + (1.96 * SMD_se)
}else{
  ci_lower <- SMD + (1.96 * SMD_se)
  ci_upper <- SMD - (1.96 * SMD_se)
}

ci_result <- c(CIlower = ci_lower, CIupper = ci_upper)

# ___________________________________--------
# Comparing to prior studies -----
#* Data frame creation -----
#From: https://rdrr.io/cran/gemtc/man/blobbogram.html
library(gemtc)
library(meta)
library(grid)
library(dplyr) # Mutate

data_Hinduism <- data.frame(
  id = "Hinduism",
  group = 1,
  pe = SMD,
  ci.l = ci_result[1], # Use the R variable here
  ci.u = ci_result[2], # Use the R variable here
  style = "normal",
  subjects = NA
)

data_Judaism <- data.frame(
  id = "Judaism",
  group = 1,
  pe = SMD,
  ci.l = ci_result[1], # Use the R variable here
  ci.u = ci_result[2], # Use the R variable here
  style = "normal",
  subjects = NA
)

# Below is from Collett, Ann Intern Med. 2025 PMID 41248499 (Summary Table)
data_mindfulness <- read.table(textConnection('
id                                       group     pe    ci.l  ci.u style      subjects 
"Physicians"          2     -0.46 -1.28  0.35 "normal" 375 
"Nurses and midwives" 2     -0.90 -1.46 -0.334 "normal" 511
"Mixed roles or HCPs" 2     -0.40 -0.65 -0.16 "normal" 687 # was 511 in this pub - seems an error 
'), header=TRUE)
#!!!!! Should line 3 above denominator be 687???

data <- rbind(data_Hinduism, data_Judaism, data_mindfulness)
rownames(data) <- NULL

data_2 <- data |>
  mutate(
    se = (ci.u - ci.l) / (2 * 1.96),
    group_lab = ifelse(group == 1,
                       "Current observational study",
                       "Meta-analyses of trials of mindfulness")
  )

m <- metagen(TE = pe, seTE = se,
             studlab = id,
             subgroup     = group_lab,
             print.subgroup.name = FALSE,
             sm = "SMD", data = data_2,
             leftlabs = NULL,
             common = FALSE,
             random = FALSE)

#* Forest plot -----
ticks <- c(-1.0, -0.8, -0.5, -0.2, 0, 0.2, 0.5, 0.8, 1.0)
meta::forest(m,
             subgroup = TRUE, 
             leftlabs = c("Study", "Subjects"),
             leftcols = c("studlab", "subjects"),
             colgap.studlab = "0mm", 
             print.byvar = TRUE,
             xlab = "",#"Standardized mean difference\u2021",
             smlab = expression("Standardised Mean Difference (SMD)"^"\u2021"),
             xlim = c(-1.4,1.4),
             fs.axis = 10,
             label = ticks,
             at =  ticks,
             plotwidth = "8cm"
             )

Title <- bquote(
  atop(
    "Results* of the current study compared to the",
    "systematic review by Collett, 2025"^"\u2020"
  )
)
grid.text(Title, 0.5, 0.85, gp=gpar(cex=1.4))

grid.text('Notes:', 0.03, 0.25, hjust=0, gp=gpar(cex=1, font=2))
Footer <- "* For results, burnout defined by the NHS as responding 'Often' or 'Always' to 'How often, if at all,
  do you feel burnt out because of your work?' Burnout in Collete is emotional exhaustion."
Footer <- paste(Footer,"\n\u2020 Collett et al. Ann Intern Med. 2025 PMID 41248499.")
Footer <- paste(Footer,"\n\u2021 SMD effect: 0.8 is strong, 0.5 is moderate, 0.2 is weak.")
grid.text(Footer,   0.03, 0.135, hjust=0, gp=gpar(cex=1))
