# This file is available at https://github.com/ebmgt/NHS-Religion/
# Author:rbadgett@kumc.edu
# Permissions: -----
#* Images/text CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
#** Plain English details https://creativecommons.org/licenses/by-nc-sa/4.0/
#* Code GNU GPLv3 https://www.gnu.org/licenses/gpl-3.0.en.html
#** Plain English details at https://www.gnu.org/licenses/quick-guide-gplv3.html
#* Commercial licensing options are available separately.
# Optimized for coding with R Studio document outline view

# Libraries/packages -----
library(gemtc)
library(meta)
library(grid)
library(dplyr) # Mutate

# ___________________________________--------
# Functions -----

function_plot_print <- function(plotname, plotwidth, plotheight, imagetype = c("png", "jpeg", "bmp", "tiff", "svg")) {
  
  # Validate the image type and default to the first item (png)
  imagetype <- match.arg(imagetype)
  
  # Clean the plot name
  # No spaces
  # plotname <- gsub(":|\\s|\\n|\\?|\\!|\\'", "", plotname)
  # Spaces ok
  plotname <- gsub(":|\\n|\\?|\\!|\\'", "", plotname)
  
  # Generate filename with timestamp
  file_name <- paste0(plotname, " - ", format(Sys.time(), "%Y-%m-%d %H-%M-%S"), ".", imagetype)
  
  # Save the plot
  rstudioapi::savePlotAsImage(
    file_name,
    format = imagetype, 
    width = plotwidth, 
    height = plotheight
  )
}


function_SMD_CI <- function(beta, se_beta) {

  #Chinn (2000, PMID 11113947
         
  OR  <- exp(beta)

  # OR to SMD -----
  # Murad, 2019
  # https://pubmed.gov/30670455/
  
  SMD <- log(OR) / (pi/sqrt(3)) # R uses 'log' for ln
  message(paste0("\033[32m \033[1m Standardized mean difference (SMD): ", sprintf(SMD, fmt='%#.3f'), "\012(Cohen: Small is 0.2; moderate is 0.5; large is 0.8. PMID 19565683)\012\033[0m"))
  
  # OR to ARR (absolute risk reduction) NOT USED -----
  # Murad, 2019
  # https://pubmed.gov/30670455/
  CER <- PO <- 0.33 # control event rate for all respondents, "(that is, baseline risk)"
  ARR <- CER - OR*CER/(1 - CER + OR * CER)
  # PO - ARR
  message(paste0("\033[32m \033[1m Absolute risk reduction, unadjusted (ARR): ", sprintf(ARR*100, fmt='%#.1f'), "%\012\033[0m"))
  
  # SMD
  SMD    <- beta * sqrt(3)/pi
  SMD_se <- se_beta * sqrt(3)/pi
  
  # SMD CI
  ci_lower <- SMD - (1.96 * SMD_se)
  ci_upper <- SMD + (1.96 * SMD_se)
  
  ci_result <- c(CIlower = ci_lower, CIupper = ci_upper)
  
  smd_results <- c(SMD, ci_result)
  return(smd_results)
}

# ___________________________________--------
# Religion calculations -----

## Data frame creation -----

# Hinduism via Murad's approach with SE and CIs added
beta     <- -0.349955039 # From "12. Table 2 - 2025-11-25.R" -> "Table 2. MV beta-coefficients-2025-11-25.csv"
se_beta  <-  0.165369417 # From "12. Table 2 - 2025-11-25.R" -> "Table 2. MV beta-coefficients-2025-11-25.csv"

smd_results <- function_SMD_CI(beta,se_beta)

data_Hinduism <- data.frame(
  id = "Hinduism",
  group = 1,
  pe = smd_results[1],
  ci.l = smd_results[2], # Use the R variable here
  ci.u = smd_results[3], # Use the R variable here
  style = "normal",
  subjects = NA
)

# Judaism via Murad's approach with SE and CIs added
beta     <- -0.198988763 # From "12. Table 2 - 2025-11-25.R" -> "Table 2. MV beta-coefficients-2025-11-25.csv"
se_beta  <-  0.08076247  # From "12. Table 2 - 2025-11-25.R" -> "Table 2. MV beta-coefficients-2025-11-25.csv"

smd_results <- function_SMD_CI(beta,se_beta)

data_Judaism <- data.frame(
  id = "Judaism",
  group = 1,
  pe = smd_results[1],
  ci.l = smd_results[2], # Use the R variable here
  ci.u = smd_results[3], # Use the R variable here
  style = "normal",
  subjects = NA
)

# Below is from Collett, Ann Intern Med. 2025 PMID 41248499 (Summary Table)
data_mindfulness <- read.table(textConnection('
id                  group     pe    ci.l  ci.u style      subjects 
"Physicians"          2     -0.46 -1.28  0.35 "normal" 375 
"Nurses and midwives" 2     -0.90 -1.46 -0.334 "normal" 511
"Mixed roles or HCPs" 2     -0.40 -0.65 -0.16 "normal" 687 # was 511 in the  pub - seems an error, author agreed
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
             smlab = expression("Standardised Mean Difference (SMD"^"b"*")"), # \u2021
             xlim = c(-1.4,1.4),
             fs.axis = 10,
             label = ticks,
             at =  ticks,
             plotwidth = "8cm"
             )

Title <- bquote(
  atop(
    "Results"^"a" ~ "of the current study compared to the",
    "systematic review by Collett, 2025"^"b" #"\u2020"
  )
)

grid.text(Title, 0.5, 0.85, gp=gpar(cex=1.4))

grid.text('Notes:', 0.03, 0.25, hjust=0, gp=gpar(cex=1, font=2))
Footer <- "* For results, burnout is defined by the NHS as responding 'Often' or 'Always' to 'How often, if at all,
  do you feel burnt out because of your work?' Burnout in Collete is emotional exhaustion."
Footer <- paste(Footer,"\n\u2020 Collett et al. Ann Intern Med. 2025 PMID 41248499.")
Footer <- paste(Footer,"\n\u2021 SMD effect: 0.8 is strong, 0.5 is moderate, 0.2 is weak.")
grid.text(Footer,   0.03, 0.135, hjust=0, gp=gpar(cex=1))

function_plot_print("Figure_S1_SMDs", 800, 500, "png")
