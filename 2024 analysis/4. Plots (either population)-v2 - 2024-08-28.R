# This file is available at https://github.com/ebmgt/NHS-Religion/
# Author: Zehan Yang, PhD. zehan.yang@uconn.edu. You may also contact rbadgett@kumc.edu
# Permission: GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
# Last edited 2024-08-29

# This file uses the datafile:
# "data_trusts_burnout.csv"

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

library(tcltk) # For interactions and troubleshooting, Oart of base so no install needed.

# Did the script load? -----
tk_messageBox(type = "ok", paste('1. ', 'Working directory:\n', getwd(), sepo=''), caption = "Hello:")

# Functions ------
`%notin%` <- Negate(`%in%`)
`%!=na%` <- function(e1, e2) (e1 != e2 | (is.na(e1) & !is.na(e2)) | (is.na(e2) & !is.na(e1))) & !(is.na(e1) & is.na(e2))
`%==na%` <- function(e1, e2) (e1 == e2 | (is.na(e1) & is.na(e2)))

current.date <- function(){
  return (as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE)))
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

function_table_nice <- function(dataframe, row_name = 'RESPONDENT_STATUS', column_name) {
  # Make the table
  contingency_table <- base::table(
    dataframe[,row_name], dataframe[,column_name], 
    dnn =  c(row_name, column_name), useNA = 'always')
  
  print(addmargins(contingency_table))
  
  # Print the column percentages
  print (round(prop.table(contingency_table, 2) * 100,1))
  
  # Perform chi-square test and display results
  chi_sq_test <- chisq.test(contingency_table)
  print(paste("chi-square: ", chi_sq_test$statistic, " p-value: ", chi_sq_test$p.value, sep=' '))
  
  # Suppress warnings
  suppressWarnings(paste("(warnings suppressed)"))
}
# Function to calculate I2
I2 <- function(res) {
  W <- diag(1/res$vi)
  X <- model.matrix(res)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))
}
# Libraries ------
packages_essential <- c('stringr','openxlsx','readr','foreach')
function_libraries_install(packages_essential)
library(ggplot2)
library(ggrepel)
#library(metafor)
#library(meta)
#library(boot)
library(plyr)

# Get data ------
#* Parameters -----
#** Select data sources ----
data.source <- tk_select.list(c('All',
                  'Trusts and CCGs only'), 
                  preselect = 'Trusts and CCGs only', multiple = FALSE,
                  title =paste0("\n\n",Sys.info()["user"], ":\n\nWho are we studying?\n\n"))
#** Select outcome ----
Outcome.source <- tk_select.list(c('Burned_out_rate',
                  'Stress'), 
                  preselect = 'Burned_out_rate', multiple = FALSE,
                  title =paste0("\n\n",Sys.info()["user"], ":\n\nWhat outcome are we studying?\n\n"))
#* Get data -----
if (data.source == 'All'){
  #** data source: all -----
  tk_messageBox(type = c('okcancel'), "data_full does not have for regression:\n\nfemale\nmedical/dental", caption = "Warning")
  sts_has <- read_csv("data_full.csv")
}else{
  #** data source: trust only -----
  sts_has <- read_csv("data_trusts_burnout.csv")
#  names(sts_has)[names(sts_has) == "religion"] <- "Religion"
#  names(sts_has)[names(sts_has) == "trust"] <- "Trust"
#  names(sts_has)[names(sts_has) == "year"] <- "Year"
  if (Outcome.source == 'Burned_out_rate'){
    names(sts_has)[names(sts_has) == "Burned_out_rate"] <- "stress"
    names(sts_has)[names(sts_has) == "Burned_out_base"] <- "stress_base"
  } else{
    names(sts_has)[names(sts_has) == "Stress_rate"] <- "stress"
    names(sts_has)[names(sts_has) == "Stress_base"] <- "stress_base"
  }
  #names(sts_has)[names(sts_has) == "Female_rate"] <- "Physical_Colleagues"
  #names(sts_has)[names(sts_has) == "Female_rate_base"] <- "Physical_Colleagues_base"
  #names(sts_has)[names(sts_has) == "Older_rate"] <- "Emotional_Colleagues"
  #names(sts_has)[names(sts_has) == "Older_rate_base"] <- "Emotional_Colleagues_base"
  names(sts_has)[names(sts_has) == "Emotional_colleagues_rate"] <- "Emotional_Colleagues"
  names(sts_has)[names(sts_has) == "Emotional_colleagues_base"] <- "Emotional_Colleagues_base"
  names(sts_has)[names(sts_has) == "Emotional_manager_rate"] <- "Emotional_Manager"
  names(sts_has)[names(sts_has) == "Emotional_manager_base"] <- "Emotional_Manager_base"
  names(sts_has)[names(sts_has) == "Emotional_public_base"] <- "Emotional_Public_base"
  names(sts_has)[names(sts_has) == "Emotional_public_rate"] <- "Emotional_Public"
  names(sts_has)[names(sts_has) == "Physical_colleagues_rate"] <- "Physical_Colleagues"
  names(sts_has)[names(sts_has) == "Physical_colleagues_base"] <- "Physical_Colleagues_base"
  names(sts_has)[names(sts_has) == "Physical_manager_rate"] <- "Physical_Manager"
  names(sts_has)[names(sts_has) == "Physical_manager_base"] <- "Physical_Manager_base"
  names(sts_has)[names(sts_has) == "Physical_public_rate"] <- "Physical_Public"
  names(sts_has)[names(sts_has) == "Physical_public_base"] <- "Physical_Public_base"
  sts_has$Religion[sts_has$Religion == "withheld"] <- "Not to say"
  vars_to_keep <- c("Religion", "Year", "stress", "stress_base", 
                    "Physical_Colleagues", "Physical_Colleagues_base", 
                    "Emotional_Colleagues", "Emotional_Colleagues_base", 
                    "Physical_Manager", "Physical_Manager_base", 
                    "Emotional_Manager", "Emotional_Manager_base", 
                    "Physical_Public", "Physical_Public_base", 
                    "Emotional_Public", "Emotional_Public_base")
  sts_has <- sts_has[, vars_to_keep]
}

names(sts_has)
sapply(sts_has, class)

# Table 1 -----
# Calculate the denominators for each of these columns
base_columns <- grep("base", colnames(sts_has), value = TRUE)
(sums <- colSums(sts_has[, base_columns]))
# Summary rate -----
respondents <- sum(sts_has$stress_base)
(Stress_overall_rate <- sum(sts_has$stress*sts_has$stress_base/respondents))

# FIGURES with ggplot -----
out <- NULL
smry <- lapply(c("Christian", "Jewish",
                 "Muslim", "Buddhist", "Hindu",
                 "Sikh", "Any other religion", "No religion",
                 "I would prefer not to say"), # missing Withheld (rather than not to say) for trusts database
               function(i) {
                 tmp_rlg <- sts_has[sts_has$Religion == i, ]
                 base_sum <- colSums(tmp_rlg[, -1])[seq(3, 15, 2)]
                 rts <- tmp_rlg[, seq(3, 15, 2)]
                 base <- tmp_rlg[, seq(4, 16, 2)]
                 tmp <- colSums(rts * base)/ base_sum
                 data.frame(religion = i, stress = tmp[1], abuse = tmp[7],
                            base_stress = base_sum[1])
               })

out <- as.data.frame(foreach(t = smry, .combine = "rbind") %do% {t})

#* Rename religions to friendly names -----
out$religion <- factor(out$religion, 
                       levels = c("I would prefer not to say", "Any other religion","No religion", 
                                  "Christian","Jewish", "Muslim", 
                                  "Buddhist", "Sikh", "Hindu"),
                       labels = c("Withheld", "Other", "No Religion", 
                                  "Christian","Jewish", "Muslim", 
                                  "Buddhist", "Sikh", "Hindu"))
out$type <- NA
out$type1 <- NA
unique(out$stress)
for (i in seq_len(nrow(out))) {
  if (out$religion[i] %in% c("Christian", "Jewish", "Muslim")) {
    out$type[i] <- "Abrahamic"
    out$type1[i] <- "Religion"
  } else if (out$religion[i] %in% c("Buddhist", "Hindu", "Sikh")) {
    out$type[i] <- "Dharmic"
    out$type1[i] <- "Religion"
  } else if (out$religion[i] == "Other") {
    out$type[i] <- "Other"
    out$type1[i] <- "Religion"
  } else if (out$religion[i] == "No Religion"){
    out$type[i] <- "None"
    out$type1[i] <- "None"
  } else if (out$religion[i] == "Withheld"){
    out$type[i] <- "Withheld"
    out$type1[i] <- "Withheld"
  }
} 
#* Colors ------
custom.col <- c("#DC143C", "#228B22", "#4169E1",
                "#DAA520", "#DA70D6")
# Revised colors
custom.col <- c("#DAA520", "#228B22", "#4169E1",
                "#DC143C", "#FF999C")
out$type <- factor(out$type, levels = c("Abrahamic", "Dharmic", "Other", "Withheld", "None"))

#* ggplot -----
#** Figure 1. Bar plot -----
ggplot(data = out, aes(x = religion, y = stress, fill = type)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.75) +
  theme(text = element_text(size = 12),
        legend.position = "bottom",
        legend.title = element_blank()) +
  xlab("Religion") +
  ylab(paste0(gsub("_", " ", Outcome.source), " (%)")) +
  scale_fill_manual(values = custom.col) +
  geom_text(aes(label = (paste0(round(stress, 1),'%'))), vjust = -0.2) +
  coord_cartesian(ylim = c(0, 60)) 

ggsave(paste0("figure 1 - ", data.source, ' - ', Outcome.source,' (', respondents, ') - ', current.date(),".pdf"), width = 10, height = 6)

#** Figure 2. Regression plot -----
ggplot(data = out, aes(x = abuse, y = stress, 
                       label = religion, color = type)) +
  geom_smooth(method=lm, color="black", se = TRUE) +
  geom_point() +
  geom_label_repel(show.legend = F) +
  ylab(paste0(gsub("_", " ", Outcome.source), " (%)")) +
  xlab("Rate of Emotional Harassment from Public (%)") +
  theme(text = element_text(size = 12),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA)) +
  scale_color_manual(values = custom.col) +
  scale_fill_manual(values = custom.col) +  # Set fill colors
  guides(color = guide_legend(override.aes = list(fill = custom.col, shape = 15, size = 5)))


ggsave(paste0("figure 2 - ", data.source, ' - ', Outcome.source  ,' (', respondents, ') - ', current.date(),".pdf"), width = 10, height = 6)

#** Figure 3. Cross tab of ever-Stress and ever_Burned_out -----
