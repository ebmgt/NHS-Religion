# This file is available at https://github.com/ebmgt/NHS-Religion/
# Author: Zehan Yang, PhD. zehan.yang@uconn.edu. You may also contact rbadgett@kumc.edu
# Permission: GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
# Last edited 2024-07-29

# Functions ------
function_libraries_install <- function(packages){
  install.packages(setdiff(packages, rownames(installed.packages())), repos = "https://cloud.r-project.org/")
  for(package_name in packages)
  {
    library(package_name, character.only=TRUE, quietly = FALSE);
    cat('Installing package: ', package_name)
  }
}
function_plot_print <- function (plotname, plotheight, plotwidth){
  plotname <- gsub(":|\\s|\\n|\\?|\\!|\\'", "", plotname)
  (current.date <- as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE)))
  rstudioapi::savePlotAsImage(
    paste(plotname,' -- ',current.date,'.png',sep=''),
    format = "png", width = plotwidth, height = plotheight)
}
current.date <- function(){
  return (as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE)))
}

# Packages ------
library(tcltk)
packages <- c("readr",'foreach','ggplot2','ggrepel','plyr','boot','metafor','meta')
function_libraries_install(packages)
packages <- c('dplyr')
function_libraries_install(packages)

# Get data ------
#* Select data sources ----
data.source <- tk_select.list(c('All','Trusts only'), preselect = 'Trusts only', multiple = FALSE,
                              title =paste0("\n\n",Sys.info()["user"], ":\n\nWho are we studying?\n\n"))
if (data.source == 'All'){
  #** data source: all -----
  tk_messageBox(type = c('okcancel'), "data_full does not have for regression:\n\nfemale\nmedical/dental", caption = "Warning")
  sts_has <- read_csv("data_full.csv")
}else{
  #** data source: trust only -----
  sts_has <- read_csv("data_trusts.csv")
  names(sts_has)[names(sts_has) == "religion"] <- "Religion"
  names(sts_has)[names(sts_has) == "trust"] <- "Trust"
  names(sts_has)[names(sts_has) == "year"] <- "Year"
  names(sts_has)[names(sts_has) == "stress.stress"] <- "stress"
  names(sts_has)[names(sts_has) == "stress.stress.base"] <- "stress_base"
  names(sts_has)[names(sts_has) == "physical.colleague"] <- "Physical_Colleagues"
  names(sts_has)[names(sts_has) == "physical.colleague.base"] <- "Physical_Colleagues_base"
  names(sts_has)[names(sts_has) == "emotional.colleague"] <- "Emotional_Colleagues"
  names(sts_has)[names(sts_has) == "emotional.colleague.base"] <- "Emotional_Colleagues_base"
  names(sts_has)[names(sts_has) == "physical.manager"] <- "Physical_Manager"
  names(sts_has)[names(sts_has) == "physical.manager.base"] <- "Physical_Manager_base"
  names(sts_has)[names(sts_has) == "emotional.manager"] <- "Emotional_Manager"
  names(sts_has)[names(sts_has) == "emotional.manager.base"] <- "Emotional_Manager_base"
  names(sts_has)[names(sts_has) == "physical.public"] <- "Physical_Public"
  names(sts_has)[names(sts_has) == "physical.public.base"] <- "Physical_Public_base"
  names(sts_has)[names(sts_has) == "emotional.public"] <- "Emotional_Public"
  names(sts_has)[names(sts_has) == "emotional.public.base"] <- "Emotional_Public_base"
  vars_to_keep <- c("Religion", "Year", "stress", "stress_base", 
                    "Physical_Colleagues", "Physical_Colleagues_base", 
                    "Emotional_Colleagues", "Emotional_Colleagues_base", 
                    "Physical_Manager", "Physical_Manager_base", 
                    "Emotional_Manager", "Emotional_Manager_base", 
                    "Physical_Public", "Physical_Public_base", 
                    "Emotional_Public", "Emotional_Public_base")
  sts_has <- sts_has[, vars_to_keep]
}
sts_has$Religion[sts_has$Religion == "Not to say"] <- "withheld"

sts_has$religion1 <- sts_has$Religion 
sts_has$religion1[which(sts_has$Religion %in% c("Hindu", "Buddhist", "Sikh"))] <- "Dharmic"
sts_has$religion1[which(sts_has$Religion %in% c("Christian", "Jewish", "Muslim"))] <- "Abraham"

sts_has$religion2 <- sts_has$religion1 
sts_has$religion2[which(sts_has$religion2 %in% c("Dharmic", "Abraham", "Other"))] <- "Has Religion"

sts_has$religion1 <- factor(sts_has$religion1,
                           levels = c("No", "Dharmic", "Abraham", "Other", "withheld"))

sts_has$religion2 <- factor(sts_has$religion2,
                           levels = c("No", "Has Religion", "withheld"))

names(sts_has)
sapply(sts_has, class)

# Tables -----
# Calculate the denominators for each of these columns
base_columns <- grep("base", colnames(sts_has), value = TRUE)
(sums <- colSums(sts_has[, base_columns]))
# Summary rate -----
respondents <- sum(sts_has$stress_base)
Stress_overall_rate <- sum(sts_has$stress*sts_has$stress_base/respondents)
cat('Overall rate of of workstress across ', data.source, ' is: ', Stress_overall_rate)

# Summarize to get total numerator and denominator for each religion2
pooled_rates <- sts_has %>%
  group_by(religion2) %>%
  summarise(
    total_stress = sum(stress * stress_base, na.rm = TRUE),  # total numerator
    total_base = sum(stress_base, na.rm = TRUE)  # total denominator
  ) %>%
  mutate(
    pooled_stress_rate = total_stress / total_base  # calculate the pooled rate
  )

# Print the result
print(pooled_rates)

# FIGURES with ggplot -----
out <- NULL
smry <- lapply(c("Christian", "Jewish",
                 "Muslim", "Buddhist", "Hindu",
                 "Sikh", "Other", "No",
                 "Withheld"), # missing Not to say (rather than not to say) for trusts database
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

sum(out$base_stress)

#* Rename religions to friendly names -----
out$religion <- factor(out$religion, 
                       levels = c("Not to say", "Other",
                                  "Buddhist","Jewish", "No", "Christian",
                                  "Sikh", "Muslim", "Hindu"),
                       labels = c("Withheld", "Other",
                                  "Buddhist", "Jewish", "No Religion", "Christian",
                                  "Sikh", "Muslim", "Hindu"))
out$type <- NA
out$type1 <- NA
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
    out$type[i] <- "No Religion"
    out$type1[i] <- "No Religion"
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
out$type <- factor(out$type, levels = c("Abrahamic", "Dharmic", "Other", "Withheld", "No Religion"))

#* ggplot -----
#** Figure 1. Bar plot -----
ggplot(data = out, aes(x = religion, y = stress, fill = type)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.75) +
  theme(text = element_text(size = 12),
        legend.position = "bottom",
        legend.title = element_blank()) +
  xlab("Religion") +
  ylab("Stress Rate") +
  scale_fill_manual(values = custom.col) +
  geom_text(aes(label = round(stress, 2)), vjust = -0.2)

ggsave(paste0("figure 1 - ", data.source, ' (', respondents, ') - ', current.date(),".pdf"), width = 10, height = 6)

#** Figure 2. Regression plot -----
ggplot(data = out, aes(x = abuse, y = stress, 
                       label = religion, color = type)) +
  geom_smooth(method=lm, color="black", se = TRUE) +
  geom_point() +
  geom_label_repel(show.legend = F) +
  ylab("Stress Rate") +
  xlab("Rate of Emotional Harassment from Public") +
  theme(text = element_text(size = 12),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA)) +
  scale_color_manual(values = custom.col) +
  scale_fill_manual(values = custom.col) +  # Set fill colors
  guides(color = guide_legend(override.aes = list(fill = custom.col, shape = 15, size = 5)))


ggsave(paste0("figure 2 - ", data.source, ' (', respondents, ') - ', current.date(),".pdf"), width = 10, height = 6)

