library(readr)
library(randomcoloR)
library(foreach)
library(ggplot2)
library(ggrepel)

n <- 8
set.seed(5)
palette <- distinctColorPalette(n)
custom.col <- palette[1:5]
custom.col2 <- palette[6:8]

sts_has <- read_csv("data_full.csv")

# Table 1 -----
base_columns <- grep("base", colnames(sts_has), value = TRUE)

# Calculate the sum for each of these columns
sums <- colSums(sts_has[, base_columns])

# Table 2 Bob's test----------------------
sum(round(sts_has$emotional_public_base,0))
function_emotional_public <- function (religion){
  temp <- sts_has[sts_has$religion == religion,]
  emotional_public_victims <- sum(round(temp$emotional_public*temp$emotional_public_base,0))
  cat('Stress rate: ', emotional_public_victims/sum(temp$emotional_public_base),'; base_stress: ', sum(temp$emotional_public_base))
}
function_emotional_public ('Christian')
function_emotional_public ('Jewish')
function_emotional_public ('Muslim')
function_emotional_public ('Buddhist')
function_emotional_public ('Hindu')
function_emotional_public ('Sikh')
function_emotional_public ('Other')
function_emotional_public ('Withheld')
function_emotional_public ('No religion')
# Zehan's function for GGplot ----------------------
smry <- lapply(c("Christian", "Jewish",
                 "Muslim", "Buddhist", "Hindu",
                 "Sikh", "Other", "No",
                 "Not to say"), 
               function(i) {
                 tmp_rlg <- sts_has[sts_has$religion == i, ]
                 colnames(sts_has)[3]
                 base_sum <- colSums(tmp_rlg[, -1])[seq(3, 15, 2)]
                 rts <- tmp_rlg[, seq(3, 15, 2)]
                 base <- tmp_rlg[, seq(4, 16, 2)]
                 tmp <- colSums(rts * base)/ base_sum
                 data.frame(religion = i, stress = tmp[1], abuse = tmp[7],
                            base_stress = base_sum[1])
               })

out <- as.data.frame(foreach(t = smry, .combine = "rbind") %do% {t})
sum(out$base_stress)
sum(out$base_stress) - sum(round(sts_has$emotional_public_base,0))

out$religion <- factor(out$religion, 
                       levels = c("Not to say", "Other",
                                  "Buddhist","Jewish", "No", "Christian",
                                  "Sikh", "Muslim", "Hindu"),
                       labels = c("Withheld", "Other",
                                  "Buddhist", "Jewish", "No religion", "Christian",
                                  "Sikh", "Muslim", "Hindu"))
out$type <- NA
out$type1 <- NA
for (i in seq_len(nrow(out))) {
  if (out$religion[i] %in% c("Christian", "Jewish", "Muslim")) {
    out$type[i] <- "Abraham"
    out$type1[i] <- "religion"
  } else if (out$religion[i] %in% c("Buddhist", "Hindu", "Sikh")) {
    out$type[i] <- "Dharmic"
    out$type1[i] <- "religion"
  } else if (out$religion[i] == "Other") {
    out$type[i] <- "Other"
    out$type1[i] <- "religion"
  } else if (out$religion[i] == "No religion"){
    out$type[i] <- "No religion"
    out$type1[i] <- "No religion"
  } else if (out$religion[i] == "Withheld"){
    out$type[i] <- "Withheld"
    out$type1[i] <- "Withheld"
  }
} 

# Plots -----
#* Bar graph ------
ggplot(data = out, aes(x = religion, y = stress, fill = type)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.75) +
  theme(text = element_text(size = 12),
        legend.position = "bottom",
        legend.title = element_blank()) +
  xlab("religion") +
  ylab("Stress Rate") +
  scale_fill_manual(values = custom.col) +
  geom_text(aes(label = round(stress, 2)), vjust = -0.2)
ggsave("figure1.pdf", width = 10, height = 6)

#* Line plot ------
ggplot(data = out, aes(x = abuse, y = stress, 
                       label = religion, color = type)) +
  geom_smooth(method=lm, color="black", se = TRUE) +
  geom_point() +
  geom_label_repel() +
  ylab("Stress Rate") +
  xlab("Rate of Emotional Harassment from Public") +
  theme(text = element_text(size = 8),
        legend.position = "bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = custom.col)
ggsave("plot1.pdf", width = 10, height = 6)
