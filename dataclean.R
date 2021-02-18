# Packages
library(dplyr)
library(tidyr)
library(table1)
library(data.table)
library(lubridate)
library(stringr)
library(scales)
library(fuzzyjoin)
library(stringr)
library(ggplot2)
library(eye)


#############
# dataframe
#############
meh <- as.data.frame(read.csv("/Users/Dunistrator/Desktop/Dropbox/dj_desktop/Research/x.2021_x_Insight/data/meh_main_share.csv", header = TRUE, stringsAsFactors = FALSE)) %>%
  mutate(
    patkey = anonpatkey,
    dat_app = appointment_wk,
    gender = case_when(
      patsex == "F" ~ "Female",
      patsex == "M" ~ "Male"),
    ethnicity = race_3cat,
    age = age_cat6,
    smoking_status = case_when(
      issmoker == TRUE ~ "Yes",
      issmoker == FALSE ~ "No or Unknown"),
    va = ETDRS_converted,
    va_70 = case_when(
      va >=70 ~ "yes",
      T ~ "no"),
    va_3.60 = legal_blindness,
    va_25 = case_when(
      va <= 25 ~ "yes", 
      T ~ "no"),
    cohort = "meh") %>%
  select(15:25) %>%
  filter(!is.na(va))

uhb <- as.data.frame(read.csv("/Users/Dunistrator/Desktop/Dropbox/dj_desktop/Research/x.2021_x_Insight/data/uhb_main_share.csv", header = TRUE, stringsAsFactors = FALSE))  %>%
  mutate(
    patkey = PatientID_Hashed,
    dat_app = appointment_wk,
    gender = Sex,
    ethnicity = Ethnicity,
    age = age_cat6,
    smoking_status = case_when(
      Smoking.Status == "Y" ~"Yes",
      T ~ "No or Unknown"),
    va = ETDRS_cb,
    va_70 = case_when(
      va >=70 ~ "yes",
      T ~ "no"),
    va_3.60 = legal_blindness,
    va_25 = case_when(
      va <= 25 ~ "yes", 
      T ~ "no"),
    cohort = "uhb") %>%
  select(14:24) %>%
  filter(!is.na(va))

df <- rbind(uhb, meh)


  
################
# Tables
###############
# DEMOGRAPHY

### 
df$cohort    <- factor(df$cohort, levels=c("meh", "uhb"), labels=c("MEH", "UHB"))

df$ethnicity    <- factor(df$ethnicity, levels=c("White", "Other", "Unknown"), labels=c("White", "Not white", "Not reported"))
df$smoking_status    <- factor(df$smoking_status, levels=c("Yes", "No or Unknown"))

label(df$age)    <- "Age"
label(df$ethnicity)      <- "Ethnicity"
label(df$gender)    <- "Gender"
label(df$smoking_status)    <- "Smoking status"


my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c("",
                                                           "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}

rndr <- function(x, name, ...) {
  if (length(x) == 0) {
    y <- df[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)) {
      p <- t.test(y ~ df$cohort)$p.value
    } else {
      p <- chisq.test(table(y, droplevels(df$cohort)))$p.value
    }
    s[2] <- sub("<", "&lt;", format.pval(p, digits=3))
    s
  } else {
    render.default(x=x, name=name, ...)
  }
}

rndr.strat <- function(label, n, ...) {
  ifelse(n==0, label, render.strat.default(label, n, ...))
}


table1(~ gender + age + ethnicity + smoking_status |cohort ,
       data=df, droplevels=F,render=rndr, render.strat=rndr.strat, overall="Overall")



#######
# Clinical features
#####
# Table
df$cohort    <- factor(df$cohort, levels=c("meh", "uhb"), labels=c("MEH", "UHB"))
df$va_70    <- factor(df$va_70, levels=c("yes", "no"), labels=c("Yes", "No"))
df$va_3.60    <- factor(df$va_3.60, levels=c("yes", "no"), labels=c("Yes", "No"))
df$va_25    <- factor(df$va_25, levels=c("yes", "no"), labels=c("Yes", "No"))

label(df$va)    <- "Visual acuity (VA)"
label(df$va_70)    <- "VA \u2265 70"
label(df$va_3.60)    <- "VA \u2264 3/60 "
label(df$va_25)    <- "VA \u2264 25"

units(df$va)    <- "ETDRS letters"

table1(~ va + va_70 + va_3.60  | cohort ,
       data=df, droplevels=F ,render=rndr, render.strat=rndr.strat, overall="Overall")


# Distribution
df_distr_va <- 
  df %>%
  # mutate(cohort = "overall") %>%
  # rbind(df) %>%
  ggplot(aes(x = factor(cohort, 
                        levels=c("overall", "MEH", "UHB"), 
                        labels=c("Overall","MEH", "UHB")), 
             y= va, 
             group = factor(cohort, 
                            levels=c("overall", "MEH", "UHB"), 
                            labels=c("Overall","MEH", "UHB")))) +
  stat_boxplot(geom = "errorbar", width = 0.05, position = position_nudge(x = -0.2)) + 
  geom_boxplot(width= 0.1, fill = "darkgrey", position = position_nudge(x = -0.2), outlier.size = 0.3) +
  geom_dotplot(binaxis = "y", binwidth = 1 , dotsize = 0.2) + 
  theme_bw() +
  labs(
    x = "",
    y = "Visual acuity (ETDRS letters)") +
  scale_y_continuous(breaks=seq(0, 110, 10)) + 
  ggtitle("Figure 1. Distribution of visual acuity")

ggsave(df_distr_va, file="Figure_1.png", width = 12, height = 7.5)






