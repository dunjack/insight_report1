#####
library(scales)
library(kableExtra)
# https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html#Table_Styles
#####

# meh_forecast 
meh_forecast <- read.csv("/Users/Dunistrator/Desktop/Dropbox/dj_desktop/Research/x.2021_x_Insight/data/meh_timeseries.csv") %>%
  mutate(
    dat_year = str_sub(appointment_wk, start = 1, end = 4),
    dat_week = str_sub(appointment_wk, start = 5, end = 6)) %>%
  filter(dat_week >=12
         & dat_week <=31
         & dat_year =="2020") %>%
  arrange(dat_week) %>%
  mutate(
    predict = paste(signif(predicted, digits = 2)),
    ci = paste("(",signif(lower95, digits = 2), " - " ,signif(upper95, digits = 2),")", sep ="")) %>%
  select(dat_week, predict, ci)

# uhb_forecast 
uhb_forecast <- read.csv("/Users/Dunistrator/Desktop/Dropbox/dj_desktop/Research/x.2021_x_Insight/data/uhb_timeseries.csv") %>%
  mutate(
    dat_year = str_sub(appointment_wk, start = 1, end = 4),
    dat_week = str_sub(appointment_wk, start = 5, end = 6)) %>%
  filter(dat_week >=12
         & dat_week <=31
         & dat_year =="2020") %>%
  arrange(dat_week) %>%
  mutate(
    predict = paste(signif(predicted, digits = 2)),
    ci = paste("(",signif(lower95, digits = 2), " - " ,signif(upper95, digits = 2),")", sep ="")) %>%
  select(dat_week, predict, ci)



# Summary
sum <- df %>%
  mutate(
    dat_year = str_sub(dat_app, start = 1, end = 4),
    dat_week = str_sub(dat_app, start = 5, end = 6))  %>%
  group_by(cohort, dat_year, dat_week) %>%
  count(dat_app) %>%
  ungroup() %>%
  filter(dat_week >=12
         & dat_week <=31
         & (dat_year == "2019"| dat_year =="2018")) %>%
  select(-dat_app) %>%
  spread(cohort, n) %>%
  replace(is.na(.), 0) 




# Tables
table_meh <- sum %>% 
  filter(dat_year == "2018") %>%
  select(-dat_year, -UHB) %>%
  rename("2018" = MEH) %>%
  cbind(
    sum %>% 
      filter(dat_year == "2019") %>%
      select(-dat_year, -dat_week, -UHB) %>%
      rename("2019" = MEH)  ) %>%
  cbind(
    meh_forecast %>% select(-dat_week) ) %>%  # Replace NA with 0
  rename(Week = dat_week,
         "2020" = predict,
         "95% CI" = ci) %>%  #Col rename
  kbl() %>%
  kable_styling() %>%
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  add_header_above(c(" " = 3, "Forecasted" = 2)) %>%
  kable_paper() %>%
  save_kable(file = "Table2_meh.html", self_contained = T)

table_uhb <- sum %>% 
  filter(dat_year == "2018") %>%
  select(-dat_year, -MEH) %>%
  rename("2018" = UHB) %>%
  cbind(
    sum %>% 
      filter(dat_year == "2019") %>%
      select(-dat_year, -dat_week, -MEH) %>%
      rename("2019" = UHB)  ) %>%
  cbind(
    uhb_forecast %>% select(-dat_week) ) %>%  # Replace NA with 0
  rename(Week = dat_week,
         "2020" = predict,
         "95% CI" = ci) %>%  #Col rename
  kbl() %>%
  kable_styling() %>%
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  add_header_above(c(" " = 3, "Forecasted" = 2)) %>%
  kable_paper() %>%
  save_kable(file = "Table2_uhb.html", self_contained = T)


  


  


#MEH forecast by week
meh_ts_share<-read.csv("/Users/Dunistrator/Desktop/Dropbox/dj_desktop/Research/x.2021_x_Insight/data/meh_timeseries.csv") %>%
  mutate(
    
    predicted = case_when(
      is.na(meh_ts_share$predicted) ~ as.numeric(n),
      T ~ meh_ts_share$predicted),
    date = paste(as.character(appointment_wk), "1", sep="")) %>%
  mutate(date = as.Date(date,"%Y%U%u"),
         cohort = "MEH")

uhb_ts_share<-read.csv("/Users/Dunistrator/Desktop/Dropbox/dj_desktop/Research/x.2021_x_Insight/data/uhb_timeseries.csv") %>%
  mutate(
    predicted = case_when(
      is.na(uhb_ts_share$predicted) ~ as.numeric(n),
      T ~ uhb_ts_share$predicted),
    date = paste(as.character(appointment_wk), "1", sep="")) %>%
  mutate(date = as.Date(date,"%Y%U%u"),
         cohort = "UHB")

d <- meh_ts_share %>% rbind(uhb_ts_share)

d1 <- d %>% filter(upper95 > 0)

p <- ggplot(data=d, aes(x=date, y=predicted, color = cohort)) + 
  geom_line() + 
  geom_ribbon(data = d1, aes(ymin=lower95, ymax=upper95), linetype=2, alpha=0.1) + 
  theme_bw() + 
  scale_x_date(date_labels = "%Y-%W", date_breaks = "8 week", limits = as.Date(c("2018-01-04","2020-12-31"))) + 
  labs(
    color= "Cohort",
    x = "Weeks",
    y = "Number of nAMD patients initiating anti-VEGF") + 
  scale_color_manual(values = c("#1380A1","#588300")) +
  facet_wrap(~cohort, nrow =2)

ggsave(p, file = "Figure_2.png", width = 12, height = 10)




plot(meh_ts_share$cont,meh_ts_share$n, type='l', col='black',xlab = "Week", ylab = "Patient count", ylim=c(0, 45),xlim=c(0,156), xaxt='n')
polygon(c(rev(meh_ts_share$cont), meh_ts_share$cont), c(rev(meh_ts_share$lower95), meh_ts_share$upper95), col = 'grey95' , border = NA)
polygon(c(rev(meh_ts_share$cont), meh_ts_share$cont), c(rev(meh_ts_share$lower80), meh_ts_share$upper80), col = 'grey80' , border = NA)
lines(meh_ts_share$cont, meh_ts_share$predicted, col='blue', lty=2)
lines(factor(meh_ts_share$cont), meh_ts_share$n, col='black')
axis(1, at=c(0,26,52,78,104,130,156), labels=c("2018.0","2018.5","2019.0","2019.5","2020.0","2020.5","2021.0"))

#UHB forecast by week
uhb_ts_share<-read.csv("/Users/Dunistrator/Desktop/Dropbox/dj_desktop/Research/x.2021_x_Insight/data/uhb_timeseries.csv")
plot(uhb_ts_share$cont,uhb_ts_share$n, type='l', col='black',xlab = "Week", ylab = "Patient count", ylim=c(0, 10),xlim=c(0,156), xaxt='n')
polygon(c(rev(uhb_ts_share$cont), uhb_ts_share$cont), c(rev(uhb_ts_share$lower95), uhb_ts_share$upper95), col = 'grey95' , border = NA)
polygon(c(rev(uhb_ts_share$cont), uhb_ts_share$cont), c(rev(uhb_ts_share$lower80), uhb_ts_share$upper80), col = 'grey80' , border = NA)
lines(uhb_ts_share$cont, uhb_ts_share$predicted, col='blue', lty=2)
lines(factor(uhb_ts_share$cont), uhb_ts_share$n, col='black')
axis(1, at=c(0,26,52,78,104,130,156), labels=c("2018.0","2018.5","2019.0","2019.5","2020.0","2020.5","2021.0"))
