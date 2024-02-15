# Load the data ============================================================
df <- survival::rotterdam
head(df)
colnames(df)


# Key columns for survival analysis
# 1. Censoring status: 1 = event happened, 0 = censored (or TRUE and FALSE)
unique(df$death)
df <- df %>% mutate(status = death)
head(df)
table(df$status)


# 2. Time-to-event (we can use either rtime or dtime)
df$dtime %>% head()
df <- df %>% mutate(dtime_yrs = dtime/365.25)
head(df)

# # If you need to calculate time-to-event from dates
df2 <- data.frame(surgery_date = sample(seq(as.Date('1999/01/01'), as.Date('2000/01/01'), by = 'day'), 12),
                  day_of_last_followup = sample(seq(as.Date('2000/01/01'), as.Date('2020/01/01'), by = 'day'), 12))

head(df2)
class(df2$surgery_date)
df2 <- df2 %>% mutate(os_yrs = as.numeric(as.duration(day_of_last_followup - surgery_date), 'years'),
                      os_days = as.numeric(as.duration(day_of_last_followup - surgery_date), 'days'))
head(df2)

glimpse(df)
# Create a survival object
surv_obj <- Surv(time = df$dtime_yrs, event = df$status)
head(surv_obj)

surv_obj_fit1 <- Surv(time = db2$t_seg, event = db2$obito)


typeof(df$dtime_yrs)
typeof(df$status)
# Create survival curve
s1 <- survfit(surv_obj ~ 1, data = df)
summary(s1)

typeof(s1)
s1

surv_obj_kidney <- Surv(time = db$t_seg, event = db$obito)
head(surv_obj_kidney)



typeof(db$t_seg)
typeof(db$obito)

fit1 <- survfit(surv_obj_fit1 ~ 1, data = db)

summary(fit1)

typeof(fit1)



# Kaplan-Meier plots ======================================================
## Plot -------------------------
km_curve <- ggsurvfit(fit1, linewidth = 1) +
  labs(x = 'Years', y = 'Overall survival') +
  add_confidence_interval() +
  add_risktable() +
  scale_ggsurvfit() + 
  biostatsquid_theme +
  coord_cartesian(xlim = c(0, 8))
