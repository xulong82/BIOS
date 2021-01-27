rm(list = ls())
easypackages::libraries("gsDesign", "ggplot2", "dplyr", "tibber", "survival", "survminer")

### real-world TTE data is a mixture of event and drop-out 
### can we estimate drop-out rate based on observed data by the product-limit estimates?

tte_data_gen = function(N, accrual, tmed.true, cmed.true, dmed) { # Function: generate TTE data
  df <- tibble(
    Arm = c(rep("Treatment", N/2), rep("Control", N/2)),
    `Time to Rand.` = runif(n = N, 0, accrual),
    `Time from Rand. to Event` = c(rexp(n = N/2, log(2)/tmed.true), rexp(n = N/2, log(2)/cmed.true)),
    `Time from Rand. to Censor` = c(rexp(n = N, log(2) / dmed)),
    `Time from Rand. to Event/Censor` = pmin(`Time from Rand. to Event`, `Time from Rand. to Censor`),
    `Time to Event/Censor` = `Time to Rand.` + `Time from Rand. to Event/Censor`,
    `Event Flag` = ifelse(`Time from Rand. to Event` <= `Time from Rand. to Censor`, 1, 0)
  ); df
}

### suppose the follow-up time was long enough (data-cut date was after the last Event/Censor)

zzz = replicate(1e3, { # simulate 1e3 trials
  df1 = tte_data_gen(N = 1e3, accrual = 6, tmed.true = 10, cmed.true = 5, dmed = 20)
  df1 = df1 %>% filter(`Arm` == "Treatment")
  
  fit1 = survfit(Surv(`Time from Rand. to Event/Censor`, `Event Flag`) ~ Arm, data = df1) %>% surv_median  # event
  fit2 = survfit(Surv(`Time from Rand. to Event/Censor`, 1 - `Event Flag`) ~ Arm, data = df1) %>% surv_median  # drop-out

  c("Event" = fit1$median, "Dropout" = fit2$median)
})

apply(zzz, 1, summary)

### obviously yes

### what about the data-cut date was before the last Event/Censor (later-enrolled patients has shorter follow-up time on average)

cut = 15 # data-cut is 15 months follow-up (we can ignore enrollment time since it is independent from the time-to-event time)

zzz = replicate(1e3, { # simulate 1e3 trials
  df1 = tte_data_gen(N = 1e3, accrual = 6, tmed.true = 10, cmed.true = 5, dmed = 20)
  df1 = df1 %>% filter(`Arm` == "Treatment")
  
  df1 = df1 %>% mutate(PFS2 = ifelse(`Time from Rand. to Event/Censor` > cut, cut, `Time from Rand. to Event/Censor`))
  df1 = df1 %>% mutate(EVFL = ifelse(`Time from Rand. to Event/Censor` > cut, 0, `Event Flag`))
  
  fit1 = survfit(Surv(PFS2, EVFL) ~ Arm, data = df1) %>% surv_median # event
  fit2 = survfit(Surv(PFS2, 1 - EVFL) ~ Arm, data = df1) %>% surv_median # drop-out

  c("Event" = fit1$median, "Dropout" = fit2$median)
})

apply(zzz, 1, summary)

### median event estimate was accurate (since follow-up time longer than the median event time) 
### median drop-out estimate was wrong (since follow-up time did not reach the median dropout time) 

### in prior simulation, subjects reached data-cut w/o actual event/dropout was set as event in the drop-out rate estimation
### what about setting these cases as censor?

df1 = tte_data_gen(N = 1e3, accrual = 6, tmed.true = 10, cmed.true = 5, dmed = 20) %>% filter(`Arm` == "Treatment")

fit1 = survfit(Surv(`Time from Rand. to Event/Censor`, `Event Flag`) ~ Arm, data = df1)
ggsurvplot(fit1, risk.table = T, surv.median.line = "hv", xlim = c(0, 25), break.time.by = 5)

cut = 10 # try different cut time

df1 = df1 %>% mutate(PFS2 = ifelse(`Time from Rand. to Event/Censor` > cut, cut, `Time from Rand. to Event/Censor`))
df1 = df1 %>% mutate(EVFL = ifelse(`Time from Rand. to Event/Censor` > cut, 0, `Event Flag`))

fit2 = survfit(Surv(PFS2, EVFL) ~ Arm, data = df1)
ggsurvplot(fit2, risk.table = T, surv.median.line = "hv", xlim = c(0, 25), break.time.by = 5)

### KM estimates are the same before the data-cut time
### with that, median event time estimates do not change as long as the data-cut time passed the median event time 

fit1 = survfit(Surv(`Time from Rand. to Event/Censor`, 1 - `Event Flag`) ~ Arm, data = df1)
ggsurvplot(fit1, risk.table = T, surv.median.line = "hv", xlim = c(0, 25), break.time.by = 5)

fit2 = survfit(Surv(PFS2, 1 - EVFL) ~ Arm, data = df1)
ggsurvplot(fit2, risk.table = T, surv.median.line = "hv", xlim = c(0, 25), break.time.by = 5)

### this is the same as to the dropout rate estimates
### with that, subject reached data-cut w/o actual event/dropout should be set as censor

df1 = df1 %>% mutate(EVFL2 = 1 - EVFL) %>% mutate(EVFL2 = ifelse(`Time from Rand. to Event/Censor` > cut, 0, EVFL2))

fit3 = survfit(Surv(PFS2, EVFL2) ~ Arm, data = df1)
ggsurvplot(fit3, risk.table = T, surv.median.line = "hv", xlim = c(0, 25), break.time.by = 5)

### as the result, the actual median dropout time will be estimated NA (not reached)

### data becomes messier when the enrollment time was taken into consideration
### this way, subject reached data-cut w/o actual event/dropout will be censored at different TTE (from randomization) time points
### as the result, data-cut date should also consider the enrollment pattern 

fit1 = survfit(Surv(`Time from Rand. to Event/Censor`, `Event Flag`) ~ Arm, data = df1)
ggsurvplot(fit1, risk.table = T, surv.median.line = "hv", xlim = c(0, 25), break.time.by = 5)

fit1 %>% surv_median

cut = 10 # try different cut time

df1 = df1 %>% mutate(PFS2 = ifelse(`Time from Rand. to Event/Censor` > cut, cut, `Time from Rand. to Event/Censor`))
df1 = df1 %>% mutate(EVFL = ifelse(`Time from Rand. to Event/Censor` > cut, 0, `Event Flag`))

fit2 = survfit(Surv(PFS2, EVFL) ~ Arm, data = df1)
ggsurvplot(fit2, risk.table = T, surv.median.line = "hv", xlim = c(0, 25), break.time.by = 5)

df1 = df1 %>% mutate(PFS3 = ifelse(`Time to Event/Censor` > cut, cut, `Time from Rand. to Event/Censor`))
df1 = df1 %>% mutate(EFL3 = ifelse(`Time to Event/Censor` > cut, 0, `Event Flag`))

fit3 = survfit(Surv(PFS3, EFL3) ~ Arm, data = df1)
ggsurvplot(fit3, risk.table = T, surv.median.line = "hv", xlim = c(0, 25), break.time.by = 5)

cut = 12 # try different cut time

df1 = df1 %>% mutate(PFS4 = ifelse(`Time to Event/Censor` > cut, cut, `Time from Rand. to Event/Censor`))
df1 = df1 %>% mutate(EFL4 = ifelse(`Time to Event/Censor` > cut, 0, `Event Flag`))

fit4 = survfit(Surv(PFS4, EFL4) ~ Arm, data = df1)
ggsurvplot(fit4, risk.table = T, surv.median.line = "hv", xlim = c(0, 25), break.time.by = 5)

### misc facts about TTE data

survfit(Surv(`Time from Rand. to Event`, rep(1, nrow(df1))) ~ Arm, data = df1) %>% surv_median # true median event 
survfit(Surv(`Time from Rand. to Event/Censor`, `Event Flag`) ~ Arm, data = df1) %>% surv_median # estimated median event

survfit(Surv(`Time from Rand. to Censor`, rep(1, nrow(df1))) ~ Arm, data = df1) %>% surv_median # true median dropout event
survfit(Surv(`Time from Rand. to Event/Censor`, 1 - `Event Flag`) ~ Arm, data = df1) %>% surv_median # estimated median dropout
  
df2 = df1 %>% filter(`Event Flag` == 1) # of course, not appropriate to estimate median event time based on observed events
survfit(Surv(`Time from Rand. to Event/Censor`, `Event Flag`) ~ Arm, data = df2) %>% surv_median

### visualize why it is not appropriate

fit1 = survfit(Surv(`Time from Rand. to Event/Censor`, `Event Flag`) ~ Arm, data = df1)
ggsurvplot(fit1, risk.table = T, surv.median.line = "hv", xlim = c(0, 25), break.time.by = 5)
fit2 = survfit(Surv(`Time from Rand. to Event/Censor`, `Event Flag`) ~ Arm, data = df2)
ggsurvplot(fit2, risk.table = T, surv.median.line = "hv", xlim = c(0, 25), break.time.by = 5)
  
########################################
