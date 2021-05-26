rm(list = ls())
easypackages::libraries("gsDesign", "ggplot2", "dplyr", "tibber", "survival", "survminer")

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

### since PFS is interval censored, disease assessment frequency affects median PFS estimates
### e.g. true event time is between the two assessments that happen before and after the true event

### Typical method to handle interval censoring data (PFS): use disease assessment time after the interval, aka, upper limit

### How the typical method affects the estimates of median event time?

tmed.true = 7.5
cmed.true = 4.5
dmed.true = 15

cut = 15 # ignore enrollment time variation
assess = 3 # disease assessment every 3 months
schedule = (1:10) * assess

zzz = replicate(1e3, { # simulate 1e3 trials
  df1 = tte_data_gen(N = 150, accrual = 12, tmed.true = tmed.true, cmed.true = cmed.true, dmed = dmed.true)
  df1 = df1 %>% filter(`Arm` == "Treatment")
  
  df1 = df1 %>% mutate(PFS1 = ifelse(`Time from Rand. to Event/Censor` > cut, cut, `Time from Rand. to Event/Censor`))
  df1 = df1 %>% mutate(EFL1 = ifelse(`Time from Rand. to Event/Censor` > cut, 0, `Event Flag`))
  
  df1 = df1 %>% rowwise %>% mutate(PFSX = schedule[schedule - PFS1 >= 0][1])
  df1 = df1 %>% mutate(PFS2 = ifelse(`PFSX` > cut, max(schedule[schedule - cut <= 0]), PFSX))
  df1 = df1 %>% mutate(EFL2 = ifelse(`PFSX` > cut, 0, EFL1))
  
  fit1 = survfit(Surv(`PFS1`, `EFL1`) ~ Arm, data = df1) %>% surv_median  # drop-out
  fit2 = survfit(Surv(`PFS2`, `EFL2`) ~ Arm, data = df1) %>% surv_median  # drop-out

  list(data = df1, stat = c("Event 1" = fit1$median, "Event 2" = fit2$median))
})

data = zzz[1, ]
stat = do.call(rbind, zzz[2, ]) %>% as.data.frame
apply(stat, 2, summary)

stat$bias = stat$`Event 2` - stat$`Event 1`
summary(stat$bias)

which.min(stat$bias)
df1 = data[[which.min(stat$bias)]]

### How the typical method affects the estimates of median event time differences, placebo and treatment?
### And HR?

tmed.true = 7.5
cmed.true = 4.5
dmed.true = 15

cut = 15 # ignore enrollment time variation

assess = 3
assess = 5 

schedule = (1:10) * assess

zzz = replicate(1e3, { # simulate 1e3 trials
  df1 = tte_data_gen(N = 150, accrual = 12, tmed.true = tmed.true, cmed.true = cmed.true, dmed = dmed.true)
  
  df1 = df1 %>% mutate(PFS1 = ifelse(`Time from Rand. to Event/Censor` > cut, cut, `Time from Rand. to Event/Censor`))
  df1 = df1 %>% mutate(EFL1 = ifelse(`Time from Rand. to Event/Censor` > cut, 0, `Event Flag`))
  
  df1 = df1 %>% rowwise %>% mutate(PFSX = schedule[schedule - PFS1 >= 0][1])
  df1 = df1 %>% mutate(PFS2 = ifelse(`PFSX` > cut, max(schedule[schedule - cut <= 0]), PFSX))
  df1 = df1 %>% mutate(EFL2 = ifelse(`PFSX` > cut, 0, EFL1))
  
  fit1 = survfit(Surv(`PFS1`, `EFL1`) ~ Arm, data = df1) %>% surv_median # drop-out
  fit2 = survfit(Surv(`PFS2`, `EFL2`) ~ Arm, data = df1) %>% surv_median # drop-out
  
  coxreg1 = coxph(Surv(PFS1, EFL1) ~ Arm, data = df1, method = "efron") %>% summary
  coxreg2 = coxph(Surv(PFS2, EFL2) ~ Arm, data = df1, method = "efron") %>% summary
  
  cox_HR1 = round(coxreg1$conf.int, digits = 2)
  cox_HR2 = round(coxreg2$conf.int, digits = 2)
  
  list(data = df1, fit1 = fit1, fit2 = fit2, HR = rbind(cox_HR1, cox_HR2))
})

data = zzz[1, ]

fit1 = zzz[2, ]
fit2 = zzz[3, ]

hr = zzz[4, ]

stat1 = sapply(fit1, function(x) x$median[2] - x$median[1])
stat2 = sapply(fit2, function(x) x$median[2] - x$median[1])

table(stat1 <= 0)
summary(stat1)
table(stat2 <= 0)
summary(stat2)

p1 = plot(stat2, stat1)
p1 + abline(h = c(0, 5, 10))

stat1[stat2 == 0] %>% summary
stat1[stat2 == 5] %>% summary
stat1[stat2 == 10] %>% summary

which.max(stat1 - stat2)
df1 = data[[which.max(stat1 - stat2)]]

fit1 = survfit(Surv(`PFS1`, `EFL1`) ~ Arm, data = df1) %>% surv_median # drop-out
fit2 = survfit(Surv(`PFS2`, `EFL2`) ~ Arm, data = df1) %>% surv_median # drop-out
  
coxreg1 = coxph(Surv(PFS1, EFL1) ~ Arm, data = df1, method = "efron") %>% summary
coxreg2 = coxph(Surv(PFS2, EFL2) ~ Arm, data = df1, method = "efron") %>% summary

ggsurvplot(survfit(Surv(PFS1, EFL1) ~ Arm, data = df1), risk.table = T, surv.median.line = "hv", xlim = c(0, 25), break.time.by = 5)
ggsurvplot(survfit(Surv(PFS2, EFL2) ~ Arm, data = df1), risk.table = T, surv.median.line = "hv", xlim = c(0, 25), break.time.by = 5)

### rule of thumb:disease assessment interval should not be longer than the median event differences
### otherwise, estimates of the median event differences will very much be biased (in either way)
### interestingly, HR estimates was not biased
