easypackages::libraries("gsDesign", "ggplot2", "dplyr", "tibber", "survival", "survminer")

## fixed versus group sequential design

## across the true HR spectrum, with power aligned to the fixed design
## what does the average target event numbers look like (ASN) in group sequential design? 

## ?

## What is the theta/delta output in gsdesign package?
## How to relate theta/delta to hazard ratio?
## To decipher the gsdesign::plot function

N = 286
accrual = 6 

tmed.true = 10
cmed.true = 6.5
ceta = log(2) / cmed.true

dmed = 16.3
deta = log(2) / dmed

## the fixed study
xfix = nEvents(hr = 0.65, alpha = 0.025, beta = 0.2, ratio = 1, sided = 1, hr0 = 1)

xgs1 = gsSurv(hr0 = 1, hr = .65, alpha = 0.025, sided = 1, beta = 0.2, ratio = 1, lambdaC = ceta, eta = deta, R = 6, T = 6, minfup = 0, 
              k = 2, timing = 0.4, test.type = 1,  sfu = sfLDOF, sfupar = 0) # stop for efficacy

xgs3 = gsSurv(hr0 = 1, hr = .65, alpha = 0.025, sided = 1, beta = 0.2, ratio = 1, lambdaC = ceta, eta = deta, R = 6, T = 6, minfup = 0, 
              k = 2, timing = 0.4, test.type = 3,  sfu = sfLDOF, sfupar = 0) # stop for efficacy and futility

plot(xgs1, plottype = 1)
plot(xgs3, plottype = 1)

plot(xgs1, plottype = 2)
plot(xgs3, plottype = 2)

plot(xgs1, plottype = 6)
plot(xgs3, plottype = 6)

xgs1p = gsProbability(d = xgs1, theta = xgs1$delta * seq(0, 2, .25))
plot(xgs1p, plottype = 2)

xgs3p = gsProbability(d = xgs3, theta = xgs3$delta * seq(0, 2, .25))
plot(xgs3p, plottype = 2)

plot(xgs1p$theta, xgs1p$en, col = "red")
points(xgs1p$en, col = "blue")

plot(xgs1p, plottype = 6)
zn2hr(xgs1p$theta, xgs1p$n.fix, ratio = 1, hr0 = 1, hr1 = 0.65)

nEvents(hr = 0.72, alpha = 0.025, ratio = 1, sided = 1, hr0 = 1, n = 169)

nEvents(hr = 0.65, alpha = 0.025, ratio = 1, sided = 1, hr0 = 1, n = 211)
nEvents(hr = 0.72, alpha = 0.025, ratio = 1, sided = 1, hr0 = 1, n = 211)


x3 = gsSurv(hr0 = 1, hr = .65, alpha = 0.025, sided = 1, beta = 1 - 0.8787534, ratio = 1, 
            lambdaC = log(2)/6.5, eta = log(2)/16.3,  # test parameter 
            k = 2, timing = 0.4, test.type = 1,  sfu = sfLDOF, sfupar = 0, # boundary
            R = 6, T = 6, minfup = 0) # accrual and dropouts
x3$en

nEvents(hr = 0.65, alpha = 0.025, ratio = 1, sided = 1, hr0 = 1, n = 184)

x1 = gsSurv(hr0 = 1, hr = .65, alpha = 0.025, sided = 1, beta = 1-0.8319119, ratio = 1, 
            lambdaC = log(2)/6.5, eta = log(2)/16.3,  # test parameter 
            k = 2, timing = 0.4, test.type = 1,  sfu = sfLDOF, sfupar = 0, # boundary
            R = 6, T = 6, minfup = 0) # accrual and dropouts
x1$en

nEvents(hr = 0.65, alpha = 0.025, ratio = 1, sided = 1, hr0 = 1, n = 184)
nEvents(hr = 0.72, alpha = 0.025, ratio = 1, sided = 1, hr0 = 1, n = 184)
nEvents(hr = 0.72, alpha = 0.025, ratio = 1, sided = 1, hr0 = 1, n = 207)

## design a group sequential study

x1 = gsSurv(hr0 = 1, hr = .65, alpha = 0.025, sided = 1, beta = 0.2, ratio = 1, 
            lambdaC = log(2)/6.5, eta = log(2)/16.3,  # test parameter 
            k = 2, timing = 0.4, test.type = 1,  sfu = sfLDOF, sfupar = 0, # boundary
            R = 6, T = 6, minfup = 0) # accrual and dropouts

x2 = gsSurv(hr0 = 1, hr = .65, alpha = 0.025, sided = 1, beta = 0.2, ratio = 1, 
            lambdaC = log(2)/6.5, eta = log(2)/16.3,  # test parameter 
            k = 2, timing = 0.4, test.type = 1,  sfu = sfLDPocock, sfupar = 0, # boundary
            R = 6, T = 6, minfup = 0) # accrual and dropouts

x4 = gsSurv(hr0 = 1, hr = .65, alpha = 0.025, sided = 1, beta = 1 - 0.8787534, ratio = 1, 
            lambdaC = log(2)/6.5, eta = log(2)/16.3,  # test parameter 
            k = 2, timing = 0.4, test.type = 1,  sfu = sfLDPocock, sfupar = 0, # boundary
            R = 6, T = 6, minfup = 0) # accrual and dropouts

x4$en

## it appears that with specified power, average sample size of gs-design is lower than the fixed design
## if HR assumption was good, gs-design is superior to SSR

## SSR address a different issue: interim observed HR was lower than expected

## what does the real power looks like with a mis-specified HR in gs-design?

## gs-design has outputs of this (plot type 2)

## to be continued ...

#########################################################

## Toy data: single arm proportion test

nBinomial1Sample(p0 = 0.4, p1 = 0.7, alpha = 0.025, n = 20:30)
nBinomial1Sample(p0 = 0.4, p1 = 0.7, alpha = 0.025, beta = 0.2, n = 20:30)

prop.test(x = 12, n = 22, p = 0.4, alternative = "greater")
prop.test(x = 13, n = 22, p = 0.4, alternative = "greater")
prop.test(x = 14, n = 22, p = 0.4, alternative = "greater")

prop.test(x = 22, n = 40, p = 0.4, alternative = "greater")
prop.test(x = 23, n = 40, p = 0.4, alternative = "greater")
prop.test(x = 24, n = 40, p = 0.4, alternative = "greater")

binom.test(x = 12, n = 22, p = 0.4, alternative = "greater")
binom.test(x = 13, n = 22, p = 0.4, alternative = "greater")
binom.test(x = 14, n = 22, p = 0.4, alternative = "greater")

1 - pbinom(8, 12, 0.7)
1 - pbinom(8, 12, 0.5)

1 - pbinom(17, 30, 0.7)
1 - pbinom(17, 30, 0.5)

1 - pbinom(8, 12, 0.4)
1 - pbinom(17, 30, 0.4)

1 - pbinom(5, 10, 0.4)

xd = 22:50

### real-world TTE data: mixture of event and drop-out 
### can we estimate drop-out rate based on observed data?

### Generate data for 286 patients

N = 286
accrual = 6 
tmed.true = 10
cmed.true = 6.5
dmed = 16.3

xdf = function() {
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

df = xdf()
survfit(Surv(`Time from Rand. to Event/Censor`, `Event Flag`) ~ Arm, data = df)
survfit(Surv(`Time from Rand. to Event/Censor`, `Event Flag`) ~ Arm, data = df[df$`Event Flag` == 1, ])

ggsurvplot(survfit(Surv(`Time to Event/Censor`, `Event Flag`) ~ Arm, data = df), risk.table = T, break.time.by = 5)
ggsurvplot(survfit(Surv(`Time to Event/Censor`, `Event Flag`) ~ Arm, data = df[df$`Event Flag` == 1, ]), risk.table = T)

df1 = df %>% filter(`Arm` == "Control")
survfit(Surv(`Time from Rand. to Event/Censor`, `Event Flag`) ~ 1, data = df1) # events
survfit(Surv(`Time from Rand. to Event/Censor`, 1 - `Event Flag`) ~ 1, data = df1) # drop-out

df2 = df1 %>% filter(`Event Flag` == 1)
median(df2$`Time from Rand. to Event/Censor`)
survfit(Surv(`Time from Rand. to Event/Censor`, `Event Flag`) ~ Arm, data = df2)
ggsurvplot(survfit(Surv(`Time to Event/Censor`, `Event Flag`) ~ Arm, data = df2), risk.table = T, break.time.by = 2)

zzz = survfit(Surv(`Time to Event/Censor`, `Event Flag`) ~ 1, data = df2)
zzz = data.frame(zzz$n, zzz$time, zzz$n.event, zzz$n.risk, zzz$surv)
zzz$time2 = sort(df2$`Time from Rand. to Event/Censor`)

zzz = replicate(1e2, {
  df = xdf()
  df1 = df %>% filter(`Arm` == "Treatment")
  fit1 = survfit(Surv(`Time from Rand. to Event/Censor`, `Event Flag`) ~ 1, data = df1) %>% surv_median # events
  fit2 = survfit(Surv(`Time from Rand. to Event/Censor`, 1 - `Event Flag`) ~ 1, data = df1) %>% surv_median # drop-out
  out = c("Event" = fit1$median, "Dropout" = fit2$median)
  out
})

zzz = as.data.frame(t(zzz))
summary(zzz$Event)
summary(zzz$Dropout)

########################################
  
d1 = 68

df = xdf()

# Create SSR data by excluding patients who hadn't enrolled yet and redefining analysis time and event flag

ssr.time <- sort(df$`Time to Event/Censor`[df$`Event Flag` %in% 1])[d1]
  
df.ssr <- df %>%
  dplyr::filter(`Time to Rand.` <= ssr.time) %>%
  dplyr::mutate(
    `Time from Rand. to Censor (SSR)` = pmin(ssr.time - `Time to Rand.`, `Time from Rand. to Censor`),
    `Time from Rand. to Event/Censor (SSR)` = pmin(`Time from Rand. to Event`, `Time from Rand. to Censor (SSR)`),
    `Event Flag (SSR)` = ifelse(`Time from Rand. to Event` <= (`Time from Rand. to Censor (SSR)` + 1e-10), 1, 0)
  )

# Fit log-rank model for P/Z value and Cox PH for HR

loca <- df.ssr$Arm %in% "Treatment"

str_logrank <- survdiff(Surv(`Time from Rand. to Event/Censor (SSR)`, `Event Flag (SSR)`) ~ Arm, data = df.ssr)
LogRank_P = round(pnorm(-sqrt(str_logrank$chisq))*2, 2)
  
str_coxreg <- coxph(Surv(`Time from Rand. to Event/Censor (SSR)`, `Event Flag (SSR)`) ~ Arm, data = df.ssr) %>% summary
cox_HR = round(str_coxreg$conf.int, digits = 5)

logrank.ssr <- fastlogranktest::logrank_test(
  df.ssr$`Time from Rand. to Event/Censor (SSR)`[loca],
  df.ssr$`Time from Rand. to Event/Censor (SSR)`[! loca],
  df.ssr$`Event Flag (SSR)`[loca],
  df.ssr$`Event Flag (SSR)`[! loca]
)

z.ssr <- logrank.ssr[2]
hr.ssr <- exp(z.ssr * sqrt(4 / d1)) # 
p.ssr <- pnorm(z.ssr) 


### Calculate CP for d* ranging from d to 2d

source("condpow.R")

d = 169
dstar = 200

d2 <- d - d1
d2star <- dstar - d1
z1 = z.ssr
hr = hr.ssr
  
pnorm(qnorm(0.025) * sqrt(dstar/d2star) - sqrt(d1/d2star) * z1 - sqrt(d2star)/2 * log(hr))

pnorm(qnorm(0.025) * sqrt(d/d2) - sqrt(d1/d2) * z1 - sqrt(dstar - d1)/2 * log(hr)) # CHW
    
dmax = ceiling(d * 1.5)
  
dstar.vals <- d: dmax
cps.est <- condpow(z1 = z.ssr, d1 = d1, d = d, dstar = dstar.vals, hr = hr.ssr) * 100
cps.true <- condpow(z1 = z.ssr, d1 = d1, d = d, dstar = dstar.vals, hr = cmed.true/tmed.true) * 100

tmed.assumed = 10
cmed.assumed = 6.5

cps.assumed <- condpow(z1 = z.ssr, d1 = d1, d = d, dstar = dstar.vals, hr = cmed.assumed/tmed.assumed) * 100

hr.tier2 = mean(c(cmed.assumed / tmed.assumed, 1))
cps.tier2 <- condpow(z1 = z.ssr, d1 = d1, d = d, dstar = dstar.vals, hr = hr.tier2) * 100

cps.min <- condpow(z1 = z.ssr, d1 = d1, d = d, dstar = dstar.vals, hr = min(hr.ssr, hr.tier2)) * 100
cps.ave <- condpow(z1 = z.ssr, d1 = d1, d = d, dstar = dstar.vals, hr = mean(c(hr.ssr, cmed.assumed/tmed.assumed))) * 100

cps.null <- condpow(z1 = z.ssr, d1 = d1, d = d, dstar = dstar.vals, hr = 1) * 100

sr_simulation <- function(
  hrcut = Inf,
  pcut = 1,
  hr.cpcheck1 = "Estimated",
  hr.cpcheck2 = hr.cpcheck1,
  hr.dstar = hr.cpcheck2,
  cpcut1 = 80,
  cpcut2 = 30,
  trials = 1000
) {

    # Check if CP(d) below upper threshold to increase d2
    if (hr.cpcheck1 %in% "Estimated") {
      cp.d <- cps.est[1]
    } else if (hr.cpcheck1 %in% "True") {
      cp.d <- cps.true[1]
    } else if (hr.cpcheck1 %in% "Assumed") {
      cp.d <- cps.assumed[1]
    } else if (hr.cpcheck1 %in% "Tier-2") {
      cp.d <- cps.tier2[1]
    } else if (hr.cpcheck1 %in% "Min(Est, Tier-2)") {
      cp.d <- cps.min[1]
    } else if (hr.cpcheck1 %in% "Ave(Est, Assumed)") {
      cp.d <- cps.ave[1]
    }
    check1.result <- cp.d < cpcut1
    
    # Check if CP(d) below lower threshold to increase d2
    if (hr.cpcheck2 %in% "Estimated") {
      check2.result <- cps.est[1] >= cpcut2
    } else if (hr.cpcheck2 %in% "True") {
      check2.result <- cps.true[1] >= cpcut2
    } else if (hr.cpcheck2 %in% "Assumed") {
      check2.result <- cps.assumed[1] >= cpcut2
    } else if (hr.cpcheck2 %in% "Tier-2") {
      check2.result <- cps.tier2[1] >= cpcut2
    } else if (hr.cpcheck2 %in% "Min(Est, Tier-2)") {
      check2.result <- cps.min[1] >= cpcut2
    } else if (hr.cpcheck2 %in% "Ave(Est, Assumed)") {
      check2.result <- cps.ave[1] >= cpcut2
    }
    
    # Determine new number of events, dstar
    check3.result <- hr.ssr < hrcut
    check4.result <- p.ssr < pcut
    if (check1.result & check2.result & check3.result & check4.result) {
      if (hr.dstar %in% "Estimated") {
        dstar <- ifelse(any(cps.est >= 80), dstar.vals[cps.est >= 80][1], dmax)
      } else if (hr.dstar %in% "True") {
        dstar <- ifelse(any(cps.true >= 80), dstar.vals[cps.true >= 80][1], dmax)
      } else if (hr.dstar %in% "Assumed") {
        dstar <- ifelse(any(cps.assumed >= 80), dstar.vals[cps.assumed >= 80][1], dmax)
      } else if (hr.dstar %in% "Tier-2") {
        dstar <- ifelse(any(cps.tier2 >= 80), dstar.vals[cps.tier2 >= 80][1], dmax)
      } else if (hr.dstar %in% "Min(Est, Tier-2)") {
        dstar <- ifelse(any(cps.min >= 80), dstar.vals[cps.min >= 80][1], dmax)
      } else if (hr.dstar %in% "Ave(Est, Assumed)") {
        dstar <- ifelse(any(cps.ave >= 80), dstar.vals[cps.ave >= 80][1], dmax)
      }
    } else {
      dstar <- d
    }
    
    # Calculate CP(dstar)
    adjcp.est <- cps.est[dstar.vals == dstar]
    adjcp.true <- condpow(z1 = z.ssr, d1 = d1, d = d, dstar = dstar, hr = cmed.true/tmed.true) * 100
    adjcp.assumed <- condpow(z1 = z.ssr, d1 = d1, d = d, dstar = dstar, hr = cmed.assumed/tmed.assumed) * 100
    adjcp.tier2 <- condpow(z1 = z.ssr, d1 = d1, d = d, dstar = dstar, hr = hr.tier2) * 100
    adjcp.min <- condpow(z1 = z.ssr, d1 = d1, d = d, dstar = dstar, hr = min(hr.ssr, hr.tier2)) * 100
    adjcp.ave <- condpow(z1 = z.ssr, d1 = d1, d = d, dstar = dstar, hr = mean(c(hr.ssr, cmed.assumed/tmed.assumed))) * 100
    
    # Perform initially planned test
    analysis.time <- ifelse(sum(df$`Event Flag`) >= d, sort(df$`Time to Event/Censor`[df$`Event Flag` %in% 1])[d], Inf)
    df.orig <- df %>%
      dplyr::filter(`Time to Rand.` <= analysis.time) %>%
      dplyr::mutate(
        `Time from Rand. to Censor (Analysis)` = pmin(analysis.time - `Time to Rand.`, `Time from Rand. to Censor`),
        `Time from Rand. to Event/Censor (Analysis)` = pmin(`Time from Rand. to Event`, `Time from Rand. to Censor (Analysis)`),
        `Event Flag (Analysis)` = ifelse(`Time from Rand. to Event` <= (`Time from Rand. to Censor (Analysis)` + 1e-10), 1, 0)
      )
    
    loca <- df.orig$Arm %in% "Treatment"
    logrank.orig <- fastlogranktest::logrank_test(
      df.orig$`Time from Rand. to Event/Censor (Analysis)`[loca],
      df.orig$`Time from Rand. to Event/Censor (Analysis)`[! loca],
      df.orig$`Event Flag (Analysis)`[loca],
      df.orig$`Event Flag (Analysis)`[! loca]
    )
    z.orig <- logrank.orig[2]
    hr.orig <- exp(z.orig * sqrt(4 / d))
    p.orig <- pnorm(z.orig)
    
    # Conduct modified study and perform final test
    if (dstar > d) {
      
      N.new <- ceiling((dstar - d) * N / d)
      if (N.new %% 2 == 1) N.new <- N.new + 1
      df.new <- tibble(
        Arm = c(rep("Treatment", N.new/2), rep("Control", N.new/2)),
        `Time to Rand.` = runif(n = N.new, accrual, accrual + (N.new / N) * accrual),
        `Time from Rand. to Event` = c(rexp(n = N.new/2, log(2)/tmed.true), rexp(n = N.new/2, log(2)/cmed.true)),
        `Time from Rand. to Censor` = c(rexp(n = N.new, log(2) / dmed)),
        `Time from Rand. to Event/Censor` = pmin(`Time from Rand. to Event`, `Time from Rand. to Censor`),
        `Time to Event/Censor` = `Time to Rand.` + `Time from Rand. to Event/Censor`,
        `Event Flag` = ifelse(`Time from Rand. to Event` <= `Time from Rand. to Censor`, 1, 0),
      )
      df <- rbind(df, df.new)
      
      analysis.time <- ifelse(sum(df$`Event Flag`) >= dstar, sort(df$`Time to Event/Censor`[df$`Event Flag` %in% 1])[dstar], Inf)
      df.full <- df %>%
        dplyr::filter(`Time to Rand.` <= analysis.time) %>%
        dplyr::mutate(
          `Time from Rand. to Censor (Analysis)` = pmin(analysis.time - `Time to Rand.`, `Time from Rand. to Censor`),
          `Time from Rand. to Event/Censor (Analysis)` = pmin(`Time from Rand. to Event`, `Time from Rand. to Censor (Analysis)`),
          `Event Flag (Analysis)` = ifelse(`Time from Rand. to Event` <= (`Time from Rand. to Censor (Analysis)` + 1e-10), 1, 0)
        )
      
      loca <- df.full$Arm %in% "Treatment"
      logrank.full <- fastlogranktest::logrank_test(
        df.full$`Time from Rand. to Event/Censor (Analysis)`[loca],
        df.full$`Time from Rand. to Event/Censor (Analysis)`[! loca],
        df.full$`Event Flag (Analysis)`[loca],
        df.full$`Event Flag (Analysis)`[! loca]
      )
      z.full <- logrank.full[2]
      hr.full <- exp(z.full * sqrt(4 / dstar))
      p.full <- pnorm(z.full)
      
      z2 <- sqrt(dstar / (dstar - d1)) * z.full - sqrt(d1 / (dstar - d1)) * z.ssr
      z.final <- sqrt(d1 / d) * z.ssr + sqrt((d - d1) / d) * z2
      hr.final <- exp(z.final * sqrt(4 / dstar))
      p.final <- pnorm(z.final)
      
    } else {
      z.full <- z.final <- z.orig
      hr.final <- hr.orig
      p.full <- p.final <- p.orig
    }
    
    tibble(
      
      # Design
      `True Median (Control)` = cmed.true,
      `True Median (Treatment)` = tmed.true,
      `True HR` = cmed.true / tmed.true,
      `Assumed Medians (HR)` = paste0(cmed.assumed, "/", tmed.assumed, " (", sprintf("%.2f", cmed.assumed / tmed.assumed), ")"),
      `True Medians (HR)` = paste0(cmed.true, "/", tmed.true, " (", sprintf("%.2f", cmed.true / tmed.true), ")"),
      `Tier-2 HR` = hr.tier2,
      `D` = d,
      `N`,
      `Max D*` = dmax,
      `Events for SSR` = d1,
      `HR for Upper CP` = hr.cpcheck1,
      `Upper CP Cutoff` = cpcut1,
      `HR for Lower CP` = hr.cpcheck2,
      `Lower CP Cutoff` = cpcut2,
      `HR for D*` = hr.dstar,
      `Max HR to Increase D` = hrcut,
      `Max P to Increase D` = pcut,
      
      # SSR results
      `HR (SSR)` = hr.ssr,
      `Z (SSR)` = z.ssr,
      `P (SSR)` = p.ssr,
      
      `CP(D; Est HR)` = cps.est[1],
      `CP(D; True HR)` = cps.true[1],
      `CP(D; Assumed HR)` = cps.assumed[1],
      `CP(D; Tier-2 HR)` = cps.tier2[1],
      `CP(D; Min(Est, Tier-2 HR))` = cps.min[1],
      
      `CP(D)` = cp.d,
      `CP(D) < Upper Cutpoint` = ifelse(check1.result, 1, 0),
      `CP(D) >= Lower Cutpoint` = ifelse(check2.result, 1, 0),
      `Est HR < Cutpoint` = ifelse(check3.result, 1, 0),
      `P (SSR) < Cutpoint` = ifelse(check4.result, 1, 0),
      
      `D*` = dstar,
      `D* Maxed` = ifelse(`D*` == dmax, 1, 0),
      `CP(D*; Est HR)` = adjcp.est,
      `CP(D*; True HR)` = adjcp.true,
      `CP(D*; Assumed HR)` = adjcp.assumed,
      `CP(D*; Tier-2 HR)` = adjcp.tier2,
      `CP(D*; Min(Est, Tier-2 HR))` = adjcp.min,
      
      # Results for original design
      `HR (Original)` = hr.orig,
      `Z (Original)` = z.orig,
      `P (Original)` = p.orig,
      `Reject H0 (Original)` = ifelse(p.orig < 0.025, 1, 0),
      
      # Results for modified design
      `HR (Final)` = hr.final,
      `Z (Final)` = z.final,
      `P (Final)` = p.final,
      `Reject H0 (Final)` = ifelse(p.final < 0.025, 1, 0),
      
      `Z (Naive)` = z.full,
      `P (Naive)` = p.full,
      `Reject H0 (Naive)` = ifelse(p.full < 0.025, 1, 0)
      
    )
    
  }))
  
}

