library(interval)
library(survival)

### how survival estimation was done? --> Turnbull interval and EM for NPMLE
### how test was done? --> some sorts of score tests

###########################################################################################

MyData = openxlsx::read.xlsx("GitHub/biostats/interval_censoring/MyFile.xlsx")

MyData <- within(MyData, censoring <- ifelse(is.na(T2) & !is.na(T1), 'right', ifelse(T2!=T1, 'interval', ifelse(T2==T1, 'none', NA))) )
table(MyData$censoring)

intervalPFS <- icfit(Surv(T1, T2, type = "interval2") ~ 1, data = MyData, conf.int=TRUE, control = icfitControl(B = 100, seed = 1234))

intPFS <- (1 - cumsum(intervalPFS$pf))
intPFS <- data.frame(time.interval = names(intPFS), PFS=intPFS, row.names = NULL)
intPFS

plot(intervalPFS)

### MIDPOINT
MyData <- within(MyData, status <- ifelse( is.na(T2), 0, 1))
MyData <- within(MyData, midpoint <- ifelse(is.na(T2), T1, (T2+T1)/2))

midpointPFS <- survfit(Surv(midpoint, status) ~ 1, data = MyData, conf.type="log-log")

summary(midpointPFS)
summary(midpointPFS, time = c(6, 12))

plot(midpointPFS)

### UPPER LIMIT (STANDARD PROCEDURE)
MyData <- within(MyData, upper <- ifelse(!is.na(T2) & !is.na(T1), T2, midpoint))

upperPFS <- survfit(Surv(upper, status) ~ 1, data = MyData, conf.type="log-log")
upperPFS

summary(upperPFS)

### LOWER LIMIT
MyData <- within(MyData, lower <- ifelse(!is.na(T2) & !is.na(T1), T1, midpoint))

lowerPFS <- survfit(Surv(lower, status) ~ 1, data = MyData, conf.type="log-log")
lowerPFS

summary(lowerPFS)

### OUTPUT & INTERPRETATION

plot(intervalPFS)
lines(midpointPFS, col = "red")

median <- with(intPFS, {i <- which(PFS == min(PFS[PFS>=0.5])); i<-c(i,tail(i,1)+1); data.frame(time=time.interval[i],PFS=PFS[i])})
median

lm1 = lm(time ~ PFS, data = data.frame(time=c(9.66,9.79), PFS=c(0.5328894,0.3379596)))
predict.lm(lm1, newdata = data.frame(PFS = 0.5))

lower <- with(intervalPFS$CI,{i<-which(lower==min(lower[lower>=0.5])); i<-c(i,tail(i,1)+1); data.frame(time=round(time[i],2),lower=lower[i])})
lower

upper <- with(intervalPFS$CI,{i<-which(upper==min(upper[upper>=0.5])); i<-c(i,tail(i,1)+1); data.frame(time=round(time[i],2),upper=upper[i])})
upper

lm2 = lm(time~lower,data=tail(lower, n=2))
predict.lm(lm2, newdata=data.frame(lower = 0.5))

lm3 = lm(time~upper,data=tail(upper, n=2))
predict.lm(lm3, newdata=data.frame(upper = 0.5))

###########################################################################################

data(bcos)

bcos = bcos %>% mutate(OS = ifelse(is.infinite(right), left, right))
bcos = bcos %>% mutate(CNSR = ifelse(is.infinite(right), 1, 0))

####################

fit1 <- icfit(Surv(left, right, type = "interval2") ~ treatment, data = bcos)
summary(fit1)
plot(fit1)

fit2 <- survfit(Surv(OS, 1 - CNSR) ~ treatment, data = bcos)
summary(fit2)
lines(fit2, col = c("blue", "red"))

zzz = bcos %>% filter(treatment == "RadChem") %>% filter(! is.infinite(right))
zzz = zzz %>% arrange(right)
zzz$right %>% unique

fit1x <- icfit(Surv(left, right, type = "interval2") ~ 1, data = bcos)
fit2x <- survfit(Surv(OS, 1 - CNSR) ~ 1, data = bcos)

plot(fit1x)
lines(fit2x, col = c("blue"))

### 

survdiff(Surv(OS, 1 - CNSR) ~ treatment, data = bcos)
ictest(Surv(left, right, type = "interval2") ~ treatment, data = bcos)

zz2 = bcos[1:5, ]
zz3 = icfit(Surv(left, right, type = "interval2") ~ 1, data = zz2)
summary(zz3)
plot(zz3)

###

xxx = data.frame(left = c(0, 0, 6, 7, 7, 17, 37, 45, 46, 46),
                 right = c(7, 8, 10, 16, 14, Inf, 44, Inf, Inf, Inf))

xxf = icfit(Surv(left, right, type = "interval2") ~ 1, data = xxx)
summary(xxf)
