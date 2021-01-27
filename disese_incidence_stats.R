library(epiR)

########## disease occurrence: risk vs rate

ncas = 4; npop = 200
data <- as.matrix(cbind(ncas, npop))

# incidence risk: proportion of initially susceptible individuals in a population who become new cases during a defined follow-up period
epi.conf(data, ctype = "inc.risk", method = "exact") * 100

# incidence rate: number of new cases that occur per unit of individual-time at risk during a defined follow-up period
epi.conf(data, ctype = "inc.rate", method = "exact") * 100

# risk is proportion (0 to 1), rate is case number per unit individual-time and its result depends on definition of individual-time (patient-years, patient months)
# risk and rate are the same when the period of risk does not vary across individuals being studied

# risk, simply as the proportion of new cases divided by the population at risk, is easy to interpret
# risk works well for closed population where no additions or subtractions occurs during the observation period, but not if otherwise
# rate can be challenging to interpret, however is the preferred measure of disease occurrence when the population being studied is not closed

############################ 

binom.test(13, 22, p = 0.4, alternative = "greater")
binom.test(13, 22, p = 0.5, alternative = "greater")

