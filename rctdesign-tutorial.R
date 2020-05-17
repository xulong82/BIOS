library(gsDesign)
library(RCTdesign)

RCTversion()

# How the drop-out rate was considered in sample size calculation?
# In practice the clinical trialist will consider a variety of accrual scenarios along with time to event and time to dropout distribution assumptions.
# Under each scenario, RCTdesign calculates the probability that a randomly chosen patient on the study might have an observed event.
# Point estimation will be derived, or by simulation?
# Is there a cost function?

# Stopping boundaries of futility and efficacy were detemined together, how were they determined?

y1 = nSurv(sided = 1, ratio = 1, alpha = 0.025, beta = 0.1, hr0 = 1, hr = 0.5, 
           lambdaC = 1/5, eta = 1/10, gamma = 20, R = 8, T = 20)

y1$d
y1$n
y1$T

d2 =  seqDesign(prob.model= "hazard", arms= 2, ratio= c(1,1),
                null.hypothesis= 1, alt.hypothesis= 0.5,
                test.type= "less", size= 0.025, power= 0.90)

d2$parameters

d3 =  seqDesign(prob.model= "hazard", arms= 2, ratio= c(1,1),
                null.hypothesis= 1, alt.hypothesis= 0.5, test.type= "less", size= 0.025, power= 0.90,
                accrualTime = 8, studyTime = 20, eventHazard = 1/5, dropoutHazard = 1/10)
d3$parameters

# Tutorial

(d1 = seqDesign(prob.model= "mean", arms= 2, ratio= c(1,1),
                null.hypothesis= 0, alt.hypothesis= -10,
                sd= c(30,30), test.type= "less", size= 0.025, power= 0.90))

(d1J2 = update(d1, nbr.analyses = 2))
(d1J4 = update(d1, nbr.analyses = 4))

# How to specify stopping only for efficacy, but not for futility?
# And how would it affect the efficacy boundaries?

(d1J4x = update(d1J4, early.stopping = "alternative"))

# bounaries values for efficacy are relaxed when futility stopping was used 
plot(d1J4, d1J4x)

(d1J4.eval = seqEvaluate(d1J4))
plot(d1J2, d1J4)

# What exact are the different bounary shape parameters?
# design.family, R, A, G et. al.

(d1J4y = update(d1J4, design.family = "OBF"))

# How RCTdesign handles sample size re-estimation?
# Looks like RCTdesign is capable of doing it.

