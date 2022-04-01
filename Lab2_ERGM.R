#### Labs 2: Fitting ERGMs

library(statnet)

#change working directory
library(here)
here()
### Import data
data <- as.matrix(read.csv("Manager_friends.csv", header = T))

### Create a network object
Fnet <- network(data, directed = T)

### Import and set attributes for nodes
att <- read.csv("Manager_att.csv", header = T, row.names = 1 )

set.vertex.attribute(Fnet, "age", att$age )
set.vertex.attribute( Fnet, "tenure", att$tenure )
set.vertex.attribute( Fnet, "level", att$level )
set.vertex.attribute( Fnet, "dept", att$dept )

### Model 1: Covariates
## Covariate Effects
# nodeicov: receiver effects for continuous variables
# nodeocov: sender effects for continuous variables
# nodecov: connection effects for continuous variables
# nodeifactor, nodeofactor, and nodefactor for categorical variables
## Homophily
# absdiff: difference for continuous variables
# nodematch: being the same for binary/categorical variables
# install.packages("ergm")

#nodeicov -- incoming variable
#nodeocov -- outgoing 
# homophily -- if categorical variable, use nodematch(). If continuous, use conversely absdiff()

model1 <- ergm(Fnet ~ edges + nodeicov("age") + nodeocov("age") + nodecov("tenure") + 
                nodefactor("dept") + absdiff("age") + absdiff("tenure") + nodematch("dept") )
summary(model1)
est <- as.matrix(summary(model1)$coefs)[,c(1,2,4)]
write.csv(est, file = "est1.csv")

### Model 2: Covariates + Local Structures
# The GW terms help model convergence.
# Instead of using raw measures, 
# the GW terms weighted down additional similar structures.
# This only contains covariates, no endogenous terms

model2 <- ergm(Fnet ~ edges + nodeicov("age") + nodeocov("age") + nodecov("tenure") +
               nodefactor("dept") + absdiff("age") + absdiff("tenure") + nodematch("dept")+
               mutual + gwidegree(0.1, fixed = TRUE) + gwesp(0.1, fixed = TRUE) + gwdsp(0.1, fixed = TRUE),
               control = control.ergm(MCMLE.maxit = 300, MCMC.burnin=10000, MCMC.interval=200, seed = 178 ))
summary(model2)
est <- as.matrix(summary(model2)$coefs)[,c(1,2,4)]
write.csv(est, file = "est2.csv")

### Compare models
AIC <- rbind(AIC(model1), AIC(model2))
AIC

## Check goodness-of-fit of the better model
## The solid lines represent observed statistics
## The dashed lines represent simulated statistics
## That solid lines are within the 95% confidence intervals of the simulated lines
## indicates model convergence.
gof2 <- gof(model2)
par(mfrow=c(1,5))
plot(gof2)

### Model 3: Covariates + Local Structures + Multiplex Network
# Use the advice-seeking network as a predictor
advice <- as.matrix(read.csv("Manager_advice.csv", header = T))
anet <- network(advice, directed = T)

# Whether advice-seeking relationships lead to friendships
model3 <- ergm(Fnet ~ edges + nodeicov("age") + nodeocov("age") + nodecov("tenure") +
                 nodefactor("dept") + absdiff("age") + absdiff("tenure") + nodematch("dept")+
                 mutual + gwidegree(0.1, fixed = TRUE) + gwesp(0.1, fixed = TRUE) + gwdsp(0.1, fixed = TRUE) +
                 edgecov(anet), control = control.ergm(MCMLE.maxit = 300, MCMC.burnin=10000, MCMC.interval=200, seed = 178 ))
summary(model3)

### Sometimes if the model does not converge, one can force using logistic regression by setting the "MPLE" option.
### Model 4: MPLE
model4 <- ergm(Fnet ~ edges + nodeicov("age") + nodeocov("age") + nodecov("tenure") +
                 nodefactor("dept") + absdiff("age") + absdiff("tenure") + nodematch("dept")+
                 mutual + gwidegree(0.1, fixed = TRUE) + gwesp(0.1, fixed = TRUE) + gwdsp(0.1, fixed = TRUE) +
                 edgecov(anet), estimate = "MPLE", control = control.ergm(MCMLE.maxit = 300, MCMC.burnin=10000, MCMC.interval=200, seed = 178 ))
summary(model4)

