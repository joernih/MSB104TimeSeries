## ----message=FALSE, warning=FALSE--------------------------------------------------------------
#knitr::purl("Applications_Fertility_static.Rmd")
# Loading data
rm(list=ls())
library(sandwich)
library(lmtest)
library(wooldridge)
library(ggplot2)
library(plotly)
phillips_rdgp <- phillips # Realized DGP


## ----------------------------------------------------------------------------------------------
model_ph <- lm(phillips_rdgp$inf ~ phillips_rdgp$unem)


## ----------------------------------------------------------------------------------------------
model_phc <- lm(phillips_rdgp$cinf ~ phillips_rdgp$unem)


## ---- echo=T, include=T------------------------------------------------------------------------
ols_ph <- summary(model_ph)
ols_phc <-summary(model_phc)


## ----------------------------------------------------------------------------------------------
ggplot2::ggplot(data=phillips_rdgp) + ggplot2::geom_line(aes(x=year,y=inf))
ggplot2::ggplot(data=phillips_rdgp) + ggplot2::geom_line(aes(x=year,y=unem))
ggplot2::ggplot(data=phillips_rdgp) + ggplot2::geom_line(aes(x=year,y=cinf))


## ----------------------------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(model_ph$res)
plot(model_phc$res)


## ----------------------------------------------------------------------------------------------
## Serial correlation with strictly exogenous regressors
#### Step 1: Find the estimated residual
resval <- model_ph$res
#### Step 2: Estimat the estimated residual on itself
model_ar <- lm(resval~lag(resval))
#### Step 3: Show the results from the estimation
summary(model_ar)


## ----------------------------------------------------------------------------------------------
## Perform Breusch-Godfrey test for first-order serial correlation:
bgtest(phillips_rdgp$inf ~ phillips_rdgp$unem)


## ----------------------------------------------------------------------------------------------
summary(model_phc, robust=FALSE)


## ----------------------------------------------------------------------------------------------
# Robust t test
coeftest(model_phc, vcov = vcovHC(model_phc, type = "HC0"))

