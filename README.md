---
title: "IRT Example"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
With difficulties so low most people got the questions right.  With no hard questions, cannot tell people with high theta's apart maybe?
199 means based on 200 simulated data sets using a rash model
```{r}
library(ltm)
data(LSAT)
descript(LSAT)

fit1 = rasch(LSAT, constraint = cbind(length(LSAT) + 1, 1))
summary(fit1)
coef(fit1, prob = TRUE, order = TRUE)
GoF.rasch(fit1, B = 199)
```
Rash with discrimination parameter different from one should probably use 2PL

Test the models with anova

2PL allows indiviudal discrimination for each item
```{r}
fit2 = rasch(LSAT)
summary(fit2)

anova(fit1, fit2)

# Now 2PL 

fit3 = ltm(LSAT ~ z1)
summary(fit3)

anova(fit2, fit3)

```
Now get ability or theta estimates for each person given their response pattern
```{r}
factor.scores(fit3)
```
Now graded response model

What does a significant rcor mean?  Bad or good?

Not sure what Extrmt1 and 2 are?  Thresholds maybe?

Looks like you need to check the two and three ways margins of residuals? Not really sure why

What constitutes a good fit?
```{r}
data("Environment")
head(Environment)
rcor.test(Environment, method = "kendall")

fitOrd1 = grm(data = Environment, constrained = TRUE, Hessian = TRUE)
summary(fitOrd1)
margins(fitOrd1)
margins(fitOrd1, type = "three")

fitOrd2 <- grm(Environment)
summary(fitOrd2)

anova(fitOrd1, fitOrd2)

information(fitOrd2, c(-4, 4))


```
Need to learn how to plot the curves and test information.
