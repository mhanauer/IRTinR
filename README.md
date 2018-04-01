---
title: "IRT Example"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
With difficulties so low most people got the questions right.  With no hard questions, cannot tell people with high theta's apart maybe?
199 means based on 200 simulated data sets using a rash model

First assumption that there must be people in each category for each item.
Don't think we need any of the point correlations, I think those are for establshing local independence
descript should provide everything that you need for a summary statistic.

Shouldn't need Rash just compare one 1PL to 2PL
```{r}
library(ltm)
data(LSAT)
descript(LSAT)
fit1 = rasch(LSAT, constraint = cbind(length(LSAT) + 1, 1))
summary(fit1)
coef(fit1, prob = TRUE, order = TRUE)
GoF.rasch(fit1, B = 199)
```
Graded response model for likert scale data.  
First thing is to establish dimensionaliity and local independence which are both established by dimensionaility tests.

Tests: CFA, EFA with Eigenvalues: margins two and three way, and 1PL versus 2PL ANOVA 

First want to correlations
```{r}
data("Environment")
descript(Environment)
head(Environment)
rcor.test(Environment, method = "kendall")
```
Now run the 1PL model, which is the constrained equals true.  Want standard errors so we include the Hessian = TRUE part.

I think Extrmt1 is the theta value associated with 50% or higher endorsement of slighly concerned over not very concerned.  Or the theta level associated with the endorsement of the second option over the first one where it is 50% more likely that on average a person will endorse this item.  
```{r}
fitOrd1 = grm(data = Environment, constrained = TRUE, Hessian = TRUE)
summary(fitOrd1)
fitOrd2 = grm(data = Environment, constrained = FALSE, Hessian = TRUE)
summary(fitOrd2)
```
Now we can assess the model fit.  First we can check the two way margins.  This will help us understand if the items are still correlated after accounting for the latent construct and item parameters.

For the first margins, we are fitting the two way margins, and we are looking at residuals and looking to see if there are statistically significant residual above the threshold set by Rizopoulus(2006).  Same for three way.  Probably just report in text no tables.  Three way is another to look at the relationship between each combination of three way items and looking if the residuals correlation (maybe not right) is statistically significnatly above the threshold.  

Finally, we can assess model fit, by comparing models 1PL versus 2PL.

The item information tells us the total item information so we can see which set of items is providing us with the most information.  If some items are not providing much information may want to drop them.  I think we are specifciying the theta range -4 to 4 must have this for some reason.

Then you can look at the theta values for different response patterns (not indivudal people, but they are nested in response patterns).  z1 has the theta values.

```{r}
margins(fitOrd2)
margins(fitOrd2, type = "three")

anova(fitOrd1, fitOrd2)

information(fitOrd2, c(-4, 4))

factor.scores(fitOrd2)
```
Now plot Item char curves and item inforamtion curves
ICCS's
```{r}
par(mfrow = c(2, 2))
plot(fit2, category = 1, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

```





