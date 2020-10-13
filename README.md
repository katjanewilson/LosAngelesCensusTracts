# Los Angeles Census Tracts
A hard to swallow assumption in the linear model framework is that a particular feature will change at the same rate across all values for a given variable. General Additive Models (GAM) give analysts the opportunity to identify nonlinear changes between covariates and the dependent variable. These nonparametric models can suffer from poor interpretability, however, and strong assumptions will have to be made regarding the data generation process. In a project using census data in the city of Los Angeles, GAM models identify outlier trends in homeless individuals, yet they also reveal limitations of the loess smoother at upper boundaries of data.
*course project for Stat 974


[GitHub action]: https://github.com/andresz1/size-limit-action
[cult-img]:      http://cultofmartians.com/assets/badges/badge.svg
[cult]:          http://cultofmartians.com/tasks/size-limit-config.html

## Data

* Census Data - included in data folder

## Packages

* [gam](https://cran.r-project.org/web/packages/randomForest/randomForest.pdf)
* [mgc](https://cran.r-project.org/web/packages/randomForest/randomForest.pdf)
* [mgcViz](https://cran.r-project.org/web/packages/randomForest/randomForest.pdf)




## How it Works

```

#set seed at 4000
# Bring in data and create initial training and test datasets
set.seed(4000)
df <- homeless
index <- sample(1:nrow(df), (nrow(df))/2, replace = F) 
Train <- na.omit(df[index,]) # Training data
Test <- na.omit(df[-index,]) # Test data
##coming to the right model:
library(leaps)
All1 <- regsubsets(StreetTotal ~ MedianIncome + PropMinority +
                     PropVacant+
                     PctResidential+
                     PCTIndustrial +
                     PctCommercial +
                     Industrial, data = Train)
# All1
# summary(All1)

Model2 <- lm(StreetTotal ~  
               PropVacant+
               Industrial +
               (PropVacant:Industrial), data = Train)
# summary(Model2)

## Generalization error ##
# Specify a model on the testing data 
Model2 <- lm(StreetTotal ~  
               PropVacant+
               Industrial +
               (PropVacant:Industrial), data = Test)
# summary(Model2)
#estimate the fitted values on the testing data, and the generalization error
preds <- predict(Model2, newdata = Test) # New fitted values derived from the test data
GE <- var(Test$StreetTotal - preds) # Estimate of generalization error (the variance of the residuals when 
# GE
# sqrt(GE)
# Bootstrap a confidence interval for the generalization error 
bootstrap_genError <-
  function(x) {
    bootstrapped_genErrors <- 0
    for (i in 1:1000) {
      index <- sample(1:nrow(x), nrow(x), replace = T)
      sample_Test <- x[index,]
      sample_Test_preds <- predict(Model2, newdata = sample_Test)
      bootstrapped_genErrors[i] <- var(sample_Test$StreetTotal - sample_Test_preds)
    }
    return(bootstrapped_genErrors)
  }

bootstrap_results_ge <- bootstrap_genError(Test)

# Check generalization error bootstrap results
# mean(bootstrap_results_ge, na.rm = T)
# summary(bootstrap_results_ge)
# hist(bootstrap_results_ge, breaks = 20) 
# qqnorm(bootstrap_results_ge)
# sd(bootstrap_results_ge)
# quantile(bootstrap_results_ge, probs = c(.025,.975))
# plot(density(bootstrap_results_ge))
# qqnorm(bootstrap_results_ge)
#use robust sandwich standard errors in the sandwich package
# install.packages("sandwich")
library(sandwich)
# vcovHC(Model2, type = "HC")
# sandwich_se <- diag(vcovHC(Model2, type = "HC1"))
# sqrt(sandwich_se)
#then can get the confidence interval limits of these
# coef(Model2) - 1.96*sandwich_se
# coef(Model2) + 1.96*sandwich_se

library(gam)
library(mgcv)
library(leaps)

homeless2 <- homeless %>%
  filter(MedianIncome > 60000 & PropVacant < .10)

##MODEL 1
out3 <- gam(StreetTotal ~
              s(PCTIndustrial) +
              s(PctCommercial) +
              s(PropVacant) +
              s(MedianIncome), data = homeless, family = gaussian)
# summary(out3)
par(mfrow=c(2,2))
# plot(out3, residual = T, cex = 1, pch = 19, shade = T, shade.col = "light blue",
#      col = "#FAD7A0")
library(mgcViz)
b<- getViz(out3)
o <- plot( sm(b, 1) )
# o + l_fitLine(colour = "#FAD7A0") + l_rug(mapping = aes(x=x, y=y), alpha = 0.9) +
#   l_ciLine(mul = 5, colour = "light blue", linetype = 2) +
#   l_points(shape = 19, size = 1, alpha = 0.7, color = "light blue") + theme_classic()


```
