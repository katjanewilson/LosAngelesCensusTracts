# Los Angeles Census Tractts
A hard to swallow assumption in the linear model framework is that a particular feature will change at the same rate across all values for a given variable. General Additive Models (GAM) give analysts the opportunity to identify nonlinear changes between covariates and the dependent variable. These nonparametric models can suffer from poor interpretability, however, and strong assumptions will have to be made regarding the data generation process. In a project using census data in the city of Los Angeles, GAM models identify outlier trends in homeless individuals, yet they also reveal limitations of the loess smoother at upper boundaries of data.
*course project for Stat 974

## Table of Contents

<p align="center">
<img src="https://github.com/katjanewilson/CORD-Random-Forest/blob/master/images/covid.png"
  alt="Size Limit comment in pull request about bundle size changes"
  width="486" height="189">
</p>


[GitHub action]: https://github.com/andresz1/size-limit-action
[cult-img]:      http://cultofmartians.com/assets/badges/badge.svg
[cult]:          http://cultofmartians.com/tasks/size-limit-config.html

## Data

* Census Data - included in data folder

## Cleaning Notes
 * Data are generated from a random sample of census tracts obtained in the city of Los Angeles in the year 2009. The sample includes 509 tracts of the over 2000 existing tracts in the city. Included in the data are 509 observations of 7 predictor variables: street total, median income, proportion vacant, proportion minority, percent residential, percent commercial, and percent industrial. Four observations are missing from the data, all of which are observations for the Proportion Vacant variable among 4 unique tracts (tracts 10, 445, 501, and 502). Three of these tracts also have a 100% minority count and all four of these tracts have a median income of 0. Given that these four unique tracts have these similarities on other variables, we infer that the Proportion Vacant was coded as NA for reasons that do not warrant removing these observations. It may be the case that these tracts are locations of prisons, or factories, rather than places of residence, and that these reasons led to the Proportion Vacant variable to be coded as NA.
 * A coding error in the variable for Proportion Minority is addressed. One value is 100.0, or 100,00%, instead of 1.0. These values are changed and recorded to address this issue. An additional coding peculiarity is addressed. The Percent Industrial variable contains many observations that have no numerical value, indicating that no percentage of these tracts are zoned for industrial enterprises. We create a new binary variable to capture this feature of the data, titled “Industrial” which is coded at 0 or 1 to distinguish between residential and industrial locations.
 * Five of the variables are skewed right, and a few large values bring the mean upwards, leaving the median unaffected. In the case of Street Total, Proportion Vacant, Percent Commercial, and Percent Industrial, the mean is larger than the median and some high tract information drags the mean upwards. This pattern is less for Median Income, but still prevalent. The other two variables, Proportion of Minority and Percent Residential, differ. Proportion Minority looks to have a bimodal distribution, and percent residential looks slightly skewed left, meaning that some tracts being the mean down. The correlation of variables with one another provides further insight. There is a moderate negative correlation between Proportion Minority and Median Income in the dataset at -0.51 (Figure 0.1). The next strongest correlation is between Percent Residential and Percent Industrial, which is also a negative correlation at -0.49. This makes sense thinking about the substance of the subject matter that contrives the data. None of the other variables show strong correlations.



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

## Model Evaluation

1. Our first gam model (Model 1.0) includes the variables Percent Industrial, Percent Commercial, and Proportion Vacant. This model, and others that were chosen through a backward elimination of insignificant predictors, share undesirable measures of GCV and explain a modest proportion of deviance at 35.9%. Since all of the variables have effective degrees of freedom greater than 1.0, this indicates that the partial response functions are nonlinear. For instance, the partial response function is relatively flat until over half of a tract is zoned for industrial purposes. The error bands are not too large during this increase period, so this increase should be considered. 
The graphs show that certain trends in homeless counts are correlated. It makes subject matter sense that land use variables are significantly associated with higher counts of homeless people, the less that land is zoned for industrial or commercial properties, or the more that lots are left vacant, the higher counts of homeless. However, we also saw from the outlier analysis and skewed distributions that certain larger groups of homeless congregate in areas that do not necessarily fit these trends. If the goal is predicting, then understanding these outliers is key. The tails give us useful information. For instance, in the Percent Commercial variable, visualized in Figure 1.1, the partial response function is relatively flat until over half of a tract is zoned for commercial purposes. Of note, though, is how the error band widens as the percent zoned for commercial increases. This indicates that there are fewer observations, and perhaps the decrease should not be taken too thoughtfully. The Proportion Vacant variable also shows a small increase in the count of homeless as tracts report more vacancy, and the error bands widen for tracts with greater vacancy, of which we have less observations for. The lessons from this first model show that certain predictors seem to be associated with counts of homeless, but that uncertainty occurs in the outliers of these variables.

2. The next group of models surveyed include smoothing parameters and interaction effects. Based on the use of our data, we were interested in some predictors fitting curves more than others. Altering the smoothing parameter of Proportion Vacant allows us to fit the curve tighter. Ideally, we would like more complexity for those variables that predict homelessness stronger, and less complexity for those that do not predict as effectively. In the bias variance tradeoff framework, choosing the fit based on a variable like proportion of vacant, which may be a changing variable, given the structure of residential dwellings in Los Angeles and the changing structure of vacancies, could be of interest to future researchers. However, regardless of the smoothing parameter used, we failed to find a much better fit. 
This second model (Model 2.0) makes use of these smoothing parameters and includes an interaction term for Median Income and Proportion Minority, two variables that have a stronger correlation (Figure 2.1), increases the deviance explained to 41%. However, scholars note that interaction terms may lack substantive sense (Berk 2008). Here, where our land use variables are more associated with homelessness, of which are highly skewed and outliers are present, focusing on the interactions may cloud the interpretation. Importantly, while interaction terms and smoothing parameters together may increase the deviance explained, these models are often too complicated to make sense in subject matter terms and interpreting the coefficients and interaction effects would be an arguably heady yet futile endeavor that little matters to the research task at hand.

3. The final approach we used in model selection was to use more insight upfront about how the data would act (Wood 2017). Our initial approaches of a hugely flexible model using automatic selection methods left out an important subject matter call: splitting homeless counts by quartiles could provide new information. Model 3.0 shows the results of this approach. Splitting the model up by quartile increases the fit statistics up to over 57% of deviance explained. This model implies that increases in the homeless count, or the effect of variables on predicting homeless, is augmented by the street total of the homeless population within them. For instance, visualizing the gam curves in Figure 3.1 shows an increase in the count of homeless in the fourth quartile of Street Count for both the variables of Proportion Vacant and Industrial, and the jump in the value of the effective degrees of freedom (EDF) for these quartiles compliment this trend. 
One can infer that the homeless concentrate in locations. This makes subject matter sense, thinking about shared communities and shared resources that the homeless rely on. It may be relatively harder to live on one’s own and be homeless, but perhaps the shared communities that those who are homeless create with each other incentivize the homeless to congregate together in areas. In fact, the interpretation of this final model is also informed by the work of the “boots on the ground” researchers and survey collectors who count the homeless. Homelessness may not be so much of a one-off phenomenon, but perhaps a pattern that is more reminiscent of congregation and community forming among people who are homeless.



## Evaluation

<p align="center">
<img src="https://github.com/katjanewilson/CORD-Random-Forest/blob/master/images/confusion_table1.png"
  alt="Size Limit comment in pull request about bundle size changes"
  width="400" height="200">
</p>


<p align="center">
<img src="https://github.com/katjanewilson/CORD-Random-Forest/blob/master/images/confusion_table2.png"
  alt="Size Limit comment in pull request about bundle size changes"
  width="400" height="200">
</p>
es in this category. In this case, the parameters can be re-sampled at different rates to achieve the desired cost ratio.
The task was to improve upon decisions that were already being made by doctors and nurses who received COVID-19 cases in the ER. Despite the difficulty in obtaining the exact cost ratios that were requested by policy officials, the model provides forecasts that are superior to baseline practice. Additionally, the model accounts for the cost ratios that were requested by policy makers, which prior baseline practices did not take into account. Such a tool could be used in conjunction with the expertise of doctors and nurses in the field for allocating COVID-19 patients proper resources.


