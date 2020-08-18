
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

############
Section 1:
  Data Cleaning: Imputation and Recoding or Missing Data
##########

```{r 1, echo=TRUE, message=TRUE, warning=TRUE}
library(tidyverse)
library(gridExtra)
library(kableExtra)

#inspect data
load("NewHomeless.rdata")
homeless <- homeless
dt <- data.frame(summary(homeless))
dt <- dt %>%
  select(Var2, Freq)

#count the missing data values

sapply(homeless, function(x) sum(is.na(x)))
which(is.na(homeless$PropVacant))

#inspect missing values
missing <- homeless %>%
  filter(is.na(PropVacant))

#impute the missing data
homeless[is.na(homeless)] = 0

#clean mis coded values
# Adjust values of PropMinority mis-coded as "100" instead of "1.0"
homeless <- mutate_at(homeless, vars(PropMinority), list(~ ifelse( . > 1.0, 1.0, .)))

# Convert values of "Pct" variables to be in decimal scale
homeless <- mutate_at(homeless, vars(PctResidential:PCTIndustrial), list(~ .*.01))

# Recode four NAs in PropVacant as zero
homeless <- mutate_at(homeless, vars(PropVacant), list(~ ifelse(is.na(.)==T, 0, .)))

#recode the industrial as a binary value
homeless <- homeless %>%
  mutate(Industrial = ifelse(PCTIndustrial >0, 1, 0))

summary(homeless) %>%
  kable()

```

############
Section 2:
  Data Visualization
##########

``` {r 2, echo = TRUE}

#visualize data
#pairs
pairs(homeless)

#correlation
library(DataExplorer)
plot_correlation(homeless, type = 'continuous','Review.Date')

# define a function to remove outliers
FindOutliers <- function(data) {
  lowerq = quantile(data)[2]
  upperq = quantile(data)[4]
  iqr = upperq - lowerq #Or use IQR(data)
  # we identify extreme outliers
  extreme.threshold.upper = (iqr * 3) + upperq
  extreme.threshold.lower = lowerq - (iqr * 3)
  result <- which(data > extreme.threshold.upper | data < extreme.threshold.lower)
}

class(homeless)

# use the function to identify outliers
temp <- FindOutliers(homeless$PropMinority)
temp #no outliers
temp <- FindOutliers(homeless$PctResidential)
temp #no outliers

#Income outliers
temp <- FindOutliers(homeless$MedianIncome)
temp
IncomeOutliers <- homeless[temp,]
#Prop Vacant outliers
temp <- FindOutliers(homeless$PropVacant)
temp
VacantOutliers <- homeless[temp,]
#Pct Commercial outliers
temp <- FindOutliers(homeless$PctCommercial)
temp
CommercialOutliers <- homeless[temp,]
#Pct Industrial outliers
temp <- FindOutliers(homeless$PCTIndustrial)
temp
IndustrialOutliers <- homeless[temp,]

# IndustrialOutliers %>%
#   kable(caption = "IndustrialOutliers")
# CommercialOutliers %>%
#   kable(caption = "CommercialOutliers")
# IncomeOutliers %>%
#   kable(caption = "IncomeOutliers")
# VacantOutliers %>%
#   kable(caption = "VacantOutliers")
```

``` {r 3, echo = TRUE}

#examine histogram of Street Total count

par(mfrow = c(1,2))
p1 <- ggplot(aes(StreetTotal), data = homeless) +
  geom_histogram(binwidth = 6) +
  geom_vline(xintercept = mean(homeless$StreetTotal), colour = "pink")
p2 <- ggplot(aes(MedianIncome), data = homeless) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = mean(homeless$MedianIncome), colour = "pink")

#Prop Vacant

p3 <- homeless %>%
  ggplot(aes(PropVacant)) +
  geom_histogram(bins = 40) +
  geom_vline(xintercept = mean(homeless$PropVacant), colour = "pink")

#Prop Minority

p4 <- homeless %>%
  ggplot(aes(PropMinority)) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = mean(homeless$PropMinority), colour = "pink")

#Pct Residential

p5 <- homeless %>%
  ggplot(aes(PctResidential)) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = mean(homeless$PctResidential), colour = "pink")

#PctCommercial

p6 <- homeless %>%
  ggplot(aes(PctCommercial)) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = mean(homeless$PctCommercial), colour = "pink")

#PctIndustrial

p7 <- homeless %>%
  ggplot(aes(PCTIndustrial)) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = mean(homeless$PCTIndustrial), colour = "pink")

grid.arrange(p1, p2, p3, p4, p5, p6, p7, nrow = 3)


sqrt(850)

```

############
Section 4:
  Automatic Methods for Variable Selection
##########

``` {r 4, echo = TRUE}


#test the full model with all predictors
Model <- lm(StreetTotal ~ MedianIncome +
              PropVacant + PropMinority+
              PctResidential + PctCommercial +
              PCTIndustrial + Industrial, data = homeless)
summary(Model)
library(leaps)
#variable selection using automatic methods via RegSubsets
All1 <- regsubsets(StreetTotal ~ MedianIncome + PropMinority +
                     PropVacant+
                     PctResidential + Industrial +
                     PCTIndustrial, data = homeless)
All1
summary(All1)
res.sum <- summary(All1)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)

Model2 <- lm(StreetTotal ~  
               PropVacant +
               
               MedianIncome  + PCTIndustrial, data = homeless)
summary(Model2)
```

``` {r 5, echo = TRUE}


#testing the model on how well is satisfies assumptions
# 
#residuals plot
p1 <- plot(Model2, which =1)

#normality plot
p2 <- plot(Model2, which = 2)

#homoesceasticity
p3 <- plot(Model2, which =3)

#leverage - Cook's distance
p4 <- plot(Model2, which = 5)


###outlier analysis

plot(fitted(Model2) , residuals(Model2), xlab = "Fitted", ylab = "Residuals")
#leverage
m1_inf <- influence(Model2)
#rule of thumb: if the leverage value h_i for observation i is greater than 2p/n, then this observation warrants greater analysis, so here the p number of predictors is 6/509 = .0118
data_lev <- data.frame(m1_inf$hat) %>%
  filter(m1_inf.hat > .0117878)
#there are 164 points that are greater than this value

#cook's distance
cook <- cooks.distance(Model2)
max(cook)
#the criteria for Cook's distance is that it is greater than 1, and none of these are)

#studentized residuals
stud <- residuals(Model2)/(summary(Model2)$sig*sqrt(1-m1_inf$hat))

#jackknife resiuals

jack <- rstudent(Model2)

jack[which.max(abs(jack))] # WHAT IS THE LARGEST JACKKNIFE RESIDUAL?

1-pt(jack[which.max(abs(jack))],40) # IS THE RESIDUAL STATISTICALLY SIGNIFICANT?
which.max(36)
library(car)
vif(Model2)

```

############
Section 5:
  Usin reduce model, evaluate genearlization and prediction error
##########

``` {r 6, echo = TRUE}

#reduced model with 3 predictors
Model2<- lm(StreetTotal ~  
              PropVacant+
              +
              MedianIncome + PCTIndustrial, data = homeless)
summary(Model2)

# Create a training and testing dataset
df <- homeless
index <- sample(1:nrow(df), (nrow(df))/2, replace = F) 
Train <- na.omit(df[index,]) # Training data
Test <- na.omit(df[-index,]) # Test data

## Generalization error ##

# Specify a model on the training data 
Model2 <- lm(StreetTotal ~  
               PropVacant+
               
               MedianIncome + PCTIndustrial, data = Train)
summary(Model2)
# Estimate the generalization error 
MSE <- var(Train$StreetTotal - Model2$fitted.values) # MSE of the model
MSE
sqrt(MSE) #RMSE

preds <- predict(Model2, newdata = Test) # New fitted values derived from the test data
GE <- var(Test$StreetTotal - preds) # Estimate of generalization error (the variance of the residuals when 
GE
sqrt(GE)

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
mean(bootstrap_results_ge, na.rm = T)
summary(bootstrap_results_ge)
hist(bootstrap_results_ge, breaks = 20) 
qqnorm(bootstrap_results_ge)
sd(bootstrap_results_ge)
quantile(bootstrap_results_ge, probs = c(.025,.975))
plot(density(bootstrap_results_ge))
qqnorm(bootstrap_results_ge)

```


############
Section 6:
  Tune the gam model to evaluate changes in fit
##########


library(gam)
library(mgcv)
library(leaps)

#preparing data
#we also investigate the possibility that quartile splits of variables could yield more insight

homeless <- within(homeless, StreetTotalQuartile <- as.factor(
  as.integer(cut(StreetTotal,
                 quantile(StreetTotal, 
                          probs=0:4/4),
                 include.lowest=TRUE))))
homeless <- within(homeless,MedianIncomeQuartile <- as.factor(
  as.integer(cut(MedianIncome,
                 quantile(MedianIncome,
                          probs=0:4/4),
                 include.lowest=TRUE))))

homeless <- homeless %>% mutate(Industrial = as.factor(if_else(PCTIndustrial > 0, 1, 0)))

##MODEL 1
out3 <- gam(StreetTotal ~
              s(PCTIndustrial) +
              s(PctCommercial) +
              s(PropVacant), data = homeless, family = gaussian)
summary(out3)
par(mfrow=c(2,2))
plot(out3, residual = T, cex = 1, pch = 19, shade = T, shade.col = "light blue",
     col = "blue")


##MODEL 2
#we increase the deviance explained by removing the variables Residential, which was not predictive
#the land use variables seem to explain more homelessness
#variables MedianIncome and PropMinority were correlated in the univariate analysis, so these were added
#to the model using a tensor product, which is used for two variables that have different scales.
out3 <- gam(StreetTotal ~
              s(PCTIndustrial) +
              te(MedianIncome, PropMinority)+
              s(PctCommercial) +
              s(PropVacant, sp = .001), data = homeless, family = gaussian)

summary(out3)
par(mfrow=c(2,2))
plot(out3, scheme = 1, residual = T, cex = 1, pch = 19, shade = T, shade.col = "light blue",
     col = "blue")

#MODEL 3
out3 <- gam(StreetTotal ~ 
              StreetTotalQuartile + 
              
              te(PCTIndustrial, by = StreetTotalQuartile) +
              s(PctCommercial) +
              s(PCTIndustrial) +
              s(PropVacant) +
              te(PropVacant, by = StreetTotalQuartile), 
            data = homeless, family = gaussian)
summary(out3)
par(mfrow=c(2,2))
plot(out3, scheme = 1, residual = T, cex = 1, pch = 19, shade = T, shade.col = "light blue",
     col = "blue")

