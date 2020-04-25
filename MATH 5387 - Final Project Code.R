rm(list = ls())

install.packages("ipumsr") ## IPUMS-specific package
library(ipumsr)
library(dplyr) ## dependency for IPUMS package
install.packages("PerformanceAnalytics") ## correlation graphic
library(PerformanceAnalytics)
install.packages("car") ## regression diagnostic tools
library(car)
require(lattice) ## graphing utility

## Read data
ddi = read_ipums_ddi("C:\\Users\\Chandler\\Desktop\\MATH 5387 Project Data\\idhs_00002.xml")
africa = read_ipums_micro(ddi, verbose = FALSE)
zap_ipums_attributes(africa)  ## Remove IPUMS labels and automatically generated columns
View(africa)
malawi = africa[africa$COUNTRY == 454,] ## subsetting malawi data
View(malawi)

## Part 1: Inspect and clean data ##
malawi[, 1:12] = NULL; malawi["BIRTHYEAR"] = NULL; malawi["RESIDENT"] = NULL
View(malawi)
summary(malawi) ## "99" represents NIU values; need to remove

## Inspect for missing values
any(malawi == 96) ## inconsistent, returns false
any(malawi == 97) ## respondent doesn't know, returns false
any(malawi == 98) ## missing values, returns false
any(malawi == 99) ## not in universe, returns true
summary(malawi) ## Max = 99.00 in categorical variables indicate NIU; 
##Max = 8 in EDACHIEVER and INSOVERYN indicates missing, neither has a missing value
table(malawi$INJHCWORKER) ## 17400 observations are "99"; dropping these observations
table(malawi$DECFEMEARN) ## 5621 observations are "99"; dropping these observations
malawi = subset(malawi, INJHCWORKER != 99 & DECFEMEARN != 99)
any(malawi == 99) ## returns false
any(malawi == "Missing") ## search for missing values in factor variables, returns false

## no missing values; NIU values removed

## Correlation Matrices
## using Spearman correlation, since some variables may not be normally distributed
round(cor(malawi, method = "spearman"), 2)
chart.Correlation(malawi, histogram = FALSE, method = "spearman", pch = 19) ## prior to converting
## categories to factors

## Convert factor variables
malawi$FEMOWNLAND = factor(malawi$FEMOWNLAND)
levels(malawi$FEMOWNLAND) = c("Does not own land", "Owns alone only", "Owns jointly", 
                              "Owns jointly only", "Owns both alone and jointly", "Other")
malawi$EDACHIEVER = factor(malawi$EDACHIEVER)
levels(malawi$EDACHIEVER) = c("No education", "Incomplete primary", "Complete primary", 
                              "Incomplete secondary", "Complete secondary", "Higher")
malawi$DECFEMEARN = factor(malawi$DECFEMEARN)
levels(malawi$DECFEMEARN) = c("Woman alone", "Woman and husband\\partner", "Woman and someone else", 
                              "Husband\\partner", "Someone else", "Mother or father", "Aunt or uncle",
                              "Family elders", "Someone else, unspecified", "Other or not applicable")
malawi$BHCPERMIT = factor(malawi$BHCPERMIT)
levels(malawi$BHCPERMIT) = c("Not a big problem", "No problem at all", "Small problem", "Is big problem")
malawi$BHCMONEY = factor(malawi$BHCMONEY)
levels(malawi$BHCMONEY) = c("Not a big problem", "No problem at all", "Small problem only", "Is big problem")
malawi$BHCDISTANCE = factor(malawi$BHCDISTANCE)
levels(malawi$BHCDISTANCE) = c("Not a big problem", "No problem at all", "Small problem", "Is big problem")
malawi$BHCALONE = factor(malawi$BHCALONE)
levels(malawi$BHCALONE) = c("Not a big problem", "No problem at all", "small problem only", "Is big problem")
malawi$BHCNOFEMDR = factor(malawi$BHCNOFEMDR)
levels(malawi$BHCNOFEMDR) = c("Not a big problem", "No problem at all", "small problem only", "Is big problem")
malawi$BHCNOPROV = factor(malawi$BHCNOPROV)
levels(malawi$BHCNOPROV) = c("Not a big problem", "No problem at all", "small problem only", "Is big problem")
malawi$INSCOVERYN = factor(malawi$INSCOVERYN)
levels(malawi$INSCOVERYN) = c("No", "Yes")

summary(malawi)
attach(malawi)

## Part 2: Exploratory Analysis ##

## Graphical summaries
## INJHCWORKER is outcome variable
summary(INJHCWORKER)
par(mfrow = c(1, 2))
plot(INJHCWORKER, main = "Injections Received")
plot(density(INJHCWORKER), main = "Injections Received") ## INJHCWORKER has 0 values; appears to be chi-square
## are these results on the upper end of the distribution outliers?

par(mfrow = c(1, 1))
qqnorm(INJHCWORKER) ## not normally distributed
## can't assume normal here; use permutation tests?

## Shapiro-Wil Test for Normal Distribution of INJHCWORKER
## H0: INJHCWORKER is normally distributed
## Ha: INJHCWORKER is not normally distributed
shapiro.test(INJHCWORKER)
## p-value is near zero; therefore reject the null; can't assume normal distribution

INJHCWORKER = sqrt(INJHCWORKER)
qqnorm(INJHCWORKER)

## Age
summary(AGE)
par(mfrow = c(1, 2))
plot(density(AGE), main = "Age")
qqnorm(AGE) ## age is approximately normal

par(mfrow = c(1, 1))
plot(AGE, INJHCWORKER, main = "Injections v Age") ## are the higher numbers outliers?  why would someone lie?
## why would someone need 50 injections in a year?

## Categorical variables
pdf("C:\\Users\\Chandler\\Desktop\\MATH 5387 Project Data\\categorical variables.pdf")
land_table = prop.table(table(FEMOWNLAND))*100
land_bar = barplot(land_table, xlab = "Land Ownership", ylab = "% Frequency")
text(land_bar, 0, round(land_table, 1), cex = 1, pos = 3)

edu_table = prop.table(table(EDACHIEVER))*100
edu_bar = barplot(edu_table, xlab = "Education", ylab = "% Frequency")
text(edu_bar, 0, round(edu_table, 1), cex = 1, pos = 3)

earn_table = prop.table(table(DECFEMEARN))*100
earn_bar = barplot(earn_table, xlab = "Decision over Woman's Earnings", ylab = "% Frequency")
text(earn_bar, 0, round(earn_table, 1), cex = 1, pos = 3)

perm_table = prop.table(table(BHCPERMIT))*100
perm_bar = barplot(perm_table, xlab = "Needs Permission to Go", ylab = "% Frequency")
text(perm_bar, 0, round(perm_table, 1), cex = 1, pos = 3)

mon_table = prop.table(table(BHCMONEY))*100
mon_bar = barplot(mon_table, xlab = "Has Enough Money to Go", ylab = "% Frequency")
text(mon_bar, 0, round(mon_table, 1), cex = 1, pos = 3)

dist_table = prop.table(table(BHCDISTANCE))*100
dist_bar = barplot(dist_table, xlab = "Distance to Healthcare Facility", ylab = "% Frequency")
text(dist_bar, 0, round(dist_table, 1), cex = 1, pos = 3)

alo_table = prop.table(table(BHCALONE))*100
alo_bar = barplot(alo_table, xlab = "Can Go to Healthcare Facility Alone", ylab = "% Frequency")
text(alo_bar, 0, round(alo_table, 1), cex = 1, pos = 3)

fem_table = prop.table(table(BHCNOFEMDR))*100
fem_bar = barplot(fem_table, xlab = "No Female Healthcare Provider", ylab = "% Frequency")
text(fem_bar, 0, round(fem_table, 1), cex = 1, pos = 3)

prov_table = prop.table(table(BHCNOPROV))*100
prov_bar = barplot(prov_table, xlab = "No Healthcare Provider", ylab = "% Frequency")
text(prov_bar, 0, round(prov_table, 1), cex = 1, pos = 3)

ins_table = prop.table(table(INSCOVERYN))*100
ins_bar = barplot(ins_table, xlab = "Has Inurance Coverage", ylab = "% Frequency")
text(ins_bar, 0, round(ins_table, 1), cex = 1, pos = 3)
dev.off()

## Part 3: Initial Regression Analysis ##
doct_mod0 = lm(INJHCWORKER ~ ., data = malawi)
summary(doct_mod0) ## NOTHING IS SIGNIFICANT EXCEPT AGE; skewed by younger or older women? 
summary(malawi)

## Intercept Interpretation: For a woman who: does not land, has no education, has the final
## say on spending her earnings, and has no insurance, and for whom getting permission, 
##lacking money, distance to facility, going alone, no female provider, no provider are not 
## big problems, the average number of injections received from a healthcare worker in the 
## 6-12 months preceding the survey was 1.63.

## Basesd on this and the graphical analysis, the "empowerment" variables seem like
## stronger predictors than the barrier variables.

## The scales on the barrier variables seem strange because one would think that "no problem
## at all" is smaller in magnitude than "not a big problem"

## Is there collinearity affecting DECFEMEARN?

doct_mod1 = lm(INJHCWORKER ~ AGE + FEMOWNLAND + EDACHIEVER + DECFEMEARN, data = malawi)
## this model excludes "barriers"
summary(doct_mod1)

doct_mod2 = lm(INJHCWORKER ~ AGE + BHCPERMIT + BHCMONEY + BHCDISTANCE + BHCALONE + BHCNOFEMDR + BHCNOPROV + INSCOVERYN, data = malawi)
summary(doct_mod2)

## Part 4: Model Selection & Testing
## F-test for overall significance
## H0: all coefficients equal zero
## Ha: at least one coefficient does not equal zero
summary(doct_mod0)
anova(doct_mod0)

ESS = anova(doct_mod0)[1, 2] + anova(doct_mod0)[2, 2] + anova(doct_mod0)[3, 2] + 
      anova(doct_mod0)[4, 2] + anova(doct_mod0)[5, 2] + anova(doct_mod0)[6, 2] + 
      anova(doct_mod0)[7, 2] + anova(doct_mod0)[8, 2] + anova(doct_mod0)[9, 2] +
      anova(doct_mod0)[10, 2] + anova(doct_mod0)[11, 2]
RSS = anova(doct_mod0)[12, 2]

f_stat = (ESS/(summary(doct_mod0)$df[1] - 1))/(RSS/(summary(doct_mod0)$df[2]))
pval_f = 1 - pf(f_stat, summary(doct_mod0)$df[1] - 1, summary(doct_mod0)$df[2])
## p-value is significant at 95% confidence level; reject H0, and data suggests that
## at least one coefficient does not equal zero

## Permutation Test, since outcome variable is likely not normal
f_obs = summary(doct_mod0)$fstat[1] ## observed f-stat from chosen model

reps = 5e3
f_perm = numeric(reps) ## intitialize permutation f-stat
for (i in 1:reps)
{
  doct0_perm = lm(sample(INJHCWORKER) ~ ., data = malawi)
  f_perm[i] = summary(doct0_perm)$fstat[1] 
}

mean(f_perm > f_obs) ## permutation p-value

## Variance Inflaction Factor Test for Collinearity
vif(doct_mod0) ## not concerned about collinearity

## Mean Squared Prediction Error
mspe0 = mean(doct_mod0$residuals^2)
rmspe0 = sqrt(mspe0)

## AIC and BIC for Model Selection
n = nrow(malawi)
## AIC
step(doct_mod1, direction = "backward", k = 2) ## do i still need leaps for this?
## minimum AIC is produced by AGE + FEMOWNLAND
step(doct_mod1, direction = "forward", k = 2) ## recommends all variables

## BIC
step(doct_mod1, direction = "backward", k = log(n)) ## recommends only AGE
step(doct_mod1, direction = "forward", k = log(n))  ## recommends all variables

## Based on regression results from model using all predictors and graphical analysis, our chosen model uses
## AGE, EDACHIEVER, FEMOWNLAND, and DECFEMEARN to predict INJHCWORKER

## F-test for joint significance
## Unrestricted model: all predictors
## Restricted model: only AGE plus "empowerment" variables, which are FEMOWNLAND,
## EDACHIEVER, DECFEMEARN

## H0: The restricted model is sufficient to explain variation in the INJHCWORKER variable
## Ha: The unrestricted model is necessary to explain variation in the INJHCWORKER variable

summary(doct_mod1)
summary(doct_mod1)$df
anova(doct_mod1)

SSEu = anova(doct_mod0)[12, 2]
SSEr = anova(doct_mod1)[5, 2]

joint_f = ((SSEr - SSEu)/(summary(doct_mod1)$df[2] - summary(doct_mod0)$df[2])) /
  (SSEu/summary(doct_mod0)$df[2])
pval_jointf = 1 - pf(joint_f, summary(doct_mod1)$df[2] - summary(doct_mod0)$df[2],
                     summary(doct_mod0)$df[2])
## p-value suggests failing to reject the null; the data provide sufficient evidence that
## the restricted model is sufficient to explain variation in the INJHCWORKER variable

## Permutation Test for Chosen Model
f_mod_obs = summary(doct_mod1)$fstat[1] ## observed f-stat from chosen model

reps = 5e3
f_mod_perm = numeric(reps) ## intitialize permutation f-stat
for (i in 1:reps)
{
  doct1_perm = lm(sample(INJHCWORKER) ~ AGE + FEMOWNLAND + EDACHIEVER + DECFEMEARN,
                  data = malawi)
  f_mod_perm[i] = summary(doct1_perm)$fstat[1] 
}

mean(f_mod_perm > f_mod_obs) ## permutation p-value

## Variance Inflaction Factor Test for Collinearity
vif(doct_mod1) ## not concerned about collinearity

## Mean Squared Prediction Error
mspe1 = mean(doct_mod1$residuals^2)
rmspe1 = sqrt(mspe)

## Part 5: Regression Diagnostics ##
## Check for constant error variance
## Plot residuals v fitted values
par(mfrow = c(2, 2))
plot(doct_mod1$fitted.values, doct_mod1$residuals, 
     main = "Residuals v Fitted Values", xlab = "Fitted", ylab = "Residuals")
abline(0, 0, col = "red")
## clear problem here

plot(doct_mod1$fitted.values, sqrt(abs(doct_mod1$residuals)),
     main = bquote(paste("Residuals v ", (sqrt(abs("Fitted"))))), 
     xlab = expression(sqrt(abs("Fitted"))), ylab = "Residuals", 
     ylim = c(-0.1, 3.1))
abline(0, 0, col = "red")
## clear problem

plot(AGE, doct_mod1$residuals,
     main = "Residuals v Age", xlab = "Age", ylab = "Residuals")
abline(0, 0, col = "red")
## closely resembles the scatterplot of AGE and INJHCWORKER

## Check for normal residuals
par(mfrow = c(1, 1))
qqnorm(doct_mod1$residuals)
qqline(doct_mod1$residuals, col = "red")

## residuals aren't normally distributed

## Check for correlated residuals
n = length(residuals(doct_mod1))
plot(tail(residuals(doct_mod1),n-1) ~ head(residuals(doct_mod1),n-1), 
     main = bquote(paste(hat(epsilon[i+1]), " v ", hat(epsilon[i]))),
     xlab= expression(hat(epsilon)[i]), ylab=expression(hat(epsilon)[i+1]))
abline(h=0,v=0,col=grey(0.75))
abline(0, 1, col = "red")

## residuals are clearly correlated

## Model Structure
avPlots(doct_mod1)
crPlots(doct_mod1)

detach(malawi)

## Testing the Model in Zambia ##
zambia = africa[africa$COUNTRY == 894,]
zambia[, 1:12] = NULL; zambia["BIRTHYEAR"] = NULL; zambia["RESIDENT"] = NULL
any(zambia == 96) ## inconsistent, returns false
any(zambia == 97) ## respondent doesn't know, returns false
any(zambia == 98) ## missing values, returns true
any(zambia == 99) ## not in universe, returns true
summary(zambia) ## Max = 99.00 in categorical variables indicate NIU
table(zambia$FEMOWNLAND) ## 33 observations are "98", none are "99"
table(zambia$EDACHIEVER) ## 10 values are "8"
table(zambia$DECFEMEARN) ## 13 values are are "98", 12,744 are "99"
table(zambia$INJHCWORKER) ## 2 values are "98", 11,453 are "99"
table(zambia$BHCPERMIT) ## 24 values are "98"
table(zambia$BHCMONEY) ## 18 values are "98"
table(zambia$BHCDISTANCE) ## 19 values are "98"
table(zambia$BHCALONE) ## 22 values are "98"
table(zambia$BHCNOFEMDR) ## 28 values are "98"
table(zambia$BHCNOPROV) ## 19 values are "98"
table(zambia$INSCOVERYN) ## 11 values are "8"
zambia = subset(zambia, FEMOWNLAND != 98 & EDACHIEVER != 8 & INJHCWORKER != 98 & INJHCWORKER != 99 & 
                DECFEMEARN != 98 & DECFEMEARN != 99 & BHCPERMIT != 98 & BHCDISTANCE != 98 & BHCALONE != 98 &
                BHCNOFEMDR != 98 & BHCNOPROV != 98 & INSCOVERYN != 8)
any(zambia == 98) ## returns false
any(zambia == 99)
summary(zambia)

zamb_test = lm(INJHCWORKER ~ AGE + FEMOWNLAND + EDACHIEVER + DECFEMEARN, data = zambia)
summary(zamb_test)

mspe2 = mean(zamb_test$residuals^2)
rmspe2 = sqrt(mspe2)
