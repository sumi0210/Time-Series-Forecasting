#############################################################################
#   Forecasting Unemployment Rate Using Economic and Political Indicators   # 
#############################################################################

rm(list=ls())
graphics.off()

# read the csv file (all rows and all columns)
data <- read.table("Forecasting_Project_Okun's Law.csv", header = T, sep=",", stringsAsFactors = F)

head(data)
tail(data)

# summary statistics and classifications
summary(data)

# define following categorical variables as factors
names <- c('CYCLES' ,'Jan' ,'Feb' ,'Mar' ,'Apr' ,'May' ,'Jun' ,'Jul' ,'Aug' , 'Sep' ,'Oct' ,'Nov')
data[,names] <- lapply(data[,names] , factor)
str(data)

summary(data)

# select only the necessary rows and columns
data.1 <- data[(1:272), -(1)]
head(data.1)
tail(data.1)


# updated summary statistics with categorical variables as factors
summary(data.1)


# create linear regression model
mymodel <- lm(UNEMPLOYMENT ~., data = data.1)
mymodel

# summary statistics for model (we can see a few high p-values)
summary(mymodel)


#######################################################################
#  Checking our assumptions                                           #
#######################################################################

# check assumptions using the residuals plot and normal Q-Q plot
# use residuals plot to test linearity and constant variance
# use normal Q-Q plot to test for normality of the residuals
graphics.off()
plot(mymodel)


# linearity assumption was violated
# transform the predictor and dependent variables; note TRADE.BALANCE
# contains negative values and must be transformed using a constant of 1 - min(x)
data['t_UNEMPLOYMENT'] <- log1p(data['UNEMPLOYMENT'])
data['t_GDP'] <- log1p(data['GDP'])

head(data)
tail(data)

# view the transformed data variables in a data tab with the new columns
View(data)

data.log <- data.frame(data[,(4:17)])
summary(data.log)
View(data.log)


# Log transformed linear model
mymodel.log <- lm(t_UNEMPLOYMENT ~., data = data.log)
mymodel.log
summary(mymodel.log)

# examine residuals plot and normal Q-Q plot on log transformed variables
plot(mymodel.log)


# correlations (we can see some high correlations)
cor(data.log[sapply(data.log, is.numeric)])


# install R package "pysch" to obtain the pairs scatterplot matrix
install.packages("psych")
library(psych)

# we can see the relationship between the variables in the pairs matrix
pairs.panels(data.log[,(13:14)])


# now we can use variance-infation-factor (VIF) to determine whether
# the correlations for the predictors are statistically significant or not

install.packages("faraway")
library(faraway)

# Note VIF values of 5 or greater suggest high multicollinearity;
# VIFs of 10 or greater suggest severe multicollinearity
# Stepwise regression may not be suitable for data with high multicollinearity

vif(mymodel.log)


##### Some more model tests based on logarithmic transformations #####
mymodel.log_1 <- lm(t_UNEMPLOYMENT ~ + t_GDP + CYCLES, data = data.log)
mymodel.log_1
summary(mymodel.log_1)
vif(mymodel.log_1)

plot(mymodel.log_1)

mymodel.log_2 <- lm(t_UNEMPLOYMENT ~ + t_GDP, data = data.log)
mymodel.log_2
summary(mymodel.log_2)
vif(mymodel.log_2)

plot(mymodel.log_2)

##### End of model tests on logarithmic transformed variables #####


# Test assumption #4 independence (or autocorrelation)
# install R package "lmtest" to test the Durbin-Watson statistic
install.packages("lmtest")
library(lmtest)
dwtest(mymodel.log)


# if durbin-watson statistic is such that the p-value < alpha,
# meaning that rho is not equal to zero (signifies autocorrelation),
# conduct first differences of the variables;
un <- cbind(data$t_UNEMPLOYMENT)
d_UNEMPLOYMENT <- diff(un, differences = 1)
head(d_UNEMPLOYMENT)

GDP <- cbind(data$t_GDP)
d_GDP <- diff(GDP, differences = 1)
head(d_GDP)

# create data frame of the first differences
data.diff <- data.frame("UNEMPLOYMENT" = (d_UNEMPLOYMENT), "GDP" = (d_GDP), 
                         "CYCLES" = (data$CYCLES[2:272]),
                         "Jan" = (data$Jan[2:272]), "Feb" = (data$Feb[2:272]), "Mar" = (data$Mar[2:272]), 
                         "Apr" = (data$Apr[2:272]), "May" = (data$May[2:272]), "Jun" = (data$Jun[2:272]), 
                         "Jul" = (data$Jul[2:272]), "Aug" = (data$Aug[2:272]), "Sep" = (data$Sep[2:272]), 
                         "Oct" = (data$Oct[2:272]), "Nov" = (data$Nov[2:272]))

# first and last six rows of first differences data frame
head(data.diff)
tail(data.diff)
summary(data.diff)


# Test model.fd; run linear regression model on the first differences
mymodel.fd <- lm(UNEMPLOYMENT ~., data = data.diff)
summary(mymodel.fd) 
vif(mymodel.fd)

# test the durbin-watson statistic 
dwtest(mymodel.fd)

# check assumptions using the residuals plot and normal Q-Q plot on
# first differences model
plot(mymodel.fd)


# Conduct test on a reduced model
mymodel.fd_1 <- lm(UNEMPLOYMENT ~ + GDP + CYCLES, data = data.diff)
mymodel.fd_1
summary(mymodel.fd_1)
vif(mymodel.fd_1)

plot(mymodel.fd_1)

mymodel.fd_2 <- lm(UNEMPLOYMENT ~ + GDP, data = data.diff)
mymodel.fd_2
summary(mymodel.fd_2)
vif(mymodel.fd_2)

plot(mymodel.fd_2)


mymodel.fd_3 <- lm(UNEMPLOYMENT ~. -GDP, data = data.diff)
summary(mymodel.fd_3) 
vif(mymodel.fd_3)

plot(mymodel.fd_3)

# confidence intervals for log transformed, first differences multiple linear
# regression model
# let's use model.fd because it includes the seasons which we determined to be
# very important to our model
confint(mymodel.fd, conf.level = 0.95)

# test model.fd_3
confint(mymodel.fd_3, conf.level = 0.95)








