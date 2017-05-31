#############################################################################
#   Forecasting Unemployment Rate Using Economic and Political Indicators   # 
#############################################################################

rm(list=ls())

# read the csv file (all rows and all columns)
data <- read.table("Forecasting_Project_Unemployment_csv2.csv", header = T, sep=",", stringsAsFactors = F)

head(data)
tail(data)

# summary statistics and classifications
summary(data)

# define following categorical variables as factors
names <- c('Democratic.President' ,'Democratic.Senate' ,'Democratic.House' ,'Jan' ,'Feb' ,'Mar' ,'Apr' ,'May' ,'Jun' ,'Jul' ,'Aug' , 'Sep' ,'Oct' ,'Nov')
data[,names] <- lapply(data[,names] , factor)
str(data)

summary(data)

# select only the necessary rows and columns
data.1 <- data[(1:333), -(29:32)]
head(data.1)
tail(data.1)

# remove Date (time) variable from data frame
data.1 <- data.1[, (2:28)]
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
data['t_CPI'] <- log1p(data['CPI'])
data['t_CONSUMER.SENTIMENT'] <- log1p(data['CONSUMER.SENTIMENT'])
data['t_CONSUMER.CONFIDENCE'] <- log1p(data['CONSUMER.CONFIDENCE'])
data['t_TRADE.BALANCE'] <- log1p(data['TRADE.BALANCE'] - min(data['TRADE.BALANCE']))
data['t_X10.YR.YIELD'] <- log1p(data['X10.YR.YIELD'])
data['t_DOLLAR'] <- log1p(data['DOLLAR'])
data['t_OIL'] <- log1p(data['OIL'])
data['t_COPPER'] <- log1p(data['COPPER'])
data['t_GOLD'] <- log1p(data['GOLD'])
data['t_TRUCK.SALES'] <- log1p(data['TRUCK.SALES'])
data['t_NEW.HOME.SALES'] <- log1p(data['NEW.HOME.SALES'])
data['t_PRIVATE.CONSTRUCTION'] <- log1p(data['PRIVATE.CONSTRUCTION'])
head(data)
tail(data)

# view the transformed data variables in a data tab with the new columns
View(data)

data.log <- data.frame(data[,(33:45)], data[,(15:28)])
summary(data.log)


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
pairs.panels(data.log[,(1:13)])


# now we can use variance-infation-factor (VIF) to determine whether
# the correlations for the predictors are statistically significant or not

install.packages("faraway")
library(faraway)

# Note VIF values of 5 or greater suggest high multicollinearity;
# VIFs of 10 or greater suggest severe multicollinearity
# Stepwise regression may not be suitable for data with high multicollinearity

vif(mymodel.log)


##### Some more model tests based on logarithmic transformations #####
# Test full model_1
mymodel.log <- lm(t_UNEMPLOYMENT ~., data = data.log)

mymodel.log
summary(mymodel.log)
vif(mymodel.log)

# Test model_2
mymodel.log_2 <- lm(t_UNEMPLOYMENT ~. -t_DOLLAR-t_OIL, data = data.log)

mymodel.log_2
summary(mymodel.log_2)
vif(mymodel.log_2)

plot(mymodel.log_2)

# Test model_3; linearity and normality assumptions are now broken; see model_4 below
mymodel.log_3 <- lm(t_UNEMPLOYMENT ~. -t_CPI-t_CONSUMER.SENTIMENT-t_DOLLAR-t_OIL-t_TRUCK.SALES-t_COPPER
              -t_GOLD-t_TRADE.BALANCE-t_X10.YR.YIELD-Democratic.Senate-Democratic.House, 
              data = data.log)

mymodel.log_3
summary(mymodel.log_3)
vif(mymodel.log_3)

plot(mymodel.log_3)

# Test model_4, note removing a multitude of variables can improve
# the issue of multicollinearity (use VIF function to test)
# **however, note that the linearity and normality assumptions are now broken
# **therefore, trying to make the model less multicollinear can have further
# **consquences on our assumptions; it is best to check the independence next
mymodel.log_4 <- lm(t_UNEMPLOYMENT ~. -t_CPI-t_CONSUMER.SENTIMENT-t_DOLLAR-t_OIL-t_TRUCK.SALES-t_COPPER
              -t_GOLD-t_TRADE.BALANCE-t_X10.YR.YIELD-t_NEW.HOME.SALES
              -Democratic.Senate-Democratic.House, data = data.log)

mymodel.log_4
summary(mymodel.log_4)
vif(mymodel.log_4)

plot(mymodel.log_4)

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

c1 <- cbind(data$t_CPI)
d_CPI <- diff(c1, differences = 1)
head(d_CPI)

cs <- cbind(data$t_CONSUMER.SENTIMENT)
d_CONSUMER.SENTIMENT <- diff(cs, differences = 1)
head(d_CONSUMER.SENTIMENT)

cc <- cbind(data$t_CONSUMER.CONFIDENCE)
d_CONSUMER.CONFIDENCE <- diff(cc, differences = 1)
head(d_CONSUMER.CONFIDENCE)

tb <- cbind(data$t_TRADE.BALANCE)
d_TRADE.BALANCE <- diff(tb, differences = 1)
head(d_TRADE.BALANCE)

yd <- cbind(data$t_X10.YR.YIELD)
d_X10.YR.YIELD <- diff(yd, differences = 1)
head(d_X10.YR.YIELD)

dl <- cbind(data$t_DOLLAR)
d_DOLLAR <- diff(dl, differences = 1)
head(d_DOLLAR)

ol <- cbind(data$t_OIL)
d_OIL <- diff(ol, differences = 1)
head(d_OIL)

cp <- cbind(data$t_COPPER)
d_COPPER <- diff(cp, differences = 1)
head(d_COPPER)

au <- cbind(data$t_GOLD)
d_GOLD <- diff(au, differences = 1)
head(d_GOLD)

tr <- cbind(data$t_TRUCK.SALES)
d_TRUCK.SALES <- diff(tr, differences = 1)
head(d_TRUCK.SALES)

hs <- cbind(data$t_NEW.HOME.SALES)
d_NEW.HOME.SALES <- diff(hs, differences = 1)
head(d_NEW.HOME.SALES) 

pc <- cbind(data$t_PRIVATE.CONSTRUCTION)
d_PRIVATE.CONSTRUCTION <- diff(pc, differences = 1)
head(d_PRIVATE.CONSTRUCTION)
tail(d_PRIVATE.CONSTRUCTION)

# create data frame of the first differences
data.diff <- data.frame("UNEMPLOYMENT" = (d_UNEMPLOYMENT), "CPI" = (d_CPI), 
                         "CONSUMER.SENTIMENT" = (d_CONSUMER.SENTIMENT), 
                         "CONSUMER.CONFIDENCE" = (d_CONSUMER.CONFIDENCE), 
                         "TRADE.BALANCE" = (d_TRADE.BALANCE), 
                         "X10.YR.YIELD" = (d_X10.YR.YIELD),
                         "DOLLAR" = (d_DOLLAR), 
                         "OIL" = (d_OIL), 
                         "COPPER" = (d_COPPER),
                         "GOLD" = (d_GOLD), 
                         "TRUCK.SALES" = (d_TRUCK.SALES), 
                         "NEW.HOME.SALES" = (d_NEW.HOME.SALES), 
                         "PRIVATE.CONSTRUCTION" = (d_PRIVATE.CONSTRUCTION), 
                         "Democratic.President" = (data$Democratic.President[2:333]), 
                         "Democratic.Senate" = (data$Democratic.Senate[2:333]), 
                         "Democratic.House" = (data$Democratic.House[2:333]), 
                         "Jan" = (data$Jan[2:333]), "Feb" = (data$Feb[2:333]), "Mar" = (data$Mar[2:333]), 
                         "Apr" = (data$Apr[2:333]), "May" = (data$May[2:333]), "Jun" = (data$Jun[2:333]), 
                         "Jul" = (data$Jul[2:333]), "Aug" = (data$Aug[2:333]), "Sep" = (data$Sep[2:333]), 
                         "Oct" = (data$Oct[2:333]), "Nov" = (data$Nov[2:333]))

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


# Conduct test on a reduced model; notice that the R-Squared does not change much;
# (due to all the other variables having been insignificant to the model);
# coefficients for the predictors do change slightly
# Test model.fd_5 on reduced model of first differences
# note we are removing May, Jul, and Nov & keeping Priv Construction & Dem. President
# note also that since we do not include a dummy variable for Dec,
# that is the y-intercept for our model
mymodel.fd_5 <- lm(UNEMPLOYMENT ~ PRIVATE.CONSTRUCTION 
              + Democratic.President + Jan + Feb + Mar + Apr + Jun 
              + Aug + Sep + Oct, data = data.diff)

summary(mymodel.fd_5)
vif(mymodel.fd_5)

plot(mymodel.fd_5)

# Conversely remove just two predictors; remove dollar and oil, the model R-squared
# does not change much, those variables having been insignificant; however
# the coefficients of the predictors do change slightly
mymodel.fd_6 <- lm(UNEMPLOYMENT ~. -DOLLAR-OIL, data = data.diff)

summary(mymodel.fd_6)
vif(mymodel.fd_6)

plot(mymodel.fd_6)

# Note, however, that if we remove all of the seasonal dummy variables which are
# highly significant, and keep just two of the significant quantitative variables,
# R-Squared now declines to 0.03, indicating that we must account for seasonality
mymodel.fd_7 <- lm(UNEMPLOYMENT ~ PRIVATE.CONSTRUCTION 
                   + Democratic.President, data = data.diff)

summary(mymodel.fd_7)
vif(mymodel.fd_7)

plot(mymodel.fd_7)


# confidence intervals for log transformed, first differences multiple linear
# regression model
# let's use model.fd_5 because it eliminates all of the insignificant variables
# with large p-values
confint(mymodel.fd_5, conf.level = 0.95)









