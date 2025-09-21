# QUESTION 1
# load the dataset
library(Stat2Data)
data(RailsTrails)
head(RailsTrails)

# fit linear model
model <- lm(Adj2007 ~ SquareFeet, data=RailsTrails) 
summary(model)

# 90 % confidence interval for slope
confint(model, 2, level=0.9)


# QUESTION 2
# load the dataset
data(LeafWidth)
head(LeafWidth)

# t-test for correlation between Width and Year
cor.test(LeafWidth$Width, LeafWidth$Year)


# linear model to predict Width from Year
model <- lm(Width ~ Year, data=LeafWidth)
summary(model)

anova(model)

# QUESTION 3
data(MothEggs)
head(MothEggs)

# compute correlation between bodymass and eggs
cor.test(MothEggs$BodyMass, MothEggs$Eggs)

# fit linear regression model
model <- lm(Eggs ~ BodyMass, data = MothEggs)
summary(model)

# identify outlier with qqplot
qqnorm(model$residuals)
qqline(model$residuals)

rstandard(model)

MothEggs[39, ]


# QUESTION 4
data(MothEggs)
head(MothEggs)

# remove outlier
MothEggs <- MothEggs[-39,]

# fit linear model for predicting eggs from body mass
model <- lm(Eggs ~ BodyMass, data = MothEggs)
summary(model)

# RVF Plot
plot(model$fitted.values, model$residuals)
abline(h = 0)

# qqnorm plot
qqnorm(model$residuals)
qqline(model$residuals)



# QUESTION 5
data(TextPrices)
head(TextPrices)

# fit linear regression model
model <- lm(Price ~ Pages, data = TextPrices)
summary(model)

# predict with 95% confidence interval
new_x <- data.frame(Pages = 450)
predict.lm(model, new_x, interval = "confidence", level = 0.95)

# predict with 95% prediction interval
predict.lm(model, new_x, interval = "prediction", level = 0.95)

# predict with 95% confidence interval for 1500-page book
new_x <- data.frame(Pages = 1500)
predict.lm(model, new_x, interval = "prediction", level = 0.95)


# QUESTION 6
# load the dataset
data(BaseballTimes2017)
head(BaseballTimes2017)

# compute correlation for each predictor variable
cor(BaseballTimes2017[, c(3, 4, 5, 6, 7)])


# fit linear regression model for predicting time based on runs
model <- lm(Time ~ Runs, data = BaseballTimes2017)
summary(model)


# perform t-test for correlation of Runs to Time
cor.test(BaseballTimes2017$Runs, BaseballTimes2017$Time)


# RVF plot
plot(model$fitted.values, model$residuals)
abline(h=0)


# Normal Q-Q plot
qqnorm(model$residuals)
qqline(model$residuals)