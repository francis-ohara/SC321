# PROBLEM 1
# load libraries and datasets
library(Stat2Data)
library(mosaic)
data(Handwriting)

head(Handwriting)

# plot correct guessing ability for each gender
boxplot(Survey2 ~ Gender, data = Handwriting)

# compute summary statistics for each group
favstats(Survey2 ~ Gender, data=Handwriting)

# Calculate residuals for each gender group
# The predicted value for each group is its mean
Handwriting <- Handwriting %>%
  group_by(Gender) %>%
  mutate(Resids = Survey2 - mean(Survey2, na.rm = TRUE)) %>%
  ungroup()

Handwriting$Gender <- as.character(Handwriting$Gender)

# plot histogram of residuals
histogram(~Resids|Gender, data=Handwriting)


# perform 2-sample t-test
t.test(Survey2~Gender, data=Handwriting, var.equal=TRUE)



# PROBLEM 2
# load Faces dataset
data(Faces)
head(Faces)

# construct a linear regression model
model <- lm(MaxGripStrength ~ Attractive, data = Faces)
summary(model)



# PROBLEM 3
# load Volts dataset
data(Volts)
head(Volts)

# plot Voltage vs Time
plot(Voltage ~ Time, data = Volts)

# construct linear regression model
model <- lm(Voltage ~ Time, data = Volts)

# plot RVF plot for model
plot(model$residuals ~ model$fitted.values,
  main = "RVF Plot of Voltage ~ Time"
)
abline(h = 0, col = "red")

# log transform Voltage
Volts$log_voltage <- log(Volts$Voltage)
head(Volts)

# plot Log Voltage vs Time
plot(log_voltage ~ Time, data = Volts)

# construct linear regression model predicting Log Voltage from Time
model <- lm(log_voltage ~ Time, data = Volts)
summary(model)


# plot RVF plot for Log Voltage vs Time model
plot(model$residuals ~ model$fitted.values)
abline(h = 0, col = "red")



# PROBLEM 4
# load the Caterpillars dataset
data(Caterpillars)
head(Caterpillars)

# scatterplot of Nassim vs. Mass
plot(Nassim ~ Mass, data = Caterpillars)

# scatterplot of logNassim vs. LogMass
plot(LogNassim ~ LogMass, data = Caterpillars)

# fit linear model for logNassim vs LogMass
model <- lm(LogNassim ~ LogMass, data = Caterpillars)
summary(model)

# plot scatterplot of LogNassim vs LogMass with char. coding by Instar
plot(LogNassim ~ LogMass, data = Caterpillars, pch = as.numeric(Instar))

# plot scatterplot of LogNassim vs LogMass with char. coding by Instar
plot(LogNassim ~ LogMass, data = Caterpillars, pch = as.numeric(Fgp))
legend("topleft", legend = levels(Caterpillars$Fgp), 
       pch = 1:length(levels(Caterpillars$Fgp)), title = "FGP")



# PROBLEM 5
# load Retirement dataset
data(Retirement)
head(Retirement)

# fit linear regression model for SRA ~ Year
model_with_outliers <- lm(SRA ~ Year, data = Retirement)

# compute Standardized residuals to easily identify outliers
rstandard(model_with_outliers)
model_with_outliers$residuals

# Select outlier years based on standardized residuals
print(Retirement$Year[c(7, 15)])


# remove outliers
Retirement <- Retirement[-c(7, 15), ]

# fit linear model
model_without_outliers <- lm(SRA ~ Year, data = Retirement)
plot(SRA ~ Year, data = Retirement)
abline(model_without_outliers, col = "green")
abline(model_with_outliers, col = "red")
legend("topleft", legend=c("Model with Outliers", "Model without Outliers"),
  col = c("red", "green"),
  lty = 1,
  bty = "n"
 )