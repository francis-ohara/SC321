# load the lego dataset
lego_data <- read.csv("data/lego_sample.csv")
head(lego_data)

# preprocess Price data
lego_data$Price <- as.numeric(substr(lego_data$Price, 2, nchar(lego_data$Price)))

# create linear regression model for prediction Price from number of Pieces
model <- lm(Price ~ Pieces, data = lego_data)
summary(model) # contains F-test and t-test

# plot data and fitted line
plot(Price ~ Pieces, data = lego_data)
abline(model)

# test for correlation
cor.test(Pieces, Price)

# compute 90 % confidence interval
new_x <- data.frame(Pieces = 200)
predict.lm(model, new_x, interval = "confidence", level = 0.9)

# for 90 % prediction interval
predict.lm(model, new_x, interval = "prediction", level = 0.9)
