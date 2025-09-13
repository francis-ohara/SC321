legos <- read.csv("lesson_1/data/lego_sample.csv")
print(head(legos))

# preprocess price column
legos$Price <- as.numeric(substr(legos$Price, 2, length(legos$Price)))
print(head(legos))

# fit a linear model
lego_model <- lm(legos$Price ~ legos$Pieces, data = legos)
summary(lego_model)

# plot RVF plot for model
residuals <- lego_model$residuals
fitted_values <- lego_model$fitted.values
plot(fitted_values, residuals)
abline(h=0)


# plot qqplot for model
qqnorm(lego_model)
