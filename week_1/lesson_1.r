# install and import required packages
# install.packages(c("mosaic", "Stat2Data"))
library(Stat2Data)
library(mosaic)

# import CSV dataset
legos <- read.csv("lesson_1/data/lego_sample.csv")
head(legos)

# plot data
data(WeightLossIncentive4)
boxplot(WeightLoss~Group, data = WeightLossIncentive4,
        main="TITLE OF PLOT",
        xlab="xlabel",
        ylab="ylabel")

# compute summary statistics
favstats(WeightLoss~Group, data = WeightLossIncentive4)

# read miterm data
midterm <- read.csv("lesson_1/data/MidtermFinal.csv")
head(midterm)

lm(midterm$Final ~ midterm$Midterm)