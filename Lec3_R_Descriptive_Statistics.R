# Lecture 03. Descriptive Statistics Using R
## Objective: summarizing, describing and presenting data
## Example data: iris

# 1. Load data and view
dat <- iris
head(dat) # first 6 observations
str(dat) # structure of dataset

# 2. Some statistics
min(dat$Sepal.Length)
max(dat$Sepal.Length)
range(dat$Sepal.Length)

## use function
range2 <- function(x) {
  range <- max(x) - min(x)
  return(range)
}
range2(dat$Sepal.Length)

mean(dat$Sepal.Length, na.rm = TRUE)  #compute the mean with the NA excluded
median(dat$Sepal.Length)
quantile(dat$Sepal.Length, 0.5)  # 50 percentile
IQR(dat$Sepal.Length) # the difference between the first and third quantile
temp <- quantile(dat$Sepal.Length, 0.75)-quantile(dat$Sepal.Length, 0.25)
temp
sd(dat$Sepal.Length) # standard deviation
var(dat$Sepal.Length) # variance

## use sapply, lapply libraries
sapply(dat[, 1:4], sd) # df 각 열의 함수(sd)를 벡터 또는 행렬 형식으로 출력 
lapply(dat[, 1:4], sd) # df 각 열의 함수(sd)를 list 형식으로 출력 

# 3. Summary statistics
summary(dat)
by(dat, dat$Species, summary) # summary by group

## use package "pastecs" for stat.desc() library
install.packages("pastecs")
library(pastecs)
stat.desc(dat, norm = TRUE)

# 4. mode
tab <- table(dat$Sepal.Length) # number of occurrences for each unique value
sort(tab, decreasing = TRUE) # sort highest to lowest

# 5. Plot
hist(dat$Sepal.Length)
boxplot(dat$Sepal.Length)
boxplot(dat$Sepal.Length ~ dat$Species)
plot(dat$Sepal.Length,type = "l") # "l" for line
plot(density(dat$Sepal.Length))

# Draw points on the qq-plot:
qqnorm(dat$Sepal.Length)
# Draw the reference line:
qqline(dat$Sepal.Length)

library(car) # package must be installed first
qqPlot(dat$Sepal.Length)

library(ggpubr)
ggqqplot(dat$Sepal.Length)

## use ggplot2

library(ggplot2)

ggplot(dat) +
  aes(x = Sepal.Length) +
  geom_histogram(bins = 30)

ggplot(dat) +
  aes(x = Species, y = Sepal.Length) +
  geom_boxplot()

ggplot(dat) +
  aes(x = Sepal.Length, y = Petal.Length) +
  geom_point()

ggplot(dat) +
  aes(x = Sepal.Length, y = Petal.Length, colour = Species) +
  geom_point() +
  scale_color_hue()

ggplot(dat) +
  aes(x = Sepal.Length) +
  geom_density()
