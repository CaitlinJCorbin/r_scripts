###################
# Sample R Script #
###################

install.packages("carData")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("janitor")
library(carData)
library(ggplot2)
library(dplyr)
library(janitor)

##############
# Question 1 #
##############

table = carData::UN
attach(UN)
# a
plot(UN$ppgdp, UN$fertility)
regression <- lm(fertility ~ ppgdp, data = carData::UN)
abline(regression, col = 'red')
# I don't think this graph is linear due to the left skewed curvature

# b & c & d & e
plot(log(UN$ppgdp), log(UN$fertility), xlab = "ppgdp", ylab = "Fertility")
regression2 <- lm(log(fertility) ~ log(ppgdp), data = carData::UN)
summary(regression2)
abline(regression2, col = 'green')
# This plot does resemble a linear function
# The slope appears to be saying as the ppgdp get's higher, fertility gets lower.

myPlot <- ggplot(data = carData::UN,
          mapping = aes(x = log(ppgdp), y = log(fertility), col = region)) +
          geom_point() +
          geom_smooth(method = 'lm', col = 'light green') +
          ggtitle("UN Data") +
          xlab("ppgdp") +
          ylab("fertility")
print(myPlot)

##############
# Question 2 #
##############

# a - Test the hypothesis that the slope is 0 versus the alternative that it is not zero (a two-tailed test).
# H0: B1 = 0, H1: B1 != 0

# Putting stuff into a able with dplyr
df1 <- table %>%
  select(fertility, ppgdp) %>%
  group_by(fertility)
df1 <- df1 %>% remove_empty("rows")
View(df1)
t.test(data = df1, y = log(fertility), x = log(ppgdp) , mu = 0, alt = 'two.sided', conf.level = 0.97)
# p-value is 2.2e-16, because our alpha is .03, we can safely reject the NULL

# b - Test the hypothesis that the slope is greater than or equal to 0 versus negative (a one-sided test).
# Use alpha = 0.03, conclude and interpret your result.
# H0: B1 < 0, H1: B1 >= 0
t.test(data = df1, y = log(fertility), x = log(ppgdp) , mu = -1, alt = 'greater', conf.level = 0.97)
# The p-value is 2.2e-16, which is very small. Since our alpha is 0.03, we reject the NULL
# Comment for a and b: the slope is -0.20715 and the intercept is 2.66551

# c - Give the value of the coefficient of determination, and explain its meaning
# Multiple R-squared:  0.526,	Adjusted R-squared:  0.5236 
# This basically is saying how linear the data is. Close to 1 means the points are closer
# together than 0 (meaning they are all random). Because the value is .5 -ish, the data is kind of
# linear, but the correlation isn't very high.

# d - For a locality not in the data with ppgdp = 1000, what is the predicted log(fertility)? In
# addition, obtain a 95% prediction interval for the response.
predict(regression2, data.frame(ppgdp = 1000))
# 1.234567 
predict(regression2, data.frame(ppgdp = 1000), interval = 'prediction')
#    fit       lwr      upr
# 1.234567 0.6258791 1.843256

# e - For a locality not in the data with ppgdp = 1000, obtain a 95% prediction interval for fertility.
# Hint: If (a, b) is a prediction interval for log(fertility), then (exp(a), exp(b)) is the
# corresponding interval for fertility.

predict(regression, data.frame(ppgdp = 1000))
# 3.1459 

##############
# Question 3 #
##############

# Use data frame Davis from package carData. Using the simple linear regression model, we are interested
# in studying changes in the height (response) based on the knowledge of the weight (predictor).
carData::Davis
attach(Davis)

# a - Construct the regression line and interpret the intercept and slope.
# AND
# b - Draw the scatterplot of height on the vertical axis versus weight on the horizontal axis and add
# the regression line to the plot. Does the line seem to represent the scatterplot? Explain.

davisRegression <- lm(height ~ weight, data = Davis)
summary(davisRegression)

myPlotDavis <- ggplot(data = Davis,
                 mapping = aes(x = weight, y = height, col = sex)) +
  geom_point() +
  geom_smooth(method = 'lm', col = 'pink') +
  ggtitle("Height to Weight Graph") +
  xlab("weight") +
  ylab("height")
print(myPlotDavis)

# intercept: 160.09312, slope: 0.15086 (slope is an upward trend)
# I used ggplot2 to see a graph of the regression model. It appears that the more you weigh, the taller you are (or vise-versa).
# Also, men are typically taller and weight more. There is an outlier though.
# The line does appear to represent the scatterplot.

# c - Perform a test of significance for the slope. Does weight help to explain height?
# I'm not sure exactly what I am supposed to be testing? Here is a correlation test. I do
# believe that weight DOES explain height.

cor.test(x = weight, y = height, data = Davis)

# 	Pearson's product-moment correlation
# data:  weight and height
# t = 2.7179, df = 198, p-value = 0.007152
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.05228435 0.31997151
# sample estimates:
#  cor 
# 0.1896496 


# d - How much of the height variation is explained by this model
# Multiple R-squared:  0.03597,	Adjusted R-squared:  0.0311 
# There is a significant amount of variation

# Observation 12 seems to be wrongly recorded. Let's use the data without this observation1 and
# repeat parts a - d. Do your results change in any meaningful way? Explain

# Putting stuff into a able with dplyr (extra)
df2 <- Davis %>%
  select(height, weight) %>%
  filter(!row_number() %in% c(12)) %>%
  group_by(height)
df2 <- df2 %>% remove_empty("rows")
View(df2)

davisRegression2 <- lm(height ~ weight, data = df2)
summary(davisRegression2)

myPlotDavis2 <- ggplot(data = df2,
                      mapping = aes(x = weight, y = height, col = height)) +
  geom_point() +
  geom_smooth(method = 'lm', col = 'light green') +
  ggtitle("Height to Weight Graph") +
  xlab("weight") +
  ylab("height")
print(myPlotDavis2)

cor.test(x = weight, y = height, data = df2)

# There is a significant difference in this new graph, because the outlier has been removed. It is more linear.

res <- resid(davisRegression2)
plot(res, col = 'blue')
abline(0, 0, col = 'red')

# The residual plot closely resembles a NULL plot



