
## libraries

library(ggpubr)
library(ggplot2)
library(ggstatsplot)
library(report)
library(performance)
library(dplyr)
library(car)
library(agricolae)
library(lubridate)


## read data

data <- read.csv('C:/Users/vidheesha/OneDrive/Desktop/Vidheesha/ReaCH PLC/Copy of horizontal_swipe_experiment.csv',header =TRUE, sep=',')

#checking for null values
is.na(data)

#boxplot to check for outliers
ggbetweenstats(data = data, x = Variant, y = pvs_per_session, fill = Variant) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  labs(x = "",
       y = "View Per Session",
       title = "Boxplot of view per session for various variant") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))

check_outliers(data$pvs_per_session) # justification for the outliers

# Removing the outliers
Q <- quantile(data$pvs_per_session, probs = c(0.25,0.75),na.rm=FALSE)
iqr <- IQR(data$pvs_per_session)

lower_fence <- Q[1]-1.5*iqr
upper_fence <- Q[2]+1.5*iqr
eliminated_outliers <- subset(data, data$pvs_per_session > (lower_fence) & data$pvs_per_session < upper_fence)


#Plotting the data without outliers
ggbetweenstats(eliminated_outliers,Variant, pvs_per_session, outlier.tagging = TRUE) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) +
  labs(y = "View per session")


## check for normality
qqnorm(data$pvs_per_session, main = "QQ Plot")
qqline(data$pvs_per_session)

# the pvs_per_session is not normal from the qqplot generated.

## data transformation to make the dependent variable normal

data <- data %>% 
  mutate(per_session_log10 = log10(pvs_per_session))

qqnorm(data$per_session_log10,main='QQplot')
qqline(data$per_session_log10)
## equality of variance

test <- leveneTest(y = data$pvs_per_session, group = data$Variant,center = mean)

test

## Equality of variance can be assumed because the p-value of the levene test is greater than the sig. value.

## Anova

## Null Hypothesis: There is no statistically significant difference between the groups of the experiment.
## Alternative Hypothesis: There is a statistically significant difference between the groups of the experiment.
model1 <- aov(per_session_log10 ~ Variant, data = data)

summary(model1)   

report(model1)

ggbetweenstats(data = data, x = Variant, y = per_session_log10) + 
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) +
  labs(y = "View per session")

## We failed to  reject the null hypothesis as the test is not statistically significant.

data %>% 
  select(Variant, pvs_per_session) %>% 
  group_by(Variant) %>% 
  dplyr::summarise(`Average View` = mean(pvs_per_session, na.rm = TRUE)) %>% 
  arrange(-`Average View`)
## The Control group performed optimally and none of the experimental groups out performed Control

## Null Hypothesis: There is no statistically significant difference between the performance of the device.
## Alternative Hypothesis: There is a statistically significant difference between the performance of the device.
model2 <- aov(per_session_log10 ~ deviceCategory, data = data)

summary(model2)   

report(model2)

ggbetweenstats(data = data, x = deviceCategory, y = per_session_log10) + 
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) +
  labs(y = "View per session")

## As the p-value is less than the significance value, so we reject the null hypothesis.
## The device category is statistically significant. So there is a difference in the means of the device category

## posthoc test

post_hoc <- duncan.test(model2, trt = "deviceCategory")
post_hoc
plot(post_hoc, variation = "IQR")

post_hoc$groups
## the mean of the mobile deviceCategory is varying from the other group means

data %>% 
  mutate(date = ymd(date)) %>% 
  ggplot(aes(x = date, y = per_session_log10, color = Variant, group = 1)) +
  #geom_line(show.legend = FALSE) +
  geom_smooth(show.legend = FALSE, se = FALSE) +
  facet_wrap(Variant ~ ., scales = "free") +
  labs(x = "",
       y = "View per session",
       color = "")
## The results for the graph state that there is no significant difference between the groups as product of time
## we can also say that the results are stable over the particular time period

