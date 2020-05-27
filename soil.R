library('tidyverse')
library('cluster')
library('factoextra')
library('shiny')
library('ggloop')
library('ggpubr')
library('parsnip')
library('tidymodels')
library('broom')
library('yardstick')
library('knitr')
library("rmarkdown")
library('readxl')
theme_set(theme_pubr())

#### 1. import data ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
soil <- read.csv('soil.csv')

#### 2. inspect data ####
### Call the functions on soil to examine the data frame
dim(soil) # 147 rows with 9 features
str(soil) # 147 obs. of  9 variables
summary(soil)
colnames(soil)
typeof(soil) # data.frame

### The head() and tail() functions default to 6 rows, but we can adjust the number of rows using the "n = " argument
head(soil, n = 10)
tail(soil, n = 10)

### While the first 6 functions are printed to the console, the View() function opens a table in another window
View(soil)

### We can arrange the data to order it look for specific values
arrange(soil, desc(soil$CEC1)) # most fertile to least fertile top-soil
select(soil, c(1,4,7)) # top-soil samples
select(soil, c(2,5,8)) # deeper soil samples
select(soil, c(3,6,9)) # deepest sub-soil samples
filter(soil, soil$CEC1 > 25.0)

CEC_topsoil <- arrange(soil, desc(soil$CEC1))
filter(CEC_topsoil, CEC_topsoil$CEC1 > 25.0) # most fertile top-soil

Clay_topsoil <- arrange(soil, desc(soil$Clay1))
filter(Clay_topsoil, Clay_topsoil$Clay1 > 60.0) # highest measured clay in top-soil

OC_tosoil <- arrange(soil, desc(soil$OC1))
filter(OC_tosoil, OC_tosoil$OC1 > 6.0) # highest measured organic carbon in top-soil

#### 3. convert to tidyverse ####
soil_tibble <- as_tibble(soil)
View(soil_tibble) # identical
typeof(soil_tibble) # list, same as soil
is_tibble(soil_tibble) # TRUE

#### 4. make plots for each row to get an overview ####
ggplot(gather(soil_tibble), aes(value)) + 
  geom_histogram(bins = 30) + 
  facet_wrap(~key, scales = 'free_x')
# some of the data (CEC1, CEC2, Clay1, OC1, OC2) is left-skewed
# mostly in the first layer (1)

#### 5. plot every clay and oc against every cec (18 different combinations) ####
# This could be done in two loops, but we didn't found out how
plots <- list()
plots$plot1 <- ggplot(data=soil_tibble, aes(x=soil_tibble$Clay1, y=soil_tibble$CEC1)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('Clay1') + ylab('CEC1')
plots$plot2 <- ggplot(data=soil_tibble, aes(x=soil_tibble$Clay2, y=soil_tibble$CEC1)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('Clay2') + ylab('CEC1')
plots$plot3 <- ggplot(data=soil_tibble, aes(x=soil_tibble$Clay5, y=soil_tibble$CEC1)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('Clay5') + ylab('CEC1')
plots$plot4 <- ggplot(data=soil_tibble, aes(x=soil_tibble$OC1, y=soil_tibble$CEC1)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('OC1') + ylab('CEC1')
plots$plot5 <- ggplot(data=soil_tibble, aes(x=soil_tibble$OC1, y=soil_tibble$CEC1)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('OC2') + ylab('CEC1')
plots$plot6 <- ggplot(data=soil_tibble, aes(x=soil_tibble$OC5, y=soil_tibble$CEC1)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('OC5') + ylab('CEC1')
plots$plot7 <- ggplot(data=soil_tibble, aes(x=soil_tibble$Clay1, y=soil_tibble$CEC2)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('Clay1') + ylab('CEC2')
plots$plot8 <- ggplot(data=soil_tibble, aes(x=soil_tibble$Clay2, y=soil_tibble$CEC2)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('Clay2') + ylab('CEC2')
plots$plot9 <- ggplot(data=soil_tibble, aes(x=soil_tibble$Clay5, y=soil_tibble$CEC2)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('Clay5') + ylab('CEC2')
plots$plot10 <- ggplot(data=soil_tibble, aes(x=soil_tibble$OC1, y=soil_tibble$CEC2)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('OC1') + ylab('CEC2')
plots$plot11 <- ggplot(data=soil_tibble, aes(x=soil_tibble$OC2, y=soil_tibble$CEC2)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('OC2') + ylab('CEC2')
plots$plot12 <- ggplot(data=soil_tibble, aes(x=soil_tibble$OC5, y=soil_tibble$CEC2)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('OC5') + ylab('CEC2')
plots$plot13 <- ggplot(data=soil_tibble, aes(x=soil_tibble$Clay1, y=soil_tibble$CEC5)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('Clay1') + ylab('CEC5')
plots$plot14 <- ggplot(data=soil_tibble, aes(x=soil_tibble$Clay2, y=soil_tibble$CEC5)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('Clay2') + ylab('CEC5')
plots$plot15 <- ggplot(data=soil_tibble, aes(x=soil_tibble$Clay5, y=soil_tibble$CEC5)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('Clay5') + ylab('CEC5')
plots$plot16 <- ggplot(data=soil_tibble, aes(x=soil_tibble$OC1, y=soil_tibble$CEC5)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('OC1') + ylab('CEC5')
plots$plot17 <- ggplot(data=soil_tibble, aes(x=soil_tibble$OC2, y=soil_tibble$CEC5)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('OC2') + ylab('CEC5')
plots$plot18 <- ggplot(data=soil_tibble, aes(x=soil_tibble$OC5, y=soil_tibble$CEC5)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('OC5') + ylab('CEC5')

# Put dependent variable on one single plot:
figures <- list()

figures$figure1 <- ggarrange(plots$plot1, plots$plot2, plots$plot3, plots$plot4, plots$plot5, plots$plot6,
                             ncol = 2, nrow = 3)
figures$figure1
# OC1 and OC2 are best for CEC1. Clay5 and OC5 have the mildest curves. OC1 is very left-skewed data.

figures$figure2 <- ggarrange(plots$plot7, plots$plot8, plots$plot9, plots$plot10, plots$plot11, plots$plot12,
                             ncol = 2, nrow = 3)
figures$figure2
# OC2 is best for CEC2. OC1's data on CEC2 is left-skewed.

figures$figure3 <- ggarrange(plots$plot13, plots$plot14, plots$plot15, plots$plot16, plots$plot18, plots$plot18,
                             ncol = 2, nrow = 3)
figures$figure3
# a middle dose of oc5 (between 1.0 and 1.5) is best for cec5. It has the steepest curve.
# Clay1, Clay2 and Clay3 look good too. OC1 is very left-skewed data and doesn't seem to have a big effect.

#### 6. train a linear model for every combination from 5. (18 models) ####
simple_linear_models <- list()

simple_linear_models$lm1 <- lm(CEC1 ~ Clay1, data = soil_tibble)
simple_linear_models$sum1 <- summary(simple_linear_models$lm1);
simple_linear_models$sum1

simple_linear_models$lm2 <- lm(CEC1 ~ Clay2, data = soil_tibble)
simple_linear_models$sum2 <- summary(simple_linear_models$lm2);
simple_linear_models$sum2

simple_linear_models$lm3 <- lm(CEC1 ~ Clay5, data = soil_tibble)
simple_linear_models$sum3 <- summary(simple_linear_models$lm3);
simple_linear_models$sum3

simple_linear_models$lm4 <- lm(CEC1 ~ OC1, data = soil_tibble)
simple_linear_models$sum4 <- summary(simple_linear_models$lm4);
simple_linear_models$sum4

simple_linear_models$lm5 <- lm(CEC1 ~ OC2, data = soil_tibble)
simple_linear_models$sum5 <- summary(simple_linear_models$lm5);
simple_linear_models$sum5

simple_linear_models$lm6 <- lm(CEC1 ~ OC5, data = soil_tibble)
simple_linear_models$sum6 <- summary(simple_linear_models$lm6);
simple_linear_models$sum6

simple_linear_models$lm7 <- lm(CEC2 ~ Clay1, data = soil_tibble)
simple_linear_models$sum7 <- summary(simple_linear_models$lm7);
simple_linear_models$sum7

simple_linear_models$lm8 <- lm(CEC2 ~ Clay2, data = soil_tibble)
simple_linear_models$sum8 <- summary(simple_linear_models$lm8);
simple_linear_models$sum8

simple_linear_models$lm9 <- lm(CEC2 ~ Clay5, data = soil_tibble)
simple_linear_models$sum9 <- summary(simple_linear_models$lm9);
simple_linear_models$sum9

simple_linear_models$lm10 <- lm(CEC2 ~ OC1, data = soil_tibble)
simple_linear_models$sum10 <- summary(simple_linear_models$lm10);
simple_linear_models$sum10

simple_linear_models$lm11 <- lm(CEC2 ~ OC2, data = soil_tibble)
simple_linear_models$sum11 <- summary(simple_linear_models$lm11);
simple_linear_models$sum11

simple_linear_models$lm12 <- lm(CEC2 ~ OC5, data = soil_tibble)
simple_linear_models$sum12 <- summary(simple_linear_models$lm12);
simple_linear_models$sum12

simple_linear_models$lm13 <- lm(CEC5 ~ Clay1, data = soil_tibble)
simple_linear_models$sum13 <- summary(simple_linear_models$lm13);
simple_linear_models$sum13

simple_linear_models$lm14 <- lm(CEC5 ~ Clay2, data = soil_tibble)
simple_linear_models$sum14 <- summary(simple_linear_models$lm14);
simple_linear_models$sum14

simple_linear_models$lm15 <- lm(CEC5 ~ Clay5, data = soil_tibble)
simple_linear_models$sum15 <- summary(simple_linear_models$lm15);
simple_linear_models$sum15

simple_linear_models$lm16 <- lm(CEC5 ~ OC1, data = soil_tibble)
simple_linear_models$sum16 <- summary(simple_linear_models$lm16);
simple_linear_models$sum16

simple_linear_models$lm17 <- lm(CEC5 ~ OC2, data = soil_tibble)
simple_linear_models$sum17 <- summary(simple_linear_models$lm17);
simple_linear_models$sum17

simple_linear_models$lm18 <- lm(CEC5 ~ OC5, data = soil_tibble)
simple_linear_models$sum18 <- summary(simple_linear_models$lm18);
simple_linear_models$sum18

#### 7. put all 18 adjusted R^2 values into a list and compare them ####
rsq_values <- list(
  simple_linear_models$sum1$r.squared,
  simple_linear_models$sum2$r.squared,
  simple_linear_models$sum3$r.squared,
  simple_linear_models$sum4$r.squared,
  simple_linear_models$sum5$r.squared,
  simple_linear_models$sum6$r.squared,
  simple_linear_models$sum7$r.squared,
  simple_linear_models$sum8$r.squared,
  simple_linear_models$sum9$r.squared,
  simple_linear_models$sum10$r.squared,
  simple_linear_models$sum11$r.squared,
  simple_linear_models$sum12$r.squared,
  simple_linear_models$sum13$r.squared,
  simple_linear_models$sum14$r.squared,
  simple_linear_models$sum15$r.squared,
  simple_linear_models$sum16$r.squared,
  simple_linear_models$sum17$r.squared,
  simple_linear_models$sum18$r.squared
)

# take rsq_values list and convert to tibble
rsq_values <- as.data.frame(rsq_values)
rsq_values <- t(rsq_values)
rsq_values <- as.data.frame(rsq_values)
rsq_values_tibble <- as_tibble(rsq_values)

# rename column
rsq_values_tibble <- rsq_values_tibble %>% rename(r_squared = V1)

# add name column
rsq_values_tibble <- rsq_values_tibble %>% add_column(name = 1:18)

# reorder data.frame / tibble
rsq_values_tibble <- rsq_values_tibble[c('name', 'r_squared')]

# order tibble by descending R^2 value
rsq_values_tibble %>% arrange(desc(r_squared))

# best: model 4 (CEC1 ~ OC1), second: model 11 (CEC2 ~ OC2)
# third: model 12 (CEC2 ~ OC5), fourth: model 5 (CEC1 ~ OC2)
# best for CEC5 on rank 9: model 18 (CEC5 ~ OC5)

# interpretation: OC levels are much more important than clay levels in general
# the most important OC level is the one from the same depth of the soil
# the second most important is the one that is beneath
# this suggests that organic carbon is able to move upwards in soil

#### 8. put all 18 p-values into a list and comprare them against R^2 values ####

# Question: if we compare p-values, do these values correspond to the R^2-values?

# Function to extract the overall ANOVA p-value out of a linear model object summary
lmp <- function(model_summary) {
  if (class(model_summary) != "summary.lm") stop("Not an object of class 'lm' ")
  f <- model_summary$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

p_values <- list(
  lmp(simple_linear_models$sum1),
  lmp(simple_linear_models$sum2),
  lmp(simple_linear_models$sum3),
  lmp(simple_linear_models$sum4),
  lmp(simple_linear_models$sum5),
  lmp(simple_linear_models$sum6),
  lmp(simple_linear_models$sum7),
  lmp(simple_linear_models$sum8),
  lmp(simple_linear_models$sum9),
  lmp(simple_linear_models$sum10),
  lmp(simple_linear_models$sum11),
  lmp(simple_linear_models$sum12),
  lmp(simple_linear_models$sum13),
  lmp(simple_linear_models$sum14),
  lmp(simple_linear_models$sum15),
  lmp(simple_linear_models$sum16),
  lmp(simple_linear_models$sum17),
  lmp(simple_linear_models$sum18)
)

# take p_values list and convert to tibble
p_values <- as.data.frame(p_values)
p_values <- t(p_values)
p_values <- as.data.frame(p_values)
p_values_tibble <- as_tibble(p_values)

# rename column
names(p_values_tibble)[1] <- 'p_value'

# add name column
p_values_tibble <- p_values_tibble %>% add_column(name = 1:18)

# reorder data.frame / tibble
p_values_tibble <- p_values_tibble[c('name', 'p_value')]

# order tibble by ascending p-value
p_values_tibble %>% arrange(p_value)

# We get the exact same sequence of models like when we order the models by descending R^2-value
# Why? Specifically for a single explanatory variable (Y = a + bX + e),
# there is a mathematical relationship between these two values.
figures$figure4 <- ggarrange(plots$plot4, plots$plot11, plots$plot18,
                             ncol = 1, nrow = 3)
figures$figure4

# by looking at the plots, we can see that there is a small confidence interval

#### 9. best predictor for sub-soil CEC value, given top-soil samples of Clay, OC and CEC ####
# CEC5 ~ Clay1: Model 13
simple_linear_models$sum13$r.squared
# CEC5 ~ OC1: Model 16
simple_linear_models$sum16$r.squared
# CEC5 ~ CEC1: New Model (model 19)
simple_linear_models$lm19 <- lm(CEC5 ~ CEC1, data = soil_tibble)
simple_linear_models$sum19 <- summary(simple_linear_models$lm19);
simple_linear_models$sum19$r.squared
# When we compare these three R^2 values, we can observe that Clay1 has the biggest effect on CEC5 levels
# OC1 has the lowest effect on CEC5 levels. This could suggest that organic carbon particles rather
# move upwards than downwards or clay rather moves downwards than organic carbon

# Plot model 19
plots$plot19 <- ggplot(data=soil_tibble, aes(x=soil_tibble$CEC1, y=soil_tibble$CEC5)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('CEC1') + ylab('CEC5')
plots$plot19

# Put Clay1, OC1 and CEC1 together in one model to predict CEC5 (model20)
simple_linear_models$lm20 <- lm(CEC5 ~ Clay1 + CEC1 + OC1, data = soil_tibble)
simple_linear_models$sum20 <- summary(simple_linear_models$lm20);
simple_linear_models$sum20$r.squared

# Model 20 (CEC5 ~ Clay1, CEC1, OC1) has an R^2 value of 0.309
# Why? Because if the top layers in a soil contain a high amount of positive ions, usually lower layers contain high amounts too.
# Additionally in soils ions can move downward ("washed out"). But they can also move toward lower levels. Or even upward.

# Straight-line equation for predictor
# CEC5 = a + b * Clay1 + c * CEC1 + d * OC1
simple_linear_models$sum20
# CEC5 = 4.12 + 0.10 * Clay1 + 0.20 * CEC1 - 0.89 * OC1

#### 10. residual plot for Model 20 ####
residuals <- list(
  predict(simple_linear_models$lm20), # Save the predicted values
  residuals(simple_linear_models$lm20) # Save the residual values
)

residuals <- as.data.frame(residuals)
residuals_tibble <- as_tibble(residuals)
names(residuals_tibble)[1] <- 'predicted'
names(residuals_tibble)[2] <- 'residuals'

plots$residual_plot <- ggplot(data=soil_tibble, aes(x=soil_tibble$CEC1, y=soil_tibble$CEC5)) +
  geom_point() +
  geom_point(data=residuals_tibble, aes(y = residuals_tibble$predicted), shape = 1) +
  geom_segment(aes(xend = soil_tibble$CEC1, yend = residuals_tibble$predicted), alpha = .1) +
  xlab('Clay1 + OC1 + CEC1') + ylab('CEC5') +
  theme_bw()
plots$residual_plot

# There is a slightly non-linear relationship between the residuals and the fitted values
# It may be better to create a model which uses quadratic predictors too

#### 11. predicting with top soil clay (model13) ####

# create list with values
predict <- list(Intercept = simple_linear_models$sum13$coefficients[1],
                Clay1 = simple_linear_models$sum13$coefficients[2])

# predict CEC5 with model 13 when there is no top soil clay (Clay1 = 0)
predict$Intercept + 0 * predict$Clay1
# result: 4.23

# predict CEC5 with model 13 when there is 70% top soil clay (Clay1 = 70.0)
predict$Intercept + 70.0 * predict$Clay1
# result: 10.10

# make a plot to visualize predictions with confidence interval

# add prediction
predict$predict <- predict(simple_linear_models$lm13, soil_tibble, interval = 'confidence')
predict$data <- cbind(soil_tibble, predict$predict)

# regression line + confidence intervals
plots$prediction <- ggplot(data=predict$data, aes(x=soil_tibble$Clay1, y=soil_tibble$CEC5)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('Clay1') + ylab('CEC5')

# add prediction intervals
plots$prediction + geom_line(aes(y = lwr), color = "red", linetype = "dashed") +
  geom_line(aes(y = upr), color = "red", linetype = "dashed")

# So we can see by our eyes that for a Clay1 value of 0 there is about 5 of CEC5
# And for an input value of 70 of Clay1 there is about 10 of CEC5.

#### 12. try out something ####

# another question to answer: Is organic carbon is rather moving upwards or downwards ?
# to elaborate on this we create two different models:

# model21: OC5 ~ OC1 + OC2
simple_linear_models$lm21 <- lm(OC5 ~ OC1 + OC2, data = soil_tibble)
simple_linear_models$sum21 <- summary(simple_linear_models$lm21)

# model22: OC1 ~ OC2 + OC5
simple_linear_models$lm22 <- lm(OC1 ~ OC2 + OC5, data = soil_tibble)
simple_linear_models$sum22 <- summary(simple_linear_models$lm22)

# examine models 21 and 22
simple_linear_models$sum21
simple_linear_models$sum22

# we can see that model 21 has a much higher R^2 value (and also a slightly lower p-value)
# we would argue that organic carbon is rather moving downwards than upwards.
# we would thus recommend to keep OC1 levels high. This is relevant to businesses, as this has a direct impact on fertility
# not only does this lead to consistently high levels of CEC1
# but ultimately also to higher levels of OC2 and OC5 (and therefor also higher levels of CEC2 and CEC5).

library("scatterplot3d")
scatterplot3d(soil_tibble$OC1, soil_tibble$OC2, soil_tibble$OC5, highlight.3d=TRUE, col.axis="blue",
              col.grid="lightblue", main='3D Scatterplot', pch=1, angle = 30,
              xlab = 'OC (0-10cm)',
              ylab = 'OC (10-20cm)',
              zlab = 'OC (30-50cm)')
