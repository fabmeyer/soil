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

#### 5. plot every clay and oc against every cec (18 different combinations) ####
# This could be done in two loops I think, but didn't found out how
plots <- list()
plots$plot1 <- ggplot(data=soil_tibble, aes(x=soil_tibble$Clay1, y=soil_tibble$CEC1)) +
    geom_point() + geom_smooth(method = 'lm') + xlab('Clay1') + ylab('CEC1')
plots$plot2 <- ggplot(data=soil_tibble, aes(x=soil_tibble$Clay1, y=soil_tibble$CEC2)) +
    geom_point() + geom_smooth(method = 'lm') + xlab('Clay1') + ylab('CEC2')
plots$plot3 <- ggplot(data=soil_tibble, aes(x=soil_tibble$Clay1, y=soil_tibble$CEC5)) +
    geom_point() + geom_smooth(method = 'lm') + xlab('Clay1') + ylab('CEC5')
plots$plot4 <- ggplot(data=soil_tibble, aes(x=soil_tibble$Clay2, y=soil_tibble$CEC1)) +
    geom_point() + geom_smooth(method = 'lm') + xlab('Clay2') + ylab('CEC1')
plots$plot5 <- ggplot(data=soil_tibble, aes(x=soil_tibble$Clay2, y=soil_tibble$CEC2)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('Clay2') + ylab('CEC2')
plots$plot6 <- ggplot(data=soil_tibble, aes(x=soil_tibble$Clay2, y=soil_tibble$CEC5)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('Clay2') + ylab('CEC5')
plots$plot7 <- ggplot(data=soil_tibble, aes(x=soil_tibble$Clay5, y=soil_tibble$CEC1)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('Clay5') + ylab('CEC1')
plots$plot8 <- ggplot(data=soil_tibble, aes(x=soil_tibble$Clay5, y=soil_tibble$CEC2)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('Clay5') + ylab('CEC2')
plots$plot9 <- ggplot(data=soil_tibble, aes(x=soil_tibble$Clay5, y=soil_tibble$CEC5)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('Clay5') + ylab('CEC5')
plots$plot10 <- ggplot(data=soil_tibble, aes(x=soil_tibble$OC1, y=soil_tibble$CEC1)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('OC1') + ylab('CEC1')
plots$plot11 <- ggplot(data=soil_tibble, aes(x=soil_tibble$OC1, y=soil_tibble$CEC2)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('OC1') + ylab('CEC2')
plots$plot12 <- ggplot(data=soil_tibble, aes(x=soil_tibble$OC1, y=soil_tibble$CEC5)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('OC1') + ylab('CEC5')
plots$plot13 <- ggplot(data=soil_tibble, aes(x=soil_tibble$OC2, y=soil_tibble$CEC1)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('OC2') + ylab('CEC1')
plots$plot14 <- ggplot(data=soil_tibble, aes(x=soil_tibble$OC2, y=soil_tibble$CEC2)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('OC2') + ylab('CEC2')
plots$plot15 <- ggplot(data=soil_tibble, aes(x=soil_tibble$OC2, y=soil_tibble$CEC5)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('OC2') + ylab('CEC5')
plots$plot16 <- ggplot(data=soil_tibble, aes(x=soil_tibble$OC5, y=soil_tibble$CEC1)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('OC5') + ylab('CEC1')
plots$plot17 <- ggplot(data=soil_tibble, aes(x=soil_tibble$OC5, y=soil_tibble$CEC2)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('OC5') + ylab('CEC2')
plots$plot18 <- ggplot(data=soil_tibble, aes(x=soil_tibble$OC5, y=soil_tibble$CEC5)) +
  geom_point() + geom_smooth(method = 'lm') + xlab('OC5') + ylab('CEC5')

# Put dependent variable on one single plot:
figures <- list()

figures$figure1 <- ggarrange(plots$plot1, plots$plot4, plots$plot7, plots$plot10, plots$plot13, plots$plot16,
                     ncol = 2, nrow = 3)
figures$figure1
# OC1 and OC2 are best for CEC1. Clay5 and OC5 have the mildest curves

figures$figure2 <- ggarrange(plots$plot2, plots$plot5, plots$plot8, plots$plot11, plots$plot14, plots$plot17,
                     ncol = 2, nrow = 3)
figures$figure2
# OC2 is best for CEC2. OC1's data on CEC2 is left-skewed.

figures$figure3 <- ggarrange(plots$plot3, plots$plot6, plots$plot9, plots$plot12, plots$plot15, plots$plot18,
                     ncol = 2, nrow = 3)
figures$figure3
# a middle dose of oc5 (between 1.0 and 1.5) is best for cec5. it has the steepest curve.
# Clay1, Clay2 and Clay3 look good too.

#### 6. train a linear model for every combination from 5. (18 models) ####
simple_linear_models <- list()

simple_linear_models$lm1 <- lm(CEC1 ~ Clay1, data = soil_tibble)
simple_linear_models$sum1 <- summary(simple_linear_models$lm1);

simple_linear_models$lm2 <- lm(CEC1 ~ Clay2, data = soil_tibble)
simple_linear_models$sum2 <- summary(simple_linear_models$lm2);

simple_linear_models$lm3 <- lm(CEC1 ~ Clay5, data = soil_tibble)
simple_linear_models$sum3 <- summary(simple_linear_models$lm3);

simple_linear_models$lm4 <- lm(CEC1 ~ OC1, data = soil_tibble)
simple_linear_models$sum4 <- summary(simple_linear_models$lm4);

simple_linear_models$lm5 <- lm(CEC1 ~ OC2, data = soil_tibble)
simple_linear_models$sum5 <- summary(simple_linear_models$lm5);

simple_linear_models$lm6 <- lm(CEC1 ~ OC5, data = soil_tibble)
simple_linear_models$sum6 <- summary(simple_linear_models$lm6);

simple_linear_models$lm7 <- lm(CEC2 ~ Clay1, data = soil_tibble)
simple_linear_models$sum7 <- summary(simple_linear_models$lm7);

simple_linear_models$lm8 <- lm(CEC2 ~ Clay2, data = soil_tibble)
simple_linear_models$sum8 <- summary(simple_linear_models$lm8);

simple_linear_models$lm9 <- lm(CEC2 ~ Clay5, data = soil_tibble)
simple_linear_models$sum9 <- summary(simple_linear_models$lm9);

simple_linear_models$lm10 <- lm(CEC2 ~ OC1, data = soil_tibble)
simple_linear_models$sum10 <- summary(simple_linear_models$lm10);

simple_linear_models$lm11 <- lm(CEC2 ~ OC2, data = soil_tibble)
simple_linear_models$sum11 <- summary(simple_linear_models$lm11);

simple_linear_models$lm12 <- lm(CEC2 ~ OC5, data = soil_tibble)
simple_linear_models$sum12 <- summary(simple_linear_models$lm12);

simple_linear_models$lm13 <- lm(CEC5 ~ Clay1, data = soil_tibble)
simple_linear_models$sum13 <- summary(simple_linear_models$lm13);

simple_linear_models$lm14 <- lm(CEC5 ~ Clay2, data = soil_tibble)
simple_linear_models$sum14 <- summary(simple_linear_models$lm14);

simple_linear_models$lm15 <- lm(CEC5 ~ Clay5, data = soil_tibble)
simple_linear_models$sum15 <- summary(simple_linear_models$lm15);

simple_linear_models$lm16 <- lm(CEC5 ~ OC1, data = soil_tibble)
simple_linear_models$sum16 <- summary(simple_linear_models$lm16);

simple_linear_models$lm17 <- lm(CEC5 ~ OC2, data = soil_tibble)
simple_linear_models$sum17 <- summary(simple_linear_models$lm17);

simple_linear_models$lm18 <- lm(CEC5 ~ OC5, data = soil_tibble)
simple_linear_models$sum18 <- summary(simple_linear_models$lm18);

#### 7. Put all 18 adjusted R^2 values into a list and compare them ####
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

# order tibble by ascending R^2 value
rsq_values_tibble %>% arrange(r_squared)

# best: model 16 (CEC5 ~ OC1), second: model 10 (CEC2 ~ OC1), third: model 17 (CEC5 ~ OC2)
# Conclusion: OC1 and OC2 do have the biggest influence on CEC levels

#### 8. Put all p-values into a list and comprare them ####
p_values <- list(
  simple_linear_models$sum1$coefficients[,4],
  simple_linear_models$sum2$coefficients[,4],
  simple_linear_models$sum3$coefficients[,4],
  simple_linear_models$sum4$coefficients[,4],
  simple_linear_models$sum5$coefficients[,4],
  simple_linear_models$sum6$coefficients[,4],
  simple_linear_models$sum7$coefficients[,4],
  simple_linear_models$sum8$coefficients[,4],
  simple_linear_models$sum9$coefficients[,4],
  simple_linear_models$sum10$coefficients[,4],
  simple_linear_models$sum11$coefficients[,4],
  simple_linear_models$sum12$coefficients[,4],
  simple_linear_models$sum13$coefficients[,4],
  simple_linear_models$sum14$coefficients[,4],
  simple_linear_models$sum15$coefficients[,4],
  simple_linear_models$sum16$coefficients[,4],
  simple_linear_models$sum17$coefficients[,4],
  simple_linear_models$sum18$coefficients[,4]
)

# take p_values list and convert to tibble
p_values <- as.data.frame(p_values)
p_values <- t(p_values)
p_values <- as.data.frame(p_values)
p_values_tibble <- as_tibble(p_values)

# rename column
p_values_tibble <- p_values_tibble %>% rename(slope = Clay1)

# add name column
p_values_tibble <- p_values_tibble %>% add_column(name = 1:18)

# reorder data.frame / tibble
p_values_tibble <- p_values_tibble[c('name', '(Intercept)', 'slope')]

# order tibble by ascending p-value
p_values_tibble %>% arrange(slope)

# best: model 4, model 11 and model 12
plots$plot4 # Clay2 for CEC1
plots$plot11 # OC1 for CEC2
plots$plot12 # OC1 for CEC5

# Above answers assignment question 7: 
# More CEC gathers in soils rich in soil humus, followed by clay minerals
# Best predictor for sub-soil CEC2 & CEC5 is OC1, if only top-soil data is available

# Correlation coefficients between CEC and OC are higher than between CEC and Clay

#### 9. More with CEC predictors ####
### CEC1 predictors (from 5.)
figures$figure4 <- ggarrange(plots$plot10, plots$plot13, ncol = 1, nrow = 1)
figures$figure4
 
### CEC2 predictors (from 5.)
plots$plot14

### CEC5 predictors (from 5.)
figures$figure5 <- ggarrange(plots$plot3, plots$plot6, plots$plot9, plots$plot18, ncol = 2, nrow = 2)
figures$figure5

# Straight-line equation for predictor, based on R^2 and top-soil data
# CEC_Sub = CEC_Top + x*Clay_Top + y*OC_Top , R^2

# residual plot for OC1?

#### 10. Predicting with predictors ####

# Predict sub-soil CEC where Clay1 = 0, using predictor OC1
# Predict sub-soil CEC where Clay1 has 70% weight in topsoil data

#### 11. Try out something ####
