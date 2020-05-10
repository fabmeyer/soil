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
str(soil)
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
View(soil_tibble) # looks identical
typeof(soil_tibble) # list
is_tibble(soil_tibble) # TRUE

#### 4. make plots to get an overview ####
ggplot(gather(soil_tibble), aes(value)) + 
  geom_histogram(bins = 30) + 
  facet_wrap(~key, scales = 'free_x')
# some of the data (CEC1, CEC2, Clay1, OC1, OC2) is left-skewed

#### 5. plot every clay and oc against every cec (18 different combinations) ####
# This could be done in two loops I think, but didn't found out how
plot1 <- ggplot(data=soil_tibble, aes(x=soil_tibble$Clay1, y=soil_tibble$CEC1)) +
    geom_point() + geom_smooth(method = 'lm')
plot2 <- ggplot(data=soil_tibble, aes(x=soil_tibble$Clay1, y=soil_tibble$CEC2)) +
    geom_point() + geom_smooth(method = 'lm')
plot3 <- ggplot(data=soil_tibble, aes(x=soil_tibble$Clay1, y=soil_tibble$CEC5)) +
    geom_point() + geom_smooth(method = 'lm')
plot4 <- ggplot(data=soil_tibble, aes(x=soil_tibble$Clay2, y=soil_tibble$CEC1)) +
    geom_point() + geom_smooth(method = 'lm')
plot5 <- ggplot(data=soil_tibble, aes(x=soil_tibble$Clay2, y=soil_tibble$CEC2)) +
  geom_point() + geom_smooth(method = 'lm')
plot6 <- ggplot(data=soil_tibble, aes(x=soil_tibble$Clay2, y=soil_tibble$CEC5)) +
  geom_point() + geom_smooth(method = 'lm')
plot7 <- ggplot(data=soil_tibble, aes(x=soil_tibble$Clay5, y=soil_tibble$CEC1)) +
  geom_point() + geom_smooth(method = 'lm')
plot8 <- ggplot(data=soil_tibble, aes(x=soil_tibble$Clay5, y=soil_tibble$CEC2)) +
  geom_point() + geom_smooth(method = 'lm')
plot9 <- ggplot(data=soil_tibble, aes(x=soil_tibble$Clay5, y=soil_tibble$CEC5)) +
  geom_point() + geom_smooth(method = 'lm')
plot10 <- ggplot(data=soil_tibble, aes(x=soil_tibble$OC1, y=soil_tibble$CEC1)) +
  geom_point() + geom_smooth(method = 'lm')
plot11 <- ggplot(data=soil_tibble, aes(x=soil_tibble$OC1, y=soil_tibble$CEC2)) +
  geom_point() + geom_smooth(method = 'lm')
plot12 <- ggplot(data=soil_tibble, aes(x=soil_tibble$OC1, y=soil_tibble$CEC5)) +
  geom_point() + geom_smooth(method = 'lm')
plot13 <- ggplot(data=soil_tibble, aes(x=soil_tibble$OC2, y=soil_tibble$CEC1)) +
  geom_point() + geom_smooth(method = 'lm')
plot14 <- ggplot(data=soil_tibble, aes(x=soil_tibble$OC2, y=soil_tibble$CEC2)) +
  geom_point() + geom_smooth(method = 'lm')
plot15 <- ggplot(data=soil_tibble, aes(x=soil_tibble$OC2, y=soil_tibble$CEC5)) +
  geom_point() + geom_smooth(method = 'lm')
plot16 <- ggplot(data=soil_tibble, aes(x=soil_tibble$OC5, y=soil_tibble$CEC1)) +
  geom_point() + geom_smooth(method = 'lm')
plot17 <- ggplot(data=soil_tibble, aes(x=soil_tibble$OC5, y=soil_tibble$CEC2)) +
  geom_point() + geom_smooth(method = 'lm')
plot18 <- ggplot(data=soil_tibble, aes(x=soil_tibble$OC5, y=soil_tibble$CEC5)) +
  geom_point() + geom_smooth(method = 'lm')

# Put dependent variable on one single plot:
figure1 <- ggarrange(plot1, plot4, plot7, plot10, plot13, plot16,
                     ncol = 2, nrow = 3)
figure1
# oc1 and oc2 are best for cec1

figure2 <- ggarrange(plot2, plot5, plot8, plot11, plot14, plot17,
                     ncol = 2, nrow = 3)
figure2
# oc2 is best for cec2

figure3 <- ggarrange(plot3, plot6, plot9, plot12, plot15, plot18,
                     ncol = 2, nrow = 3)
figure3
# between 1.0 and 1.5 of oc5 is best for cec5

#### 6. train a linear model for every combination from 5. (18 models) ####
lm1 <- lm(CEC1 ~ Clay1, data = soil_tibble)
sum1 <- summary(lm1); sum1

lm2 <- lm(CEC1 ~ Clay2, data = soil_tibble)
sum2 <- summary(lm2); sum2

lm3 <- lm(CEC1 ~ Clay5, data = soil_tibble)
sum3 <- summary(lm3); sum3

lm4 <- lm(CEC1 ~ OC1, data = soil_tibble)
sum4 <- summary(lm4); sum4

lm5 <- lm(CEC1 ~ OC2, data = soil_tibble)
sum5 <- summary(lm5); sum5

lm6 <- lm(CEC1 ~ OC5, data = soil_tibble)
sum6 <- summary(lm6); sum6

lm7 <- lm(CEC2 ~ Clay1, data = soil_tibble)
sum7 <- summary(lm7); sum7

lm8 <- lm(CEC2 ~ Clay2, data = soil_tibble)
sum8 <- summary(lm8); sum8

lm9 <- lm(CEC2 ~ Clay5, data = soil_tibble)
sum9 <- summary(lm9); sum9

lm10 <- lm(CEC2 ~ OC1, data = soil_tibble)
sum10 <- summary(lm10); sum10

lm11 <- lm(CEC2 ~ OC2, data = soil_tibble)
sum11 <- summary(lm11); sum11

lm12 <- lm(CEC2 ~ OC5, data = soil_tibble)
sum12 <- summary(lm12); sum12

lm13 <- lm(CEC5 ~ Clay1, data = soil_tibble)
sum13 <- summary(lm13); sum13

lm14 <- lm(CEC5 ~ Clay2, data = soil_tibble)
sum14 <- summary(lm14); sum14

lm15 <- lm(CEC5 ~ Clay5, data = soil_tibble)
sum15 <- summary(lm15); sum15

lm16 <- lm(CEC5 ~ OC1, data = soil_tibble)
sum16 <- summary(lm16); sum16

lm17 <- lm(CEC5 ~ OC2, data = soil_tibble)
sum17 <- summary(lm17); sum17

lm18 <- lm(CEC5 ~ OC5, data = soil_tibble)
sum18 <- summary(lm18); sum18
#### 7. Put all 18 adjusted R^2 values into a list and compare them ####
rsq_values <- list(
  sum1$r.squared,
  sum2$r.squared,
  sum3$r.squared,
  sum4$r.squared,
  sum5$r.squared,
  sum6$r.squared,
  sum7$r.squared,
  sum8$r.squared,
  sum9$r.squared,
  sum10$r.squared,
  sum11$r.squared,
  sum12$r.squared,
  sum13$r.squared,
  sum14$r.squared,
  sum15$r.squared,
  sum16$r.squared,
  sum17$r.squared,
  sum18$r.squared
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
# so, OC1 and OC2 do have the biggest influence on CEC levels