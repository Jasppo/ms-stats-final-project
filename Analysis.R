# Documentation =============================================================

## Session Info -----------------
# R version 4.3.3 (2024-02-29)
# R Studio Version 2023.12.1+402

## About ------------------
# Code for STAT 632 Final Project (Spring 2024)
# Note: This R script is a cleaned up version of our Quarto Markdown file that generates the PDF report.

## How to run script -----------
# 1. Open "Project.Rproj" - This sets the appropriate working directory required to run this script
# 2. Run `install.packages("renv")` - Version 1.0.2 or higher required
# 3. Run `renv::restore()` - Will install the requisite packages needed to execute this script

## Script Outline --------------
# Open Outline View (recommended)

## Dependency Files ----------------
# 1. data/CO2 Emissions.csv - dataset to analyze; Sourced from Kaggle
# 2. data/randomForestModel.RDS - randomForest.formula obect; Ran random forest on dataset once and saved output
# 3. info/metadata.xlsx - manually-created metadata

## Author Info --------------------
# Authors: Humza Shah, Faizan Khan, Jaspreet Kang, Nishanth Reddy Lankala
# Script Created: May 7, 2024
# Script Last Modified: May 8, 2024

# Load packages =================================================

# Data manipulation packages
library(dplyr) # Data wrangling
library(tidyr) # Data reshaping
library(readxl) # Read in xlsx files
library(stringr) # String manipulation

# Table Package
library(flextable)

# Chart Packages
library(ggplot2)
library(GGally) # For scatterplot and correlation matrix

# Modeling Packages
library(car) # VIF
library(MASS) # boxcox
library(performance) # Constant variance and normality tests
library(randomForest) 

# Preparation ==================================================

## Read in dependency files --------------------------

### Data ------------
data <- read.csv("data/CO2 Emissions.csv")
origColNames <- names(data)
names(data) <- c("make", "model", "class", "engine_size", "cylinders", "transmission", "fuel_type", 
                 "fuel_consume_city", "fuel_consume_hwy", "fuel_consume_comb_l", "fuel_consume_comb_mpg", 
                 "co2_emissions")

### Info ------------
metadata <- read_excel("info/metadata.xlsx")

## Set ggplot Theme ----------------
stdTheme <- theme_bw() +
  theme(
    plot.title = element_blank(),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 10, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10)
  )

theme_set(stdTheme)

# Functions =======================================================

## Summary Plots -----------------------
# Description: Creates 4 plots in a 2x2 grid to check: Linearity, Constant Variance, Normality, and Influential Points 
# Plot 1: Observed vs Fitted Values
# Plot 2: Standardized Residuals vs Fitted Values
# Plot 3: Normal Q-Q Plot
# Plot 4: Standardized Residuals vs Leverage Points

# Arguments:
# 1. myModel: lm object
# 2. myData: data frame object used in model
# 3. myY: Response variable vector (transformed if applicable)
plotChecks <- function(myModel, myData, myY) {

  stdTheme <- theme_gray() +
    theme(
      plot.title = element_text(size = 12, face = "bold", color = "darkblue"), 
      plot.subtitle = element_text(size = 10),
      axis.title = element_text(size = 10, face = "bold"),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      strip.text = element_text(size = 10, face = "bold")
    )
  
  # Slight curvature
  aLinear <- tibble(x = myModel$fitted.values, y = myY) %>% 
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    stat_smooth(method = "loess") +
    labs(x = "Fitted Values", y = "Observed Values", 
         title = "Fitted vs Observed Values") +
    stdTheme
  
  aVar <- tibble(y = rstandard(myModel), x = myY) %>% 
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    geom_hline(yintercept = c(-4, 0, 4), color = "red", linetype = "dashed") +
    stat_smooth(method = "loess", se = T) +
    labs(x = "Fitted Values", y = "Standardized Residuals", 
         title = "Std. Residuals vs Fitted Values") +
    stdTheme
  
  aNormality <- tibble(x = rstandard(myModel)) %>% 
    ggplot(aes(sample = x)) +
    geom_qq() +
    geom_qq_line(color = "blue") +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles", 
         title = "Normal Q-Q Plot") +
    stdTheme
  
  
  aLev <- tibble(y = rstandard(myModel), x = hatvalues(myModel)) %>% 
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    geom_hline(yintercept = c(-4, 0, 4), color = "red", linetype = "dashed") +
    geom_vline(xintercept = 2*myModel$rank/nrow(myData), color = "red", linetype = "dashed") +
    labs(x = "Leverage Points", y = "Standardized Residuals", 
         title = "Std. Residuals vs Leverage Points") +
    stdTheme
  
  
  cowplot::plot_grid(aLinear, aVar, aNormality, aLev, ncol = 2, nrow = 2)
  
}

## % Frequencies - Categorical Variable -----------
# Description: Gets % frequencies of a categorical variable

# Arguments:
# myCol: column name of categorical variable of interest
# myName: Name to give categorical variable when plotting
# myN: Number of top categories to obtain and display
getFreq <- function(myCol, myName, myN) {
  data %>% 
    rename(group = !!as.symbol(myCol)) %>% 
    mutate(group = as.character(group), 
           group = case_when(group == "MID-SIZE" ~ "MID SIZE", 
                             group == "FULL-SIZE" ~ "FULL SIZE", 
                             group == "COMPACT" ~ "COM- PACT",
                             group == "SUV - STANDARD" ~ "SUV STD",
                             group == "Z" ~ "Premium\nGasoline",
                             group == "X" ~ "Regular\nGasoline",
                             group == "E" ~ "Ethanol (E85)",
                             group == "D" ~ "Diesel",
                             group == "N" ~ "Natural Gas",
                             TRUE ~ group)) %>% 
    count(group) %>% 
    arrange(desc(n)) %>% 
    slice_head(n=myN) %>%
    mutate(perc = n/nrow(data), 
           variable = myName) %>%
    dplyr::select(group, variable, perc)
  
}

## Boxplots of Response vs Categorical Predictors ------------
# Description: Creates box plots of CO2 emissions for each category, faceted by categorical predictors 

# Arguments:
# 1. myVars: character value or vector of categorical predictor column name(s)
# 2. myRows: number of rows to display when facet wrapping
# 3. titleX: TRUE or FALSE - include X axis title?
tBox <- function(myVars, myRows = 1, titleX = F) {
  tPlot <- tDat %>% 
    filter(variable %in% myVars) %>% 
    ggplot(aes(x = reorder(category, `CO2 Emissions`), y = `CO2 Emissions`)) +
    geom_boxplot(fill = "lightblue") +
    labs(x = "Category", y = "CO2 Emissions") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    facet_wrap(~variable, scales = "free", nrow = myRows) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 12, face = "bold", color = "darkblue"),
      axis.title = element_text(size = 10, face = "bold"),
      axis.text = element_text(size = 10),
      strip.text = element_text(size = 10, face = "bold"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
  
  if (!titleX) {
    tPlot <- tPlot + 
      theme(axis.title.x = element_blank())
  }
  
  tPlot
  
}

## Compute RMSE ----------------------------
# Arguments:
# 1. o: observed values
# 2. f: fitted values
cRMSE <- function(o, f) {
  sqrt(mean((o - f)^2))
}

# Data Description ===================================================

## About the data -----------------
dim(data) # Data Dimensions
colSums(is.na(data)) # Check for any missing values
str(data) # Data structure

## Plot: % Frequencies of Categorical Variables ------------
bind_rows(
  getFreq("class", "Class", 5), 
  getFreq("transmission", "Transmission", 10), 
  getFreq("fuel_type", "Fuel Type", 15),
  getFreq("cylinders", "Cylinders", 15)
) %>% 
  ggplot(aes(x = reorder(group, -perc), y = perc)) +
  geom_col(position = "dodge", fill = "blue") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Percentage Frequency", x = "Category") +
  facet_wrap(~variable, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

## Plot: Distributions of Quantitative Variables ------------
data %>% 
  dplyr::select(`Engine\nSize` = engine_size, 
                `Fuel\nCity (L/100km)` = fuel_consume_city,
                `Fuel\nHwy (L/100km)` = fuel_consume_hwy,
                `Fuel\nComb (L/100km)` = fuel_consume_comb_l, 
                `Fuel\nComb (MPG)` = fuel_consume_comb_mpg,
                `CO2\nEmissions (g/km)` = co2_emissions) %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(-id, names_to = "group", values_to = "value") %>% 
  ggplot(aes(x = value)) +
  geom_density(fill = "blue", color = "blue") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Value", y = "Density") +
  facet_wrap(~group, scales = "free")

## Plot: Categorical Predictor and CO2 Emissions -----------------
tDat <- data %>% 
  mutate(cylinders = as.character(cylinders)) %>% 
  dplyr::select(`Fuel Type` = fuel_type, 
                `Class` = class,
                `Cylinders` = cylinders,
                `Transmission` = transmission, 
                `CO2 Emissions` = co2_emissions) %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(-c("id", "CO2 Emissions"), names_to = "variable", values_to = "category") %>% 
  mutate(category = case_when(category == "SPECIAL PURPOSE VEHICLE" ~ "SPECIAL PURPOSE", 
                              category == "STATION WAGON - SMALL" ~ "SMALL STATION WAGONS", 
                              category == "PICKUP TRUCK - SMALL" ~ "SMALL PICKUP", 
                              category == "PICKUP TRUCK - STANDARD" ~ "STANDARD PICKUP",
                              category == "Z" ~ "Premium Gasoline",
                              category == "X" ~ "Regular Gasoline",
                              category == "E" ~ "Ethanol (E85)",
                              category == "D" ~ "Diesel",
                              category == "N" ~ "Natural Gas",
                              TRUE ~ category
  ))


cowplot::plot_grid(tBox("Class"), tBox("Transmission"),
                   tBox(c("Cylinders", "Fuel Type"), 1, T), nrow = 3, rel_heights = c(1.5, 1, 1))

## Plot: Scatterplot and Correlation Matrix of CO2 Emissions and Quantitative Predictors ------------------
tDat <- data %>% 
  dplyr::select(
    `Engine\nSize` = engine_size, 
    `Fuel\nCity` = fuel_consume_city,
    `Fuel\nHwy` = fuel_consume_hwy,
    `Fuel\nComb` = fuel_consume_comb_l, 
    `Fuel\nComb (MPG)` = fuel_consume_comb_mpg,
    `CO2\nEmissions` = co2_emissions)

ggpairs(data = tDat,
        lower = list(continuous = "cor"), 
        upper = list(continuous = "points")) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

## Plot: Scatterplots of Fuel Efficiency Predictors and CO2 Emissions by Fuel Type ------------
data %>% 
  dplyr::select(`Fuel\nCity (L/100km)` = fuel_consume_city,
                `Fuel\nHwy (L/100km)` = fuel_consume_hwy,
                `Fuel\nComb (L/100km)` = fuel_consume_comb_l, 
                `Fuel\nComb (MPG)` = fuel_consume_comb_mpg,
                `Fuel Type` = fuel_type,
                `CO2 Emissions (g/km)` = co2_emissions) %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(-c("id", "CO2 Emissions (g/km)", "Fuel Type"), names_to = "group", values_to = "value") %>% 
  mutate(`Fuel Type` = case_when(`Fuel Type` == "Z" ~ "Premium Gasoline",
                                 `Fuel Type` == "X" ~ "Regular Gasoline",
                                 `Fuel Type` == "E" ~ "Ethanol (E85)",
                                 `Fuel Type` == "D" ~ "Diesel",
                                 `Fuel Type` == "N" ~ "Natural Gas",
                                 TRUE ~ `Fuel Type`)) %>% 
  ggplot(aes(x = value, y = `CO2 Emissions (g/km)`)) +
  geom_point(aes(color = `Fuel Type`)) +
  facet_wrap(~group, scales = "free") +
  labs(x = "Fuel Efficiency Values") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

# Model Pre-Processing ========================================================================

## One-Hot Encoding ---------------------

data2 <- data %>%
  mutate(id = row_number(),
         cylinders = as.character(cylinders), 
         fuel_type = case_when(fuel_type == "Z" ~ "Premium_Gas",
                               fuel_type == "X" ~ "Regular_Gas",
                               fuel_type == "E" ~ "Ethanol",
                               fuel_type == "D" ~ "Diesel",
                               fuel_type == "N" ~ "Natural_Gas",
                               TRUE ~ fuel_type)) %>%
  pivot_wider(names_from = "class", values_from = "class", names_prefix = "Class_") %>%
  pivot_wider(names_from = cylinders, values_from = cylinders, names_prefix = "Cylinders_") %>%
  pivot_wider(names_from = transmission, values_from = transmission, names_prefix = "Transmission_") %>%
  pivot_wider(names_from = fuel_type, values_from = fuel_type, names_prefix = "FuelType_") %>%
  rename(`CO2_Emissions` = co2_emissions,
         `Fuel_Efficiency_City` = fuel_consume_city,
         `Fuel_Efficiency_Highway` = fuel_consume_hwy,
         `Fuel_Efficiency_Combined` = fuel_consume_comb_l,
         `Fuel_Efficiency_Combined_MPG` = fuel_consume_comb_mpg) %>%
  rename_with(.cols = everything(), .fn = ~gsub("-", "_", .)) %>%
  rename_with(.cols = everything(), .fn = ~gsub(" ", "", .)) %>%
  rename_with(.cols = everything(), .fn = ~gsub(" ", "_", .)) %>%
  mutate(across(where(is.character), ~ifelse(is.na(.x), 0, 1))) %>%
  relocate(id, 1) %>%
  dplyr::select(-make, -model)

## Create Training and Testing Sets ------------------------------
# Split the data by vehicle class
classNames <- names(data2)[grepl("Class_", names(data2))]
nClasses <- length(classNames)

splitData <- lapply(classNames, function(x) {
  data2 %>%
    filter(!!as.symbol(x) == 1)
})

# Get indices
indices <- lapply(1:nClasses, function(x) {
  numRows <- nrow(splitData[[x]])
  set.seed(30)
  sample.int(numRows, floor(0.7*numRows))
})

# Training Data
dataTrain <- lapply(1:nClasses, function(x) {
  splitData[[x]][indices[[x]], ]
}) %>%
  bind_rows()

# Testing Data
dataTest <- lapply(1:nClasses, function(x) {
  splitData[[x]][-indices[[x]], ]
}) %>%
  bind_rows()

# Validate
nrow(dataTrain) + nrow(dataTest) == nrow(data2)
length(unique(dataTrain$id)) + length(unique(dataTest$id)) == length(unique(data2$id))

# Model: Multiple Linear Regression Model =======================================

## Full Model -----------------
lm1 <- lm(`CO2_Emissions` ~ .-id - Class_SPECIALPURPOSEVEHICLE - Cylinders_5 - Transmission_AV10 - FuelType_Natural_Gas - 
            Fuel_Efficiency_Highway - Fuel_Efficiency_Combined - Fuel_Efficiency_City - Fuel_Efficiency_Combined_MPG + 
            log(Fuel_Efficiency_Highway) - log(Fuel_Efficiency_Combined) + log(Fuel_Efficiency_City) + 
            log(Fuel_Efficiency_Combined_MPG), data = dataTrain)
summary(lm1)

# Summary Chart
plotChecks(lm1, dataTrain, dataTrain$CO2_Emissions)

## Multicollinearity -----------------

sort(vif(lm1)) # VIF

# Remove one by one
# Removed Fuel_X (Highest)
lm2_1 <- lm(`CO2_Emissions` ~ .-id - Class_SPECIALPURPOSEVEHICLE - Cylinders_5 - Transmission_AV10 - FuelType_Natural_Gas - 
              Fuel_Efficiency_Highway - Fuel_Efficiency_Combined - Fuel_Efficiency_City - Fuel_Efficiency_Combined_MPG + 
              log(Fuel_Efficiency_Highway) - log(Fuel_Efficiency_Combined) + log(Fuel_Efficiency_City) + 
              log(Fuel_Efficiency_Combined_MPG) - FuelType_Regular_Gas, data = dataTrain)
sort(vif(lm2_1))

# Removed Fuel City (Highest)
lm2_2 <- lm(`CO2_Emissions` ~ .-id - Class_SPECIALPURPOSEVEHICLE - Cylinders_5 - Transmission_AV10 - FuelType_Natural_Gas - 
              Fuel_Efficiency_Highway - Fuel_Efficiency_Combined - Fuel_Efficiency_City - Fuel_Efficiency_Combined_MPG + 
              log(Fuel_Efficiency_Highway) - log(Fuel_Efficiency_Combined) + log(Fuel_Efficiency_Combined_MPG) - 
              FuelType_Regular_Gas, data = dataTrain)
sort(vif(lm2_2))

# Removed Transmission_AS6 (Highest)
lm2_3 <- lm(`CO2_Emissions` ~ .-id - Class_SPECIALPURPOSEVEHICLE - Cylinders_5 - Transmission_AV10 - FuelType_Natural_Gas - 
              Fuel_Efficiency_Highway - Fuel_Efficiency_Combined - Fuel_Efficiency_City - Fuel_Efficiency_Combined_MPG + 
              log(Fuel_Efficiency_Highway) - log(Fuel_Efficiency_Combined) + log(Fuel_Efficiency_Combined_MPG) - 
              FuelType_Regular_Gas - Transmission_AS6, data = dataTrain)
sort(vif(lm2_3))

# Removed cylinders 4 (Highest)
lm2_4 <- lm(`CO2_Emissions` ~ .-id - Class_SPECIALPURPOSEVEHICLE - Cylinders_5 - Transmission_AV10 - FuelType_Natural_Gas - 
              Fuel_Efficiency_Highway - Fuel_Efficiency_Combined - Fuel_Efficiency_City - Fuel_Efficiency_Combined_MPG + 
              log(Fuel_Efficiency_Highway) - log(Fuel_Efficiency_Combined) + log(Fuel_Efficiency_Combined_MPG) - 
              FuelType_Regular_Gas - Transmission_AS6 - Cylinders_4, data = dataTrain)
sort(vif(lm2_4))

# Removed Fuel_Efficiency_Highway (Highest)
lm2_5 <- lm(`CO2_Emissions` ~ .-id - Class_SPECIALPURPOSEVEHICLE - Cylinders_5 - Transmission_AV10 - FuelType_Natural_Gas - 
              Fuel_Efficiency_Highway - Fuel_Efficiency_Combined - Fuel_Efficiency_City - Fuel_Efficiency_Combined_MPG + 
              log(Fuel_Efficiency_Combined) + log(Fuel_Efficiency_Combined_MPG) - FuelType_Regular_Gas - 
              Transmission_AS6 - Cylinders_4, data = dataTrain)
sort(vif(lm2_5))

# Removed Fuel_Efficiency_Comb (Highest)
lm2_6 <- lm(`CO2_Emissions` ~ .-id - Class_SPECIALPURPOSEVEHICLE - Cylinders_5 - Transmission_AV10 - FuelType_Natural_Gas - 
              Fuel_Efficiency_Highway - Fuel_Efficiency_Combined - Fuel_Efficiency_City - Fuel_Efficiency_Combined_MPG + 
              log(Fuel_Efficiency_Combined_MPG) - FuelType_Regular_Gas - Transmission_AS6 - Cylinders_4, data = dataTrain)
sort(vif(lm2_6))

# Removed Class SUV Small (Highest)
lm2_7 <- lm(`CO2_Emissions` ~ .-id - Class_SPECIALPURPOSEVEHICLE - Cylinders_5 - Transmission_AV10 - FuelType_Natural_Gas - 
              Fuel_Efficiency_Highway - Fuel_Efficiency_Combined - Fuel_Efficiency_City - Fuel_Efficiency_Combined_MPG + 
              log(Fuel_Efficiency_Combined_MPG) - FuelType_Regular_Gas - Transmission_AS6 - Cylinders_4 - Class_SUV_SMALL, 
            data = dataTrain)
sort(vif(lm2_7))

# Removed Engine Size (Highest)
lm2_8 <- lm(`CO2_Emissions` ~ .-id - Class_SPECIALPURPOSEVEHICLE - Cylinders_5 - Transmission_AV10 - FuelType_Natural_Gas - 
              Fuel_Efficiency_Highway - Fuel_Efficiency_Combined - Fuel_Efficiency_City - Fuel_Efficiency_Combined_MPG + 
              log(Fuel_Efficiency_Combined_MPG) - FuelType_Regular_Gas - Transmission_AS6 - Cylinders_4 - Class_SUV_SMALL - 
              engine_size, data = dataTrain)
sort(vif(lm2_8))

setdiff(names(lm1$coefficients), names(lm2_8$coefficients)) # What predictors were removed?

## Response Transformation ---------------------------------------
bc <- MASS::boxcox(lm2_8, seq(-1, 1, by = 0.01), plotit = F)
summary(powerTransform(lm2_8)) # Recommends log transformation
bc2 <- tibble(x = bc$x, y = bc$y)
maxY <- max(bc2$y)

bc2 %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_vline(xintercept = c(0.0347, 0.0507, 0.0667), linetype = "dashed") +
  geom_hline(yintercept = maxY, linetype = "dashed") +
  labs(x = "Lambda", y = "Log-Likelihood") +
  geom_text(x = 0.12, y = maxY, label = "95%", hjust = 1, vjust = 0, color = "black") +
  theme(
    plot.title = element_text(size = 12, face = "bold", color = "darkblue"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

lm2_9 <- lm(`CO2_Emissions`^0.05 ~ .-id - Class_SPECIALPURPOSEVEHICLE - Cylinders_5 - Transmission_AV10 - FuelType_Natural_Gas - 
              Fuel_Efficiency_Highway - Fuel_Efficiency_Combined - Fuel_Efficiency_City - Fuel_Efficiency_Combined_MPG + 
              log(Fuel_Efficiency_Combined_MPG) - FuelType_Regular_Gas - Transmission_AS6 - Cylinders_4 - Class_SUV_SMALL - 
              engine_size, data = dataTrain)
summary(lm2_9)

## Stepwise Variable Selection (AIC) ------------------------------
lm3 <- step(lm2_9, trace = 0)

## Outliers ------------------------------
tOutlier <- dataTrain %>%
  mutate(stres = rstandard(lm3)) %>%
  relocate(stres, .after = id) %>%
  arrange(desc(abs(stres))) %>%
  filter(abs(stres) > 4) %>% 
  dplyr::select(id, stres)

# 9 Outliers
dataTrain2 <- dataTrain

# Mercedes Benz GLA 250 4MATIC
# Reported 128.. Way too loiw.. Cant find information with high confidence... REMOVE
dataTrain2 <- dataTrain2 %>% filter(id != 4084)

# CHEVROLET IMPALA DUAL FUEL
# Only model with fuel type N... Remove
dataTrain2 <- dataTrain2 %>% filter(id != 2440)

# Mercedes Benz E 300 4Matic
# Other 2 cars with identical specs: 227, 228 while this benz = 246.. Set to 228
dataTrain2[dataTrain2$id %in% c(4075), "CO2_Emissions"] <- 228

# GMC Sierra
dataTrain2 <- dataTrain2 %>% filter(id != 3762)

# MERCEDES-BENZ GL 450 4MATIC
dataTrain2 <- dataTrain2 %>% filter(id != 3005)

# Chevy Malibu
# Cant find definitive info, plus inconsistency between many data points for this car.. REMOVE
dataTrain2 <- dataTrain2 %>% filter(id != 6700)

# MERCEDES-BENZ AMG CLS 53 4MATIC+
dataTrain2 <- dataTrain2 %>% filter(!id %in% c(6161, 7170))

# Mercedes Benz b250 
# Other 2 cars with identical specs has emissions of 184 and 188, while this benz b250 had 179. Set to 186
dataTrain2[dataTrain2$id %in% c(4069), "CO2_Emissions"] <- 186

# One new outlier: Remove
dataTrain2 <- dataTrain2 %>% filter(id != 6201)

## Final Model -------------------------------------------------------

# Data outliers addressed -- New Model and retransformed to 0.03
# Note: Ran model and boxcox with no transformation first, which recommended lambda = 0.03.. Didnt include that part to save space
lm4_1 <- lm(formula = CO2_Emissions^0.03 ~ Class_COMPACT + Class_MID_SIZE + 
              Class_SUV_STANDARD + Class_VAN_PASSENGER + Class_PICKUPTRUCK_STANDARD + 
              Class_MINIVAN + Class_PICKUPTRUCK_SMALL + Cylinders_6 + Cylinders_12 + 
              Cylinders_8 + Cylinders_10 + Cylinders_3 + Cylinders_16 + 
              Transmission_AS5 + Transmission_A6 + Transmission_AM7 + Transmission_AS8 + 
              Transmission_A4 + Transmission_M5 + Transmission_AV + Transmission_AS7 + 
              Transmission_A9 + Transmission_AS9 + Transmission_AV6 + Transmission_AM5 + 
              Transmission_AM8 + Transmission_AM9 + Transmission_AS10 + 
              FuelType_Premium_Gas + FuelType_Diesel + FuelType_Ethanol + log(Fuel_Efficiency_Combined_MPG), 
            data = dataTrain2)

# Check if another transformation is needed
MASS::boxcox(lm4_1, seq(-5, 5, by = 0.01))
summary(powerTransform(lm4_1))
summary(lm4_1)

# Summary Plots
plotChecks(lm4_1, dataTrain2, dataTrain2$CO2_Emissions^0.03)

# Assumption tests
check_heteroskedasticity(lm4_1)
check_normality(lm4_1)

# Check for any new outliers
tOutlier <- dataTrain2 %>%
  mutate(stres = rstandard(lm4_1)) %>%
  relocate(stres, .after = id) %>%
  arrange(desc(abs(stres))) %>%
  filter(abs(stres) > 4) 

# Model: Weighted Multiple Linear Regression ===============================================

## Get weights and run model -----------------------
wt <- 1 / lm(abs(lm4_1$residuals) ~ lm4_1$fitted.values)$fitted.values^2

lmW <- lm(formula = CO2_Emissions^0.03 ~ Class_COMPACT + Class_MID_SIZE + 
            Class_SUV_STANDARD + Class_VAN_PASSENGER + Class_PICKUPTRUCK_STANDARD + 
            Class_MINIVAN + Class_PICKUPTRUCK_SMALL + Cylinders_6 + Cylinders_12 + 
            Cylinders_8 + Cylinders_10 + Cylinders_3 + Cylinders_16 + 
            Transmission_AS5 + Transmission_A6 + Transmission_AM7 + Transmission_AS8 + 
            Transmission_A4 + Transmission_M5 + Transmission_AV + Transmission_AS7 +
            Transmission_A9 + Transmission_AS9 + Transmission_AV6 + Transmission_AM5 + 
            Transmission_AM8 + Transmission_AM9 + Transmission_AS10 + 
            FuelType_Premium_Gas + FuelType_Diesel + FuelType_Ethanol + log(Fuel_Efficiency_Combined_MPG), 
          data = dataTrain2, weights = wt)
summary(lmW)

## Outliers Check ----------------------
tOutlier <- dataTrain2 %>%
  mutate(stres = rstandard(lmW)) %>%
  relocate(stres, .after = id) %>%
  arrange(desc(abs(stres))) %>%
  filter(abs(stres) > 4) 

## Response transformation check -------------------------
MASS::boxcox(lmW, seq(-5, 5, by = 0.01))
summary(powerTransform(lmW)) 

## VIF Check -----------------------------
sort(vif(lmW))

## Assumptions Check ------------------------
plotChecks(lmW, dataTrain2, dataTrain2$CO2_Emissions^0.03) # Summary plots
check_heteroskedasticity(lmW) # Test
check_normality(lmW) # Test

# Random Forest =====================================================

## Run model ----------------------

# Note: Since it takes some time to run, ran once and saved output as RDS
if (F) {
  set.seed(30)
  rf.fit <- randomForest(CO2_Emissions ~ . - id, data=dataTrain2, ntree=500,
                         keep.forest=TRUE, importance=TRUE)
  saveRDS(rf.fit, "data/randomForestModel.RDS")
}

rf.fit <- readRDS("data/randomForestModel.RDS") # Read in random forest model

## Variable importance ----------------------------------------------

# Get variable importance from the model fit
ImpData <- as.data.frame(importance(rf.fit))
ImpData$Var.Names <- row.names(ImpData)
ImpData <- ImpData %>% 
  arrange(desc(`%IncMSE`)) %>% 
  slice_head(n=10)

ggplot(ImpData, aes(x=reorder(Var.Names, `%IncMSE`), y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  labs(y = "% Increase in MSE if Predictor is Removed", x = "Predictor") +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

# Model Performnace and Comparison =======================================

## Non-weighted MLR----------------------------------
aic1 <- stats::AIC(lm4_1)
r2 <- summary(lm4_1)$r.squared
r2a <- summary(lm4_1)$adj.r.squared
trainRMSE <- cRMSE(dataTrain2$CO2_Emissions, lm4_1$fitted.values^(1/0.03))
preds <- predict(lm4_1, newdata = dataTest)^(1/0.03)
testRMSE <- cRMSE(dataTest$CO2_Emissions, preds)

## Weighted MLR ----------------------------------------
w_aic <- stats::AIC(lmW)
w_r2 <- summary(lmW)$r.squared
w_r2a <- summary(lmW)$adj.r.squared
w_trainRMSE <- cRMSE(dataTrain2$CO2_Emissions, lmW$fitted.values^(1/0.03))
w_preds <- predict(lmW, newdata = dataTest)^(1/0.03)
w_testRMSE <- cRMSE(dataTest$CO2_Emissions, preds)

## Random Forest -----------------------------------------
rf_trainRMSE <- sqrt(rf.fit$mse[length(rf.fit$mse)])
predValues <- predict(rf.fit, newdata = dataTest, type = "response")
rf_testRMSE <- cRMSE(dataTest$CO2_Emissions, predValues)

## Create table ------------------------------------------
tibble(Model = c("Multiple Linear Regression (MLR)", "Weighted MLR", "Random Forest"),
       AIC = c(aic1, w_aic, NA),
       R2 = c(r2, w_r2, NA),
       `R2 (Adjusted)` = c(r2a, w_r2a, NA),
       `Training RMSE` = c(trainRMSE, w_trainRMSE, rf_trainRMSE), 
       `Testing RMSE` = c(testRMSE, w_testRMSE, rf_testRMSE)
) %>% 
  mutate(`% Change Train -> Test RMSE` = (`Testing RMSE` - `Training RMSE`)/`Training RMSE`,
         across(c("R2", "R2 (Adjusted)", "% Change Train -> Test RMSE"), ~scales::percent(.x, accuracy = 0.0001))) %>% 
  flextable() %>% 
  theme_apa() %>% 
  colformat_double(j = 2, digits = 2) %>% 
  colformat_double(j = 5:6, digits = 5)

# Appendix Tables ================================================

## Head of dataset --------------------
tDat <- data %>% 
  rename(Make = make, Model = model, Class = class, `Engine Size (L)` = engine_size, `Cylin-ders` = cylinders, `Trans-mission` = transmission, `Fuel Type` = fuel_type, `City (L/ 100km)` = fuel_consume_city, `High-way (L/ 100km)` = fuel_consume_hwy, `Comb-ined (L/ 100km)` = fuel_consume_comb_l, `Comb-ined (MPG)` = fuel_consume_comb_mpg , `CO2 Emissions (g/km)` = co2_emissions)

widthTotal <- 6.5
widthFirst3 <- 2/3
widthLast9 <- (widthTotal - 3*widthFirst3)/9

flextable(data = head(tDat)) %>% 
  add_header_row(values = c("", "Fuel Efficiency", ""), colwidths = c(7, 4, 1)) %>% 
  width(j = 1:ncol(tDat), width = c(rep(widthFirst3, 3), rep(widthLast9, 9))) %>% 
  fontsize(size = 10, part = "all") %>% 
  add_footer_lines("L = Liters; L/100km = Liters per 100 kilometers; MPG = milers per gallon; g/km = grams per kilometer") %>% 
  theme_apa()

## Metadata ----------------------------
widthTotal <- 6.5
width_3_4 <- 2.5
width_1_2 <- (widthTotal - 2*width_3_4)/2

flextable(metadata)%>%
  theme_apa() %>%
  bg(bg = "gray", part = "header") %>%
  bold(j = 1, bold = TRUE, part = "body") %>%
  fontsize(size = 10, part = "all") %>%
  padding(padding = 2) %>%
  width(width = c(rep(width_1_2, 2), rep(width_3_4, 2)))

## Final Non-weighted MLR Model - Summary Table ---------------------
flextable::as_flextable(lm4_1) %>% 
  flextable::colformat_double(digits = 6) %>% 
  flextable::padding(padding = 1)
