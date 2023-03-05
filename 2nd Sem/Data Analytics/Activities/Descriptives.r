install.packages("modeest")
install.packages("tidyverse")

library(tidyverse)
library(modeest)
library(readxl)

# Set working directory to the location of smokingsurvey.xlsx
smokingsurvey <- read_excel("smokingsurvey-1.xlsx") 
View(smokingsurvey)
df = data.frame(smokingsurvey)


# Central Tendency
# Mean
mean = df %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)

# Mode
mode = df %>%
    summarise_if(is.numeric, mfv, na.rm = TRUE)

# Median
median = df %>%
    summarise_if(is.numeric, median, na.rm = TRUE)

# Measure of Variations
# Range
range = (df %>%
    summarise_if(is.numeric, range, na.rm = TRUE)) %>%
		summarise_if(is.numeric, diff, na.rm = TRUE)    

# IQR
iqr = df %>%
    summarise_if(is.numeric, IQR, na.rm = TRUE)

# Variance
var = df %>%
    summarise_if(is.numeric, var, na.rm = TRUE)

# Standard Deviation
stdev = df %>%
    summarise_if(is.numeric, sd, na.rm = TRUE)

# Summary of Descriptives
summary(df)
