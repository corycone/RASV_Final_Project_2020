# load libraries
library(tidyverse)
library(RColorBrewer)
library(lubridate)
library(stringr)
library(data.table)

# load data, rename columns

cneos_closeapproach_data <- read.csv("datafiles/cneos_closeapproach_data.csv", stringsAsFactors = FALSE)

cneos_sentry_summary_data <- read.csv("datafiles/cneos_sentry_summary_data.csv", stringsAsFactors = FALSE)

colnames(cneos_closeapproach_data) <- c("Object", "Close-Approach_Date", "CA_Distance_Nominal", 
                                        "CA_Distance_Minimum", "V_Relative", "V_Infinity", "H", "Est._Diameter", "UniqueID")

colnames(cneos_sentry_summary_data) <- c("Object", "Year_Range", "Potential_Impacts", 
                                         "Cum_Impact_Probability", "V_Infinity", "H", "Est._Diameter", 
                                         "Palermo_Scale_cum", "Palermo_Scale_max", "Torino_Scale", "UniqueID")

# fix dates (no time)

cneos_closeapproach_data$`Close-Approach_Date` <-  substring(cneos_closeapproach_data$`Close-Approach_Date`,1,11)
# create collision dataset, fix Object name

collision_NEO <- inner_join(cneos_closeapproach_data, cneos_sentry_summary_data, by = "UniqueID")
collision_NEO$Object.x <- gsub('[\\(\\)]', '', collision_NEO$Object.x)

# narrow down columns
collision_NEO <- collision_NEO %>% select(colnames(collision_NEO)[c(1,2,3,4,5,6,7,8,9,11, 12,13,14,16, 17, 18,19)])

# fix join naming errors
colnames(collision_NEO)[c(1,6,7,8)] <- c("Object", "V_Infinity", "H", "Est._Diameter")

# create average diameter column

#create min diameter
collision_NEO$diam_min <- str_split_fixed(collision_NEO$Est._Diameter, "-", 2)[1:nrow(collision_NEO)]

#isolate min diameter
collision_NEO[collision_NEO$diam_min %like% 'm',]$diam_min


#remove spaces 3 times, just to be safe
collision_NEO$diam_min <- str_remove(collision_NEO[collision_NEO$diam_min %like% 'm',]$diam_min, "[ ]") 
collision_NEO$diam_min <- str_remove(collision_NEO[collision_NEO$diam_min %like% 'm',]$diam_min, "[ ]") 
collision_NEO$diam_min <- str_remove(collision_NEO[collision_NEO$diam_min %like% 'm',]$diam_min, "[ ]")
#remove the m
collision_NEO$diam_min <- str_remove(collision_NEO$diam_min, "[m]") 
#isolate the k and adjust to meters
collision_NEO[collision_NEO$diam_min %like% 'k',]$diam_min <- c("1000","1000","1000","1000","1000","1000","1000","1000")
#convert to numeric
collision_NEO$diam_min <- as.numeric(collision_NEO$diam_min)



#create max diameter
collision_NEO$diam_max <- str_split_fixed(collision_NEO$Est._Diameter, "-", 2)[1:nrow(collision_NEO),2]
#remove spaces 3 times, just to be safe
collision_NEO$diam_max <- str_remove(collision_NEO$diam_max, "[ ]") 
collision_NEO$diam_max <- str_remove(collision_NEO$diam_max, "[ ]") 
collision_NEO$diam_max <- str_remove(collision_NEO$diam_max, "[ ]")
#remove the m
collision_NEO$diam_max <- str_remove(collision_NEO$diam_max, "[m]") 
#isolate the k and convert to meters
collision_NEO[collision_NEO$diam_max %like% 'k',]$diam_max <- c("2300", "2300", "1100", "1000",
                                                                "2300", "1200", "2300", "2300",
                                                                "2300", "2300", "2300")
#convert to rounded numeric
collision_NEO$diam_max <- as.numeric(collision_NEO$diam_max)

#create average diameter
collision_NEO$avg_diameter <- (collision_NEO$diam_min + collision_NEO$diam_max)/2

#diameter histogram
all_diam <- data.frame(collision_NEO$avg_diameter)
colnames(all_diam) <- "Avg_Diameter"

ggplot(all_diam, aes(x = Avg_Diameter)) +
  geom_histogram(bins = 45, fill = "#253494", color = "#081d58") +
  theme_classic() +
  labs(title = "Histogram of Average Diameter of NEOs",
       subtitle = "200 meters or fewer",
       x = "Avg Diameter",
       y = "Number of NEOs") +
  xlim(0,180)