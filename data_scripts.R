library(tidyverse)
library(RColorBrewer)
library(lubridate)
library(stringr)
library(data.table)
cneos_closeapproach_data <- read.csv("datafiles/cneos_closeapproach_data.csv", stringsAsFactors = FALSE)

cneos_sentry_summary_data <- read.csv("datafiles/cneos_sentry_summary_data.csv", stringsAsFactors = FALSE)

colnames(cneos_closeapproach_data) <- c("Object", "Close-Approach Date", "CA Distance Nominal", 
                                        "CA Distance Minimum", "V Relative", "V Infinity", "H", "Est. Diameter", "UniqueID")

colnames(cneos_sentry_summary_data) <- c("Object", "Year Range", "Potential Impacts", 
                                         "Cum Impact Probability", "V Infinity", "H", "Est. Diameter", 
                                         "Palermo Scale cum", "Palermo Scale max", "Torino Scale", "UniqueID")
#fix dates (no time)
cneos_closeapproach_data$`Close-Approach Date` <-  substring(cneos_closeapproach_data$`Close-Approach Date`,1,11)

collision_NEO <- inner_join(cneos_closeapproach_data, cneos_sentry_summary_data, by = "UniqueID")
collision_NEO$Object.x <- gsub('[\\(\\)]', '', collision_NEO$Object.x)

str(cneos_closeapproach_data)
# NEO dataset has 148022 observations of 9 variables
# 7 Character Variables, (One which will be converted to date), 2 Number Variables

str(cneos_sentry_summary_data)
# 986 observations of 11 variables
# 3 character, 2 integer, and 6 number

str(collision_NEO)
#This is a joined dataset for exploration of the impact objects. It is 4840 Observations of 19 Variables.
cneos_closeapproach_data[1:10, ]

#find most frightening object
collision_count_by_object <- as.data.frame(table(collision_NEO$Object.x))
collision_count_by_object[which.max(collision_count_by_object$Freq),]
#example histo
ggplot(cneos_sentry_summary_data, aes(x = cneos_sentry_summary_data$`Potential Impacts`)) +
  geom_histogram(bins = 45, fill = "#253494", color = "#081d58") +
  theme_classic() +
  labs(title = "Histogram of Possible Collisions Per NEO",
       x = "# of Objects",
       y = "Frequency of Possible Collisions")

y <- dnorm(cneos_sentry_summary_data$`Potential Impacts`)

#Exlore data dictionary
# analytics of Sizes, distances, impact dates, impact counts, distance/size, Velocity to size, velocity analytics, Hazard Scales!

object_count <- nrow(distinct(cneos_closeapproach_data %>% select("Object")))
#17853 Unique Objects`
impact_count <- nrow(distinct(cneos_sentry_summary_data %>% select("Object")))
#986 Potential Impacts
impact_count/object_count*100

# 5.52% of all known objects have a chance to impact Earth.

cneos_sentry_summary_data %>% select("Object") 
cneos_closeapproach_data %>% select("Object")



joined_objects <- collision_NEO %>% select("Object")

nrow(distinct(cneos_sentry_summary_data %>% select("Object")))
#986 possible collision points

str(cneos_closeapproach_data)

#finding years with most close calls
ca_years <- ymd(collision_NEO$`Close-Approach Date`)

colnames(ca_years) <- "Year"

str(ca_years)

year(ca_years)

ggplot(data.frame(ca_years), aes(x = year(ca_years))) + geom_histogram()

year_count <- as.data.frame(table(year(ca_years)))

view(year_count)

max(year_count$Freq)
year_count[which.max(year_count$Freq),]
year_count[which.min(year_count$Freq),]

colnames(collision_NEO)


collision_NEO$Object.x <- gsub('[\\(\\)]', '', collision_NEO$Object.x)

collision_NEO$`Est. Diameter.x`

diameter <- unlist(strsplit(collision_NEO$`Est. Diameter.x`, "[-]"))

recursive indexing failed at level 2

list <- strsplit(collision_NEO$`Est. Diameter.x`, "[-]")[[c(2,3,4)]][1]

unlist <- unlist(list)
data.frame(unlist)
mean(c(20,45))

#create min diameter
collision_NEO$diam_min <- str_split_fixed(collision_NEO$`Est. Diameter.x`, "-", 2)[1:nrow(collision_NEO)]

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
collision_NEO$diam_min <- as.numeric(round(collision_NEO$diam_min, digits = 1))



#create max diameter
collision_NEO$diam_max <- str_split_fixed(collision_NEO$`Est. Diameter.x`, "-", 2)[1:nrow(collision_NEO),2]
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
collision_NEO$diam_max <- as.numeric(round(collision_NEO$diam_max,digits = 1))

#create average diameter
collision_NEO$avg_diameter <- collision_NEO$diam_min + collision_NEO$diam_max/2

all_diam <- data.frame(collision_NEO$avg_diameter)

colnames(all_diam) <- "Avg_Diameter"
#diameter plot
ggplot(all_diam, aes(x = Avg_Diameter, y = ..density..)) +
  geom_histogram(bins = 45, fill = "#253494", color = "#081d58") +
  theme_classic() +
  labs(title = "Histogram of Possible Collisions Per NEO",
       x = "# of Objects",
       y = "Frequency of Possible Collisions")
