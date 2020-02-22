library(tidyverse)
library(RColorBrewer)
library(lubridate)
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




mean(c(20,45))
