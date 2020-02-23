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

#remove undesired columns. These columns will not be considered for this particular analysis.

cneos_closeapproach_data  <- select(cneos_closeapproach_data, -V_Infinity, -H, CA_Distance_Minimum)

cneos_sentry_summary_data <- select(cneos_sentry_summary_data, -Est._Diameter, -Object, -V_Infinity, -H)

 
# create collision dataset, fix Object name

collision_NEO <- inner_join(cneos_closeapproach_data, cneos_sentry_summary_data, by = "UniqueID")
collision_NEO_left <- left_join(cneos_closeapproach_data, cneos_sentry_summary_data, by = "UniqueID")
collision_NEO$Object <- gsub('[\\(\\)]', '', collision_NEO$Object)

# narrow down columns
#collision_NEO <- collision_NEO %>% select(colnames(collision_NEO)[c(1,2,3,4,5,6,7,8,9,11, 12,13,14,16, 17, 18,19)])

# fix join naming errors
#colnames(collision_NEO)[c(1,6,7,8)] <- c("Object", "V_Infinity", "H", "Est._Diameter")

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

#remove original estimated diameter column.
collision_NEO <- select(collision_NEO, -Est._Diameter)



# Palermo Scale Translation

collision_NEO$pscale[collision_NEO$Palermo_Scale_max < -2 ] <- "No Concern"
collision_NEO$pscale[collision_NEO$Palermo_Scale_max < 1 & collision_NEO$Palermo_Scale_max > -2] <- "Careful Monitoring"
collision_NEO$pscale[collision_NEO$Palermo_Scale_max > 0 ] <- "Some Concern"


collision_NEO$approach_year <- year(ymd(collision_NEO$`Close-Approach_Date`))
#P


#diameter histogram
all_diam <- data.frame(collision_NEO$avg_diameter)
colnames(all_diam) <- "Avg_Diameter"

#Initial Histogram of NEO Size
ggplot(all_diam, aes(x = Avg_Diameter)) +
  geom_histogram(bins = 45, fill = "#253494", color = "#081d58") +
  theme_classic() +
  labs(title = "Histogram of Average Diameter of NEOs",
       subtitle = "200 meters or fewer",
       x = "Avg Diameter",
       y = "Number of NEOs") +
  xlim(0,180)

Total_NEO <- nrow(data.table(unique(cneos_closeapproach_data$Object)))
Total_NEO_Collision <- nrow(data.table(unique(cneos_sentry_summary_data$UniqueID)))

#percent of NEOs with Strike probabiltiy
(Total_NEO_Collision/Total_NEO)*100
#min max
max(collision_NEO$V_Relative)
min(collision_NEO$V_Relative)

ggplot(collision_NEO, aes(x = collision_NEO$V_Relative, y = collision_NEO$avg_diameter)) +
  geom_point() + stat_smooth()

#negligible correlation
cor(collision_NEO$V_Relative, collision_NEO$avg_diameter)


#filter Jan12000 - Dec 31 2020
collision_NEO_00_20 <- collision_NEO %>% filter(year(ymd(collision_NEO$`Close-Approach_Date`)) > "1999") 

#plot
ggplot(collision_NEO_00_20, aes(x = collision_NEO_00_20$V_Relative, y = collision_NEO_00_20$avg_diameter)) +
  geom_point() + stat_smooth()

mod <- lm(collision_NEO_00_20$avg_diameter ~ collision_NEO_00_20$V_Relative, data = collision_NEO_00_20)
summary(mod)

fitted(mod)

coefficients(mod) 

ggplot(collision_NEO_00_20, aes(x = collision_NEO_00_20$V_Relative, y = collision_NEO_00_20$Object)) +
  geom_boxplot()

gsub("\\..*","",collision_NEO_00_20$CA_Distance_Nominal)

t.test(collision_NEO_00_20$avg_diameter, collision_NEO$avg_diameter)

#scale

max(collision_NEO_00_20$Palermo_Scale_max)

collision_NEO_00_20$pscale <- collision_NEO_00_20$Palermo_Scale_max

collision_NEO_00_20$pscale[collision_NEO_00_20$Palermo_Scale_max < -2 ] <- "No Concern"
collision_NEO_00_20$pscale[collision_NEO_00_20$Palermo_Scale_max < 1 & collision_NEO_00_20$Palermo_Scale_max > -2] <- "Careful Monitoring"
collision_NEO_00_20$pscale[collision_NEO_00_20$Palermo_Scale_max > 0 ] <- "Some Concern"

ggplot(collision_NEO_00_20, aes(x = factor(collision_NEO_00_20$pscale), y = collision_NEO_00_20$V_Relative)) +
  geom_point(size = collision_NEO_00_20$avg_diameter/65, alpha = .1) +
  labs()

#color prep
future_neo <- collision_NEO[year(ymd(collision_NEO$`Close-Approach_Date`)) > 2019,]


min(year(ymd(collision_NEO$`Close-Approach_Date`)))


#plotting pscale concern levels. Diameter divided by 65 for point size comparrison.
ggplot(collision_NEO, aes(x = factor(collision_NEO$pscale), y = collision_NEO$V_Relative)) +
  geom_point(position = "jitter", size = collision_NEO$avg_diameter/65, alpha = .3, color = "skyblue") +
  geom_point(position = "jitter", data = future_neo, aes(x = factor(future_neo$pscale), y = future_neo$V_Relative), size = future_neo$avg_diameter/65, alpha = .3, colour = "red") +
  labs(title = "Palermo Scale of All NEOs vs Velocity (km/second)",
       subtitle = "Red = Future NEO, Blue = Past NEO. Point size relative to avg. Diameter.",
       x = "Palermo Scale",
       y = "Velocity(km/s)") +
  theme_minimal()

#future
ggplot(collision_NEO, aes(x = factor(collision_NEO$pscale), y = collision_NEO$V_Relative)) +
  geom_point(position = "jitter", size = collision_NEO$avg_diameter/65, alpha = .3, color = "skyblue") +
  geom_point(position = "jitter", data = future_neo, aes(x = factor(future_neo$pscale), y = future_neo$V_Relative), size = future_neo$avg_diameter/65, alpha = .3, colour = "red") +
  labs(title = "Palermo Scale of All NEOs vs Velocity (km/second)",
       subtitle = "Red = Future NEO, Blue = Past NEO. Point size relative to avg. Diameter.",
       x = "Palermo Scale",
       y = "Velocity(km/s)") +
  theme_minimal()

#
ggplot(collision_NEO, aes(x = factor(collision_NEO$pscale), y = collision_NEO$V_Relative)) +
  geom_point(position = "jitter", size = collision_NEO$avg_diameter/65, alpha = .3, color = "skyblue") +
  geom_point(position = "jitter", data = future_neo, aes(x = factor(future_neo$pscale), y = future_neo$V_Relative), size = future_neo$avg_diameter/65, alpha = .3, colour = "red") +
  labs(title = "Velocity (km/second) of all NEOs, by Palermo Scale Level",
       subtitle = "Red = Future NEO, Blue = Past NEO. Point size relative to avg. Diameter.
One point = 1 Approach",
       x = "Palermo Scale",
       y = "Velocity(km/s)") +
  theme_minimal()


neo_coll_scale_speed <- collision_NEO %>% select("Object", "V_Relative", "pscale", "avg_diameter")
neo_coll_scale_speed <- unique(neo_coll_scale_speed)
#x 
ggplot(neo_coll_scale_speed, aes(x = factor(neo_coll_scale_speed$pscale), y = neo_coll_scale_speed$V_Relative)) +
  geom_point(position = "jitter", size = neo_coll_scale_speed$avg_diameter/65, alpha = .3, color = "skyblue") +
  #geom_point(position = "jitter", data = future_neo, aes(x = factor(future_neo$pscale), y = future_neo$V_Relative), size = future_neo$avg_diameter/65, alpha = .3, colour = "red") +
  labs(title = "Velocity (km/second) of all NEOs, by Palermo Scale Level",
       subtitle = "Red = Future NEO, Blue = Past NEO. Point size relative to avg. Diameter.
One point = 1 Approach",
       x = "Palermo Scale",
       y = "Velocity(km/s)") +
  theme_minimal()



ggplot(collision_NEO, aes(x = collision_NEO$avg_diameter, y = collision_NEO$Palermo_Scale_max)) +
  geom_point(position = "jitter",  alpha = .3, color = "skyblue") +
  geom_point(position = "jitter", data = future_neo, aes(x = future_neo$avg_diameter), y = future_neo$Palermo_Scale_max, alpha = .3, colour = "red") +
  labs(title = "Velocity (km/second) of all NEOs, by Palermo Scale Level",
       subtitle = "Red = Future NEO, Blue = Past NEO. Point size relative to avg. Diameter.",
       x = "Palermo Scale",
       y = "Velocity(km/s)") +
  scale_y_log10() +
  theme_minimal()

#scatter

neo_model <- lm(collision_NEO$avg_diameter ~ collision_NEO$Palermo_Scale_max, data = collision_NEO)
summary(neo_model)
coefficients(neo_model)

ggplot(collision_NEO, aes(x = collision_NEO$Palermo_Scale_max, y = collision_NEO$avg_diameter)) +
  geom_point(alpha = .6, 
             color = "skyblue") +
  geom_point(data = future_neo, aes(x = future_neo$Palermo_Scale_max, y = future_neo$avg_diameter), 
             alpha = .6, 
             colour = "red") +
  #geom_abline(slope = coefficients(neo_model)[[2]], intercept = coefficients(neo_model)[[1]], color="Green") +
  labs(title = "Palermo Scale vs Average Diameter (log10 scale)",
       subtitle = "Red = Future NEO, Blue = Past NEO. Point size relative to avg. Diameter.",
       x = "Palermo Scale",
       y = "Average Diameter (log10)") +
  scale_y_log10() +
  #stat_smooth() +
  theme_minimal()

cor(collision_NEO$Palermo_Scale_max,collision_NEO$avg_diameter )


ggplot(collision_NEO[collision_NEO$approach_year > "1999" & collision_NEO$approach_year < "2020",], 
       aes(x = collision_NEO[collision_NEO$approach_year > "1999" & collision_NEO$approach_year < "2020",]$Palermo_Scale_max, 
           y = collision_NEO[collision_NEO$approach_year > "1999" & collision_NEO$approach_year < "2020",]$avg_diameter)) +
  geom_point(alpha = .6, color = color) +
  scale_y_log10() +
  #stat_smooth() +
  facet_wrap(~approach_year)


color <- ifelse(collision_NEO[collision_NEO$approach_year > "1999" & collision_NEO$approach_year < "2020",]$Palermo_Scale_max > -2,'red','skyblue')

palermo_concern <- collision_NEO[collision_NEO$pscale == "Careful Monitoring",]

ggplot(palermo_concern, aes(x = palermo_concern$avg_diameter, y = palermo_concern$Potential_Impacts)) +
         geom_point()



collision_NEO %>% mutate(decade = floor(year(ymd(collision_NEO$`Close-Approach_Date`))/10)*10) %>% 
  group_by(decade) %>% 
  summarize_all(sum) %>% 
  select(-year)

collision_NEO_2010$`Close-Approach_Date`
ggplot(collision_NEO_2010_grp) + geom_histogram(aes(x = collision_NEO_2010_grp$objects), stat = "count") +
  labs(title = "NEO Objects on the Sentry Monitoring System",
       subtitle = "Years 2010-2020",
       x = "Object Name",
       y = "Nbr. of Close Approaches") +
  theme_minimal()