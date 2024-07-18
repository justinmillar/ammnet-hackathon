## Libraries
library(tidyverse)

## Download example data

## Load example data
mosquito_data  <- read_csv("01_data-vis/data/mosq_mock.csv")

#' Challenge 3: Create ggplot2 visualizations of the mosquito_data dataset
#'   * Are their any interesting patterns in individual variables/columns?
#'   * How can we use the aes() function to view multiple variables in a single plot?
#'   * Are there any additional geometries that may be useful for visualizing this dataset?

#' CHALLENGE 1 Create a figure showing how the Anopheles gambiae total counts vary each day and by location
#' CHALLENGE 2 Create a figure showing the hourly Anopheles gambiae total counts each hour

#' An answer to challenge 1
#' Some rearrangement of the data is used here to summarise the daily mosquito counts
library(dplyr)
day_mosq = mosquito_data %>% 
  group_by(Compound.ID,Location) %>%
  summarize(Ag.fed = sum(Ag.fed,na.rm=T),
            tot.gamb = sum(tot.gamb,na.rm=T)) 

#' And then we can consider any differences in mosquito location?
day_mosq$Compound = as.character(day_mosq$Compound.ID)
ggplot(data = day_mosq, aes(x = Location, y = tot.gamb, fill = Location)) +
  geom_boxplot() +
  geom_point(aes(shape = Compound), 
             position=position_jitterdodge(), size = 3) +
  scale_fill_manual(values = c("#C6E0FF", "#136F63")) +
  labs(title = "Mosquito counts by location varying daily",
       subtitle = "Data from 4 compounds",
       x = "Location",
       y = "Anopheles gambiae counts",
       fill = "Location") +
  theme_classic()

#' An answer to challenge 2
#' Some added information is required first to ensure the hours are read 
#' overnight as they were sampled
#' List hours of the night in an order
# 
# mosquito_data$hours_ordered = ifelse(mosquito_data$hour == unique(mosquito_data$hour)[1],1,
#                                ifelse(mosquito_data$hour == unique(mosquito_data$hour)[2],2,
#                                       ifelse(mosquito_data$hour == unique(mosquito_data$hour)[3],3,
#                                              ifelse(mosquito_data$hour == unique(mosquito_data$hour)[4],4,
#                                                     ifelse(mosquito_data$hour == unique(mosquito_data$hour)[5],5,
#                                                            ifelse(mosquito_data$hour == unique(mosquito_data$hour)[6],6,
#                                                                   ifelse(mosquito_data$hour == unique(mosquito_data$hour)[7],7,
#                                                                          ifelse(mosquito_data$hour == unique(mosquito_data$hour)[8],8,
#                                                                                 ifelse(mosquito_data$hour == unique(mosquito_data$hour)[9],9,
#                                                                                        ifelse(mosquito_data$hour == unique(mosquito_data$hour)[10],10,
#                                                                                               ifelse(mosquito_data$hour == unique(mosquito_data$hour)[11],11,
#                                                                                                      ifelse(mosquito_data$hour == unique(mosquito_data$hour)[12],12,13))))))))))))
# 
# Update csv so that this field is included
# write_csv(mosquito_data, "01_data-vis/data/mosq_mock.csv")

#' Create a custom color scale
library(RColorBrewer)
myColors <- c("#E0CA3C", "#3E2F5B")
names(myColors) <- levels(mosquito_data$Location)
colScale <- scale_colour_manual(name = "Location",values = myColors)

#' And plot the mosquito counts per hour
ggplot(data = mosquito_data, aes(x = hours_ordered, y = tot.gamb, color = Location)) +
  geom_point(size = 2) + colScale +
  geom_line() +
  facet_wrap(~ Compound.ID) +
  labs(title = "Hourly mosquito counts by location and compound",
       subtitle = "Data from 4 compounds tested using human landing catches",
       x = "Location",
       y = "Anopheles gambiae counts",
       fill = "location") +
  scale_x_discrete(name ="Collection hour", limits=unique(mosquito_data$hour), guide = guide_axis(angle = 45)) +
  theme_classic()

