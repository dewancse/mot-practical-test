install.packages("tidyverse")
library(tidyverse)

getwd()
setwd("/home/dewan/Downloads/R Project/Ministry of Transport")

data <- read_csv("Crash_Analysis_System_(CAS)_data.csv")
head(data)

colnames(data)

# Number of crashes by Year 
ggplot(data, aes(x = factor(crashYear))) + 
  geom_bar(aes(y = ..count..), stat = "count", fill = "#016064") +
  geom_text(aes(y= ..count.., label= ..count..), color = "red", size = 2.5, vjust = -0.5, stat = "count") +
  xlab("Crash Year") +
  ylab("Number of Crashes") +
  ggtitle("Number of crashes in New Zealand by Year") +
  theme(axis.title.x = element_text(colour = "Black", size = 10),
        axis.title.y = element_text(colour = "Black", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(colour = "Black", size = 15),
        panel.background = element_rect(fill = "White"))

# Number of crashes by Region
region <- data$region[!is.na(data$region)]

df <- data.frame(region)

ggplot(df, aes(x = region)) + 
  geom_bar(aes(y = ..count..), stat = "count", fill = "#00AFBB") +
  geom_text(aes(y= ..count.., label= ..count..), color = "red", size = 3, vjust = -0.5, stat = "count") +
  xlab("Region in New Zealand") +
  ylab("Number of Crashes") +
  ggtitle("Number of crashes between 2000 and 2021 by Region") +
  theme(axis.title.x = element_text(colour = "Black", size = 10),
        axis.title.y = element_text(colour = "Black", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(colour = "Black", size = 15),
        panel.background = element_rect(fill = "White")) + 
  scale_y_continuous(breaks = seq(0, 1000000, by = 50000))

# Number of crashes by Year - block filled up by Region 
region <- data$region[!is.na(data$region)]
crashYear <- data$crashYear[!is.na(data$region)]

df <- data.frame(crashYear, region)

ggplot(df, aes(x = factor(crashYear), fill = region)) + 
  geom_bar() +
  xlab("Crash Year") +
  ylab("Number of Crashes") +
  ggtitle("Number of crashes between 2000 and 2021 by Year") +
  guides(fill = guide_legend(title = "Region in New Zealand")) +
  theme(axis.title.x = element_text(colour = "Black", size = 10),
        axis.title.y = element_text(colour = "Black", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.title = element_text(colour = "Black", size = 15),
        panel.background = element_rect(fill = "White"))

# Number of vehicle crashes by Region - block filled up by Type of Vehicle
vehicle <- data.frame(region = data$region, 
                      bicycle = data$bicycle,
                      bus = data$bus,
                      carStationWagon = data$carStationWagon,
                      motorcycle = data$motorcycle,
                      parkedVehicle = data$parkedVehicle,
                      schoolBus = data$schoolBus,
                      suv = data$suv,
                      taxi = data$taxi,
                      train = data$train,
                      truck = data$truck,
                      moped = data$moped)

vehicle <- vehicle %>% gather(bicycle,
                              bus,
                              carStationWagon,
                              motorcycle,
                              parkedVehicle,
                              schoolBus,
                              suv,
                              taxi,
                              train,
                              truck,
                              moped,
                              key = "vehicleType", 
                              value = "crashCount")

crashCount <- vehicle$crashCount[!is.na(vehicle$crashCount) & vehicle$crashCount > 0 & !is.na(data$region)]
vehicleType <- vehicle$vehicleType[!is.na(vehicle$crashCount) & vehicle$crashCount > 0  & !is.na(data$region)]
region <- vehicle$region[!is.na(vehicle$crashCount) & vehicle$crashCount > 0 & !is.na(data$region)]

df = data.frame(region, vehicleType, crashCount)

df <- df %>% 
  group_by(region, vehicleType) %>% 
  summarise(crashCount = sum(crashCount))

ggplot(df, aes(x = region, y = crashCount, fill = vehicleType)) + 
  geom_col() +
  xlab("Region in New Zealand") +
  ylab("Number of Vehicle Crashes") +
  ggtitle("Number of vehicle crashes between 2000 and 2021 by Region") +
  guides(fill = guide_legend(title = "Type of Vehicle")) +
  theme(axis.title.x = element_text(colour = "Black", size = 10),
        axis.title.y = element_text(colour = "Black", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(colour = "Black", size = 15),
        panel.background = element_rect(fill = "White")) +
  scale_y_continuous(breaks = seq(0, 10000000, by = 50000))

# Number of crashes by Region - block filled up by Severity of Crashes
severity <- data$crashSeverity[!is.na(data$crashSeverity) & !is.na(data$region)]
region <- data$region[!is.na(data$crashSeverity) & !is.na(data$region)]

df <- data.frame(region, severity)

ggplot(df, aes(x = region, fill = severity)) + 
  geom_bar() +
  xlab("Region in New Zealand") +
  ylab("Number of Crashes") +
  ggtitle("Number of crashes between 2000 and 2021 by Region") +
  guides(fill = guide_legend(title = "Severity of Crashes")) +
  theme(axis.title.x = element_text(colour = "Black", size = 10),
        axis.title.y = element_text(colour = "Black", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(colour = "Black", size = 15),
        panel.background = element_rect(fill = "White")) +
  scale_y_continuous(breaks = seq(0, 1000000, by = 50000))

# Number of crashes by Region - block filled up by State Highway Crash
region <- data$region[!is.na(data$region)]
crashSHDescription <- data$crashSHDescription[!is.na(data$region)]

df <- data.frame(region, crashSHDescription)

ggplot(df, aes(x = region, fill = crashSHDescription)) + 
  geom_bar() +
  xlab("Region in New Zealand") +
  ylab("Number of Crashes") +
  ggtitle("Number of crashes between 2000 and 2021 by Region") +
  guides(fill = guide_legend(title = "State Highway Crash")) +
  theme(axis.title.x = element_text(colour = "Black", size = 10),
        axis.title.y = element_text(colour = "Black", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(colour = "Black", size = 15),
        panel.background = element_rect(fill = "White")) +
  scale_y_continuous(breaks = seq(0, 1000000, by = 50000))

# Number of serious injuries by Region - block filled up by Type of Traffic Control
region <- data$region[!is.na(data$region)]
seriousInjuryCount <- data$seriousInjuryCount[!is.na(data$region)]
trafficControl <- data$trafficControl[!is.na(data$region)]

df <- data.frame(region, seriousInjuryCount, trafficControl)

df <- df %>% 
  group_by(region, trafficControl) %>% 
  summarise(seriousInjuryCount = sum(seriousInjuryCount, na.rm = TRUE))

ggplot(df, aes(x = region, y = seriousInjuryCount, fill = trafficControl)) + 
  geom_col() +
  xlab("Region in New Zealand") +
  ylab("Number of Serious Injuries") +
  ggtitle("Number of serious injuries between 2000 and 2021 by Region") +
  guides(fill = guide_legend(title = "Type of Traffic Control")) +
  theme(axis.title.x = element_text(colour = "Black", size = 10),
        axis.title.y = element_text(colour = "Black", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(colour = "Black", size = 15),
        panel.background = element_rect(fill = "White"))

# Number of crashes by Region - block filled up by Flat/Hill Road
region <- data$region[!is.na(data$region)]
flatHill <- data$flatHill[!is.na(data$region)]

df <- data.frame(region, flatHill)

ggplot(df, aes(x = region, fill = flatHill)) + 
  geom_bar() +
  xlab("Region in New Zealand") +
  ylab("Number of Crashes") +
  ggtitle("Number of crashes between 2000 and 2021 by Region") +
  guides(fill = guide_legend(title = "Flat/Hill Road")) +
  theme(axis.title.x = element_text(colour = "Black", size = 10),
        axis.title.y = element_text(colour = "Black", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(colour = "Black", size = 15),
        panel.background = element_rect(fill = "White")) +
  scale_y_continuous(breaks = seq(0, 1000000, by = 50000))

# Number of crashes by Region - block filled up by Road Type 
region <- data$region[!is.na(data$region)]
urban <- data$urban[!is.na(data$region)]

df <- data.frame(urban, region)

ggplot(df, aes(x = region, fill = urban)) + 
  geom_bar() +
  xlab("Region in New Zealand") +
  ylab("Number of Crashes") +
  ggtitle("Number of crashes between 2000 and 2021 by Region") +
  guides(fill = guide_legend(title = "Road Type")) +
  theme(axis.title.x = element_text(colour = "Black", size = 10),
        axis.title.y = element_text(colour = "Black", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.title = element_text(colour = "Black", size = 15),
        panel.background = element_rect(fill = "White")) + 
  scale_y_continuous(breaks = seq(0, 1000000, by = 50000))

# Number of crashes by Region - block filled up by Type of Lane
region <- data$region[!is.na(data$region)]
roadLane <- data$roadLane[!is.na(data$region)]

df <- data.frame(region, roadLane)

ggplot(df, aes(x = region, fill = roadLane)) + 
  geom_bar() +
  xlab("Region in New Zealand") +
  ylab("Number of Crashes") +
  ggtitle("Number of crashes between 2000 and 2021 by Region") +
  guides(fill = guide_legend(title = "Type of Lane")) +
  theme(axis.title.x = element_text(colour = "Black", size = 10),
        axis.title.y = element_text(colour = "Black", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(colour = "Black", size = 15),
        panel.background = element_rect(fill = "White")) +
  scale_y_continuous(breaks = seq(0, 1000000, by = 50000))

# Number of crashes by Region - block filled up by Type of Light
region <- data$region[!is.na(data$region)]
light <- data$light[!is.na(data$region)]

df <- data.frame(region, light)

ggplot(df, aes(x = region, fill = light)) + 
  geom_bar() +
  xlab("Region in New Zealand") +
  ylab("Number of Crashes") +
  ggtitle("Number of crashes between 2000 and 2021 by Region") +
  guides(fill = guide_legend(title = "Type of Light")) +
  theme(axis.title.x = element_text(colour = "Black", size = 10),
        axis.title.y = element_text(colour = "Black", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(colour = "Black", size = 15),
        panel.background = element_rect(fill = "White")) +
  scale_y_continuous(breaks = seq(0, 1000000, by = 50000))

# Number of serious injuries by Region - block filled up by Weather Condition
region <- data$region[!is.na(data$region) & data$weatherA != "Null" & data$weatherB != "Null" & data$weatherA != "None" & data$weatherB != "None"]
seriousInjuryCount <- data$seriousInjuryCount[!is.na(data$region) & data$weatherA != "Null" & data$weatherB != "Null" & data$weatherA != "None" & data$weatherB != "None"]
weatherA <- data$weatherA[!is.na(data$region) & data$weatherA != "Null" & data$weatherB != "Null" & data$weatherA != "None" & data$weatherB != "None"]
weatherB <- data$weatherB[!is.na(data$region) & data$weatherA != "Null" & data$weatherB != "Null" & data$weatherA != "None" & data$weatherB != "None"]

weather <- data.frame(region, seriousInjuryCount, weatherA, weatherB)

weather <- weather %>% 
  gather(weatherA, weatherB, key = "weatherType", value = "weatherCondition")

weather <- weather %>% 
  group_by(region, weatherCondition) %>% 
  summarise(seriousInjuryCount = sum(seriousInjuryCount, na.rm = TRUE))

ggplot(weather, aes(x = region, y = seriousInjuryCount, fill = weatherCondition)) + 
  geom_col() +
  xlab("Region in New Zealand") +
  ylab("Number of Serious Injuries") +
  ggtitle("Number of serious injuries between 2000 and 2021 by Region") +
  guides(fill = guide_legend(title = "Weather Condition")) +
  theme(axis.title.x = element_text(colour = "Black", size = 10),
        axis.title.y = element_text(colour = "Black", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(colour = "Black", size = 15),
        panel.background = element_rect(fill = "White"))

# Number of crashes by Region - block filled up by Type of Holiday
holiday <- data$holiday[!is.na(data$holiday) & !is.na(data$region)]
region <- data$region[!is.na(data$holiday) & !is.na(data$region)]

df <- data.frame(region, holiday)

ggplot(df, aes(x = region, fill = holiday)) + 
  geom_bar() +
  xlab("Region in New Zealand") +
  ylab("Number of Crashes") +
  ggtitle("Number of crashes between 2000 and 2021 by Region") +
  guides(fill = guide_legend(title = "Type of Holiday")) +
  theme(axis.title.x = element_text(colour = "Black", size = 10),
        axis.title.y = element_text(colour = "Black", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(colour = "Black", size = 15),
        panel.background = element_rect(fill = "White"))
