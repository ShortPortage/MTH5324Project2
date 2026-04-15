library(archive)
library(readr)
library(dplyr)
library(ggplot2)

# Responses: Fatal accident indicator, number of vehicles
# Predictors: Speed, weather, lighting, road type, time, driver age, alcohol, location

# Load .csv files
zip = "./data/data.zip"
accident = read_csv(archive_read(zip, file = "FARS2022NationalCSV/accident.csv")) # useful  
#cevent = read_csv(archive_read(zip, file = "FARS2022NationalCSV/cevent.csv"))     # not useful, lists how it happened
#crashrf = read_csv(archive_read(zip, file = "FARS2022NationalCSV/crashrf.csv"))   # not useful
damage = read_csv(archive_read(zip, file = "FARS2022NationalCSV/damage.csv"))     # useful, gives number of vehicles
#distract = read_csv(archive_read(zip, file = "FARS2022NationalCSV/distract.csv")) # not useful, gives if distracted driver
drimpair = read_csv(archive_read(zip, file = "FARS2022NationalCSV/drimpair.csv")) # useful, alcohol/drugs
driverrf = read_csv(archive_read(zip, file = "FARS2022NationalCSV/driverrf.csv")) # useful - indicates the drivers fault 
drugs = read_csv(archive_read(zip, file = "FARS2022NationalCSV/drugs.csv"))       # useful, gives drug info
#factor = read_csv(archive_read(zip, file = "FARS2022NationalCSV/factor.csv"))     # not useful
#maneuver = read_csv(archive_read(zip, file = "FARS2022NationalCSV/maneuver.csv")) # not useful, gives if driver tried to avoid
#miacc = read_csv(archive_read(zip, file = "FARS2022NationalCSV/MIACC.CSV"))       # not useful
#midrvacc = read_csv(archive_read(zip, file = "FARS2022NationalCSV/MIDRVACC.CSV")) # not useful
#miper = read_csv(archive_read(zip, file = "FARS2022NationalCSV/MIPER.CSV"))       # not useful
#nmcrash = read_csv(archive_read(zip, file = "FARS2022NationalCSV/nmcrash.csv"))   # not useful, gives reason for crash
nmdistract = read_csv(archive_read(zip, file = "FARS2022NationalCSV/nmdistract.csv"))  # useful, gives if distracted in a simpler way
nmimpair = read_csv(archive_read(zip, file = "FARS2022NationalCSV/nmimpair.csv")) # useful, gives if impaired
#nmprior = read_csv(archive_read(zip, file = "FARS2022NationalCSV/nmprior.csv"))   # not useful
parkwork = read_csv(archive_read(zip, file = "FARS2022NationalCSV/parkwork.csv")) # useful - indicates time of the incident and more info 
pbtype = read_csv(archive_read(zip, file = "FARS2022NationalCSV/pbtype.csv"))     # useful indicates age
person = read_csv(archive_read(zip, file = "FARS2022NationalCSV/person.csv"))     # useful, age, gender, race of person (white, black, etc) - all sorts of stuff
#personrf = read_csv(archive_read(zip, file = "FARS2022NationalCSV/personrf.csv")) # not useful
#pvehiclesf = read_csv(archive_read(zip, file = "FARS2022NationalCSV/pvehiclesf.csv")) # not useful
race = read_csv(archive_read(zip, file = "FARS2022NationalCSV/race.csv"))         # also gives race
#safetyeq = read_csv(archive_read(zip, file = "FARS2022NationalCSV/safetyeq.csv")) # not useful, gives safety equipment
vehicle = read_csv(archive_read(zip, file = "FARS2022NationalCSV/vehicle.csv"))   # useful
#vehiclesf = read_csv(archive_read(zip, file = "FARS2022NationalCSV/vehiclesf.csv")) # not useful
#vevent = read_csv(archive_read(zip, file = "FARS2022NationalCSV/vevent.csv"))     # not useful
#violatn = read_csv(archive_read(zip, file = "FARS2022NationalCSV/violatn.csv"))   # not useful
vision = read_csv(archive_read(zip, file = "FARS2022NationalCSV/vision.csv"))# vision interference  useful
vpicdecode = read_csv(archive_read(zip, file = "FARS2022NationalCSV/vpicdecode.csv")) # maybe useful, gives vehicle info
#vpictrailer = read_csv(archive_read(zip, file = "FARS2022NationalCSV/vpictrailerdecode.csv")) # not useful
#vsoe = read_csv(archive_read(zip, file = "FARS2022NationalCSV/vsoe.csv"))  # not useful
weather = read_csv(archive_read(zip, file = "FARS2022NationalCSV/weather.csv"))  #useful


accident_clean <- accident[, c(
  "ST_CASE",
  "FATALS",
  "VE_TOTAL",
  "PERSONS",
  "HOUR",
  "DAY_WEEK",
  "MONTH",
  "RUR_URB",
  "FUNC_SYS"
)]

summary(accident_clean$FATALS)
plot(accident_clean$VE_TOTAL, accident_clean$FATALS)

# this one is comparing weather to the average fatality number
accident_weather <- accident_clean %>%
  left_join(weather, by = "ST_CASE") %>%
  group_by(WEATHERNAME) %>%
  summarise(avg_fatal = mean(FATALS, na.rm = TRUE), .groups = "drop")

ggplot(accident_weather, aes(x = WEATHERNAME, y = avg_fatal)) +
  geom_col(fill = "steelblue") +
  labs(title = "Average Fatalities by Weather Condition",
       x = "Weather",
       y = "Average Fatalities") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
  )

#weather_summary <- accident_weather %>%
#  group_by(WEATHERNAME) %>%
#  summarise(avg_fatal = mean(FATALS, na.rm = TRUE))

#ggplot(weather_summary, aes(x = WEATHERNAME, y = avg_fatal)) +
#  geom_col(fill = "steelblue") +
#  labs(title = "Average Fatalities by Weather Condition",
#       x = "Weather",
#       y = "Average Fatalities") +
#  theme_minimal() +
#  theme(
#    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
#  )

# average age pedestrian
pbtype$PBAGE <- as.numeric(pbtype$PBAGE)
pbtype$age_group <- cut(
  pbtype$PBAGE,
  breaks = seq(0, 100, by = 10),
  right = FALSE,
  include.lowest = TRUE,
  labels = c("0-9","10-19","20-29","30-39","40-49",
             "50-59","60-69","70-79","80-89","90-99")
)

age_summary_pb <- pbtype %>%
  group_by(age_group) %>%
  summarise(count = n())

ggplot(age_summary_pb, aes(x = age_group, y = count)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Distribution of People in Crashes by Age Group (Pedestrian)",
    x = "Age Group",
    y = "Number of People"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# average age
person$AGE <- as.numeric(person$AGE)
person$age_group <- cut(
  person$AGE,
  breaks = seq(0, 100, by = 10),
  right = FALSE,
  include.lowest = TRUE,
  labels = c("0-9","10-19","20-29","30-39","40-49",
             "50-59","60-69","70-79","80-89","90-99")
)

age_summary <- person %>%
  group_by(age_group) %>%
  summarise(count = n())

ggplot(age_summary, aes(x = age_group, y = count)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Distribution of People in Crashes by Age Group (Car)",
    x = "Age Group",
    y = "Number of People"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

 
