library(archive)
library(readr)
library(dplyr)
library(ggplot2)
library(forcats)

# Responses: Fatal accident indicator, number of vehicles
# Predictors: Speed, weather, lighting, road type, time, driver age, alcohol, location

# DATA HIERARCHY:
# accident 1
# | vehicle 1
#   | Person 1
#   | Person 2
#   | ...
# | vehicle 2
#   | Person 1
# | vehicle 3
#   | Person 1
#   | Person 2
#   | Person 3
# | ...
# accident 2
# |...

# Load .csv files
zip = "./data/data.zip"
accident = read_csv(archive_read(zip, file = "FARS2022NationalCSV/accident.csv")) # useful  
#cevent = read_csv(archive_read(zip, file = "FARS2022NationalCSV/cevent.csv"))     # not useful, lists how it happened
#crashrf = read_csv(archive_read(zip, file = "FARS2022NationalCSV/crashrf.csv"))   # not useful
#damage = read_csv(archive_read(zip, file = "FARS2022NationalCSV/damage.csv"))     # not useful, gives number of vehicles
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
  "ROUTE",
  "FUNC_SYS",
  "LGT_COND",
  "LATITUDE",
  "LONGITUD",
  "WEATHER"
)]

# Get vehicle number and travel speed
vehicle_clean = vehicle[, c(
  "ST_CASE",
  "VEH_NO",
  "TRAV_SP"
)] 

# Select only drivers (this is what PER_TYP == 1 is doing)
person_clean = person[person$PER_TYP == 1, c(
  "ST_CASE", 
  "VEH_NO", 
  "AGE", 
  "DRINKING"
)]

driver_vehicle = inner_join(person_clean, vehicle_clean, by = c("ST_CASE", "VEH_NO"))
data_joined = inner_join(driver_vehicle, accident_clean, by = c("ST_CASE"))

data_filtered_trimmed <- data_joined %>%
  filter(
    TRAV_SP < 140,        # filter out speeds that aren't real
    AGE < 100,            # Filter out age codes 
    DRINKING %in% c(0, 1) # 1 = Yes, 0 = No or 9 is often unknown
  ) %>%
  mutate(
    DRINKING = factor(DRINKING, labels = c("No Alcohol", "Alcohol Involved")),
    LGT_COND = factor(LGT_COND, labels = c("Daylight", "Dark-Not Lit", "Dark-Lit", 
                                           "Dawn", "Dusk", "Dark-Unknown", "Other", "Unknown")[1:n_distinct(LGT_COND)])
  )

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
accident_weather_vehicles <- accident_clean %>%
  left_join(weather, by = "ST_CASE") %>%
  group_by(WEATHERNAME) %>%
  summarise(
    avg_vehicles = mean(VE_TOTAL, na.rm = TRUE),
    .groups = "drop"
  )
ggplot(accident_weather_vehicles, aes(x = WEATHERNAME, y = avg_vehicles)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average Number of Vehicles Involved by Weather Condition",
    x = "Weather Condition",
    y = "Average Number of Vehicles"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
  )

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

# Responses: Fatal accident indicator, number of vehicles
# Predictors: Speed, weather, lighting, road type, time, driver age, alcohol, location

# Speed vs. Number of Fatalities
ggplot(data_filtered_trimmed, aes(x = TRAV_SP, y = FATALS)) +
  geom_jitter(alpha = 0.1, color = "steelblue") + # jitter helps with overlapping points
  geom_smooth(method = "gam", color = "red") +
  labs(title = "Travel Speed vs. Number of Fatalities", x = "Travel Speed (MPH)", y = "Fatalities")
ggplot(data_filtered_trimmed, aes(x = TRAV_SP, y = VE_TOTAL)) +
  geom_jitter(alpha = 0.2, color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Speed vs Number of Vehicles Involved",
    x = "Travel Speed (MPH)",
    y = "Number of Vehicles"
  ) +
  theme_minimal()
# Age vs. Number of Vehicles Involved
ggplot(data_filtered_trimmed, aes(x = AGE, y = VE_TOTAL)) +
  geom_jitter(alpha = 0.1, color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Driver Age vs. Total Vehicles Involved", x = "Driver Age", y = "Total Vehicles")

# Alcohol vs. Fatalities
data_filtered_trimmed %>%
  group_by(DRINKING) %>%
  summarise(mean_fatalities = mean(FATALS, na.rm = TRUE)) %>%
  ggplot(aes(x = DRINKING, y = mean_fatalities, fill = DRINKING)) +
  geom_col() +
  labs(title = "Mean Fatalities: Alcohol vs. No Alcohol",
       y = "Average Number of Fatalities") +
  theme_minimal()

# Lighting Condition vs. Total Vehicles (Mean Comparison)
data_filtered_trimmed %>%
  group_by(LGT_COND) %>%
  summarise(mean_ve = mean(VE_TOTAL, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(LGT_COND, -mean_ve), y = mean_ve)) +
  geom_col(fill = "blue") +
  labs(title = "Average Vehicles Involved by Lighting Condition", x = "Light Condition", y = "Avg Vehicles") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Interaction of Speed and Alcohol on Fatalities
ggplot(data_filtered_trimmed, aes(x = TRAV_SP, y = FATALS, color = DRINKING)) +
  geom_smooth(se = FALSE) +
  labs(title = "Fatality Trend: Speed vs. Alcohol Interaction", 
       x = "Travel Speed", y = "Expected Fatalities") +
  theme_minimal()
# Create accident-level alcohol indicator
accident_alcohol <- data_filtered_trimmed %>%
  group_by(ST_CASE) %>%
  summarise(
    alcohol_involved = any(DRINKING == "Alcohol Involved")
  ) %>%
  mutate(
    alcohol_involved = factor(alcohol_involved,
                              labels = c("No Alcohol", "Alcohol Involved"))
  )

# Count accidents
accident_counts <- accident_alcohol %>%
  group_by(alcohol_involved) %>%
  summarise(count = n())

# Plot
ggplot(accident_counts, aes(x = alcohol_involved, y = count, fill = alcohol_involved)) +
  geom_col() +
  labs(
    title = "Number of Accidents: Alcohol vs No Alcohol",
    x = "Alcohol Involvement",
    y = "Number of Accidents"
  ) +
  theme_minimal()
# Keep only drivers and valid sex values
driver_sex <- person %>%
  filter(PER_TYP == 1, SEXNAME %in% c("Male", "Female")) %>%
  select(ST_CASE, SEXNAME)

# Collapse to accident level
accident_sex <- driver_sex %>%
  group_by(ST_CASE) %>%
  summarise(
    male_involved = any(SEXNAME == "Male"),
    female_involved = any(SEXNAME == "Female")
  )

# Convert to long format for counting
accident_counts <- accident_sex %>%
  summarise(
    Male = sum(male_involved),
    Female = sum(female_involved)
  ) %>%
  tidyr::pivot_longer(cols = everything(),
                      names_to = "Sex",
                      values_to = "count")

# Plot
ggplot(accident_counts, aes(x = Sex, y = count, fill = Sex)) +
  geom_col() +
  labs(
    title = "Number of Accidents Involving Male vs Female Drivers",
    x = "Driver Sex",
    y = "Number of Accidents"
  ) +
  theme_minimal()


# 1. Create the grouping variable
day_comparison <- accident_clean %>%
  mutate(day_group = ifelse(DAY_WEEK %in% c(6, 7, 1), "Fri-Sun", "Mon-Thu")) %>%
  group_by(day_group) %>%
  summarise(
    mean_fatalities = mean(FATALS, na.rm = TRUE),
    total_accidents = n(),
    .groups = "drop"
  )

# 2. Plot the comparison
ggplot(day_comparison, aes(x = day_group, y = mean_fatalities, fill = day_group)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(mean_fatalities, 4)), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Fri-Sun" = "#e41a1c", "Mon-Thu" = "#377eb8")) +
  labs(
    title = "Mean Fatalities per Accident: Fri-Sun vs. Mon-Thu",
    subtitle = "Based on 2022 FARS Data",
    x = "Days of the Week",
    y = "Average Fatalities per Accident"
  ) +
  theme_minimal()
data_filtered_trimmed %>%
  mutate(AREA = factor(RUR_URB,
                       levels = c(1, 2),
                       labels = c("Rural", "Urban"))) %>%
  group_by(AREA) %>%
  summarise(mean_fatalities = mean(FATALS, na.rm = TRUE)) %>%
  ggplot(aes(x = AREA, y = mean_fatalities, fill = AREA)) +
  geom_col(width = 0.6) +
  labs(
    title = "Average Fatalities: Urban vs Rural",
    x = "Area Type",
    y = "Average Number of Fatalities"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# 1. Define labels for the Functional System
road_labels <- c(
  "Interstate", 
  "Arterial-Freeway", 
  "Arterial-Other", 
  "Minor Arterial", 
  "Major Collector", 
  "Minor Collector", 
  "Local"
)

# 2. Summarize the data
road_type_comparison <- accident_clean %>%
  filter(FUNC_SYS >= 1 & FUNC_SYS <= 7) %>%
  mutate(Road_Type = factor(FUNC_SYS, labels = road_labels)) %>%
  group_by(Road_Type) %>%
  summarise(
    avg_fatalities = mean(FATALS, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Plot the results with vertical bars
ggplot(road_type_comparison, aes(x = reorder(Road_Type, -avg_fatalities), y = avg_fatalities, fill = Road_Type)) +
  geom_col() +
  # Removed coord_flip() to make bars vertical
  labs(
    title = "Average Fatalities by Road Type",
    x = "Road Classification",
    y = "Average Fatalities per Accident"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    # Rotate x-axis labels so they don't overlap
    axis.text.x = element_text(angle = 45, hjust = 1)
  )