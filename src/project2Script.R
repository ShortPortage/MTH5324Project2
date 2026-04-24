library(archive)
library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(olsrr)
library(MASS)
library(splines)
library(ggeffects)
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
                                           "Dawn", "Dusk", "Dark-Unknown", "Other", "Unknown")[1:n_distinct(LGT_COND)]),
    FUNC_SYS = as.factor(FUNC_SYS),
    RUR_URB = as.factor(RUR_URB),
    WEATHER = as.factor(WEATHER),
    DAY_WEEK = as.factor(DAY_WEEK)
  )

summary(accident_clean$FATALS)

# Plot vehicles involved vs. fatalities
ggplot(accident_clean, aes(x = VE_TOTAL, y = FATALS)) +
  # alpha = 0.1 makes points 90% transparent; darker areas indicate more data
  # width/height = 0.2 keeps the points near their original integer values
  geom_jitter(alpha = 0.2, width = 0.2, height = 0.2) +
  labs(
    title = "Total Vehicles vs. Fatalities",
    x = "Total Vehicles Involved",
    y = "Number of Fatalities"
  ) +
  theme_minimal()
ggsave("figures/01_total_vehicles_vs_fatalities.png")

# this one is comparing weather to the average fatality number
accident_weather <- accident_clean %>%
  left_join(weather, by = "ST_CASE") %>%
  group_by(WEATHERNAME) %>%
  summarise(avg_fatal = mean(FATALS, na.rm = TRUE), .groups = "drop")

ggplot(accident_weather, aes(x = WEATHERNAME, y = avg_fatal)) +
  geom_col(fill = "blue") +
  geom_text(aes(label = round(avg_fatal, 3)), vjust = -0.5, fontface = "bold") +
  labs(title = "Average Fatalities by Weather Condition",
       x = "Weather Condition",
       y = "Average Fatalities") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
  )
ggsave("figures/02_avg_fatalities_by_weather.png")
accident_weather_vehicles <- accident_clean %>%
  left_join(weather, by = "ST_CASE") %>%
  group_by(WEATHERNAME) %>%
  summarise(
    avg_vehicles = mean(VE_TOTAL, na.rm = TRUE),
    .groups = "drop"
  )
ggplot(accident_weather_vehicles, aes(x = WEATHERNAME, y = avg_vehicles)) +
  geom_col(fill = "blue") +
  geom_text(aes(label = round(avg_vehicles, 3)), vjust = -0.5, fontface = "bold") +
  labs(
    title = "Average Number of Vehicles Involved by Weather Condition",
    x = "Weather Condition",
    y = "Average Number of Vehicles"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
  )
ggsave("figures/03_avg_vehicles_by_weather.png")

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
  geom_col(fill = "blue") +
  geom_text(aes(label = round(count, 3)), vjust = -0.5, fontface = "bold") +
  labs(
    title = "Distribution of People in Crashes by Age Group (Pedestrian)",
    x = "Age Group",
    y = "Number of People"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/04_pedestrian_age_distribution.png")

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
  geom_col(fill = "blue") +
  geom_text(aes(label = round(count, 3)), vjust = -0.5, fontface = "bold") +
  labs(
    title = "Distribution of People in Crashes by Age Group (Car)",
    x = "Age Group",
    y = "Number of People"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/05_car_age_distribution.png")

# Responses: Fatal accident indicator, number of vehicles
# Predictors: Speed, weather, lighting, road type, time, driver age, alcohol, location

# Speed vs. Number of Fatalities
ggplot(data_filtered_trimmed, aes(x = TRAV_SP, y = FATALS)) +
  geom_jitter(alpha = 0.1) + # jitter helps with overlapping points
  geom_smooth(method = "gam") +
  labs(title = "Speed vs. Number of Fatalities", x = "Travel Speed (mph)", y = "Fatalities") +
  theme_minimal()
ggsave("figures/06_speed_vs_fatalities.png")

# Speed vs. Number of Vehicles Involved
ggplot(data_filtered_trimmed, aes(x = TRAV_SP, y = VE_TOTAL)) +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "lm") +
  labs(
    title = "Speed vs Number of Vehicles Involved",
    x = "Travel Speed (mph)",
    y = "Number of Vehicles"
  ) +
  theme_minimal()
ggsave("figures/07_speed_vs_vehicles.png")

# Age vs. Number of Vehicles Involved
ggplot(data_filtered_trimmed, aes(x = AGE, y = VE_TOTAL)) +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "lm") +
  labs(title = "Driver Age vs. Total Vehicles Involved", x = "Driver Age", y = "Total Vehicles") +
  theme_minimal()
ggsave("figures/08_age_vs_vehicles_lm.png")

# Alcohol vs. Fatalities
data_filtered_trimmed %>%
  group_by(DRINKING) %>%
  summarise(mean_fatalities = mean(FATALS, na.rm = TRUE)) %>%
  ggplot(aes(x = DRINKING, y = mean_fatalities, fill = DRINKING)) +
  geom_col() +
  geom_text(aes(label = round(mean_fatalities, 3)), vjust = -0.5, fontface = "bold") +
  labs(title = "Average Fatalities: Alcohol vs. No Alcohol",
       y = "Average Number of Fatalities") +
  theme_minimal()
ggsave("figures/09_alcohol_vs_fatalities.png")

# Lighting Condition vs. Total Vehicles (Mean Comparison)
data_filtered_trimmed %>%
  group_by(LGT_COND) %>%
  summarise(mean_ve = mean(VE_TOTAL, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(LGT_COND, -mean_ve), y = mean_ve)) +
  geom_col(fill = "blue") +
  geom_text(aes(label = round(mean_ve, 3)), vjust = -0.5, fontface = "bold") +
  labs(title = "Average Vehicles Involved by Lighting Condition", x = "Light Condition", y = "Avg Vehicles") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()
ggsave("figures/10_lighting_vs_vehicles.png")

# Interaction of Speed and Alcohol on Fatalities
ggplot(data_filtered_trimmed, aes(x = TRAV_SP, y = FATALS, color = DRINKING)) +
  geom_smooth(se = FALSE) +
  labs(title = "Fatality Trend: Speed vs. Alcohol Interaction", 
       x = "Travel Speed (mph)", y = "Expected Fatalities") +
  theme_minimal()
ggsave("figures/11_speed_alcohol_fatalities.png")

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
  geom_text(aes(label = round(count, 3)), vjust = -0.5, fontface = "bold") +
  labs(
    title = "Number of Accidents: Alcohol vs No Alcohol",
    x = "Alcohol Involvement",
    y = "Number of Accidents"
  ) +
  theme_minimal()
ggsave("figures/12_accidents_alcohol_vs_no.png")

# Keep only drivers and valid sex values
driver_sex <- person %>%
  filter(PER_TYP == 1, SEXNAME %in% c("Male", "Female")) %>%
  dplyr::select(ST_CASE, SEXNAME)

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
  geom_text(aes(label = round(count, 3)), vjust = -0.5, fontface = "bold") +
  labs(
    title = "Number of Accidents Involving Male vs Female Drivers",
    x = "Driver Sex",
    y = "Number of Accidents"
  ) +
  theme_minimal()
ggsave("figures/13_accidents_by_sex.png")



day_comparison <- accident_clean %>%
  mutate(day_group = ifelse(DAY_WEEK %in% c(6, 7, 1), "Fri-Sun", "Mon-Thu")) %>%
  group_by(day_group) %>%
  summarise(
    mean_fatalities = mean(FATALS, na.rm = TRUE),
    total_accidents = n(),
    .groups = "drop"
  )


ggplot(day_comparison, aes(x = day_group, y = mean_fatalities, fill = day_group)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(mean_fatalities, 3)), vjust = -0.5, fontface = "bold") +
  labs(
    title = "Average Fatalities per Accident: Fri-Sun vs. Mon-Thu",
    x = "Days of the Week",
    y = "Average Fatalities per Accident"
  ) +
  theme_minimal()
ggsave("figures/14_mean_fatalities_frisun_vs_weekday.png")


# Have to run this twice to get rid of N/A column
data_filtered_trimmed %>%
  mutate(AREA = factor(RUR_URB,
                       levels = c(1, 2),
                       labels = c("Rural", "Urban"))) %>%
  group_by(AREA) %>%
  summarise(mean_fatalities = mean(FATALS, na.rm = TRUE)) %>%
  ggplot(aes(x = AREA, y = mean_fatalities, fill = AREA)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(mean_fatalities, 3)), vjust = -0.5, fontface = "bold") +
  labs(
    title = "Average Fatalities: Urban vs Rural",
    x = "Location Type",
    y = "Average Number of Fatalities"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
data_filtered_trimmed %>%
  mutate(AREA = factor(RUR_URB,
                       levels = c(1, 2),
                       labels = c("Rural", "Urban"))) %>%
  group_by(AREA) %>%
  summarise(mean_fatalities = mean(FATALS, na.rm = TRUE)) %>%
  ggplot(aes(x = AREA, y = mean_fatalities, fill = AREA)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(mean_fatalities, 3)), vjust = -0.5, fontface = "bold") +
  labs(
    title = "Average Fatalities: Urban vs Rural",
    x = "Location Type",
    y = "Average Number of Fatalities"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("figures/15_avg_fatalities_urban_vs_rural.png")

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


road_type_comparison <- accident_clean %>%
  filter(FUNC_SYS >= 1 & FUNC_SYS <= 7) %>%
  mutate(Road_Type = factor(FUNC_SYS, labels = road_labels)) %>%
  group_by(Road_Type) %>%
  summarise(
    avg_fatalities = mean(FATALS, na.rm = TRUE),
    .groups = "drop"
  )


ggplot(road_type_comparison, aes(x = reorder(Road_Type, -avg_fatalities), y = avg_fatalities, fill = Road_Type)) +
  geom_col() +
  geom_text(aes(label = round(avg_fatalities, 3)), vjust = -0.5, fontface = "bold") +
  labs(
    title = "Average Fatalities by Road Type",
    x = "Road Type",
    y = "Average Fatalities per Accident"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    # Rotate x-axis labels so they don't overlap
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave("figures/16_avg_fatalities_by_road_type.png")

road_vehicle_summary <- accident_clean %>%
  filter(FUNC_SYS >= 1 & FUNC_SYS <= 7) %>%
  mutate(Road_Type = factor(FUNC_SYS, labels = road_labels)) %>%
  group_by(Road_Type) %>%
  summarise(
    avg_vehicles = mean(VE_TOTAL, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(road_vehicle_summary,
       aes(x = reorder(Road_Type, -avg_vehicles),
           y = avg_vehicles,
           fill = Road_Type)) +
  geom_col() +
  geom_text(aes(label = round(avg_vehicles, 3)), vjust = -0.5, fontface = "bold") +
  labs(
    title = "Average Number of Vehicles Involved by Road Type",
    x = "Road Type",
    y = "Average Number of Vehicles"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave("figures/17_avg_vehicles_by_road_type.png")


vehicle_day_comparison <- accident_clean %>%
  # Categorize 1 (Sun), 6 (Fri), 7 (Sat) as Fri-Sun
  mutate(day_group = ifelse(DAY_WEEK %in% c(6, 7, 1), "Fri-Sun", "Mon-Thu")) %>%
  group_by(day_group) %>%
  summarise(
    avg_vehicles = mean(VE_TOTAL, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(vehicle_day_comparison, aes(x = day_group, y = avg_vehicles, fill = day_group)) +
  geom_col(width = 0.5) +
  # Adding text labels on top of the bars for clarity
  geom_text(aes(label = round(avg_vehicles, 3)), vjust = -0.5, fontface = "bold") +
  labs(
    title = "Average Number of Vehicles Involved: Fri-Sun vs. Mon-Thu",
    x = "Days of the Week",
    y = "Average Vehicles per Accident"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("figures/18_avg_vehicles_frisun_vs_weekday.png")

ggplot(data_filtered_trimmed, aes(x = AGE, y = VE_TOTAL)) +
  # Jitter adds a small amount of random noise to help see density
  geom_jitter(alpha = 0.1, width = 0.5, height = 0.5) +
  # Add a smooth trend line
  geom_smooth(method = "gam", size = 1.2) +
  labs(
    title = "Driver Age vs. Total Vehicles Involved",
    x = "Driver Age",
    y = "Total Vehicles"
  ) +
  theme_minimal()
ggsave("figures/19_age_vs_vehicles_gam.png")

# 1. Summarize the data by location type
loc_vehicle_comparison <- accident_clean %>%
  filter(RUR_URB %in% c(1, 2)) %>%
  mutate(Location = factor(RUR_URB, labels = c("Rural", "Urban"))) %>%
  group_by(Location) %>%
  summarise(
    avg_vehicles = mean(VE_TOTAL, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Plot with vertical bars
ggplot(loc_vehicle_comparison, aes(x = Location, y = avg_vehicles, fill = Location)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = round(avg_vehicles, 3)), vjust = -0.5, fontface = "bold") +
  labs(
    title = "Average Vehicles Involved: Rural vs. Urban",
    x = "Location Type",
    y = "Average Vehicles per Accident"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("figures/20_avg_vehicles_rural_vs_urban.png")

# 1. Summarize fatalities by Lighting Condition
lighting_summary <- data_filtered_trimmed %>%
  filter(LGT_COND %in% c("Daylight", "Dark-Not Lit", "Dark-Lit", "Dawn", "Dusk")) %>%
  group_by(LGT_COND) %>%
  summarise(avg_fatalities = mean(FATALS, na.rm = TRUE))

# 2. Plot vertical bars
ggplot(lighting_summary, aes(x = reorder(LGT_COND, -avg_fatalities), y = avg_fatalities, fill = LGT_COND)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(avg_fatalities, 3)), vjust = -0.5, fontface = "bold") +
  labs(
    title = "Average Fatalities by Lighting Condition",
    x = "Lighting Condition",
    y = "Average Fatalities per Accident"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave("figures/21_avg_fatalities_by_lighting.png")

# 1. Summarize average vehicles by Lighting Condition
lighting_vehicle_summary <- data_filtered_trimmed %>%
  filter(LGT_COND %in% c("Daylight", "Dark-Not Lit", "Dark-Lit", "Dawn", "Dusk")) %>%
  group_by(LGT_COND) %>%
  summarise(avg_vehicles = mean(VE_TOTAL, na.rm = TRUE), .groups = "drop")

# 2. Plot vertical bars for vehicles
ggplot(lighting_vehicle_summary, aes(x = reorder(LGT_COND, -avg_vehicles), y = avg_vehicles, fill = LGT_COND)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(avg_vehicles, 3)), vjust = -0.5, fontface = "bold") +
  labs(
    title = "Average Vehicles Involved by Lighting Condition",
    x = "Lighting Condition",
    y = "Average Vehicles per Accident"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 3. Save the figure
ggsave("figures/21a_avg_vehicles_by_lighting.png")

# Plotting Driver Age vs. Fatalities
ggplot(data_filtered_trimmed, aes(x = AGE, y = FATALS)) +
  # Jitter adds small random noise to points so you can see the 'clouds' of data
  geom_jitter(alpha = 0.15, width = 0.5, height = 0.2) +
  # Adding a trend line to see if fatalities increase/decrease with age
  geom_smooth(method = "gam") +
  labs(
    title = "Driver Age vs. Total Fatalities in Accident",
    x = "Driver Age",
    y = "Number of Fatalities"
  ) +
  theme_minimal()
ggsave("figures/22_age_vs_fatalities.png")

# Alcohol vs. Number of Vehicles Involved (Mean Comparison)
data_filtered_trimmed %>%
  group_by(DRINKING) %>%
  summarise(mean_vehicles = mean(VE_TOTAL, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = DRINKING, y = mean_vehicles, fill = DRINKING)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = round(mean_vehicles, 3)), vjust = -0.5, fontface = "bold") +
  labs(
    title = "Average Number of Vehicles Involved: Alcohol vs. No Alcohol",
    x = "Drinking Status",
    y = "Average Number of Vehicles"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("figures/23_avg_vehicles_alcohol_vs_no.png")

# Responses: Fatal accident indicator, number of vehicles
# Predictors: Speed, weather, lighting, road type, time, driver age, alcohol, location

# Response: Fatalities
# Relevant Predictors: Speed, road type, location (urban vs rural), alcohol (somewhat weak but important), weather, lighting, age
# Weak Predictors: time (weak)

# Response: Number of vehicles
# Predictors: Location(urban vs rural), road type, lighting, speed, weather, time, alcohol
# Weak Predictors: Age

# Setup poisson model for fatalities
glm_fatals_full <- glm(
  FATALS ~ ns(TRAV_SP, df=4) + FUNC_SYS + RUR_URB + DRINKING + WEATHER + LGT_COND + AGE, 
  data = data_filtered_trimmed, 
  family = "poisson"
)

# Stepwise selection
best_glm_fatals <- stepAIC(glm_fatals_full, direction = "both", trace = FALSE)

# Summary
summary(best_glm_fatals)

# Step AIC doesn't work with quasipoisson but we need it to correct underdispersion.
# We now use the full model here

glm_fatals_v2 <- glm(
  FATALS ~ ns(TRAV_SP, df = 4) + FUNC_SYS + RUR_URB + DRINKING + WEATHER + LGT_COND + AGE,
  data = data_filtered_trimmed,
  family = quasipoisson(link = "log")
)

# New summary
summary(glm_fatals_v2)

# Lighting has high p-values and drinking has a tiny coefficient. So we can remove them

# Create the new grouped weather variable and filter out unknown codes
data_filtered_trimmed <- data_filtered_trimmed %>%
  filter(
    !FUNC_SYS %in% c("96", "98", "99"), # Included 99 just in case it exists as "Unknown"
    !RUR_URB %in% c("6", "8", "9")      # Included 8 just in case it exists as "Not Reported"
  ) %>%
  mutate(
    FUNC_SYS = droplevels(FUNC_SYS),
    RUR_URB = as.factor(RUR_URB),
    RUR_URB = droplevels(RUR_URB),
    WEATHER_GROUPED = case_when(
      WEATHER == "1" ~ "Clear",
      WEATHER == "4" ~ "Condition 4 (Snow/Sleet)",
      WEATHER == "7" ~ "Condition 7 (Severe Winds/Sand)",
      TRUE ~ "Other Non-Clear" # This catches everything else
    ),
    WEATHER_GROUPED = relevel(as.factor(WEATHER_GROUPED), ref = "Clear") # Clear set as baseline
  )


glm_fatals_v3 <- glm(
  FATALS ~ ns(TRAV_SP, df = 4) + FUNC_SYS + RUR_URB + WEATHER_GROUPED + AGE, 
  data = data_filtered_trimmed, 
  family = quasipoisson(link = "log")
)


summary(glm_fatals_v3)

# Find speed vs. fatalities
speed_effect <- ggpredict(glm_fatals_v3, terms = "TRAV_SP [all]") 
plot(speed_effect) +
  labs(
    title = "Predicted Fatalities by Speed",
    x = "Travel Speed (mph)",
    y = "Predicted Number of Fatalities"
  ) +
  theme_minimal()
ggsave("figures/24_predicted_fatalities_by_speed.png")

# 1. Generate the predicted fatalities for AGE
# "[all]" ensures it calculates predictions across the full range of ages in your data
eff_age <- ggpredict(glm_fatals_v3, terms = "AGE [all]")

# 2. Plot using the standard statistical style (black line/grey ribbon)
plot(eff_age, colors = "black") +
  labs(
    title = "Predicted Fatalities by Driver Age",
    x = "Driver Age",
    y = "Predicted Number of Fatalities"
  ) +
  theme_minimal()

# 3. Save the figure
ggsave("figures/24a_predicted_fatalities_by_age.png")

# Find fatalities by road type
road_effect <- ggpredict(glm_fatals_v3, terms = "FUNC_SYS")
road_df <- as.data.frame(road_effect)

ggplot(road_df, aes(x = x, y = predicted)) +
  geom_point(size = 2.5, color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black") +
  geom_text(aes(label = round(predicted, 3)), hjust = 1.3, fontface = "bold") +
  scale_x_discrete(labels = road_labels) +
  labs(
    title = "Predicted Fatalities by Road Type",
    x = "Road Type",
    y = "Predicted Fatalities"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave("figures/25_predicted_fatalities_by_road_type.png")

# 2. Location (Rural vs Urban)
eff_location <- ggpredict(glm_fatals_v3, terms = "RUR_URB")
location_df <- as.data.frame(eff_location)

ggplot(location_df, aes(x = x, y = predicted)) +
  geom_point(size = 2.5, color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black") +
  geom_text(aes(label = round(predicted, 3)), hjust = 1.3, fontface = "bold") +
  scale_x_discrete(labels = c("1" = "Rural", "2" = "Urban")) +
  labs(
    title = "Predicted Fatalities by Location Type",
    x = "Location",
    y = "Predicted Fatalities"
  ) +
  theme_minimal()
ggsave("figures/25a_predicted_fatalities_by_location.png")

# 3. Visualize the Effect of Extreme Weather
weather_effect <- ggpredict(glm_fatals_v3, terms = "WEATHER_GROUPED")

# Convert the ggeffects object into a standard R data frame
weather_df <- as.data.frame(weather_effect)

# Build the plot manually using native ggplot2
ggplot(weather_df, aes(x = x, y = predicted)) +
  geom_point(size = 2.5, color = "black") +
  # width = 0.1 makes the horizontal caps on the error bars match the road plot
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, color = "black") +
  geom_text(aes(label = round(predicted, 3)), hjust = 1.3, fontface = "bold") +
  labs(
    title = "Predicted Fatalities by Weather Condition",
    x = "Weather Condition",
    y = "Predicted Fatalities"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave("figures/26_predicted_fatalities_by_weather.png")


# Initial model
glm_vehicles_full <- glm(
  VE_TOTAL ~ ns(TRAV_SP, df = 5) + FUNC_SYS + RUR_URB + LGT_COND + 
    WEATHER_GROUPED + DAY_WEEK + DRINKING, 
  data = data_filtered_trimmed, 
  family = "poisson"
)


# trace = FALSE keeps the console output clean
best_glm_vehicles <- stepAIC(glm_vehicles_full, direction = "both", trace = FALSE)


summary(best_glm_vehicles)

# Filter out useless data
data_filtered_trimmed = data_filtered_trimmed %>%
  filter(
    !(LGT_COND %in% c("Other", "Unknown", "NA"))
  ) %>%
  mutate(
    LGT_COND = droplevels(LGT_COND)
  )

# Re-run model
glm_vehicles_final = glm(
  VE_TOTAL ~ ns(TRAV_SP, df = 5) + FUNC_SYS + RUR_URB + 
    LGT_COND + WEATHER_GROUPED + DRINKING, 
  data = data_filtered_trimmed,
  family = quasipoisson(link = "log")
)

# Give summary
summary(glm_vehicles_final)

# 1. Marginal Effect of Speed (Natural Spline)
eff_speed = ggpredict(glm_vehicles_final, terms = "TRAV_SP [all]")
plot(eff_speed, colors = "black") + 
  labs(
    title = "Effect of Speed on Vehicle Involvement",
    x = "Travel Speed (mph)", 
    y = "Predicted Number of Vehicles"
  ) +
  theme_minimal()
ggsave("figures/27_effect_speed_on_vehicles.png")

# 1. Road Classification (FUNC_SYS)
eff_road = ggpredict(glm_vehicles_final, terms = "FUNC_SYS")
road_df = as.data.frame(eff_road)

ggplot(road_df, aes(x = x, y = predicted)) +
  geom_point(size = 2.5, color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black") +
  geom_text(aes(label = round(predicted, 3)), hjust = 1.3, fontface = "bold") +
  scale_x_discrete(labels = road_labels) +
  labs(
    title = "Predicted Vehicles by Road Type",
    x = "Road Type",
    y = "Predicted Vehicles"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/28_predicted_vehicles_by_road_type.png")

# 2. Location (Rural vs Urban)
eff_location <- ggpredict(glm_vehicles_final, terms = "RUR_URB")
location_df <- as.data.frame(eff_location)

ggplot(location_df, aes(x = x, y = predicted)) +
  geom_point(size = 2.5, color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black") +
  geom_text(aes(label = round(predicted, 3)), hjust = 1.3, fontface = "bold") +
  scale_x_discrete(labels = c("1" = "Rural", "2" = "Urban")) +
  labs(
    title = "Predicted Vehicles by Location Type",
    x = "Location",
    y = "Predicted Vehicles"
  ) +
  theme_minimal()
ggsave("figures/29_predicted_vehicles_by_location.png")

# 3. Lighting Condition (LGT_COND)
eff_light <- ggpredict(glm_vehicles_final, terms = "LGT_COND")
light_df <- as.data.frame(eff_light)

ggplot(light_df, aes(x = reorder(x, -predicted), y = predicted)) +
  geom_point(size = 2.5, color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black") +
  geom_text(aes(label = round(predicted, 3)), hjust = 1.3, fontface = "bold") +
  labs(
    title = "Predicted Vehicles by Lighting Condition",
    x = "Lighting",
    y = "Predicted Vehicles"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/30_predicted_vehicles_by_lighting.png")

# 4. Weather Group (WEATHER_GROUPED)
eff_weather <- ggpredict(glm_vehicles_final, terms = "WEATHER_GROUPED")
weather_df <- as.data.frame(eff_weather)

ggplot(weather_df, aes(x = reorder(x, -predicted), y = predicted)) +
  geom_point(size = 2.5, color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black") +
  geom_text(aes(label = round(predicted, 3)), hjust = 1.3, fontface = "bold") +
  labs(
    title = "Predicted Vehicles by Weather Condition",
    x = "Weather Group",
    y = "Predicted Vehicles"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/31_predicted_vehicles_by_weather.png")

# 5. Alcohol Involvement (DRINKING)
eff_alcohol <- ggpredict(glm_vehicles_final, terms = "DRINKING")
alcohol_df <- as.data.frame(eff_alcohol)

ggplot(alcohol_df, aes(x = x, y = predicted)) +
  geom_point(size = 2.5, color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black") +
  geom_text(aes(label = round(predicted, 3)), hjust = 1.3, fontface = "bold") +
  labs(
    title = "Predicted Vehicles by Alcohol Involvement",
    x = "Drinking Status",
    y = "Predicted Vehicles"
  ) +
  theme_minimal()
ggsave("figures/32_predicted_vehicles_by_alcohol.png")