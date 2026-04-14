library(archive)
library(readr)

# Load .csv files
zip = "./data/data.zip"
accident = read_csv(archive_read(zip, file = "FARS2022NationalCSV/accident.csv"))# useful  
cevent = read_csv(archive_read(zip, file = "FARS2022NationalCSV/cevent.csv"))
crashrf = read_csv(archive_read(zip, file = "FARS2022NationalCSV/crashrf.csv")) #not useful
damage = read_csv(archive_read(zip, file = "FARS2022NationalCSV/damage.csv"))
distract = read_csv(archive_read(zip, file = "FARS2022NationalCSV/distract.csv"))
drimpair = read_csv(archive_read(zip, file = "FARS2022NationalCSV/drimpair.csv"))
driverrf = read_csv(archive_read(zip, file = "FARS2022NationalCSV/driverrf.csv")) #useful - indicates the drivers fault 
drugs = read_csv(archive_read(zip, file = "FARS2022NationalCSV/drugs.csv")) #useful
factor = read_csv(archive_read(zip, file = "FARS2022NationalCSV/factor.csv"))
maneuver = read_csv(archive_read(zip, file = "FARS2022NationalCSV/maneuver.csv"))
miacc = read_csv(archive_read(zip, file = "FARS2022NationalCSV/MIACC.CSV"))
midrvacc = read_csv(archive_read(zip, file = "FARS2022NationalCSV/MIDRVACC.CSV"))
miper = read_csv(archive_read(zip, file = "FARS2022NationalCSV/MIPER.CSV"))
nmcrash = read_csv(archive_read(zip, file = "FARS2022NationalCSV/nmcrash.csv"))
nmdistract = read_csv(archive_read(zip, file = "FARS2022NationalCSV/nmdistract.csv"))
nmimpair = read_csv(archive_read(zip, file = "FARS2022NationalCSV/nmimpair.csv"))
nmprior = read_csv(archive_read(zip, file = "FARS2022NationalCSV/nmprior.csv"))
parkwork = read_csv(archive_read(zip, file = "FARS2022NationalCSV/parkwork.csv")) #useful - indicates time of the incident and more info 
pbtype = read_csv(archive_read(zip, file = "FARS2022NationalCSV/pbtype.csv"))   # useful indicates age
person = read_csv(archive_read(zip, file = "FARS2022NationalCSV/person.csv"))  # race of person (white, black,etc)
personrf = read_csv(archive_read(zip, file = "FARS2022NationalCSV/personrf.csv"))
pvehiclesf = read_csv(archive_read(zip, file = "FARS2022NationalCSV/pvehiclesf.csv"))
race = read_csv(archive_read(zip, file = "FARS2022NationalCSV/race.csv"))
safetyeq = read_csv(archive_read(zip, file = "FARS2022NationalCSV/safetyeq.csv"))
vehicle = read_csv(archive_read(zip, file = "FARS2022NationalCSV/vehicle.csv"))# car type not sure if useful
vehiclesf = read_csv(archive_read(zip, file = "FARS2022NationalCSV/vehiclesf.csv"))
vevent = read_csv(archive_read(zip, file = "FARS2022NationalCSV/vevent.csv"))
violatn = read_csv(archive_read(zip, file = "FARS2022NationalCSV/violatn.csv"))
vision = read_csv(archive_read(zip, file = "FARS2022NationalCSV/vision.csv"))# vision interference  useful
vpicdecode = read_csv(archive_read(zip, file = "FARS2022NationalCSV/vpicdecode.csv"))
vpictrailer = read_csv(archive_read(zip, file = "FARS2022NationalCSV/vpictrailerdecode.csv"))
vsoe = read_csv(archive_read(zip, file = "FARS2022NationalCSV/vsoe.csv"))
weather = read_csv(archive_read(zip, file = "FARS2022NationalCSV/weather.csv"))  #useful
