#load/install packages

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")
if(!require(rgeos)) install.packages("rgeos", repos = "http://cran.us.r-project.org")
if(!require(rnaturalearth)) install.packages("rnaturalearth", repos = "http://cran.us.r-project.org")
if(!require(rnaturalearthdata)) install.packages("rnaturalearthdata", repos = "http://cran.us.r-project.org")
if(!require(ggspatial)) install.packages("ggspatial", repos = "http://cran.us.r-project.org")

#downloading period of record (-por) vice year to date (-ytd) for UAE and Quatar 
#downloading station list file
AE <- tempfile()
download.file("https://www1.ncdc.noaa.gov/pub/data/igra/derived/derived-por/AEM00041217-drvd.txt.zip", AE)
uaeDat <- unzip(AE, "AEM00041217-drvd.txt")

QA <- tempfile()
download.file("https://www1.ncdc.noaa.gov/pub/data/igra/derived/derived-por/QAM00041169-drvd.txt.zip", QA)
quatarDat <- unzip(QA, "QAM00041169-drvd.txt")

statList <- tempfile()
download.file("https://www1.ncdc.noaa.gov/pub/data/igra/igra2-station-list.txt", statList)

#function to parse out the headers
v2_drvd_header_parser <- function(file, ...) {
  readr::read_fwf(
    file = file,
    col_positions = readr::fwf_positions(
      c(1, 2, 14, 19, 22, 25, 28, 32, 38, 44, 50, 56, 62, 68, 74, 80, 86, 92, 98, 104, 110, 116, 122, 128, 134, 140, 146, 152),
      c(1, 12, 17, 20, 23, 26, 31, 36, 43, 49, 55, 61, 67, 73, 79, 85, 91, 97, 103, 109, 115, 121, 127, 133, 139, 145, 151, 157),
      c("HEADREC", "ID", "YEAR", "MONTH", "DAY", "HOUR", "RELTIME", "NUMLEV", "PW", "INVPRESS", "INVHGT", "INVTEMPDIF", "MIXPRESS", "MIXHGT", "FRZPRESS", "FRZHGT", "LCLPRESS", "LCLHGT", "LFCPRESS", "LFCHGT", "LNBPRESS", "LNBHGT", "LI", "SI", "KI", "TTI", "CAPE", "CIN")
    ),
    col_types = readr::cols(
      HEADREC = readr::col_character(),
      ID = readr::col_character(),
      YEAR = readr::col_integer(),
      MONTH = readr::col_integer(),
      DAY = readr::col_integer(),
      HOUR = readr::col_integer(),
      RELTIME = readr::col_integer(),
      NUMLEV = readr::col_integer(),
      PW = readr::col_integer(),
      INVPRESS = readr::col_integer(),
      INVHGT = readr::col_integer(),
      INVTEMPDIF = readr::col_integer(),
      MIXPRESS = readr::col_integer(),
      MIXHGT = readr::col_integer(),
      FRZPRESS = readr::col_integer(),
      FRZHGT = readr::col_integer(),
      LCLPRESS = readr::col_integer(),
      LCLHGT = readr::col_integer(),
      LFCPRESS = readr::col_integer(),
      LFCHGT = readr::col_integer(),
      LNBPRESS = readr::col_integer(),
      LNBHGT = readr::col_integer(),
      LI = readr::col_integer(),
      SI = readr::col_integer(),
      KI = readr::col_integer(),
      TTI = readr::col_integer(),
      CAPE = readr::col_integer(),
      CIN = readr::col_integer()
    )
  )
}

#function to parse out the data
v2_drvd_data_parser <- function(file, ...) {
  readr::read_fwf(
    file = file,
    col_positions = readr::fwf_positions(
      c(1, 9, 17, 25, 33, 41, 49, 57, 65, 73, 81, 89, 97, 105, 113, 121, 129, 137, 145),
      c(7, 15, 23, 31, 39, 47, 55, 63, 71, 79, 87, 95, 103, 111, 119, 127, 135, 143, 151),
      c("PRESS", "REPGPH", "CALCGPH", "TEMP", "TEMPGRAD", "PTEMP", "PTEMPGRAD", "VTEMP", "VPTEMP", "VAPPRESS", "SATVAP", "REPRH", "CALCRH", "RHGRAD", "UWND", "UWDGRAD", "VWND", "VWNDGRAD", "N")
    ),
    col_types = readr::cols(
      PRESS = readr::col_integer(),
      REPGPH = readr::col_integer(),
      CALCGPH = readr::col_integer(),
      TEMP = readr::col_integer(),
      TEMPGRAD = readr::col_integer(),
      PTEMP = readr::col_integer(),
      PTEMPGRAD = readr::col_integer(),
      VTEMP = readr::col_integer(),
      VPTEMP = readr::col_integer(),
      VAPPRESS = readr::col_integer(),
      SATVAP = readr::col_integer(),
      REPRH = readr::col_integer(),
      CALCRH = readr::col_integer(),
      RHGRAD = readr::col_integer(),
      UWND = readr::col_integer(),
      UWDGRAD = readr::col_integer(),
      VWND = readr::col_integer(),
      VWNDGRAD = readr::col_integer(),
      N = readr::col_integer()
    ),
    comment = '#'
  )
}

#function to id the headers
is_header <- function(line0){
  substr(line0, 1, 1) == '#'
}

#function to parse station list
stations_parser <- function(file, ...) {
  readr::read_fwf(
    file = file,
    col_positions = readr::fwf_positions(
      c(1,13,22,32,39,42,73,78,83),
      c(11,20,30,37,40,71,76,81,88),
      c("ID","LATITUDE","LONGITUDE","ELEVATION","STATE","NAME","FSTYEAR","LSTYEAR","NOBS")
    ),
    col_types = readr::cols(
      ID = readr::col_character(),
      LATITUDE = readr::col_number(),
      LONGITUDE = readr::col_number(),
      ELEVATION = readr::col_number(),
      STATE = readr::col_character(),
      NAME = readr::col_character(),
      FSTYEAR = readr::col_integer(),
      LSTYEAR = readr::col_integer(),
      NOBS = readr::col_integer()
    )
  )
}

#function to parse everything into temp files
parse_IGRA <- function(file, header_parser, data_parser) {
  # Parsing the file
  cat("Parsing the file...")
  data0 <- data_parser(file)
  
  # Parsing and inserting the metadata separately
  lines <- readLines(file)
  header_flag <- sapply(lines, is_header)
  header_lines <- lines[header_flag]
  header_coverage <- table(cumsum(as.numeric(header_flag)))
  
  # Create temporary file for the header lines
  temp_file <- tempfile()
  write(header_lines, file = temp_file)
  metadata0 <- header_parser(temp_file)
  file.remove(temp_file)
  
  metadata <- dplyr::bind_rows(purrr::map2(
    1:nrow(metadata0), header_coverage,
    function(i, times) {
      metadata0[rep(i, times - 1), ]
    }
  ))
  
  cat("Done! \n")
  dplyr::bind_cols(metadata, data0)
}

#call to implement functions above on UAE and Quatar data and combine them
UAE <- parse_IGRA(uaeDat, v2_drvd_header_parser, v2_drvd_data_parser)[, -1]
QUATAR <- parse_IGRA(quatarDat, v2_drvd_header_parser, v2_drvd_data_parser)[, -1]
df <- rbind(UAE, QUATAR)

#call to parse station list
stations <- stations_parser(statList)

#cleaning up the environment
rm(uaeDat,quatarDat,AE,QA,is_header,parse_IGRA,v2_drvd_data_parser,
   v2_drvd_header_parser, QUATAR, UAE, stations_parser, statList)
file.remove("AEM00041217-drvd.txt","QAM00041169-drvd.txt")


#plot the stations on a world map
plot_stations <- st_as_sf(stations %>% filter(LSTYEAR == 2019, ELEVATION != -998.8), coords = c("LONGITUDE","LATITUDE"), crs = 4326, agr = "constant")

world <- ne_countries(scale = "medium", returnclass = "sf")
currentStatNum <- stations %>% filter(LSTYEAR == 2019) %>% tally()

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = plot_stations, size = 1, shape = 23, fill = "darkred")+
  ggtitle("Worldwide Radiosconde Launch Stations")

#plot the stations on a regional mid-east map
ggplot(data = world) +
  geom_sf() +
  geom_sf(data = plot_stations, size = 1, shape = 23, fill = "darkred")+
  coord_sf(xlim = c(45, 65), ylim = c(18, 32), expand = FALSE) +
  ggtitle("Radiosconde Launch Stations Around The UAE")


#plot 2 stations with data from the same days
df %>% group_by(DAY) %>%
  filter(HOUR == 12 & DAY == 1) %>%
  ggplot(aes(x = ((TEMP/10)-272.15), y= (CALCGPH))) +
  geom_point(aes(col=MONTH)) +
  facet_grid(ID ~ .)+
  ylab("Altitude (meters)") +
  xlab("Temperature (degrees of celcius)")

#plot just the alt we want to predict
df %>% group_by(DAY) %>%
  filter(HOUR == 12 & DAY == 1, ID == "AEM00041217") %>%
  ggplot(aes(x = ((TEMP/10)-272.15), y= (CALCGPH))) +
  geom_point(aes(col=MONTH)) +
  ylab("Altitude (meters)") +
  xlab("Temperature (degrees of celcius)") +
  ylim(900,1100) +
  xlim(10,40)+
  ggtitle("UAE Data for the 1st of Every Month Since 1984")

#environment cleanup and get to the data we want
new_df <- df %>% filter(ID == "AEM00041217" & CALCGPH > 900 & CALCGPH <1100)

rm(currentStatNum, df, plot_stations, stations, world)

#Generate an RMSE function
RMSE <- function(training_df, predicted_temperature){
  sqrt(mean((training_df - predicted_temperature)^2))
}

#calculate the daily mu
daily_mu <- training_df %>%
  group_by(monthDay) %>%
  summarize(mu = mean(TEMP)) %>%
  mutate(mu = (mu/10)-272.15)

#join the daily mu to the data frame
training_df <- left_join(training_df, daily_mu)

#calculate the RMSE on the training data
RMSE(training_df$tempC, training_df$mu)

#apply the mu to validation and calculate the RMSE on the validation data
validation_df <- left_join(validation_df, daily_mu)
RMSE(validation_df$tempC, validation_df$mu)