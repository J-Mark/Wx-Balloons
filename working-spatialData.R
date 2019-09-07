#load/install spatial package
if(!require(sp)) install.packages("sp", repos = "http://cran.us.r-project.org")

#download the station list
statList <- tempfile()
download.file("https://www1.ncdc.noaa.gov/pub/data/igra/igra2-station-list.txt", statList)

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

stations <- stations_parser(statList)
rm(stations_parser, statList)