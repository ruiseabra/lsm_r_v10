Sys.setenv(TZ = "UTC")
options(digits = 10)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(meteoForecast))

# var <- grepVar("", "meteogalicia", day = Sys.Date() - 15, complete = TRUE)
# var[29,]

load("weather.Rdata")
w2 <- w; rm(w)

# define service provider
mfService("meteogalicia")

# projections
## wgs84 projection
wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
## used in the meteogalicia server
lcc <- mfProj4("meteogalicia")

# target variables
vars <- c("lwm","mod","temp","sst","swflx","lwflx","rh","mslp","prec")

t0 <- ymd_h("2017-10-10 00")
t1 <- ymd_h("2018-03-01 00")
lat <- 41.84
lon <- -8.85 # -8.85 is the eastermost (coastalmost) sea pixel

spoint <- SpatialPoints(cbind(lon, lat))
projection(spoint) <- wgs84
# project point to lcc so it overlays with the data at the server
spoint <- spTransform(spoint, lcc)

# get data at coordinates
w <- getPointDays(spoint,vars = vars, start = t0, end = t1, resolution = 4)

# change units and re-arrange data

### must be a tibble with the following columns (colnames must be respected):
# time - YYYY-mm-dd HH:MM
# wind - wind speed (m/s)
# air  - surface air temperature (K)
# sst  - sea surface temperature (K)
# sw   - incoming short wave radiation (W/m2)
# lw   - incoming  long wave radiation (W/m2)
# rh   - relative humidity (%)
# pres - surface pressure (hPa)
# rain - precipitation rate (mm/h, kg/m2/h)
# wave - NOT YET IMPLEMENTED sig or max wave height (m)

w <- tibble(
  time = time(w),
  wind = round(w$mod,   2),
  air  = round(w$temp,  1),
  sst  = round(w$sst,   1),
  sw   = round(w$swflx, 1),
  lw   = round(w$lwflx, 1),
  rh   = round(w$rh   * 100, 1),
  pres = round(w$mslp / 100, 0),
  rain = round(ifelse(w$prec < 0, 0, w$prec), 2)) %>% mutate_at(-1, as.numeric)

save(w, file = "weather.RData")
