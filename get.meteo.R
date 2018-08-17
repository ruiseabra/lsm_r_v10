Sys.setenv(TZ = "UTC")
options(digits = 10)
library(tidyverse)
library(lubridate)
library(stringr)
library(xts)
library(meteoForecast)

########################-
## collect tide data ###-
########################-
# lon
# lat
# T_RANGE = POSIXct date:time range (2 values)
# DT = every 'DT' seconds
########################-
fes.tides <- function(lat, lon, T_RANGE, DT) {
	Sys.setenv(HDF5_DISABLE_VERSION_CHECK = "2")
	# prepare
	ORIGIN  <- ymd("1950-01-01")
	t_range <- (T_RANGE + c(0, DT)) %>%
		julian(origin = ORIGIN) %>%
		as.numeric %>%
		formatC(format = "f")
	DT <- DT / 60
	CALL  <- str_c("fes_slev", lat, lon, t_range[1], t_range[2], DT, sep = " ")
	# run fes_slev
	tides <- system(CALL, intern = TRUE)[-(1:2)] %>% str_split(",")
	# extract timestamps
	times <- map_chr(tides, 1) %>%
		as.numeric %>%
		"*"(., (24 * 3600)) %>%
		round
	times <- (times - (times %% 60)) %>%
		as.POSIXct(origin = ORIGIN)
	times <- times - (as.numeric(times) %% 60)
	steady <- diff(times) %>%
		unique %>%
		length %>%
		"=="(., 1)
	if (!steady) stop("the period of 'times' is irregular")
	# extract tide elevation
	tides <- map_chr(tides, 2) %>% as.numeric
	# combine
	tides <- tibble(time = times, tide = tides)
	# filter to ensure that the data return does not exceed the t_range supplied
	tides <- filter(tides, time %within% interval(T_RANGE[1], T_RANGE[2]))
	# return
	tides
}
########################-
########################-
########################-

DIR <- getwd() %>% dirname %>% str_c("/io/weather/")
dir.create(DIR, showWarnings = FALSE, recursive = TRUE)

### generate a xts with the following columns (colnames must be respected):
## index = time (YYYY-mm-dd HH:MM:SS)
# wind - wind speed (m/s)
# air  - surface air temperature (K)
# sst  - sea surface temperature (K)
# sw   - incoming short wave radiation (W/m2)
# lw   - incoming  long wave radiation (W/m2)
# rh   - relative humidity (0-100%)
# pres - surface pressure (Pa)
# rain - precipitation rate (mm/s, kg/m2/s) (always >= 0)
# wave - NOT YET IMPLEMENTED sig or max wave height (m)

# var <- grepVar("", "meteogalicia", day = Sys.Date() - 15, complete = TRUE)
# var[29,]

# interpolation intervals
dt <- NULL
dt <- c(1800, 900)

### collect data ####
# define service provider
mfService("meteogalicia")

# projections
## wgs84 projection
wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
## used in the meteogalicia server
lcc <- mfProj4("meteogalicia")

# target variables
vars <- c("lwm","mod","temp","sst","swflx","lwflx","rh","mslp","prec")
cols <- c("wind","air","sst","sw","lw","rh","pres","rain")

t0 <- ymd_h("2017-10-10 00")
t1 <- ymd_h("2018-03-01 00")
lat <- 41.84
lon <- -8.89 # -8.85 is the eastermost (coastalmost) sea pixel in the meteogalicia model, but -8.89 is the easternmost sea pixel to get fes tides

loc <- SpatialPoints(cbind(lon, lat))
projection(loc) <- wgs84
# project point to lcc so it overlays with the data at the server
loc <- spTransform(loc, lcc)

# get data at coordinates
buf <- (3600 * 24)
w <- getPointDays(loc, vars = vars, start = t0 - buf, end = t1 + buf, resolution = 4)
w <- w[time(w) >= t0 & time(w) <= t1, ]
if (t0 != first(time(w))) stop("t0 has changed")
if (t1 !=  last(time(w))) stop("t1 has changed")
colnames(w) <- c("lwm", cols)

### check quality ####
# check that the pixel selected fall in water (== 1)
if (!w$lwm[1]) stop("inland location")
w <- w[,-1]

# fun DQSDT (within SatMix) is only valid for air temperatures between 173 and 373 K (-100 to +100 C)
if (any(w$air < 173 | w$air > 373)) stop("DQSDT")

# negative values for precipitation may occur, but they cannot be fed into the lsm model
w$rain <- ifelse(w$rain < 0, 0, w$rain)
w$rain <- w$rain / 3600

### get tide ####
tides <- fes.tides(lat, lon, c(t0, t1), 3600)$tide
# barometric pressure correction (+10 hPa = -10 cm; REF = 1013 hPa)
# this approach is crude but can be extended to the future as well as the past, with the same level of bias
tides <- tides - ((w$pres / 100) - 1013)
w$tide <- tides

# future # include waverunup
# future # include atmospheric effects (pressure, wind surge)

### clean up ####
w$rh <- w$rh * 100
w <- xts(coredata(w), time(w))
save(w, file = str_c(DIR, "weather_3600.RData"))
w2 <- w

### interpolate to other dt ####
T0 <- time(w2)
if (!any(is.null(dt))) {
	for (t in dt) {
		T1 <- seq.POSIXt(t0, t1, by = t)
		w  <- apply(coredata(w2), 2, function(x) approx(T0, x, T1, method = "linear", rule = 2)$y)
		w <- xts(w, T1)
		tides <- fes.tides(lat, lon, c(t0, t1), t)$tide
		w$tide <- tides - ((w$pres / 100) - 1013)
		save(w, file = str_c(DIR, "weather_", t, ".RData"))
	}
}
