########################-
## forcing data ########-
########################-
# 1. read weather data
# 2. interpolate to dt
# 3. get tide height and compute submersion
# 4. use sfc pressure to adjust tide height
# ?? read wave data and compute wave run-up
#------------------------------#
forcing.data <- function() {
  ## read weather data
	DIR <- getwd() %>% dirname %>% str_c("/io/weather/")
	wfile <- str_c(DIR, "weather_", DT, ".RData")
  if (!file.exists(wfile)) stop("the weather file is missing")
  load(wfile)

  ### must be a xts with the following columns (colnames must be respected):
  ## index = time (YYYY-mm-dd HH:MM:SS)
  # wind - wind speed (m/s)
  # air  - surface air temperature (K)
  # sst  - sea surface temperature (K)
  # sw   - incoming short wave radiation (W/m2)
  # lw   - incoming  long wave radiation (W/m2)
  # rh   - relative humidity (0-100%)
  # pres - surface pressure (Pa)
  # rain - precipitation rate (mm/s, kg/m2/s) (always >= 0)
  # tide - height from mean water level (cm)
  # wave - NOT YET IMPLEMENTED sig or max wave height (m)

  # transform tide heights into tide in or tide out
  # 1 = underwater, 0 = out-of-water
  w$tide <- ifelse(w$tide > LOC$height, 1, 0)

  w
}
########################-
########################-
########################-
