Sys.setenv(TZ = "UTC")
options(digits = 10)
library(tidyverse)
library(lubridate)
library(stringr)
library(xts)
library(dygraphs)

DIR <- str_c(dirname(getwd()), "/io/")

T0  <- "2017-10-10 00"
T1  <- "2018-03-01 00"
DT  <- 1800
LOC <- list(lon = -8.876, lat = 41.839, height = 30)

# load parameters
for (f in dir(pattern = "PARAMS.")) source(f)

# load functions
for (f in dir(pattern = "FUNS.")) source(f)

# prepare forcing data
w <- forcing.data()

# set up matrix to store soil layer temperatures at each time step
t <- matrix(NA, nrow = nrow(w), NSOIL + 1)

### in loop
pb <- txtProgressBar(1, NRUN, style = 3)
for (i in 1:NRUN) {
  # i <- 1
  # grab line [i] of the forcing data tibble
  read.env(i, w)

  # calculate land-surface physics
  housekeeping()
  sflx()

  # store layer temperatures
  t[i,] <- c(TSKIN, STC)
  setTxtProgressBar(pb, i)
}
close(pb)

colnames(t) <- c("tskin", str_c("l", 1:(ncol(t) - 1)))
t <- xts(t - 273.15, TIMESTAMPS)
colnames(t) <- c("time", str_c("l", 0:(ncol(t) - 2)))
t$time <- as.POSIXct(t$time, origin = origin)

dygraph(t) %>%
	dyRangeSelector %>%
	dyLegend(
		show = "always",
		hideOnMouseOut = FALSE,
		labelsSeparateLines = TRUE)

