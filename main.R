Sys.setenv(TZ = "UTC")
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(xts))
options(digits = 10)

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
  read.env(i)
  
  # calculate land-surface physics
  housekeeping()
  sflx()
  
  # store layer temperatures
  t[i,] <- c(TSKIN, STC)
  setTxtProgressBar(pb, i)
}
close(pb)

t <- cbind(TIMESTAMPS, t - 273.15) %>% as_tibble
colnames(t) <- c("time", str_c("l", 0:(ncol(t) - 2)))
t$time <- as.POSIXct(t$time, origin = origin)

t2 <- select(t, time, l1, l3, l5, l7, l9) %>%
  filter(time < ymd_h("2017-10-20 00")) %>%
  gather("l1", "l3", "l5", "l7", "l9", key = "l", value = "temp")

ggplot(t2) +
  geom_line(aes(time, temp, color = l)) + 
  xlab("") + ylab("")

# out_r <- t
# save(out_r, file = "lsm_out_r.RData")

