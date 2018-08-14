# gravitational acceleration (m s-2)
G <- 9.8

# freezing temperature (K)
FRZ <- 273.15

# (water)/(dry air) molecular mass ratio, epsilon
EPS <- 0.622

# latent heat of vaporization of water (condensing, J kg−1) 
#   aka LSUBC
LVH2O <- 2.501e6 # wikipedia = 2.264e6

# ???
SIGMA <- 5.67e-8

# heat capacity (J kg-1 K-1)
# cpv = water vapor at constant pressure
CPV <- 1870
# cw = liquid water
CW  <- 4187
# cp = dry air at constant pressure
CP  <- 1004.5

# ideal gas constant for water vapor (J kg-1 K-1)
RV <- 461.5

# saturation vapor pressure at freezing temperature (Pa)
ESO <- 611.2

# porosity
# i.e. saturated (max) value of soil moisture (volumetric)
SMCMAX <- 0.181

# saturated soil potential
SATPSI <- 0.04

# saturated soil hydraulic conductivity
DKSAT <- 0.01

# constant C in Zilitinkevich, S. S.1995
# used to calculate roughness length of heat
CZIL <- 0.075

# bare soil evaporation exponent
FXEXP <- 2

# lower boundary soil temperature
# depth (m)
ZBOT <- -8

# a function of REFKDT and DKSAT
# KDT = REFKDT * DKSAT / REFDK
# REFDK = 2e-6 is the DKSAT value for soil type 2
KDT <- 3 * DKSAT / 2e-6

# 'B' parameter (????)
BB <- 4

# saturated soil diffusivity
DWSAT <- BB * DKSAT * (SATPSI / SMCMAX)

# wilting point
# dry soil moisture threshold where direct evaporation from
# top layer ends (volumetric)
SMCWLT <- SMCMAX * ((200 / SATPSI)^(-1 / BB))
# SMLOW = 0.5
SMCWLT <- SMCWLT - 0.5 * SMCWLT


# ?
ELCP  <- 2488.8 # unknow meaning, used in PENMAN
R     <- 287.04
