# Doc x13_refresh..

# (faire code structuré)

# comments



# free parameters doesn't reestimate arima coeffs : true ?

# caomportement avec la partie reg arim
## - n'efface pas set transform avec complete !! ISSUE ? voir GUI

# comportement avec partie x11
    ## DOC correction de x11 spec
    ## DOC correction de set_x11

## avec complete effacement de tous les params

# comportement avec partie benchmarking

    ## Doc correction set_benchmarking

library(rjd3toolkit)
library(rjd3x13)
library(rjd3tramoseats)

###
y_raw<-rjd3toolkit::ABS$X0.2.08.10.M

## spec name

r5<-x13_spec("rsa5")
r3<-x13_spec("rsa3")

#config 0: ref= blank / perso name but default
x13_spec_ref <- x13_refresh(
    spec= r3, # point spec to be refreshed
    # refspec= "rsa3", # domain spec (set of constraints)
    policy = "Complete"
)
x13_spec_ref
# ramene  rsa3 à rsa4 par defaut


#config 1: default / default : works if names aout od
x13_spec_ref <- x13_refresh(
    spec= r5, # point spec to be refreshed
    refspec= r3, # domain spec (set of constraints)
    policy = "Complete"
)

# ramene rsa5 à rsa3

#config 3: default / perso

x13_spec_d<- set_transform(r3,
                           fun = "Log",
                           adjust="LengthOfPeriod",
                           outliers = TRUE)
x13_spec_d

x13_spec_ref <- x13_refresh(
    spec= x13_spec_d, # point spec to be refreshed
    refspec= r3, # domain spec (set of constraints)
    policy = "Complete"
)
x13_spec_d
r3

#### Example with user defined parameters

# Customized arima model specification
x13_spec_d <- rjd3x13::x13_spec("rsa3") # re init
# disable automatic arima modelling
x13_spec_d <- set_automodel(x13_spec_d, enabled = FALSE)
# customize arima model
x13_spec_d <- set_arima(
    x13_spec_d,
    mean = 0.2,
    mean.type = "Fixed",
    p = 1,
    d = 2,
    q = 0,
    bp = 1,
    bd = 1,
    bq = 0,
    coef = c(0.6, 0.7),
    coef.type = c("Initial", "Fixed")
)

x13_spec_d

#
x13_spec_ref <- x13_refresh(
    spec= x13_spec_d,
    policy = "Complete")

x13_spec_ref


x13_spec_ref <- x13_refresh(
    spec= x13_spec_d, # point spec to be refreshed
    refspec= r3, # domain spec (set of constraints)
    policy = "Complete"
)

x13_spec_ref

## user defined on x11 part  + complete

set_x11(
    x,
    mode = c(NA, "Undefined", "Additive", "Multiplicative", "LogAdditive",
             "PseudoAdditive"),
    seasonal.comp = NA,
    seasonal.filter = NA,
    henderson.filter = NA,
    lsigma = NA,
    usigma = NA,
    fcasts = NA,
    bcasts = NA,
    calendar.sigma = c(NA, "None", "Signif", "All", "Select"),
    sigma.vector = NA,
    exclude.forecast = NA,
    bias = c(NA, "LEGACY")
)

# x11 total set

x13_spec_d11<-set_x11(x13_spec_d,
    mode="Additive",
    seasonal.comp = NA,
    seasonal.filter = NA,
    henderson.filter = NA,
    lsigma = 2,
    usigma = 3,
    fcasts = -2,
    bcasts = -1,
    calendar.sigma = "All",
    sigma.vector = NA,
    exclude.forecast = TRUE,
    bias = "LEGACY")

x13_spec_d11

# cas complete
# x13_spec_d<-set_x11(x13_spec_d, seasonal.filter="S3X1")

x13_spec_d11

# cas complete
x13_spec_ref <- x13_refresh(
    spec= x13_spec_d11, # point spec to be refreshed
    refspec= r3, # domain spec (set of constraints)
    policy = "Complete"
)

x13_spec_ref

# cas autre
x13_spec_ref <- x13_refresh(spec= x13_spec_d11,
    refspec= r3,
    policy = "FixedAutoRegressiveParameters"
)

x13_spec_ref



## user defined on benchmarking part + complete

x13_spec_db<- set_benchmarking(
    x13_spec_d11,
    enabled = TRUE,
    target = "Original",
    rho = 0.7,
    lambda = 0.5,
    forecast = TRUE,
    bias = "Multiplicative")

x13_spec_db

# cas complete
x13_spec_ref <- x13_refresh(
    spec= x13_spec_db, # point spec to be refreshed
    refspec= r3, # domain spec (set of constraints)
    policy = "Complete"
)

x13_spec_ref

# cas autre
x13_spec_ref <- x13_refresh(spec= x13_spec_db,
                            refspec= r3,
                            policy = "FixedAutoRegressiveParameters"
)

x13_spec_ref

# TEST: with complete : OK !
# X11 and Benchmarking part parameters are entirely
# reset to values in the reference spec.

# cas complete
x13_spec_ref <- x13_refresh(
    spec= r3, # point spec to be refreshed
    refspec= x13_spec_db, # domain spec (set of constraints)
    policy = "Complete"
)

x13_spec_ref

# cas autre
x13_spec_ref <- x13_refresh(spec= r3,
                            refspec= x13_spec_db,
                            policy = "FixedAutoRegressiveParameters"
)

x13_spec_ref

# test n°2
# refspec reference specification
 By default this is the `"RG4c"` or `"rsa4"` specification.
x13_spec_ref <- x13_refresh(
    spec= x13_spec_db, # point spec to be refreshed
    policy = "Complete"
)

x13_spec_ref
class(x13_spec_ref)


# arima only

# Customized arima model specification
r_spec_5 <- rjd3x13::regarima_spec("rg5c") # re init

# disable automatic arima modelling
r_spec_d <- set_automodel(r_spec_5, enabled = FALSE)
# customize arima model
r_spec_d <- set_arima(
    r_spec_d,
    mean = 0.2,
    mean.type = "Fixed",
    p = 1,
    d = 2,
    q = 0,
    bp = 1,
    bd = 1,
    bq = 0,
    coef = c(0.6, 0.7),
    coef.type = c("Initial", "Fixed")
)

r_spec_d


r_spec_ref <- regarima_refresh(
    spec= r_spec_d,
    policy = "Complete"
)

class(r_spec_ref)

############################################################
## Example pour doc

# Create refrence spec, here the default "rsa3"
rsa3<- rjd3x13::x13_spec("rsa3")



# Customize this spec
## In the Reg-Arma part
### disable automatic arima modelling
user_spec <- set_automodel(rsa3, enabled = FALSE)
### set arima model
user_spec <- set_arima(
    user_spec,
    mean = 0.2,
    mean.type = "Fixed",
    p = 1,
    d = 2,
    q = 0,
    bp = 1,
    bd = 1,
    bq = 0,
    coef = c(0.6, 0.7),
    coef.type = c("Initial", "Fixed")
)

user_spec
## Customize the x11 part
user_spec<-set_x11(user_spec,
                      lsigma = 2,
                      usigma = 3,
                      fcasts = -2,
                      bcasts = -1)

user_spec
## Customize the benchmarking part
user_spec<- set_benchmarking(
    user_spec,
    enabled = TRUE,
    target = "Original",
    rho = 0.7,
    lambda = 0.5,
    forecast = TRUE,
    bias = "Multiplicative")
user_spec

# "Outliers_StochasticComponent"

x13_spec_ref <- x13_refresh(spec= user_spec,
                            refspec= r3,
                            policy = "Outliers_StochasticComponent"
)

# print x13_spec_ref
# user defined reg-arima model is reset and outliers will be re-identified
# on the whole series as no start and end specified, X11 and Benchmarking parameters
# are left unchanged

# Complete

x13_spec_ref <- x13_refresh(spec= user_spec,
                            refspec= r3,
                            policy = "Complete"
)

x13_spec_ref
# print x13_spec_ref
# all user defined parameters are reset and replaced with "rsa3" parameters,
# including for X11 and Benchmarking parameters

## Practical re-estimation example

y <- rjd3toolkit::ABS$X0.2.08.10.M

 # raw series for first estimation
y_raw <- window(y, end = c(2016, 12))

# raw series for second (refreshed) estimation
y_new <- window(y, end = c(2017, 6))


# first estimation
sa_x13 <- x13(y_raw, user_spec)

# refreshing the specification resulting from the first estimation
# to partially adapt to new data

spec_to_refresh <- sa_x13$result_spec
reference_spec <- sa_x13$estimation_spec

# policy = "Fixed"
spec_x13_ref <- x13_refresh(spec_to_refresh,
     reference_spec,
     policy = "Fixed"
 )
 # 2nd estimation with refreshed specification
 sa_x13_ref <- x13(y_new, spec_x13_ref)

 # policy = "Outliers"
 spec_x13_ref <- x13_refresh(spec_to_refresh,
     reference_spec,
     policy = "Outliers",
     period = 12,
     start = c(2017, 1)
 ) # outliers will be re-detected from January 2017 included
 # 2nd estimation with refreshed specification
 sa_x13_ref <- x13(y_new, spec_x13_ref)

 # policy = "Current"
 spec_x13_ref <- x13_refresh(spec_to_refresh,
     reference_spec,
     policy = "Current",
     period = 12,
     start = c(2017, 1),
     end = end(y_new)
 )
# Points from January 2017 (included) until the end of the series will be
# treated as Additive Outliers, the previous reg-Arima model being otherwise
# kept fixed 2nd estimation with refreshed specification
sa_x13_ref <- x13(y_new, spec_x13_ref)

# Procedure is the same procedure using regarima_refresh instead of x13_refresh

################# BUGS ######################################
# Refresh with default

x13_spec_ref <- x13_refresh(spec= user_spec)

x13_spec_ref

## free parameters : BUG ???

x13_spec_ref <- x13_refresh(spec= user_spec,
                            refspec= r3,
                            policy = "FreeParameters"
)

x13_spec_ref
