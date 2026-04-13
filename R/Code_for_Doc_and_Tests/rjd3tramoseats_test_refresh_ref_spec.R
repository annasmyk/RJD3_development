# Doc tramoseats_refresh..revamp avril 2026 (idem pour tramoseats)

# (faire code structuré)

# comments

# rsa full vs rsa5 ? td test

# 1 dans spec= ET refspec = ...
# on ne peut pas utiliser "rsa5" etc , mais il un output de tramoseats_spec

# defaut if ref non specified: rsa5, rg5: NON rsa4

# free parameters doesn't reestimate arima coeffs : true ?

# caomportement avec la partie reg arim
## - n'efface pas set transform avec complete !! ISSUE ? voir GUI

# comportement avec partie seats

- print n'affiche pas tous les params de la spec cf fcast and back cast '

    ## DOC correction de seats spec
    ## DOC correction de set_seats

## avec complete effacement de tous les params

# comportement avec partie benchmarking

    ## Doc correction set_benchmarking

library(rjd3toolkit)
library(rjd3tramoseats)

###
y_raw<-rjd3toolkit::ABS$X0.2.08.10.M

## spec name
rf<-tramoseats_spec("rsafull")
r5<-tramoseats_spec("rsa5")
r3<-tramoseats_spec("rsa3")

#config 0: ref= blank / perso name but default
tramoseats_spec_ref <- tramoseats_refresh(
    spec= r3, # point spec to be refreshed
    # refspec= , #default rsa5
    policy = "Complete"
)
tramoseats_spec_ref
r5
rf
# ramene  rsa3 à rsa full par defaut


#config 1: default / default : works if names aout od
tramoseats_spec_ref <- tramoseats_refresh(
    spec= r5, # point spec to be refreshed
    refspec= r3, # domain spec (set of constraints)
    policy = "Complete"
)

# ramene rsa5 à rsa3

#config 3: default / perso

tramoseats_spec_d<- set_transform(r3,
                           fun = "Log",
                           adjust="LengthOfPeriod",
                           outliers = TRUE)
tramoseats_spec_d

tramoseats_spec_ref <- tramoseats_refresh(
    spec= tramoseats_spec_d, # point spec to be refreshed
    refspec= r3, # domain spec (set of constraints)
    policy = "Complete"
)
tramoseats_spec_d

#### Example with user defined parameters

# Customized arima model specification
tramoseats_spec_d <- rjd3tramoseats::tramoseats_spec("rsa3") # re init
# disable automatic arima modelling
tramoseats_spec_d <- set_automodel(tramoseats_spec_d, enabled = FALSE)
# customize arima model
tramoseats_spec_d <- set_arima(
    tramoseats_spec_d,
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

tramoseats_spec_d

#
tramoseats_spec_ref <- tramoseats_refresh(
    spec= tramoseats_spec_d,
    policy = "Complete")

tramoseats_spec_ref


tramoseats_spec_ref <- tramoseats_refresh(
    spec= tramoseats_spec_d, # point spec to be refreshed
    refspec= r3, # domain spec (set of constraints)
    policy = "Complete"
)

tramoseats_spec_ref

## user defined on seats part  + complete

# seats total set

tramoseats_spec_d11<-set_seats(tramoseats_spec_d,
    fcasts = -2,
    bcasts = -1,
    trend.boundary=0.6,
    seas.boundary=0.75,
    algorithm= "KalmanSmoother")

## here Print spec incomplete
tramoseats_spec_d11

# cas complete
# tramoseats_spec_d<-set_seats(tramoseats_spec_d, seasonal.filter="S3X1")

tramoseats_spec_d11

# cas complete
tramoseats_spec_ref <- tramoseats_refresh(
    spec= tramoseats_spec_d11, # point spec to be refreshed
    refspec= r3, # domain spec (set of constraints)
    policy = "Complete"
)

tramoseats_spec_ref

# cas autre
tramoseats_spec_ref <- tramoseats_refresh(spec= tramoseats_spec_d11,
    refspec= r3,
    policy = "FixedAutoRegressiveParameters"
)

tramoseats_spec_ref



## user defined on benchmarking part + complete

tramoseats_spec_db<- set_benchmarking(
    tramoseats_spec_d11,
    enabled = TRUE,
    target = "Original",
    rho = 0.7,
    lambda = 0.5,
    forecast = TRUE,
    bias = "Multiplicative")

tramoseats_spec_db

# cas complete
tramoseats_spec_ref <- tramoseats_refresh(
    spec= tramoseats_spec_db, # point spec to be refreshed
    refspec= r3, # domain spec (set of constraints)
    policy = "Complete"
)

tramoseats_spec_ref

# cas autre
tramoseats_spec_ref <- tramoseats_refresh(spec= tramoseats_spec_db,
                            refspec= r3,
                            policy = "FixedAutoRegressiveParameters"
)

tramoseats_spec_ref

# TEST: with complete : OK !
# seats and Benchmarking part parameters are entirely
# reset to values in the reference spec.

# cas complete
tramoseats_spec_ref <- tramoseats_refresh(
    spec= r3, # point spec to be refreshed
    refspec= tramoseats_spec_db, # domain spec (set of constraints)
    policy = "Complete"
)

tramoseats_spec_ref

# cas autre
tramoseats_spec_ref <- tramoseats_refresh(spec= r3,
                            refspec= tramoseats_spec_db,
                            policy = "FixedAutoRegressiveParameters"
)

tramoseats_spec_ref

# test n°2
# refspec reference specification
 By default this is the `"RG4c"` or `"rsa4"` specification.
tramoseats_spec_ref <- tramoseats_refresh(
    spec= tramoseats_spec_db, # point spec to be refreshed
    policy = "Complete"
)

tramoseats_spec_ref
class(tramoseats_spec_ref)


# arima only

# Customized arima model specification
r_spec_5 <- rjd3tramoseats::regarima_spec("rg5c") # re init

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
rsa3<- rjd3tramoseats::tramoseats_spec("rsa3")



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
## Customize the seats part
user_spec<-set_seats(user_spec,
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

tramoseats_spec_ref <- tramoseats_refresh(spec= user_spec,
                            refspec= r3,
                            policy = "Outliers_StochasticComponent"
)

# print tramoseats_spec_ref
# user defined reg-arima model is reset and outliers will be re-identified
# on the whole series as no start and end specified, seats and Benchmarking parameters
# are left unchanged

# Complete

tramoseats_spec_ref <- tramoseats_refresh(spec= user_spec,
                            refspec= r3,
                            policy = "Complete"
)

tramoseats_spec_ref
# print tramoseats_spec_ref
# all user defined parameters are reset and replaced with "rsa3" parameters,
# including for seats and Benchmarking parameters

## Practical re-estimation example

y <- rjd3toolkit::ABS$X0.2.08.10.M

 # raw series for first estimation
y_raw <- window(y, end = c(2016, 12))

# raw series for second (refreshed) estimation
y_new <- window(y, end = c(2017, 6))


# first estimation
sa_tramoseats <- tramoseats(y_raw, user_spec)

# refreshing the specification resulting from the first estimation
# to partially adapt to new data

spec_to_refresh <- sa_tramoseats$result_spec
reference_spec <- sa_tramoseats$estimation_spec

# policy = "Fixed"
spec_tramoseats_ref <- tramoseats_refresh(spec_to_refresh,
     reference_spec,
     policy = "Fixed"
 )
 # 2nd estimation with refreshed specification
 sa_tramoseats_ref <- tramoseats(y_new, spec_tramoseats_ref)

 # policy = "Outliers"
 spec_tramoseats_ref <- tramoseats_refresh(spec_to_refresh,
     reference_spec,
     policy = "Outliers",
     period = 12,
     start = c(2017, 1)
 ) # outliers will be re-detected from January 2017 included
 # 2nd estimation with refreshed specification
 sa_tramoseats_ref <- tramoseats(y_new, spec_tramoseats_ref)

 # policy = "Current"
 spec_tramoseats_ref <- tramoseats_refresh(spec_to_refresh,
     reference_spec,
     policy = "Current",
     period = 12,
     start = c(2017, 1),
     end = end(y_new)
 )
# Points from January 2017 (included) until the end of the series will be
# treated as Additive Outliers, the previous reg-Arima model being otherwise
# kept fixed 2nd estimation with refreshed specification
sa_tramoseats_ref <- tramoseats(y_new, spec_tramoseats_ref)

# Procedure is the same procedure using regarima_refresh instead of tramoseats_refresh

################# BUGS ######################################
# Refresh with default

tramoseats_spec_ref <- tramoseats_refresh(spec= user_spec)

tramoseats_spec_ref

## free parameters : BUG ???

tramoseats_spec_ref <- tramoseats_refresh(spec= user_spec,
                            refspec= r3,
                            policy = "FreeParameters"
)

tramoseats_spec_ref
