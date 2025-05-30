##### List of scripts on refresh

#### Focusing on spec writing (not yet on reestimation with refreshed spec)
# Part 1 default spec,  no user def params
## start rsa5c : test policies which work

## ISUES (cf Notes....txt as well)
# rsa 4c et pas 4 normally

#####################
library("rjd3toolkit")
library("rjd3tramoseats")
# Data  :
ipi <- read.csv2("C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Data/IPI_nace4.csv")
ipi$date <- as.Date(ipi$date, format = "%d/%m/%Y")
ipi[, -1] <- sapply(ipi[, -1], as.numeric)
# creating a TS object from a data frame
y_raw <- ts(ipi[, "RF0811"], frequency = 12, start = c(1990, 1), end = c(2021, 12))
y_new <- ts(ipi[, "RF0811"], frequency = 12, start = c(1990, 1), end = c(2022, 9))

## make refresh period long too see restimations

################# LAYERS

## Layer 1: spec (1 default 2 customized 3 customized and userdef regressors)
##
# y_raw<-rjd3toolkit::ABS$X0.2.08.10.M
# spec_tramoseats(name = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5"))
spec_ts_d <- rjd3tramoseats::spec_tramoseats("rsa5") #### HERE PB !!! issue : rsa4 et pas rsa4c
spec_ts_d
## Layer 2: estimation spec
sa_ts_d <- rjd3tramoseats::tramoseats(y_raw, spec_ts_d)
# V2 could be this : sa_ts_d<- rjd3tramoseats::tramoseats(y_raw, "rsa")
sa_ts_d$estimation_spec
## Layer 3: result spec
sa_ts_d$estimation_spec
## Layer 4: refreshed spec : policy: 1 policy per file ?
current_result_spec <- sa_ts_d$result_spec
current_domain_spec <- sa_ts_d$estimation_spec
spec_tramoseats_ref <- tramoseats_refresh(current_result_spec, # point spec to be refreshed
    current_domain_spec, # domain spec (set of constraints)
    policy = "FixedParameters"
)

### comprendre ce que ça fait
# spec_tramoseats_ref <- x13_refresh(current_domain_spec, # point spec to be refreshed
#                             current_result_spec, #domain spec (set of constraints)
#                             policy = "FixedParameters")

# policy = c("FreeParameters",
# "Complete", "Outliers_StochasticComponent",
# "Outliers",
#            "FixedParameters", "FixedAutoRegressiveParameters", "Fixed")
####
# "FixedAutoRegressiveParameters" : works with x13


## Layer 5: estimation with spec from refresh
sa_ts_ref <- tramoseats(y_new, spec_tramoseats_ref)
sa_ts_ref$estimation_spec
## layer 6: result spec from
sa_ts_ref$result_spec

## basic (switch to TS)
spec_ts_d$tramo$basic$span$type
spec_ts_d$tramo$basic$span$type
sa_ts_d$estimation_spec$tramo$basic$span$type
sa_ts_d$result_spec$tramo$basic$span$type
spec_tramoseats_ref$tramo$basic$span$type
sa_ts_ref$estimation_spec$tramo$basic$span$type
sa_ts_ref$result_spec$tramo$basic$span$type

spec_ts_d$tramo$basic$span$d0
sa_ts_d$estimation_spec$tramo$basic$span$d0
sa_ts_d$result_spec$tramo$basic$span$d0
spec_tramoseats_ref$tramo$basic$span$d0
sa_ts_ref$estimation_spec$tramo$basic$span$d0
sa_ts_ref$result_spec$tramo$basic$span$d0

spec_ts_d$tramo$basic$span$d1
sa_ts_d$estimation_spec$tramo$basic$span$d1
sa_ts_d$result_spec$tramo$basic$span$d1
spec_tramoseats_ref$tramo$basic$span$d1
sa_ts_ref$estimation_spec$tramo$basic$span$d1
sa_ts_ref$result_spec$tramo$basic$span$d1

spec_ts_d$tramo$basic$span$n0
sa_ts_d$estimation_spec$tramo$basic$span$n0
sa_ts_d$result_spec$tramo$basic$span$n0
spec_tramoseats_ref$tramo$basic$span$n0
sa_ts_ref$estimation_spec$tramo$basic$span$n0
sa_ts_ref$result_spec$tramo$basic$span$n0

spec_ts_d$tramo$basic$span$n1
sa_ts_d$estimation_spec$tramo$basic$span$n1
sa_ts_d$result_spec$tramo$basic$span$n1
spec_tramoseats_ref$tramo$basic$span$n1
sa_ts_ref$estimation_spec$tramo$basic$span$n1
sa_ts_ref$result_spec$tramo$basic$span$n1

spec_ts_d$tramo$basic$preprocessing
sa_ts_d$estimation_spec$tramo$basic$preprocessing
sa_ts_d$result_spec$tramo$basic$preprocessing
spec_tramoseats_ref$tramo$basic$preprocessing
sa_ts_ref$estimation_spec$tramo$basic$preprocessing
sa_ts_ref$result_spec$tramo$basic$preprocessing

spec_ts_d$tramo$basic$preliminaryCheck
sa_ts_d$estimation_spec$tramo$basic$preliminaryCheck
sa_ts_d$result_spec$tramo$basic$preliminaryCheck
spec_tramoseats_ref$tramo$basic$preliminaryCheck
sa_ts_ref$estimation_spec$tramo$basic$preliminaryCheck
sa_ts_ref$result_spec$tramo$basic$preliminaryCheck

## transform
## refresh : is transported, never touched (exception = complete)
### PB with refresh seems to reestimate schema
spec_ts_d$tramo$transform$fn
sa_ts_d$estimation_spec$tramo$transform$fn
sa_ts_d$result_spec$tramo$transform$fn
spec_tramoseats_ref$tramo$transform$fn
sa_ts_ref$estimation_spec$tramo$transform$fn
sa_ts_ref$result_spec$tramo$transform$fn

## here pb de adjust and leap year : not clear
spec_ts_d$tramo$transform$adjust
sa_ts_d$estimation_spec$tramo$transform$adjust
sa_ts_d$result_spec$tramo$transform$adjust
spec_tramoseats_ref$tramo$transform$adjust
sa_ts_ref$estimation_spec$tramo$transform$adjust
sa_ts_ref$result_spec$tramo$transform$adjust

## outlier (auto detection params) mieux ecrit que ds x12
spec_ts_d$tramo$outlier$enabled
sa_ts_d$estimation_spec$tramo$outlier$enabled
sa_ts_d$result_spec$tramo$outlier$enabled
spec_tramoseats_ref$tramo$outlier$enabled
sa_ts_ref$estimation_spec$tramo$outlier$enabled
sa_ts_ref$result_spec$tramo$outlier$enabled

spec_ts_d$tramo$outlier$span$type
sa_ts_d$estimation_spec$tramo$outlier$span$type
sa_ts_d$result_spec$tramo$outlier$span$type
spec_tramoseats_ref$tramo$outlier$span$type
sa_ts_ref$estimation_spec$tramo$outlier$span$type
sa_ts_ref$result_spec$tramo$outlier$span$type


spec_ts_d$tramo$outlier$span$d0
sa_ts_d$estimation_spec$tramo$outlier$span$d0
sa_ts_d$result_spec$tramo$outlier$span$d0
spec_tramoseats_ref$tramo$outlier$span$d0
sa_ts_ref$estimation_spec$tramo$outlierspan$d0
sa_ts_ref$result_spec$tramo$outlier$span$d0

spec_ts_d$tramo$outlier$span$d1
sa_ts_d$estimation_spec$tramo$outlier$span$d1
sa_ts_d$result_spec$tramo$outlier$span$d1
spec_tramoseats_ref$tramo$outlier$span$d1
sa_ts_ref$estimation_spec$tramo$outlierspan$d1
sa_ts_ref$result_spec$tramo$outlier$span$d1


spec_ts_d$tramo$outlier$span$n0
sa_ts_d$estimation_spec$tramo$outlier$span$n0
sa_ts_d$result_spec$tramo$outlier$span$n0
spec_tramoseats_ref$tramo$outlier$span$n0
sa_ts_ref$estimation_spec$tramo$outlier$span$n0
sa_ts_ref$result_spec$tramo$outlier$span$n0

spec_ts_d$tramo$outlier$span$n1
sa_ts_d$estimation_spec$tramo$outlier$span$n1
sa_ts_d$result_spec$tramo$outlier$span$n1
spec_tramoseats_ref$tramo$outlier$span$n1
sa_ts_ref$estimation_spec$tramo$outlier$span$n1
sa_ts_ref$result_spec$tramo$outlier$span$n1

spec_ts_d$tramo$outlier$va
sa_ts_d$estimation_spec$tramo$outlier$va
sa_ts_d$result_spec$tramo$outlier$va
spec_tramoseats_ref$tramo$outlier$va
sa_ts_ref$estimation_spec$tramo$outlier$va
sa_ts_ref$result_spec$tramo$outlier$va

## check ml
spec_ts_d$tramo$outlier$ml
sa_ts_d$estimation_spec$tramo$outlier$ml
sa_ts_d$result_spec$tramo$outlier$ml
spec_tramoseats_ref$tramo$outlier$ml
sa_ts_ref$estimation_spec$tramo$outlier$ml
sa_ts_ref$result_spec$tramo$outlier$ml

spec_ts_d$tramo$outlier$tcrate
sa_ts_d$estimation_spec$tramo$outlier$tcrate
sa_ts_d$result_spec$tramo$outlier$tcrate
spec_tramoseats_ref$tramo$outlier$tcrate
sa_ts_ref$estimation_spec$tramo$outlier$tcrate
sa_ts_ref$result_spec$tramo$outlier$tcrate

spec_ts_d$tramo$outlier$ao
sa_ts_d$estimation_spec$tramo$outlier$ao
sa_ts_d$result_spec$tramo$outlier$ao
spec_tramoseats_ref$tramo$outlier$ao
sa_ts_ref$estimation_spec$tramo$outlier$ao
sa_ts_ref$result_spec$tramo$outlier$ao

spec_ts_d$tramo$outlier$ls
sa_ts_d$estimation_spec$tramo$outlier$ls
sa_ts_d$result_spec$tramo$outlier$ls
spec_tramoseats_ref$tramo$outlier$ls
sa_ts_ref$estimation_spec$tramo$outlier$ls
sa_ts_ref$result_spec$tramo$outlier$ls

spec_ts_d$tramo$outlier$tc
sa_ts_d$estimation_spec$tramo$outlier$tc
sa_ts_d$result_spec$tramo$outlier$tc
spec_tramoseats_ref$tramo$outlier$tc
sa_ts_ref$estimation_spec$tramo$outlier$tc
sa_ts_ref$result_spec$tramo$outlier$tc

spec_ts_d$tramo$outlier$so
sa_ts_d$estimation_spec$tramo$outlier$so
sa_ts_d$result_spec$tramo$outlier$so
spec_tramoseats_ref$tramo$outlier$so
sa_ts_ref$estimation_spec$tramo$outlier$so
sa_ts_ref$result_spec$tramo$outlier$so

## arima
spec_ts_d$tramo$arima$period
sa_ts_d$estimation_spec$tramo$arima$period
sa_ts_d$result_spec$tramo$arima$period
spec_tramoseats_ref$tramo$arima$period
sa_ts_ref$estimation_spec$tramo$arima$period
sa_ts_ref$result_spec$tramo$arima$period


spec_ts_d$tramo$arima$d
sa_ts_d$estimation_spec$tramo$arima$d
sa_ts_d$result_spec$tramo$arima$d
spec_tramoseats_ref$tramo$arima$d
sa_ts_ref$estimation_spec$tramo$arima$d
sa_ts_ref$result_spec$tramo$arima$d

spec_ts_d$tramo$arima$bd
sa_ts_d$estimation_spec$tramo$arima$bd
sa_ts_d$result_spec$tramo$arima$bd
spec_tramoseats_ref$tramo$arima$bd
sa_ts_ref$estimation_spec$tramo$arima$bd
sa_ts_ref$result_spec$tramo$arima$bd

spec_ts_d$tramo$arima$phi
sa_ts_d$estimation_spec$tramo$arima$phi
sa_ts_d$result_spec$tramo$arima$phi
spec_tramoseats_ref$tramo$arima$phi
sa_ts_ref$estimation_spec$tramo$arima$phi
sa_ts_ref$result_spec$tramo$arima$phi

spec_ts_d$tramo$arima$theta
sa_ts_d$estimation_spec$tramo$arima$theta
sa_ts_d$result_spec$tramo$arima$theta
spec_tramoseats_ref$tramo$arima$theta
sa_ts_ref$estimation_spec$tramo$arima$theta
sa_ts_ref$result_spec$tramo$arima$theta

spec_ts_d$tramo$arima$bphi
sa_ts_d$estimation_spec$tramo$arima$bphi
sa_ts_d$result_spec$tramo$arima$bphi
spec_tramoseats_ref$tramo$arima$bphi
sa_ts_ref$estimation_spec$tramo$arima$bphi
sa_ts_ref$result_spec$tramo$arima$bphi

spec_ts_d$tramo$arima$btheta
sa_ts_d$estimation_spec$tramo$arima$btheta
sa_ts_d$result_spec$tramo$arima$btheta
spec_tramoseats_ref$tramo$arima$btheta
sa_ts_ref$estimation_spec$tramo$arima$btheta
sa_ts_ref$result_spec$tramo$arima$btheta

# ## automodel
spec_ts_d$tramo$automodel$enabled
sa_ts_d$estimation_spec$tramo$automodel$enabled
sa_ts_d$result_spec$tramo$automodel$enabled
spec_tramoseats_ref$tramo$automodel$enabled
sa_ts_ref$estimation_spec$tramo$automodel$enabled
sa_ts_ref$result_spec$tramo$automodel$enabled

# pcr to check
spec_ts_d$tramo$automodel$pcr
sa_ts_d$estimation_spec$tramo$automodel$pcr
sa_ts_d$result_spec$tramo$automodel$pcr
spec_tramoseats_ref$tramo$automodel$pcr
sa_ts_ref$estimation_spec$tramo$automodel$pcr
sa_ts_ref$result_spec$tramo$automodel$pcr

spec_ts_d$tramo$automodel$pc
sa_ts_d$estimation_spec$tramo$automodel$pc
sa_ts_d$result_spec$tramo$automodel$pc
spec_tramoseats_ref$tramo$automodel$pc
sa_ts_ref$estimation_spec$tramo$automodel$pc
sa_ts_ref$result_spec$tramo$automodel$pc


spec_ts_d$tramo$automodel$tsig
sa_ts_d$estimation_spec$tramo$automodel$tsig
sa_ts_d$result_spec$tramo$automodel$tsig
spec_tramoseats_ref$tramo$automodel$tsig
sa_ts_ref$estimation_spec$tramo$automodel$tsig
sa_ts_ref$result_spec$tramo$automodel$tsig

spec_ts_d$tramo$automodel$amicompare
sa_ts_d$estimation_spec$tramo$automodel$amicompare
sa_ts_d$result_spec$tramo$automodel$amicompare
spec_tramoseats_ref$tramo$automodel$amicompare
sa_ts_ref$estimation_spec$tramo$automodel$amicompare
sa_ts_ref$result_spec$tramo$automodel$amicompare

spec_ts_d$tramo$automodel$ub1
sa_ts_d$estimation_spec$tramo$automodel$ub1
sa_ts_d$result_spec$tramo$automodel$ub1
spec_tramoseats_ref$tramo$automodel$ub1
sa_ts_ref$estimation_spec$tramo$automodel$ub1
sa_ts_ref$result_spec$tramo$automodel$ub1

spec_ts_d$tramo$automodel$ub2
sa_ts_d$estimation_spec$tramo$automodel$ub2
sa_ts_d$result_spec$tramo$automodel$ub2
spec_tramoseats_ref$tramo$automodel$ub2
sa_ts_ref$estimation_spec$tramo$automodel$ub2
sa_ts_ref$result_spec$tramo$automodel$ub2


spec_ts_d$tramo$automodel$cancel
sa_ts_d$estimation_spec$tramo$automodel$cancel
sa_ts_d$result_spec$tramo$automodel$cancel
spec_tramoseats_ref$tramo$automodel$cancel
sa_ts_ref$estimation_spec$tramo$automodel$cancel
sa_ts_ref$result_spec$tramo$automodel$cancel


spec_ts_d$tramo$automodel$acceptdef
sa_ts_d$estimation_spec$tramo$automodel$acceptdef
sa_ts_d$result_spec$tramo$automodel$acceptdef
spec_tramoseats_ref$tramo$automodel$acceptdef
sa_ts_ref$estimation_spec$tramo$automodel$acceptdef
sa_ts_ref$result_spec$tramo$automodel$acceptdef

#
# ## regression
spec_ts_d$tramo$regression$mean
sa_ts_d$estimation_spec$tramo$regression$mean
sa_ts_d$result_spec$tramo$regression$mean
spec_tramoseats_ref$tramo$regression$mean # nothing in spec
sa_ts_ref$estimation_spec$tramo$regression$mean
sa_ts_ref$result_spec$tramo$regression$mean # estimated value only here

spec_ts_d$tramo$regression$check_mean
sa_ts_d$estimation_spec$tramo$regression$check_mean
sa_ts_d$result_spec$tramo$regression$check_mean
spec_tramoseats_ref$tramo$regression$check_mean
sa_ts_ref$estimation_spec$tramo$regression$check_mean
sa_ts_ref$result_spec$tramo$regression$check_mean


## regression$td
### what is this
### how to change it
spec_ts_d$tramo$regression$td$td
sa_ts_d$estimation_spec$tramo$regression$td$td
sa_ts_d$result_spec$tramo$regression$td$td
spec_tramoseats_ref$tramo$regression$td$td
sa_ts_ref$estimation_spec$tramo$regression$td$td
sa_ts_ref$result_spec$tramo$regression$td$td
#
spec_ts_d$tramo$regression$td$lp
sa_ts_d$estimation_spec$tramo$regression$td$lp
sa_ts_d$result_spec$tramo$regression$td$lp
spec_tramoseats_ref$tramo$regression$td$lp
sa_ts_ref$estimation_spec$tramo$regression$td$lp
sa_ts_ref$result_spec$tramo$regression$td$lp

spec_ts_d$tramo$regression$td$holidays
sa_ts_d$estimation_spec$tramo$regression$td$holidays
sa_ts_d$result_spec$tramo$regression$td$holidays
spec_tramoseats_ref$tramo$regression$td$holidays
sa_ts_ref$estimation_spec$tramo$regression$td$holidays
sa_ts_ref$result_spec$tramo$regression$td$holidays
#
spec_ts_d$tramo$regression$td$users
sa_ts_d$estimation_spec$tramo$regression$td$users
sa_ts_d$result_spec$tramo$regression$td$users
spec_tramoseats_ref$tramo$regression$td$users
sa_ts_ref$estimation_spec$tramo$regression$td$users
sa_ts_ref$result_spec$tramo$regression$td$users

spec_ts_d$tramo$regression$td$w
sa_ts_d$estimation_spec$tramo$regression$td$w
sa_ts_d$result_spec$tramo$regression$td$w
spec_tramoseats_ref$tramo$regression$td$w
sa_ts_ref$estimation_spec$tramo$regression$td$w
sa_ts_ref$result_spec$tramo$regression$td$w

spec_ts_d$tramo$regression$td$test
sa_ts_d$estimation_spec$tramo$regression$td$test
sa_ts_d$result_spec$tramo$regression$td$test
spec_tramoseats_ref$tramo$regression$td$test
sa_ts_ref$estimation_spec$tramo$regression$td$test
sa_ts_ref$result_spec$tramo$regression$td$test

spec_ts_d$tramo$regression$td$auto
sa_ts_d$estimation_spec$tramo$regression$td$auto
sa_ts_d$result_spec$tramo$regression$td$auto
spec_tramoseats_ref$tramo$regression$td$auto
sa_ts_ref$estimation_spec$tramo$regression$td$auto
sa_ts_ref$result_spec$tramo$regression$td$auto

spec_ts_d$tramo$regression$td$autoadjust
sa_ts_d$estimation_spec$tramo$regression$td$autoadjust
sa_ts_d$result_spec$tramo$regression$td$autoadjust
spec_tramoseats_ref$tramo$regression$td$autoadjust
sa_ts_ref$estimation_spec$tramo$regression$td$autoadjust
sa_ts_ref$result_spec$tramo$regression$td$autoadjust

spec_ts_d$tramo$regression$td$tdcoefficients
sa_ts_d$estimation_spec$tramo$regression$td$tdcoefficients
sa_ts_d$result_spec$tramo$regression$td$tdcoefficients
spec_tramoseats_ref$tramo$regression$td$tdcoefficients
sa_ts_ref$estimation_spec$tramo$regression$td$tdcoefficients
sa_ts_ref$result_spec$tramo$regression$td$tdcoefficients

spec_ts_d$tramo$regression$td$lpcoefficient
sa_ts_d$estimation_spec$tramo$regression$td$lpcoefficient
sa_ts_d$result_spec$tramo$regression$td$lpcoefficient
spec_tramoseats_ref$tramo$regression$td$lpcoefficient
sa_ts_ref$estimation_spec$tramo$regression$td$lpcoefficient
sa_ts_ref$result_spec$tramo$regression$td$lpcoefficient
#
# ## regression$easter
spec_ts_d$tramo$regression$easter$type
sa_ts_d$estimation_spec$tramo$regression$easter$type
sa_ts_d$result_spec$tramo$regression$easter$type
spec_tramoseats_ref$tramo$regression$easter$type
sa_ts_ref$estimation_spec$tramo$regression$easter$type
sa_ts_ref$result_spec$tramo$regression$easter$type

spec_ts_d$tramo$regression$easter$duration
sa_ts_d$estimation_spec$tramo$regression$easter$duration
sa_ts_d$result_spec$tramo$regression$easter$duration
spec_tramoseats_ref$tramo$regression$easter$duration
sa_ts_ref$estimation_spec$tramo$regression$easter$duration
sa_ts_ref$result_spec$tramo$regression$easter$duration

spec_ts_d$tramo$regression$easter$test
sa_ts_d$estimation_spec$tramo$regression$easter$test
sa_ts_d$result_spec$tramo$regression$easter$test
spec_tramoseats_ref$tramo$regression$easter$test
sa_ts_ref$estimation_spec$tramo$regression$easter$test
sa_ts_ref$result_spec$tramo$regression$easter$test

spec_ts_d$tramo$regression$easter$coefficient
sa_ts_d$estimation_spec$tramo$regression$easter$coefficient
sa_ts_d$result_spec$tramo$regression$easter$coefficient
spec_tramoseats_ref$tramo$regression$easter$coefficient
sa_ts_ref$estimation_spec$tramo$regression$easter$coefficient
sa_ts_ref$result_spec$tramo$regression$easter$coefficient
#
## outliers / ramps / user def vars
spec_ts_d$tramo$regression$outliers
sa_ts_d$estimation_spec$tramo$regression$outliers
sa_ts_d$result_spec$tramo$regression$outliers
spec_tramoseats_ref$tramo$regression$outliers
sa_ts_ref$estimation_spec$tramo$regression$outliers
sa_ts_ref$result_spec$tramo$regression$outliers

spec_ts_d$tramo$regression$ramps
sa_ts_d$estimation_spec$tramo$regression$ramps
sa_ts_d$result_spec$tramo$regression$ramps
spec_tramoseats_ref$tramo$regression$ramps
sa_ts_ref$estimation_spec$tramo$regression$ramps
sa_ts_ref$result_spec$tramo$regression$ramps

spec_ts_d$tramo$regression$users
sa_ts_d$estimation_spec$tramo$regression$users
sa_ts_d$result_spec$tramo$regression$users
spec_tramoseats_ref$tramo$regression$users
sa_ts_ref$estimation_spec$tramo$regression$users
sa_ts_ref$result_spec$tramo$regression$users


## estimate
spec_ts_d$tramo$estimate$span$type
sa_ts_d$estimation_spec$tramo$estimate$span$type
sa_ts_d$result_spec$tramo$estimate$span$type
spec_tramoseats_ref$tramo$estimate$span$type
sa_ts_ref$estimation_spec$tramo$estimate$span$type
sa_ts_ref$result_spec$tramo$estimate$span$type


spec_ts_d$tramo$estimate$span$d0
sa_ts_d$estimation_spec$tramo$estimate$span$d0
sa_ts_d$result_spec$tramo$estimate$span$d0
spec_tramoseats_ref$tramo$estimate$span$d0
sa_ts_ref$estimation_spec$tramo$estimate$span$d0
sa_ts_ref$result_spec$tramo$estimate$span$d0


spec_ts_d$tramo$estimate$span$d1
sa_ts_d$estimation_spec$tramo$estimate$span$d1
sa_ts_d$result_spec$tramo$estimate$span$d1
spec_tramoseats_ref$tramo$estimate$span$d1
sa_ts_ref$estimation_spec$tramo$estimate$span$d1
sa_ts_ref$result_spec$tramo$estimate$span$d1


spec_ts_d$tramo$estimate$span$n0
sa_ts_d$estimation_spec$tramo$estimate$span$n0
sa_ts_d$result_spec$tramo$estimate$span$n0
spec_tramoseats_ref$tramo$estimate$span$n0
sa_ts_ref$estimation_spec$tramo$estimate$span$n0
sa_ts_ref$result_spec$tramo$estimate$span$n0

spec_ts_d$tramo$estimate$span$n1
sa_ts_d$estimation_spec$tramo$estimate$span$n1
sa_ts_d$result_spec$tramo$estimate$span$n1
spec_tramoseats_ref$tramo$estimate$span$n1
sa_ts_ref$estimation_spec$tramo$estimate$span$n1
sa_ts_ref$result_spec$tramo$estimate$span$n1

spec_ts_d$tramo$estimate$tol
sa_ts_d$estimation_spec$tramo$estimate$tol
sa_ts_d$result_spec$tramo$estimate$tol
spec_tramoseats_ref$tramo$estimate$tol
sa_ts_ref$estimation_spec$tramo$estimate$tol
sa_ts_ref$result_spec$tramo$estimate$tol
#
# ### decomp avec SEATS
spec_ts_d$seats$xl
sa_ts_d$estimation_spec$seats$xl
sa_ts_d$result_spec$seats$xl
spec_tramoseats_ref$seats$xl
sa_ts_ref$estimation_spec$seats$xl
sa_ts_ref$result_spec$seats$xl

spec_ts_d$seats$approximation
sa_ts_d$estimation_spec$seats$approximation
sa_ts_d$result_spec$seats$approximation
spec_tramoseats_ref$seats$approximation
sa_ts_ref$estimation_spec$seats$approximation
sa_ts_ref$result_spec$seats$approximation

spec_ts_d$seats$epsphi
sa_ts_d$estimation_spec$seats$epsphi
sa_ts_d$result_spec$seats$epsphi
spec_tramoseats_ref$seats$epsphi
sa_ts_ref$estimation_spec$seats$epsphi
sa_ts_ref$result_spec$seats$epsphi

spec_ts_d$seats$rmod
sa_ts_d$estimation_spec$seats$rmod
sa_ts_d$result_spec$seats$rmod
spec_tramoseats_ref$seats$rmod
sa_ts_ref$estimation_spec$seats$rmod
sa_ts_ref$result_spec$seats$rmod

spec_ts_d$seats$sbound
sa_ts_d$estimation_spec$seats$sbound
sa_ts_d$result_spec$seats$sbound
spec_tramoseats_ref$seats$sbound
sa_ts_ref$estimation_spec$seats$sbound
sa_ts_ref$result_spec$seats$sbound

spec_ts_d$seats$sboundatpi
sa_ts_d$estimation_spec$seats$sboundatpi
sa_ts_d$result_spec$seats$sboundatpi
spec_tramoseats_ref$seats$sboundatpi
sa_ts_ref$estimation_spec$seats$sboundatpi
sa_ts_ref$result_spec$seats$sboundatpi

spec_ts_d$seats$nfcasts
sa_ts_d$estimation_spec$seats$nfcasts
sa_ts_d$result_spec$seats$nfcasts
spec_tramoseats_ref$seats$nfcasts
sa_ts_ref$estimation_spec$seats$nfcasts
sa_ts_ref$result_spec$seats$nfcasts

spec_ts_d$seats$nbcasts
sa_ts_d$estimation_spec$seats$nbcasts
sa_ts_d$result_spec$seats$nbcasts
spec_tramoseats_ref$seats$nbcasts
sa_ts_ref$estimation_spec$seats$nbcasts
sa_ts_ref$result_spec$seats$nbcasts


spec_ts_d$seats$algorithm
sa_ts_d$estimation_spec$seats$algorithm
sa_ts_d$result_spec$seats$algorithm
spec_tramoseats_ref$seats$algorithm
sa_ts_ref$estimation_spec$seats$algorithm
sa_ts_ref$result_spec$seats$algorithm

spec_ts_d$seats$bias
sa_ts_d$estimation_spec$seats$bias
sa_ts_d$result_spec$seats$bias
spec_tramoseats_ref$seats$bias
sa_ts_ref$estimation_spec$seats$bias
sa_ts_ref$result_spec$seats$bias

#
### benchmarking

spec_ts_d$benchmarking$enabled
sa_ts_d$estimation_spec$benchmarking$enabled
sa_ts_d$result_spec$benchmarking$enabled
spec_tramoseats_ref$benchmarking$enabled
sa_ts_ref$estimation_spec$benchmarking$enabled
sa_ts_ref$result_spec$benchmarking$enabled

spec_ts_d$benchmarking$target
sa_ts_d$estimation_spec$benchmarking$target
sa_ts_d$result_spec$benchmarking$target
spec_tramoseats_ref$benchmarking$target
sa_ts_ref$estimation_spec$benchmarking$target
sa_ts_ref$result_spec$benchmarking$target

spec_ts_d$benchmarking$lambda
sa_ts_d$estimation_spec$benchmarking$lambda
sa_ts_d$result_spec$benchmarking$lambda
spec_tramoseats_ref$benchmarking$lambda
sa_ts_ref$estimation_spec$benchmarking$lambda
sa_ts_ref$result_spec$benchmarking$lambda

spec_ts_d$benchmarking$bias
sa_ts_d$estimation_spec$benchmarking$bias
sa_ts_d$result_spec$benchmarking$bias
spec_tramoseats_ref$benchmarking$bias
sa_ts_ref$estimation_spec$benchmarking$bias
sa_ts_ref$result_spec$benchmarking$bias

spec_ts_d$benchmarking$forecast
sa_ts_d$estimation_spec$benchmarking$forecast
sa_ts_d$result_spec$benchmarking$forecast
spec_tramoseats_ref$benchmarking$forecast
sa_ts_ref$estimation_spec$benchmarking$forecast
sa_ts_ref$result_spec$benchmarking$forecast
#
