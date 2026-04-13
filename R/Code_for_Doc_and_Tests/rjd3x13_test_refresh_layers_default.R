##### List of scripts on refresh

#### Focusing on spec writing (not yet on reestimation with refreshed spec)
# Part 1 default spec,  no user def params
## start rsa5c : test policies which work

## ISUES (cf Notes....txt as well)
# rsa 4c et pas 4 normally

#####################
library("rjd3toolkit")
library("rjd3x13")
# Data  :
ipi <- read.csv2(
    "C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Data/data/IPI_nace4.csv"
)
ipi$date <- as.Date(ipi$date, format = "%d/%m/%Y")
ipi[, -1] <- sapply(ipi[, -1], as.numeric)
# creating a TS object from a data frame
y_raw <- ts(
    ipi[, "RF0811"],
    frequency = 12,
    start = c(1990, 1),
    end = c(2022, 1)
)
y_new <- ts(
    ipi[, "RF0811"],
    frequency = 12,
    start = c(1990, 1),
    end = c(2022, 9)
)

## make refresh period long too see re-estimations

################# code for refresh LAYERS

## Layer 1: spec (1 default 2 customized)
##
# y_raw<-rjd3toolkit::ABS$X0.2.08.10.M
# x13_spec(name = c("rsa4", "rsa0", "rsa1", "rsa2c", "rsa3", "rsa5c"))
x13_spec_d <- rjd3x13::x13_spec("rsa5c") #### HERE PB !!! issue : rsa4 et pas rsa4c

## Layer 2: estimation spec
sa_x13_d <- rjd3x13::x13(y_raw, "Rsa5c")



# V2 could be this : sa_x13_d <- rjd3x13::x13(y_raw, "rsa")
sa_x13_d$estimation_spec
## Layer 3: result spec
sa_x13_d$result_spec
## Layer 4: refreshed spec : policy: 1 policy per file ?
current_result_spec <- sa_x13_d$result_spec
current_domain_spec <- sa_x13_d$estimation_spec
x13_spec_ref <- x13_refresh(
    current_result_spec, # point spec to be refreshed
    current_domain_spec, # domain spec (set of constraints)
    policy = "FixedParameters"
)

x13_spec_ref <- x13_refresh(
    current_result_spec, # point spec to be refreshed
    current_domain_spec, # domain spec (set of constraints)
    policy = "Current"
)


# x13_spec_ref <- x13_refresh(current_domain_spec, # point spec to be refreshed
#                             current_result_spec, #domain spec (set of constraints)
#                             policy = "FixedParameters")

# policy = c("FreeParameters",
# "Complete", "Outliers_StochasticComponent",
# "Outliers",
#            "FixedParameters", "FixedAutoRegressiveParameters", "Fixed")
####
# "FixedAutoRegressiveParameters" : works with x13

## Layer 5: estimation with spec from refresh
sa_x13_ref <- x13(y_new, x13_spec_ref)
# sa_x13_ref$estimation_spec
## layer 6: result spec from
# sa_x13_ref$result_spec
## Layer 1 ESTIMATION // RESULT // SPEC
### REGARIMA part (first part)
########## NOT useful for refresh (might have to check that stays the same ?)
## comment 1 = what should be touched by which policy and what not
## comment 2: understanding of all param purpose (DETAIL HOLES ONLY)
## .... + how to change them here, in v2 and in GUI
## basic
#### if refreshed: always untouched
# x13_spec_d$regarima$basic$span$type
# sa_x13_d$estimation_spec$regarima$basic$span$type
# sa_x13_d$result_spec$regarima$basic$span$type
# x13_spec_ref$regarima$basic$span$type
# sa_x13_ref$estimation_spec$regarima$basic$span$type
# sa_x13_ref$result_spec$regarima$basic$span$type
#
# x13_spec_d$regarima$basic$span$d0
# sa_x13_d$estimation_spec$regarima$basic$span$d0
# sa_x13_d$result_spec$regarima$basic$span$d0
# x13_spec_ref$regarima$basic$span$d0
# sa_x13_ref$estimation_spec$regarima$basic$span$d0
# sa_x13_ref$result_spec$regarima$basic$span$d0
#
# x13_spec_d$regarima$basic$span$d1
# sa_x13_d$estimation_spec$regarima$basic$span$d1
# sa_x13_d$result_spec$regarima$basic$span$d1
# x13_spec_ref$regarima$basic$span$d1
# sa_x13_ref$estimation_spec$regarima$basic$span$d1
# sa_x13_ref$result_spec$regarima$basic$span$d1
#
# x13_spec_d$regarima$basic$span$n0
# sa_x13_d$estimation_spec$regarima$basic$span$n0
# sa_x13_d$result_spec$regarima$basic$span$n0
# x13_spec_ref$regarima$basic$span$n0
# sa_x13_ref$estimation_spec$regarima$basic$span$n0
# sa_x13_ref$result_spec$regarima$basic$span$n0
#
# x13_spec_d$regarima$basic$span$n1
# sa_x13_d$estimation_spec$regarima$basic$span$n1
# sa_x13_d$result_spec$regarima$basic$span$n1
# x13_spec_ref$regarima$basic$span$n1
# sa_x13_ref$estimation_spec$regarima$basic$span$n1
# sa_x13_ref$result_spec$regarima$basic$span$n1
#
# x13_spec_d$regarima$basic$preprocessing
# sa_x13_d$estimation_spec$regarima$basic$preprocessing
# sa_x13_d$result_spec$regarima$basic$preprocessing
# x13_spec_ref$regarima$basic$preprocessing
# sa_x13_ref$estimation_spec$regarima$basic$preprocessing
# sa_x13_ref$result_spec$regarima$basic$preprocessing
#
# x13_spec_d$regarima$basic$preliminaryCheck
# sa_x13_d$estimation_spec$regarima$basic$preliminaryCheck
# sa_x13_d$result_spec$regarima$basic$preliminaryCheck
# x13_spec_ref$regarima$basic$preliminaryCheck
# sa_x13_ref$estimation_spec$regarima$basic$preliminaryCheck
# sa_x13_ref$result_spec$regarima$basic$preliminaryCheck
#
# ## transform
# ## refresh : is transported, never touched (exception = complete)
# ### PB with refresh seems to reestimate schema
# x13_spec_d$regarima$transform$fn
# sa_x13_d$estimation_spec$regarima$transform$fn
# sa_x13_d$result_spec$regarima$transform$fn
# x13_spec_ref$regarima$transform$fn
# sa_x13_ref$estimation_spec$regarima$transform$fn
# sa_x13_ref$result_spec$regarima$transform$fn
#
# ## here pb de adjust and leap year : not clear
# x13_spec_d$regarima$transform$adjust
# sa_x13_d$estimation_spec$regarima$transform$adjust
# sa_x13_d$result_spec$regarima$transform$adjust
# x13_spec_ref$regarima$transform$adjust
# sa_x13_ref$estimation_spec$regarima$transform$adjust
# sa_x13_ref$result_spec$regarima$transform$adjust
#
# x13_spec_d$regarima$transform$aicdiff
# sa_x13_d$estimation_spec$regarima$transform$aicdiff
# sa_x13_d$result_spec$regarima$transform$aicdiff
# x13_spec_ref$regarima$transform$aicdiff
# sa_x13_ref$estimation_spec$regarima$transform$aicdiff
# sa_x13_ref$result_spec$regarima$transform$aicdiff
#
# ## outlier (auto detection params)
# x13_spec_d$regarima$outlier$outliers
# sa_x13_d$estimation_spec$regarima$outlier$outliers
# sa_x13_d$result_spec$regarima$outlier$outliers
# x13_spec_ref$regarima$outlier$outliers
# sa_x13_ref$estimation_spec$regarima$outlier$outliers
# sa_x13_ref$result_spec$regarima$outlier$outliers
#
# x13_spec_d$regarima$outlier$span$type
# sa_x13_d$estimation_spec$regarima$outlier$span$type
# sa_x13_d$result_spec$regarima$outlier$span$type
# x13_spec_ref$regarima$outlier$span$type
# sa_x13_ref$estimation_spec$regarima$outlierspan$type
# sa_x13_ref$result_spec$regarima$outlier$span$type
#
#
# x13_spec_d$regarima$outlier$span$d0
# sa_x13_d$estimation_spec$regarima$outlier$span$d0
# sa_x13_d$result_spec$regarima$outlier$span$d0
# x13_spec_ref$regarima$outlier$span$d0
# sa_x13_ref$estimation_spec$regarima$outlierspan$d0
# sa_x13_ref$result_spec$regarima$outlier$span$d0
#
# x13_spec_d$regarima$outlier$span$d1
# sa_x13_d$estimation_spec$regarima$outlier$span$d1
# sa_x13_d$result_spec$regarima$outlier$span$d1
# x13_spec_ref$regarima$outlier$span$d1
# sa_x13_ref$estimation_spec$regarima$outlierspan$d1
# sa_x13_ref$result_spec$regarima$outlier$span$d1
#
#
# x13_spec_d$regarima$outlier$span$n0
# sa_x13_d$estimation_spec$regarima$outlier$span$n0
# sa_x13_d$result_spec$regarima$outlier$span$n0
# x13_spec_ref$regarima$outlier$span$n0
# sa_x13_ref$estimation_spec$regarima$outlierspan$n0
# sa_x13_ref$result_spec$regarima$outlier$span$n0
#
# x13_spec_d$regarima$outlier$span$n1
# sa_x13_d$estimation_spec$regarima$outlier$span$n1
# sa_x13_d$result_spec$regarima$outlier$span$n1
# x13_spec_ref$regarima$outlier$span$n1
# sa_x13_ref$estimation_spec$regarima$outlierspan$n1
# sa_x13_ref$result_spec$regarima$outlier$span$n1
#
# x13_spec_d$regarima$outlier$span$defva
# sa_x13_d$estimation_spec$regarima$outlier$span$defva
# sa_x13_d$result_spec$regarima$outlier$span$defva
# x13_spec_ref$regarima$outlier$span$defva
# sa_x13_ref$estimation_spec$regarima$outlierspan$defva
# sa_x13_ref$result_spec$regarima$outlier$span$defva
#
# x13_spec_d$regarima$outlier$span$defva
# sa_x13_d$estimation_spec$regarima$outlier$span$defva
# sa_x13_d$result_spec$regarima$outlier$span$defva
# x13_spec_ref$regarima$outlier$span$defva
# sa_x13_ref$estimation_spec$regarima$outlierspan$defva
# sa_x13_ref$result_spec$regarima$outlier$span$defva
#
# x13_spec_d$regarima$outlier$span$method
# sa_x13_d$estimation_spec$regarima$outlier$span$method
# sa_x13_d$result_spec$regarima$outlier$span$method
# x13_spec_ref$regarima$outlier$span$method
# sa_x13_ref$estimation_spec$regarima$outlierspan$method
# sa_x13_ref$result_spec$regarima$outlier$span$method
#
# # default value doesn't appear ?
# x13_spec_d$regarima$outlier$span$monthlytcrate
# sa_x13_d$estimation_spec$regarima$outlier$span$monthlytcrate
# sa_x13_d$result_spec$regarima$outlier$span$monthlytcrate
# x13_spec_ref$regarima$outlier$span$monthlytcrate
# sa_x13_ref$estimation_spec$regarima$outlierspan$monthlytcrate
# sa_x13_ref$result_spec$regarima$outlier$span$monthlytcrate
#
# x13_spec_d$regarima$outlier$span$maxiter
# sa_x13_d$estimation_spec$regarima$outlier$span$maxiter
# sa_x13_d$result_spec$regarima$outlier$span$maxiter
# x13_spec_ref$regarima$outlier$span$maxiter
# sa_x13_ref$estimation_spec$regarima$outlierspan$maxiter
# sa_x13_ref$result_spec$regarima$outlier$span$maxiter
#
# x13_spec_d$regarima$outlier$span$lsrun
# sa_x13_d$estimation_spec$regarima$outlier$span$lsrun
# sa_x13_d$result_spec$regarima$outlier$span$lsrun
# x13_spec_ref$regarima$outlier$span$lsrun
# sa_x13_ref$estimation_spec$regarima$outlierspan$lsrun
# sa_x13_ref$result_spec$regarima$outlier$span$lsrun

## arima
x13_spec_d$regarima$arima$period
sa_x13_d$estimation_spec$regarima$arima$period
sa_x13_d$result_spec$regarima$arima$period
x13_spec_ref$regarima$arima$period
sa_x13_ref$estimation_spec$regarima$arima$period
sa_x13_ref$result_spec$regarima$arima$period


x13_spec_d$regarima$arima$d
sa_x13_d$estimation_spec$regarima$arima$d
sa_x13_d$result_spec$regarima$arima$d
x13_spec_ref$regarima$arima$d
sa_x13_ref$estimation_spec$regarima$arima$d
sa_x13_ref$result_spec$regarima$arima$d

x13_spec_d$regarima$arima$bd
sa_x13_d$estimation_spec$regarima$arima$bd
sa_x13_d$result_spec$regarima$arima$bd
x13_spec_ref$regarima$arima$bd
sa_x13_ref$estimation_spec$regarima$arima$bd
sa_x13_ref$result_spec$regarima$arima$bd

x13_spec_d$regarima$arima$phi
sa_x13_d$estimation_spec$regarima$arima$phi
sa_x13_d$result_spec$regarima$arima$phi
x13_spec_ref$regarima$arima$phi
sa_x13_ref$estimation_spec$regarima$arima$phi
sa_x13_ref$result_spec$regarima$arima$phi

x13_spec_d$regarima$arima$theta
sa_x13_d$estimation_spec$regarima$arima$theta
sa_x13_d$result_spec$regarima$arima$theta
x13_spec_ref$regarima$arima$theta
sa_x13_ref$estimation_spec$regarima$arima$theta
sa_x13_ref$result_spec$regarima$arima$theta

x13_spec_d$regarima$arima$bphi
sa_x13_d$estimation_spec$regarima$arima$bphi
sa_x13_d$result_spec$regarima$arima$bphi
x13_spec_ref$regarima$arima$bphi
sa_x13_ref$estimation_spec$regarima$arima$bphi
sa_x13_ref$result_spec$regarima$arima$bphi

x13_spec_d$regarima$arima$btheta
sa_x13_d$estimation_spec$regarima$arima$btheta
sa_x13_d$result_spec$regarima$arima$btheta
x13_spec_ref$regarima$arima$btheta
sa_x13_ref$estimation_spec$regarima$arima$btheta
sa_x13_ref$result_spec$regarima$arima$btheta

# ## automodel
# x13_spec_d$regarima$automodel$enabled
# sa_x13_d$estimation_spec$regarima$automodel$enabled
# sa_x13_d$result_spec$regarima$automodel$enabled
# x13_spec_ref$regarima$automodel$enabled
# sa_x13_ref$estimation_spec$regarima$automodel$enabled
# sa_x13_ref$result_spec$regarima$automodel$enabled
#
# x13_spec_d$regarima$automodel$ljungbox
# sa_x13_d$estimation_spec$regarima$automodel$ljungbox
# sa_x13_d$result_spec$regarima$automodel$ljungbox
# x13_spec_ref$regarima$automodel$ljungbox
# sa_x13_ref$estimation_spec$regarima$automodel$ljungbox
# sa_x13_ref$result_spec$regarima$automodel$ljungbox
#
# x13_spec_d$regarima$automodel$tsig
# sa_x13_d$estimation_spec$regarima$automodel$tsig
# sa_x13_d$result_spec$regarima$automodel$tsig
# x13_spec_ref$regarima$automodel$tsig
# sa_x13_ref$estimation_spec$regarima$automodel$tsig
# sa_x13_ref$result_spec$regarima$automodel$tsig
#
# x13_spec_d$regarima$automodel$predcv
# sa_x13_d$estimation_spec$regarima$automodel$predcv
# sa_x13_d$result_spec$regarima$automodel$predcv
# x13_spec_ref$regarima$automodel$predcv
# sa_x13_ref$estimation_spec$regarima$automodel$predcv
# sa_x13_ref$result_spec$regarima$automodel$predcv
#
# x13_spec_d$regarima$automodel$ubfinal
# sa_x13_d$estimation_spec$regarima$automodel$ubfinal
# sa_x13_d$result_spec$regarima$automodel$ubfinal
# x13_spec_ref$regarima$automodel$ubfinal
# sa_x13_ref$estimation_spec$regarima$automodel$ubfinal
# sa_x13_ref$result_spec$regarima$automodel$ubfinal
#
# x13_spec_d$regarima$automodel$ub1
# sa_x13_d$estimation_spec$regarima$automodel$ub1
# sa_x13_d$result_spec$regarima$automodel$ub1
# x13_spec_ref$regarima$automodel$ub1
# sa_x13_ref$estimation_spec$regarima$automodel$ub1
# sa_x13_ref$result_spec$regarima$automodel$ub1
#
# x13_spec_d$regarima$automodel$ub2
# sa_x13_d$estimation_spec$regarima$automodel$ub2
# sa_x13_d$result_spec$regarima$automodel$ub2
# x13_spec_ref$regarima$automodel$ub2
# sa_x13_ref$estimation_spec$regarima$automodel$ub2
# sa_x13_ref$result_spec$regarima$automodel$ub2
#
#
# x13_spec_d$regarima$automodel$cancel
# sa_x13_d$estimation_spec$regarima$automodel$cancel
# sa_x13_d$result_spec$regarima$automodel$cancel
# x13_spec_ref$regarima$automodel$cancel
# sa_x13_ref$estimation_spec$regarima$automodel$cancel
# sa_x13_ref$result_spec$regarima$automodel$cancel
#
# x13_spec_d$regarima$automodel$fct
# sa_x13_d$estimation_spec$regarima$automodel$fct
# sa_x13_d$result_spec$regarima$automodel$fct
# x13_spec_ref$regarima$automodel$fct
# sa_x13_ref$estimation_spec$regarima$automodel$fct
# sa_x13_ref$result_spec$regarima$automodel$fct
#
# x13_spec_d$regarima$automodel$acceptdef
# sa_x13_d$estimation_spec$regarima$automodel$acceptdef
# sa_x13_d$result_spec$regarima$automodel$acceptdef
# x13_spec_ref$regarima$automodel$acceptdef
# sa_x13_ref$estimation_spec$regarima$automodel$acceptdef
# sa_x13_ref$result_spec$regarima$automodel$acceptdef
#
# x13_spec_d$regarima$automodel$mixed
# sa_x13_d$estimation_spec$regarima$automodel$mixed
# sa_x13_d$result_spec$regarima$automodel$mixed
# x13_spec_ref$regarima$automodel$mixed
# sa_x13_ref$estimation_spec$regarima$automodel$mixed
# sa_x13_ref$result_spec$regarima$automodel$mixed
#
#
# x13_spec_d$regarima$automodel$balanced
# sa_x13_d$estimation_spec$regarima$automodel$balanced
# sa_x13_d$result_spec$regarima$automodel$balanced
# x13_spec_ref$regarima$automodel$balanced
# sa_x13_ref$estimation_spec$regarima$automodel$balanced
# sa_x13_ref$result_spec$regarima$automodel$balanced
#
# ## regression
# x13_spec_d$regarima$regression$mean
# sa_x13_d$estimation_spec$regarima$regression$mean
# sa_x13_d$result_spec$regarima$regression$mean
# x13_spec_ref$regarima$regression$mean # nothing in spec
# sa_x13_ref$estimation_spec$regarima$regression$mean
# sa_x13_ref$result_spec$regarima$regression$mean # estimated value only here
#
# x13_spec_d$regarima$regression$check_mean
# sa_x13_d$estimation_spec$regarima$regression$check_mean
# sa_x13_d$result_spec$regarima$regression$check_mean
# x13_spec_ref$regarima$regression$check_mean
# sa_x13_ref$estimation_spec$regarima$regression$check_mean
# sa_x13_ref$result_spec$regarima$regression$check_mean
#
#
# ## regression$td
# ### what is this
# ### how to change it
# x13_spec_d$regarima$regression$td$td
# sa_x13_d$estimation_spec$regarima$regression$td$td
# sa_x13_d$result_spec$regarima$regression$td$td
# x13_spec_ref$regarima$regression$td$td
# sa_x13_ref$estimation_spec$regarima$regression$td$td
# sa_x13_ref$result_spec$regarima$regression$td$td
#
# x13_spec_d$regarima$regression$td$lp
# sa_x13_d$estimation_spec$regarima$regression$td$lp
# sa_x13_d$result_spec$regarima$regression$td$lp
# x13_spec_ref$regarima$regression$td$lp
# sa_x13_ref$estimation_spec$regarima$regression$td$lp
# sa_x13_ref$result_spec$regarima$regression$td$lp
#
# x13_spec_d$regarima$regression$td$holidays
# sa_x13_d$estimation_spec$regarima$regression$td$holidays
# sa_x13_d$result_spec$regarima$regression$td$holidays
# x13_spec_ref$regarima$regression$td$holidays
# sa_x13_ref$estimation_spec$regarima$regression$td$holidays
# sa_x13_ref$result_spec$regarima$regression$td$holidays
#
# x13_spec_d$regarima$regression$td$users
# sa_x13_d$estimation_spec$regarima$regression$td$users
# sa_x13_d$result_spec$regarima$regression$td$users
# x13_spec_ref$regarima$regression$td$users
# sa_x13_ref$estimation_spec$regarima$regression$td$users
# sa_x13_ref$result_spec$regarima$regression$td$users
#
# x13_spec_d$regarima$regression$td$w
# sa_x13_d$estimation_spec$regarima$regression$td$w
# sa_x13_d$result_spec$regarima$regression$td$w
# x13_spec_ref$regarima$regression$td$w
# sa_x13_ref$estimation_spec$regarima$regression$td$w
# sa_x13_ref$result_spec$regarima$regression$td$w
#
# x13_spec_d$regarima$regression$td$test
# sa_x13_d$estimation_spec$regarima$regression$td$test
# sa_x13_d$result_spec$regarima$regression$td$test
# x13_spec_ref$regarima$regression$td$test
# sa_x13_ref$estimation_spec$regarima$regression$td$test
# sa_x13_ref$result_spec$regarima$regression$td$test
#
# x13_spec_d$regarima$regression$td$auto
# sa_x13_d$estimation_spec$regarima$regression$td$auto
# sa_x13_d$result_spec$regarima$regression$td$auto
# x13_spec_ref$regarima$regression$td$auto
# sa_x13_ref$estimation_spec$regarima$regression$td$auto
# sa_x13_ref$result_spec$regarima$regression$td$auto
#
# x13_spec_d$regarima$regression$td$autoadjust
# sa_x13_d$estimation_spec$regarima$regression$td$autoadjust
# sa_x13_d$result_spec$regarima$regression$td$autoadjust
# x13_spec_ref$regarima$regression$td$autoadjust
# sa_x13_ref$estimation_spec$regarima$regression$td$autoadjust
# sa_x13_ref$result_spec$regarima$regression$td$autoadjust
#
# x13_spec_d$regarima$regression$td$tdcoefficients
# sa_x13_d$estimation_spec$regarima$regression$td$tdcoefficients
# sa_x13_d$result_spec$regarima$regression$td$tdcoefficients
# x13_spec_ref$regarima$regression$td$tdcoefficients
# sa_x13_ref$estimation_spec$regarima$regression$td$tdcoefficients
# sa_x13_ref$result_spec$regarima$regression$td$tdcoefficients
#
# x13_spec_d$regarima$regression$td$lpcoefficient
# sa_x13_d$estimation_spec$regarima$regression$td$lpcoefficient
# sa_x13_d$result_spec$regarima$regression$td$lpcoefficient
# x13_spec_ref$regarima$regression$td$lpcoefficient
# sa_x13_ref$estimation_spec$regarima$regression$td$lpcoefficient
# sa_x13_ref$result_spec$regarima$regression$td$lpcoefficient
#
# ## regression$easter
# x13_spec_d$regarima$regression$td$lpcoefficient
# sa_x13_d$estimation_spec$regarima$regression$td$lpcoefficient
# sa_x13_d$result_spec$regarima$regression$td$lpcoefficient
# x13_spec_ref$regarima$regression$td$lpcoefficient
# sa_x13_ref$estimation_spec$regarima$regression$td$lpcoefficient
# sa_x13_ref$result_spec$regarima$regression$td$lpcoefficient
#
# x13_spec_d$regarima$regression$easter$type
# sa_x13_d$estimation_spec$regarima$regression$easter$type
# sa_x13_d$result_spec$regarima$regression$easter$type
# x13_spec_ref$regarima$regression$easter$type
# sa_x13_ref$estimation_spec$regarima$regression$easter$type
# sa_x13_ref$result_spec$regarima$regression$easter$type
#
# x13_spec_d$regarima$regression$easter$duration
# sa_x13_d$estimation_spec$regarima$regression$easter$duration
# sa_x13_d$result_spec$regarima$regression$easter$duration
# x13_spec_ref$regarima$regression$easter$duration
# sa_x13_ref$estimation_spec$regarima$regression$easter$duration
# sa_x13_ref$result_spec$regarima$regression$easter$duration
#
# x13_spec_d$regarima$regression$easter$test
# sa_x13_d$estimation_spec$regarima$regression$easter$test
# sa_x13_d$result_spec$regarima$regression$easter$test
# x13_spec_ref$regarima$regression$easter$test
# sa_x13_ref$estimation_spec$regarima$regression$easter$test
# sa_x13_ref$result_spec$regarima$regression$easter$test
#
# x13_spec_d$regarima$regression$easter$coefficient
# sa_x13_d$estimation_spec$regarima$regression$easter$coefficient
# sa_x13_d$result_spec$regarima$regression$easter$coefficient
# x13_spec_ref$regarima$regression$easter$coefficient
# sa_x13_ref$estimation_spec$regarima$regression$easter$coefficient
# sa_x13_ref$result_spec$regarima$regression$easter$coefficient
#
# ## outliers / ramps / user def vars
# x13_spec_d$regarima$regression$outliers
# sa_x13_d$estimation_spec$regarima$regression$outliers
# sa_x13_d$result_spec$regarima$regression$outliers
# x13_spec_ref$regarima$regression$outliers
# sa_x13_ref$estimation_spec$regarima$regression$outliers
# sa_x13_ref$result_spec$regarima$regression$outliers
#
# x13_spec_d$regarima$regression$ramps
# sa_x13_d$estimation_spec$regarima$regression$ramps
# sa_x13_d$result_spec$regarima$regression$ramps
# x13_spec_ref$regarima$regression$ramps
# sa_x13_ref$estimation_spec$regarima$regression$ramps
# sa_x13_ref$result_spec$regarima$regression$ramps
#
# x13_spec_d$regarima$regression$users
# sa_x13_d$estimation_spec$regarima$regression$users
# sa_x13_d$result_spec$regarima$regression$users
# x13_spec_ref$regarima$regression$users
# sa_x13_ref$estimation_spec$regarima$regression$users
# sa_x13_ref$result_spec$regarima$regression$users
#
#
# ## estimate
# ### question = estimate vs span: c doc ? clear ?
# x13_spec_d$regarima$estimate$span$type
# sa_x13_d$estimation_spec$regarima$estimate$span$type
# sa_x13_d$result_spec$regarima$estimate$span$type
# x13_spec_ref$regarima$estimate$span$type
# sa_x13_ref$estimation_spec$regarima$estimate$span$type
# sa_x13_ref$result_spec$regarima$estimate$span$type
#
#
# x13_spec_d$regarima$estimate$span$d0
# sa_x13_d$estimation_spec$regarima$estimate$span$d0
# sa_x13_d$result_spec$regarima$estimate$span$d0
# x13_spec_ref$regarima$estimate$span$d0
# sa_x13_ref$estimation_spec$regarima$estimate$span$d0
# sa_x13_ref$result_spec$regarima$estimate$span$d0
#
#
# x13_spec_d$regarima$estimate$span$d1
# sa_x13_d$estimation_spec$regarima$estimate$span$d1
# sa_x13_d$result_spec$regarima$estimate$span$d1
# x13_spec_ref$regarima$estimate$span$d1
# sa_x13_ref$estimation_spec$regarima$estimate$span$d1
# sa_x13_ref$result_spec$regarima$estimate$span$d1
#
#
# x13_spec_d$regarima$estimate$span$n0
# sa_x13_d$estimation_spec$regarima$estimate$span$n0
# sa_x13_d$result_spec$regarima$estimate$span$n0
# x13_spec_ref$regarima$estimate$span$n0
# sa_x13_ref$estimation_spec$regarima$estimate$span$n0
# sa_x13_ref$result_spec$regarima$estimate$span$n0
#
# x13_spec_d$regarima$estimate$span$n1
# sa_x13_d$estimation_spec$regarima$estimate$span$n1
# sa_x13_d$result_spec$regarima$estimate$span$n1
# x13_spec_ref$regarima$estimate$span$n1
# sa_x13_ref$estimation_spec$regarima$estimate$span$n1
# sa_x13_ref$result_spec$regarima$estimate$span$n1
#
# x13_spec_d$regarima$estimate$tol
# sa_x13_d$estimation_spec$regarima$estimate$tol
# sa_x13_d$result_spec$regarima$estimate$tol
# x13_spec_ref$regarima$estimate$tol
# sa_x13_ref$estimation_spec$regarima$estimate$tol
# sa_x13_ref$result_spec$regarima$estimate$tol
#
# ### decomp avec X11
# x13_spec_d$x11$mode
# sa_x13_d$estimation_spec$x11$mode
# sa_x13_d$result_spec$x11$mode
# x13_spec_ref$x11$mode
# sa_x13_ref$estimation_spec$x11$mode
# sa_x13_ref$result_spec$x11$mode
#
# x13_spec_d$x11$seasonal
# sa_x13_d$estimation_spec$x11$seasonal
# sa_x13_d$result_spec$x11$seasonal
# x13_spec_ref$x11$seasonal
# sa_x13_ref$estimation_spec$x11$seasonal
# sa_x13_ref$result_spec$x11$seasonal
#
# x13_spec_d$x11$henderson
# sa_x13_d$estimation_spec$x11$henderson
# sa_x13_d$result_spec$x11$henderson
# x13_spec_ref$x11$henderson
# sa_x13_ref$estimation_spec$x11$henderson
# sa_x13_ref$result_spec$x11$henderson
#
# x13_spec_d$x11$sfilters
# sa_x13_d$estimation_spec$x11$sfilters
# sa_x13_d$result_spec$x11$sfilters
# x13_spec_ref$x11$sfilters
# sa_x13_ref$estimation_spec$x11$sfilters
# sa_x13_ref$result_spec$x11$sfilters
#
#
# x13_spec_d$x11$lsig
# sa_x13_d$estimation_spec$x11$lsig
# sa_x13_d$result_spec$x11$lsig
# x13_spec_ref$x11$lsig
# sa_x13_ref$estimation_spec$x11$lsig
# sa_x13_ref$result_spec$x11$lsig
#
# x13_spec_d$x11$usig
# sa_x13_d$estimation_spec$x11$usig
# sa_x13_d$result_spec$x11$usig
# x13_spec_ref$x11$usig
# sa_x13_ref$estimation_spec$x11$usig
# sa_x13_ref$result_spec$x11$usig
#
# x13_spec_d$x11$nfcasts
# sa_x13_d$estimation_spec$x11$nfcasts
# sa_x13_d$result_spec$x11$nfcasts
# x13_spec_ref$x11$nfcasts
# sa_x13_ref$estimation_spec$x11$nfcasts
# sa_x13_ref$result_spec$x11$nfcasts
#
# x13_spec_d$x11$nbcasts
# sa_x13_d$estimation_spec$x11$nbcasts
# sa_x13_d$result_spec$x11$nbcasts
# x13_spec_ref$x11$nbcasts
# sa_x13_ref$estimation_spec$x11$nbcasts
# sa_x13_ref$result_spec$x11$nbcasts
#
#
# x13_spec_d$x11$vsigmas
# sa_x13_d$estimation_spec$x11$vsigmas
# sa_x13_d$result_spec$x11$vsigmas
# x13_spec_ref$x11$vsigmas
# sa_x13_ref$estimation_spec$x11$vsigmas
# sa_x13_ref$result_spec$x11$vsigmas
#
# x13_spec_d$x11$excludefcasts
# sa_x13_d$estimation_spec$x11$excludefcasts
# sa_x13_d$result_spec$x11$excludefcasts
# x13_spec_ref$x11$excludefcasts
# sa_x13_ref$estimation_spec$x11$excludefcasts
# sa_x13_ref$result_spec$x11$excludefcasts
#
#
# x13_spec_d$x11$bias
# sa_x13_d$estimation_spec$x11$bias
# sa_x13_d$result_spec$x11$bias
# x13_spec_ref$x11$bias
# sa_x13_ref$estimation_spec$x11$bias
# sa_x13_ref$result_spec$x11$bias
#
#
# ### benchmarking
#
# x13_spec_d$benchmarking$enabled
# sa_x13_d$estimation_spec$benchmarking$enabled
# sa_x13_d$result_spec$benchmarking$enabled
# x13_spec_ref$benchmarking$enabled
# sa_x13_ref$estimation_spec$benchmarking$enabled
# sa_x13_ref$result_spec$benchmarking$enabled
#
# x13_spec_d$benchmarking$target
# sa_x13_d$estimation_spec$benchmarking$target
# sa_x13_d$result_spec$benchmarking$target
# x13_spec_ref$benchmarking$target
# sa_x13_ref$estimation_spec$benchmarking$target
# sa_x13_ref$result_spec$benchmarking$target
#
# x13_spec_d$benchmarking$lambda
# sa_x13_d$estimation_spec$benchmarking$lambda
# sa_x13_d$result_spec$benchmarking$lambda
# x13_spec_ref$benchmarking$lambda
# sa_x13_ref$estimation_spec$benchmarking$lambda
# sa_x13_ref$result_spec$benchmarking$lambda
#
# x13_spec_d$benchmarking$bias
# sa_x13_d$estimation_spec$benchmarking$bias
# sa_x13_d$result_spec$benchmarking$bias
# x13_spec_ref$benchmarking$bias
# sa_x13_ref$estimation_spec$benchmarking$bias
# sa_x13_ref$result_spec$benchmarking$bias
#
# x13_spec_d$benchmarking$forecast
# sa_x13_d$estimation_spec$benchmarking$forecast
# sa_x13_d$result_spec$benchmarking$forecast
# x13_spec_ref$benchmarking$forecast
# sa_x13_ref$estimation_spec$benchmarking$forecast
# sa_x13_ref$result_spec$benchmarking$forecast
#
