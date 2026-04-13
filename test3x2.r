# from Brian Monsell

library("rjd3toolkit")
library("rjd3x13")

y <- rjd3toolkit::ABS$X0.2.09.10.M
x13_fast(y, "rsa3")
x13(y, "rsa5c")
regarima_fast(y, "rg0")
regarima(y, "rg3")

sp <- x13_spec("rsa5c")
sp <- add_outlier(sp,
    type = c("AO"), c("2015-01-01", "2010-01-01")
)
sp <- set_transform(
    set_tradingdays(
        set_easter(sp, enabled = FALSE),
        option = "workingdays"
    ),
    fun = "None"
)

# 3x2 filter indication first shows up here.
x13(y, spec = sp)
sp <- set_x11(sp,
    henderson.filter = 13
)
# 3x2 filter indication also shows up here.
x13_fast(y, spec = sp)
