ts_adjust(s, method = c("LeapYear", "LengthOfPeriod"), reverse = FALSE)

y <- ABS$X0.2.09.10.M
yc_lp <- ts_adjust(y)
m<-cbind(y,yc_lp)

# with reverse we can find the
all.equal(ts_adjust(ts_adjust(y), reverse = TRUE), y)


y <- ABS$X0.2.09.10.M
yc_lp <- ts_adjust(y)
m<-cbind(y,yc_lp)
m

# multplicative adj = pas de regression ? bof : JAVA ?
# r= nbe moyen de j/ nbe de j
### TP 

# ex 
y <- ABS$X0.2.09.10.M
yc_lp <- ts_adjust(y, method="LengthOfPeriod")
yc_lp28<-yc_lp*30.4373/28
yc_lp29<-yc_lp*30.4373/29
yc_lp30<-yc_lp*30.4373/30
yc_lp31<-yc_lp*30.4373/31
m<-cbind(y,yc_lp,yc_lp28,yc_lp29,yc_lp30,yc_lp31)
m


## allocation tests 