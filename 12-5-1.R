#data in run order
Tg<-c(327.45,319.63,320.91,337.65,351.91,336.12,343.01,330.84,321.5,345.18,336.95,346.19,325.04,333.48,
      361.67,331.59,331.12,305.83,354.58,310.64,331.92,332.33,340.63,322.09)
Tx<-c(392.67,366.38,373.45,394.44,413.98,392.13,417.01,383.21,372.08,396.21,400.15,406.16,378.66,
      383.07,418,389.37,376.83,370.83,409.60,379.70,401.84,391.19,404.76,371.82)
A<-c(-1,1,-1,1,1,-1,-1,1,-1,1,1,-1,-1,1,1,1,1,-1,1,-1,-1,1,-1,-1)
B<-c(-1,-1,1,-1,1,1,-1,-1,1,1,-1,1,1,1,1,-1,1,-1,1,-1,-1,-1,1,-1)
C<-c(1,-1,-1,1,1,1,1,-1,-1,-1,1,1,-1,-1,1,-1,-1,-1,1,-1,1,1,1,-1)
D<-c(-1,-1,-1,-1,1,-1,1,1,1,1,1,-1,1,-1,1,1,-1,-1,-1,1,1,-1,1,-1)
mod.Tg<-lm(Tg~A*B*C*D) 
mod.Tx<-lm(Tx~A*B*C*D)
#calculation of main effects and standard error for Tg by repeated runs
summary(mod.Tg)$coefficient*2  
#calculation of main effects and standard errorfor Tx by repeated runs
summary(mod.Tx)$coefficient*2  
####################################################################
#take average of repeated runs
Tg2<-c(313.96,319.63,320.91,332.3,327.45,334.99,341.155,
       354.58,310.64,331.215,323.27,345.18,337.465,336.95,340.63,356.79)
Tx2<-c(371.325,366.38,373.45,379.95,392.67,392.815,399.145,
       409.6,379.7,386.29,375.37,396.21,409.425,400.15,404.76,415.99)
A2=rep(c(-1,1),8)
B2=rep(c(-1,-1,1,1),4)
C2=rep(c(-1,-1,-1,-1,1,1,1,1),2)
D2=c(rep(-1,8),rep(1,8))
mod.Tg2=lm(Tg2~A2*B2*C2*D2) 
mod.Tx2=lm(Tx2~A2*B2*C2*D2)
library(FrF2)
par(mfrow = c(1,2))
DanielPlot(mod.Tg2,autolab = F)
DanielPlot(mod.Tx2,autolab = F)
####################################################################
mod.Tg3=lm(Tg2~A2*B2+A2*C2+A2*D2+B2*C2+B2*D2+C2*D2)
mod.Tx3=lm(Tx2~A2*B2+A2*C2+A2*D2+B2*C2+B2*D2+C2*D2)
#calculation of main effects and standard error for Tg by neglecting high order interactions
summary(mod.Tg3)$coefficient*2  
#calculation of main effects and standard error for Tx by neglecting high order interactions
summary(mod.Tx3)$coefficient*2  
####################################################################
#####check assumptions
par(mfrow = c(1,4))
plot(residuals(mod.Tg3)~fitted(mod.Tg3))
qqnorm(residuals(mod.Tg3))
plot(residuals(mod.Tg3), type = 'o')
acf(residuals(mod.Tg3))
plot(residuals(mod.Tx3)~fitted(mod.Tx3))
qqnorm(residuals(mod.Tx3))
plot(residuals(mod.Tx3), type = 'o')
acf(residuals(mod.Tx3))
par(mfrow = c(1,1))

