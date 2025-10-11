

t <- seq(from=0, to=80)



get_dists <- function(sh, sc){ 
  f <- dweibull(x = t, shape=sh, scale = sc)
  F <- pweibull(q = t, shape=sh, scale = sc, lower.tail=T)
  S <- pweibull(q = t, shape=sh, scale = sc, lower.tail=F)
  h <- f/S
  return(list("f"=f, "F"=F,"S"=S,"h"=h))
  }


d1 <- get_dists(sh = 1.5, sc = 1/0.05)
d2 <- get_dists(sh = 1.5, sc = 1/0.10)


par(mfrow = c(2,3))
plot(x = t, y = d1$f, type="l", col="blue", ylim = c(0,max(d1$f,d2$f)))
lines(x = t, y = d2$f, col="green", type="l")

plot(x = t, y = d1$S, col="blue", type="l", ylim = c(0,max(d1$S,d2$S)))
lines(x = t, y = d2$S, col="green", type="l")

plot(x = t, y = d1$h, type="l", col="blue", ylim = c(0,max(d1$h,d2$h)))
lines(x = t, y = d2$h, col="green", type="l")


d1$h / d2$h




















library(survival)

tt <- c(7,6,6,5,2,4)
cens <- c(0,1,0,0,1,1)
Surv(tt, cens)

'
# type : an older argument that combined stype and ctype, now deprecated. 
# "kaplan-meier"        which is equivalent to stype=1, ctype=1, 
# "fleming-harrington"  which is equivalent to stype=2, ctype=1, 
# "fh2"                 which is equivalent to stype=2, ctype=2.
' 

# estimation 
result.km <- survfit(Surv(tt, cens) ~ 1, conf.type="log-log", type="kaplan-meier")
summary(result.km )

result.fh <- survfit(Surv(tt, cens) ~ 1, conf.type="log-log", type="fleming-harrington")
summary(result.fh)




