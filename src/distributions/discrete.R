

#-----------------------
# binomial
xx <- seq(0,10)

n = 8

p1 = 0.05
p2 = 0.50
p3 = 0.80

f_x1 = dbinom(x=xx, size=n, prob=p1, log = FALSE)
f_x2 = dbinom(x=xx, size=n, prob=p2, log = FALSE)
f_x3 = dbinom(x=xx, size=n, prob=p3, log = FALSE)

plot(xx, f_x1, ylim = c(0.0, 1.0), type = "n")

lines(xx, f_x1, type = "b", col= "green")
lines(xx, f_x2, type = "b", col= "violet")
lines(xx, f_x3, type = "b", col= "blue")

#-----------------------
# geometric

xx <- seq(0,10)

p1 = 0.05
p2 = 0.50
p3 = 0.80

f_x1 = dgeom(x = xx, prob = p1)
f_x2 = dgeom(x = xx, prob = p2)
f_x3 = dgeom(x = xx, prob = p3)

plot(xx, f_x1, ylim = c(0.0, 1.0), type = "n")

lines(xx, f_x1, type = "b", col= "green")
lines(xx, f_x2, type = "b", col= "violet")
lines(xx, f_x3, type = "b", col= "blue")


#-----------------------
# negative binomial 

xx <- seq(0,30)

p1 = 0.75

f_x1 = dnbinom(x = xx, size = 1,  prob = p1)
f_x2 = dnbinom(x = xx, size = 5,  prob = p1)
f_x3 = dnbinom(x = xx, size = 20, prob = p1)

f_x4 = dnbinom(x = xx, size = 5, mu = 10)


plot(xx, f_x1, ylim = c(0.0, 1.0), type = "n")

lines(xx, f_x1, type = "b"col= "green")
lines(xx, f_x2, type = "b", col= "violet")
lines(xx, f_x3, type = "b", col= "blue")
lines(xx, f_x4, type = "b", col= "black")



#-----------------------
# dexp() 

xx <- seq(0,5, by = 0.01)

f_x1 = dexp(x = xx, rate = 1.0)
f_x2 = dexp(x = xx, rate = 5)
f_x3 = dexp(x = xx, rate = 10)

plot(xx, f_x1, ylim = c(0.0, 10.0), type = "n")

lines(xx, f_x1, ylim = c(0.0, 1.0), type = 'l', col= "green")
lines(xx, f_x2, ylim = c(0.0, 1.0), type = 'l', col= "violet")
lines(xx, f_x3, ylim = c(0.0, 1.0), type = 'l', col= "blue")

abline(h = 0.0, col = "gray")
abline(v = 0.0, col = "gray")



? dgamma



