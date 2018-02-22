
## MONTY HALL PROBLEM

door <- 1:3

n.sim <- 100000
Car <- sample(door, n.sim, rep=T)
Chose <- sample(door, n.sim, rep=T)

Open <- rep(NA,n.sim)
ChangeWin <- rep(NA, n.sim)

for (i in 1:n.sim) {
 if (Car[i] != Chose[i]) Open[i] <- door[-c(Car[i],Chose[i])]
 if (Car[i] == Chose[i]) Open[i] <- sample(door[-c(Car[i])],1)
}

for (i in 1:n.sim) {
 if (Car[i] != Chose [i]) ChangeWin[i] <- 1
 if (Car[i] == Chose [i]) ChangeWin[i] <- 0
}


cbind(Car,Chose ,Open,ChangeWin)[1:10,]




cat("\n",
    " win probability after changing:      ", round(sum(ChangeWin)/n.sim,3), "\n",
    " win probability without changeing:   ", round((n.sim-sum(ChangeWin))/n.sim,3), "\n")

