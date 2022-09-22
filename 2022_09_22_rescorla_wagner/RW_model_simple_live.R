### Simple RW simulation 

# simple learning relationship

# rates - must be from 0-1
alphaA <- 1
beta <- .2
lambda <- 1

VA <- array(NA,c(15)) # array of 15 NA's # array(NA,c(15)) giver det samme
VA[1] <- 0

delta_VA <- array(NA, c(15))
delta_VA[1] <- 0



for (t in 2:15){ # starts with 2, because there's no learning on the first trial

  delta_VA[t] <- alphaA * beta * (lambda - VA[t-1])
  VA[t] <- VA[t-1] + delta_VA[t]
}

plot(VA)

# lambda = the maximum associative strength (surprise)
# VA [t-1] associative strength on the last trial 
# alphaA = strength of the CS of stim A
# beta = learning rate 