# simple learning relationship
# learning rate from 0-1

alphaA <- 1 # salience of stimuli

beta <- .2 # learning rate

lambda <- 1 # maximum associative strength

VA <- array(0, c(15)) # initializing array of associations
VA[1] <- 0

delta_VA <- array(0, c(15)) # initializing array of learning
delta_VA[1] <- 0

for (t in 2:15) {
  delta_VA[t] <- alphaA * beta * (lambda - VA[t-1]) # Rescorla-Wagner simulation on 15 trials
  VA[t] <- VA[t-1] + delta_VA[t]
}


# seeing if it works as intended
plot(VA)
