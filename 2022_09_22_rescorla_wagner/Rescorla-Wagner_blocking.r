# Blocking
# learning rate from 0-1

alphaA <- 1 # salience of stimuli A
alphaX <- 1 # salience of stimuli X 

beta <- .2 # learning rate

lambda <- 1 # maximum associative strength

VA <- array(0, c(15)) # initializing array of associations between A and stimuli
VA[1] <- 0

VX <- array(0, c(15)) # initializing array of associations between X and stimuli
VX[1] <- 0

VAX <- array(0, c(15)) # initializing compound learning
VAX[1] <- 0

delta_VA <- array(0, c(15)) # initializing array of learning for A
delta_VA[1] <- 0

delta_VX <- array(0, c(15)) # initializing array of learning for X
delta_VX[1] <- 0


# simple model for stimulus A - notice for loop and commenting out
for (t in 2:7) {
  delta_VA[t] <- alphaA * beta * (lambda - VAX[t-1]) # association with A
  VA[t] <- VA[t-1] + delta_VA[t]
  # delta_VX[t] <- alphaX * beta * (lambda - VAX[t-1]) # association with X
  # VX[t] <- VX[t-1] + delta_VX[t]
  VAX[t] <- VA[t] + VX[t]
  
}


# compound model for A and X
for (t in 8:15) {
  delta_VA[t] <- alphaA * beta * (lambda - VAX[t-1]) # association with A
  VA[t] <- VA[t-1] + delta_VA[t]
  delta_VX[t] <- alphaX * beta * (lambda - VAX[t-1]) # association with X
  VX[t] <- VX[t-1] + delta_VX[t]
  VAX[t] <- VA[t] + VX[t]
  
}

# seeing if it works as intended
par(mfrow = c(1, 2))
plot(VA, ylim = c(0, 1))
plot(VX, ylim = c(0, 1))

