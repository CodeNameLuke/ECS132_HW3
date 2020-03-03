# Ecs 132 HW3 Problem D

# Helper function
probDice <- function(desiredAmount){
  sums <- c()
  
  if(desiredAmount < 0){
    
    return (0)
  }
  for(roll1 in c(1,2,3,4,5,6)){
    for(roll2 in c(1,2,3,4,5,6)){
      sums <- c(sums, roll1 + roll2)
    }
  }
  count <- 0
  for(sum in sums){
    if(sum == desiredAmount){
      count <- count + 1
    }
  }
  return (count / length(sums))
}


daccum <- function(i,k) {
  result <- 0.0 
  
  # Base Case (i == 1)
  if( i == 1) {
    for(roll in 2:12) {
      if( (k - roll) <= 0) {
        result <- result + probDice(roll)
      }
    }
  }
  else {
    for(roll in 2:12) {
      if( (k-roll) > 0 ){
        result <- result + ((probDice(roll) * (daccum(i-1, k-roll))))
      }else{
        next
      }
    }
  }
  return(result)
}

x = daccum(2,4)
print(x)

# Find P(X <= i)
paccum <- function(i,k){
  result <- 0.0
  for(x in 1:i){
    if( j == 1){
      for(roll in 2:12){
        if( (k-roll) <= 0){
          result <- result + probDice(roll)
        }
      }
    }else{
      for(roll in 2:12){
        if((k-roll) > 0 ){
          result <- result + (probDice(roll) * paccum(x - 1, k - roll))
        }
      }
    }
  }
  return (result)
  
}

# Find c s.t P(X <= c) == q
qaccum <- function(i,k){
  result <- 0
  c <- 0
  while(result <= i){
    c <- c + 1
    result <- paccum(c,k)
  }
  return (c)
}


# Generate N independent values of X
raccum <- function(nreps, k){
  result = c()
  for(i in 1:nreps){
    
    currentDots <- 0
    rolls <- 0
    while(currentDots < k){
      currentDots <- currentDots + sample(2:12, 1)
      rolls <- rolls + 1
    }
    result <- c(result, rolls)
  }
  
  return (result)
  
}

r = raccum(4,6)
print(r)
