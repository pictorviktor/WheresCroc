# File:         wheresCrocFunction.r 
# Description:  Implemented-solution for Assignment 2, 
#               in the Artificial Intelligence course 2023, UU.

# Authors:      Niclas Björkqvist, Viktor Evestam and Felix Thulinsson


# Taken from wheresCroc.r
getOptions=function(point,edges) {
  c(edges[which(edges[,1]==point),2],edges[which(edges[,2]==point),1],point)
}

# Create an emissions matrix based on the readings
emissionsVector <- function(readings, probs){
  salinity = dnorm(readings[1], probs[["salinity"]][, 1], probs[["salinity"]][, 2], FALSE)
  phosphate = dnorm(readings[2], probs[["phosphate"]][, 1], probs[["phosphate"]][, 2], FALSE)
  nitrogen = dnorm(readings[3], probs[["nitrogen"]][, 1], probs[["nitrogen"]][, 2], FALSE)
  
  prob = replicate(40, 0)
  for (i in 1:40) {
    prob[i] = salinity[i] * phosphate[i] * nitrogen[i]
  }
  sum = sum(prob)
  for (i in 1:40) {
    prob[i] = prob[i] / sum # normalize
  }
  return(prob) # prob of each pos for croc given readings (Vector)
}

#Create a transition matrix. Only done once at the start of a new game
transitionMatrix <- function(edges) {
  matrix = matrix(0, nrow = 40, ncol = 40)
  transitionMatrix = matrix(matrix, nrow = 40)
  for (i in 1:40){
    neighbors = getOptions(i, edges)
    for (n in neighbors){
      transitionMatrix[i,n] = 1/length(neighbors)
    }
  }
  return(transitionMatrix)
}

# Breath-first search to find a path to goal node
bfs <- function(goal, ourPos, edges) {
  if (ourPos == goal){
    return(c(0,0))
  }
  else {
    queue = list(list(pos = ourPos, path = c()))
    visited = c(ourPos)
    
    while (length(queue) != 0){
      node = queue[[1]]
      if (node$pos == goal){
        if (length(node$path) <= 2){return(c(node$path[1],0))}
        else{return(c(node$path[1],node$path[2]))}
      }
      else{
        queue = queue[-1]
        neighbors = getOptions(node$pos, edges)
        for (n in neighbors){
          if (!(n %in% visited)) {
            newNode = list(pos = n, path = c(node$path, n))
            queue = append(queue, list(newNode))
            visited = c(visited, c(n))
          }
        }
      }
    }
  }
}

# Hidden markov to find which waterhole to search in
hiddenMarkov <- function(transitionMatrix, prevProb, readings, positions, edges, probs){
  emissions = emissionsVector(readings, probs)
  newProb = prevProb%*%transitionMatrix
  markovProb = newProb*emissions
  return (markovProb)
}


myFunction <- function(moveInfo, readings, positions, edges, probs){
  # Set up a new game
  if (moveInfo$mem$status == 0 || moveInfo$mem$status == 1) {
    moveInfo$mem$prevProb <- replicate(40,1)
    moveInfo$mem$transitionMatrix <- transitionMatrix(edges)
  }
  
  # Find the probability of each node through HMM
  transitionMatrix <- moveInfo$mem$transitionMatrix
  prevProb <- moveInfo$mem$prevProb
  newProb <- hiddenMarkov(transitionMatrix, prevProb,readings, positions, edges, probs)
  
  
  # Check for hikers
  if (!is.na(positions[1])){
    if (positions[1]<0){
      newProb[-1*positions[1]] = 1
    }
    else{
      newProb[positions[1]] = 0
    }
  }
  if (!is.na(positions[2])){
    if (positions[2]<0){
      newProb[-1*positions[2]] = 1
    }
    else{
      newProb[positions[2]] = 0
    }
  }
  
  # Find highest probable node, and find path to that node
  goal <- which.max(newProb)
  moves <- bfs(goal, positions[3],edges)
  moveInfo$moves <- moves
  
  moveInfo$mem$prevProb <- newProb
  moveInfo$mem$status <- 2
  return(moveInfo)
}
