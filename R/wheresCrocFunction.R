# Install the package
install.packages("WheresCroc_1.2.2.tar.gz", repos = NULL, type="source")

# Load the library
library("WheresCroc")

getOptions=function(point,edges) {
  c(edges[which(edges[,1]==point),2],edges[which(edges[,2]==point),1],point)
}

emissionsVector <- function(readings, probs){
  salinity = dnorm(readings[1], probs[["salinity"]][, 1], probs[["salinity"]][, 2], FALSE)
  phosphate = dnorm(readings[2], probs[["phosphate"]][, 1], probs[["phosphate"]][, 2], FALSE)
  nitrogen = dnorm(readings[3], probs[["nitrogen"]][, 1], probs[["nitrogen"]][, 2], FALSE)
  
  p = replicate(40, 0)
  for (i in 1:40) {
    p[i] = salinity[i] * phosphate[i] * nitrogen[i]
  }
  sum = sum(p)
  for (i in 1:40) {
    p[i] = p[i] / sum # normalize
  }
  return(p) # prob of each pos for croc given readings (Vector)
}

transitionMatrix <- function() {
  return(matrix(1:1600, nrow = 40))
}

# Breath-first serach to find a path to porbPos
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
        if (length(node$path) == 1){return(c(node$path,0))}
        else{return(c(node$path[1],node$path[2]))}
      }
      else{
        queue = queue[-1]
        neighbors = getOptions(node$pos, edges)
        for (n in neighbors){
          if (!(n %in% visited)) {
            newNode = list(pos = n, path = c(node$path, node$pos))
            queue = append(queue, list(newNode))
            visited = c(visited, c(n))
          }
        }
      }
    }
  }
}

# Which waterhole to search in
hiddenMarkov <- function(prevProb, readings, positions, edges, probs){
  # TODO: 
  transMatrix = transitionMatrix()
  #print(transMatrix)
  emissions = t(emissionsVector(readings, probs))
  print(emissions)
  #newProb = prevProb%*%transitionMatrix%*%emissions
  
  newProb = transitionMatrix%*%emissionsT
  print(newProb)
  return (newProb)
  
}

myFunction <- function(moveInfo, readings, positions, edges, probs){
  #prevProb <- moveInfo$mem$prevProb
  prevProb= 1:40
  newProb <- hiddenMarkov(prevProb,readings, positions, edges, probs)
  goal = 40
  
  #goal <- which.max(newProb)
  moves <- bfs(goal, positions[3],edges)
  
  moveInfo$moves = moves
  moveInfo$mem$prevProb <- newProb
  return(moveInfo)
}



runWheresCroc(myFunction, doPlot = T, showCroc = T, pause = 1,
              verbose = T, returnMem = F, mem = NA)

