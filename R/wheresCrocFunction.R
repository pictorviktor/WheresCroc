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
    #print(sum(p))
    p[i] = p[i] / sum # normalize
  }
  return(p) # highest prob pos for croc (Vector)
}

transitionMatrix <- function() {
  pass
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
hiddenMarkov <- function(prevProbPos, readings, positions, edges, probs){
  # TODO: 
  # 1. Trans matrix = transitionMatrix()
  emissions = emissionsVector(readings, probs)
  #newProbPos = prevProbPos*transitionMatrix()*emissions
  newProbPos = prevProbPos*emissions
  return (which.max(newProbPos))
  
}

myFunction <- function(moveInfo, readings, positions, edges, probs){
  prevProbPos = 40
  probPos <- hiddenMarkov(prevProbPos,readings, positions, edges, probs)
  goal = #We need to find a a goal node, based on probPos
  moves <- bfs(goal, positions[3],edges)
  moveInfo$moves = moves
  print(moves)
  return(moveInfo)
}



runWheresCroc(myFunction, doPlot = T, showCroc = T, pause = 1,
              verbose = T, returnMem = F, mem = NA)
