# Install the package
install.packages("WheresCroc_1.2.2.tar.gz", repos = NULL, type="source")

# Load the library
library("WheresCroc")

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
    print(sum(p))
    p[i] = p[i] / sum # normalize
  }
  return(p) # highest prob pos for croc (Vector)
}

transitionMatrix <- function() {
  pass
}
bfs <- function(goal, ourPos, edges) {
  
}
# Which waterhole to search in
hiddenMarkov <- function(prevProbPos, readings, positions, edges, probs){
  # TODO: 
  # 1. Trans matrix = transitionMatrix()
  emissions = emissionsVector(readings, probs)
  newProbPos = prevProbPos*transitionMatric()*emissions
  return (which.max(newProbPos))
  
}

myFunction <- function(moveInfo, readings, positions, edges, probs){
  
  probPos <- hiddenMarkov(readings, positions, edges, probs)
  goal =
  # 2. Search positions - if 2 - take 2 steps - if 1 take step -> search
  print(probPos)
  options=getOptions(positions[3],edges)
  
  moveInfo$moves=c(options[1],0)
  return(moveInfo)
}





getOptions=function(point,edges) {
  c(edges[which(edges[,1]==point),2],edges[which(edges[,2]==point),1],point)
}
runWheresCroc(myFunction, doPlot = T, showCroc = T, pause = 1,
              verbose = T, returnMem = F, mem = NA)
