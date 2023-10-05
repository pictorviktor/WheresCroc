# Install the package
install.packages("WheresCroc_1.2.2.tar.gz", repos = NULL, type="source")

# Load the library
library("WheresCroc")

hiddenMarkov <- function(readings, positions, edges, probs){
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
    p[i] = p[i] / sum
  }
  return(p)
}

myFunction <- function(moveInfo, readings, positions, edges, probs){
  
  p <- hiddenMarkov(readings, positions, edges, probs)
  new_p <- which.max(p)
  print(new_p)
  options=getOptions(positions[3],edges)
  
  moveInfo$moves=c(options[1],0)
  return(moveInfo)
}





getOptions=function(point,edges) {
  c(edges[which(edges[,1]==point),2],edges[which(edges[,2]==point),1],point)
}
runWheresCroc(myFunction, doPlot = T, showCroc = T, pause = 1,
              verbose = T, returnMem = F, mem = NA)
