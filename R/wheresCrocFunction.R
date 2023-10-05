# Install the package
install.packages("WheresCroc_1.2.2.tar.gz", repos = NULL, type="source")

# Load the library
library("WheresCroc")

myFunction <- function(moveInfo, readings, positions, edges, probs){
  print('moveInfo')
  print(moveInfo)
  # print('crocInfo')
  # print(crocInfo)
  # print('touristInfo')
  # print(touristInfo)
  # print('pathMatrix')
  # print(pathMatrix)
  # print('waterInfo')
  # print(waterInfo)
  options=getOptions(positions[3],edges)
  print("Move 1 options (plus 0 for search):")
  print(options)
  
  moveInfo$moves=c(options[1],0)
  return(moveInfo)
}
getOptions=function(point,edges) {
  c(edges[which(edges[,1]==point),2],edges[which(edges[,2]==point),1],point)
}
runWheresCroc(myFunction, doPlot = T, showCroc = F, pause = 1,
              verbose = T, returnMem = F, mem = NA)
