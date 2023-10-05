#' getTransitionProbability
#'
#'
getTransitionProbability = function(node, edges) {
  neighbors = getOptions(node, edges)
  return (1/length(neighbors))
}

#'
#'
#'
getEmissionVector = function(readings, probs) {
  salinity = dnorm(readings[1], probs[["salinity"]][, 1], probs[["salinity"]][, 2], FALSE)
  phosphate = dnorm(readings[2], probs[["phosphate"]][, 1], probs[["phosphate"]][, 2], FALSE)
  nitrogen = dnorm(readings[3], probs[["nitrogen"]][, 1], probs[["nitrogen"]][, 2], FALSE)
  e = replicate(40, 0)
  for (i in 1:40) {
    e[i] = salinity[i] * phosphate[i] * nitrogen[i]
  }
  
  sum = sum(e)
  for (i in 1:40) {
    e[i] = e[i] / sum
  }
  
  return (e)
}

#' getNodeProbability
#'
#'
getNodeProbability = function(node, prev_f, edges, emissions) {
  #Compute the probability of Croc reaching the waterhole considering what we know of the previous state
  neighbors = getOptions(node, edges)
  sum = 0
  for (n in neighbors) {
    sum = sum + getTransitionProbability(n, edges) * prev_f[n]
  }
  #  new_f = sum(prev_f * T) * e
  new_f = sum * emissions[node]
  return(new_f)
}

#' hiddenMarkov
#'
#'
hiddenMarkov = function(prev_f, probs, readings, positions, edges) {
  tourist1 = positions[1]
  tourist2 = positions[2]
  
  new_f = replicate(40, 0)
  # check if tourist 1 has been eaten this turn
  if (!is.na(tourist1) && tourist1 < 0) {
    crocNode = -1 * tourist1
    new_f[crocNode] = 1
  }
  # chekc if tourist 2 has been eaten this turn
  else if (!is.na(tourist2) && tourist2 < 0) {
    crocNode = -1 * tourist2
    new_f[crocNode] = 1
  } else {
    emissions = getEmissionVector(readings, probs)
    # compute the probabilities for each node
    for (i in 1:length(new_f)) {
      new_f[i] = getNodeProbability(i, prev_f, edges, emissions)
    }
    currentNode = positions[3]
    # probability in current node is 0
    new_f[currentNode] = 0
    # Normalize probabilities
    sum = sum(new_f)
    for (i in 1:40) {
      new_f[i] = new_f[i] / sum
    }
  }
  return (new_f)
}

#' bfsSearch
#'
#'
bfsSearch = function(node, goal, edges) {
  visited = c(node)
  queue = c(node)
  parents = replicate(40, 0)
  parents[node] = -1
  while (length(queue) != 0) {
    currentNode = head(queue, n=1)
    queue = setdiff(queue, c(currentNode))
    neighbors = getOptions(currentNode, edges)
    neighbors = setdiff(neighbors, c(currentNode))
    neighbors = setdiff(neighbors, visited)
    for (node in neighbors) {
      if (!(node %in% visited)) {
        queue = c(queue, node)
        parents[node] = currentNode
        visited = c(visited, c(node))
      }
    }
  }
  
  currentNode = goal
  path = numeric()
  while (currentNode != -1) {
    if (parents[currentNode] != -1) {
      path = c(c(currentNode), path)
    }
    currentNode = parents[currentNode]
  }
  
  return (path)
}

#' Finit
#'
#'
fInit = function(tourist1, tourist2) {
  prev_f = replicate(40, 0)
  counter = 0
  # loop thorugh all nodes
  for (node in 1:40) {
    # check if both tourists are alive 
    if (!is.na(tourist1) && !is.na(tourist2)) {
      # if tourist is at waterhole and alive, probability = 0
      if (tourist1 == node || tourist2 == node) {
        prev_f[node] = 0
      } else {
        prev_f[node] = 1
        counter = counter + 1
      }
    }
  }
  # probabilities equal at every possible node
  prev_f = prev_f / counter
  return (prev_f)
}

#' myFunction
#'
#'
myFunction = function(moveInfo,readings,positions,edges,probs) {
  me = positions[[3]]
  status = moveInfo[["mem"]][["status"]]
  # check if new game
  if (status == 0 || status == 1) {
    moveInfo[["mem"]][["prev_f"]] = fInit(positions[[1]], positions[[2]])
  }
  prev_f = moveInfo[["mem"]][["prev_f"]]
  new_f = hiddenMarkov(prev_f, probs, readings, positions, edges)
  goal = which.max(new_f)
  
  # if goal is in neighboring node, go there
  neighbors = getOptions(positions[3], edges)
  if(goal %in% neighbors){
    moveInfo$moves = c(goal,0)
    return (moveInfo)
  }
  
  # make bfs search for the shortest path to goal node
  path = bfsSearch(positions[3], goal, edges)
  
  # two nodes away from goal
  if(length(path) >= 2) {
    moveInfo$moves = c(path[1], path[2])
  }
  
  # onde node away from goal
  if(length(path) == 1){
    moveInfo$moves = c(path[1], 0)
  }
  
  # at goal node, search for croc
  if(length(path) == 0){
    moveInfo$moves=c(0,0)  
  }
  
  moveInfo[['mem']][["prev_f"]] = new_f
  moveInfo[["mem"]][["status"]] = 2
  
  return(moveInfo)
}
