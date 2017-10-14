bestWC=function(moveInfo,readings,positions,edges,probs) {
  moveInfo$mem = getState(moveInfo,readings,positions,edges,probs)
  crocAt = match(max(moveInfo$mem$lastState),moveInfo$mem$lastState) 
  direction = search(positions[3], crocAt, edges)
  moveInfo$moves=c(direction, 0)
  return(moveInfo)
}

getState=function(moveInfo,readings,positions,edges,probs){
  #first round
  if(length(moveInfo$mem) == 0){
    moveInfo$mem = list(Tmat = getTmat(edges), lastState = matrix(1/40, nrow = 1, ncol = 40))
    return (moveInfo$mem)
  }
  #backpacker died
  for (i in seq_len(2)){
    if (!is.na(positions[i]) && positions[i] < 0){
      state = matrix(0, nrow = 1, ncol = 40)
      state[-positions[i]] = 1
      moveInfo$mem$lastState = state
      return (moveInfo$mem)
    }
  }
  state = moveInfo$mem$lastState %*% moveInfo$mem$Tmat
  probbies = getProbs(readings, probs)
  state = t(t(state) * probbies)
  sumState = sum(state)
  stateNormalized = state/sumState
  moveInfo$mem$lastState = stateNormalized
  return (moveInfo$mem)
}

search=function(currentPosition, crocAt, edges){
  #calculate best path, return a direction to go
  #probably dijsktra, since we have no heuristic for Astar
  #or greedy hill climb or something
  visited = list()
  frontier = list(list(pos = currentPosition, cost = 0, prev = NULL))
  while(indexOf(visited, crocAt) == -1){
    bestindex = best(frontier)
    current = frontier[[bestindex]]
    frontier[[bestindex]] = NULL
    frontier = addFrontier(current, visited, frontier,edges)
    visited[[length(visited)+1]] = current
  }
  end = visited[[length(visited)]]
  while(!is.null(end$prev$prev)){
    end = end$prev
  }
  return(end$pos)
}

best=function(frontier){
  best1 = 1
  min = 100000;
  for (i in seq_len(length(frontier)))
  {
    if (frontier[[i]]$cost < min){
      min = frontier[[i]]$cost
      best1 = i
    }
  }
  return (best1)
}
#takes a list of nodes (pos, cost, prev) and a pos (int) and returns the index of it, or -1
indexOf=function(list, current){
  for(i in seq_len(length(list))){
    if (!is.na(list[[i]]$pos == current)){
      if (list[[i]]$pos == current){
        return (i)
      }
    }
  }
  return (-1)
}

addFrontier=function(current, visited, frontier,edges){
  options = getOptions(current$pos, edges)
  for (option in options){
    if (indexOf(visited, option) == -1){
      indexinfrontier = indexOf(frontier, option)
      if(indexinfrontier != -1){
        if(frontier[[indexinfrontier]]$cost > current$cost + 1){
          frontier[[indexinfrontier]] = list(pos = option, cost = current$cost + 1, prev = current)
        }
      } else {
        frontier[[length(frontier)+1]] = list(pos = option, cost = current$cost + 1, prev = current)
      }
      
    }
  }
  return (frontier)
  
}

getTmat=function(edges){
  #should return a 40x40 matrix with equal probability for each possible transition
  #so if the croc is in 40 and can go to 39 and 38, then Tmat[40,39] and Tmat[40,38] and Tmat[40,40] should all be 1/3 (it can stay)
  #could use getOptions from WheresCroc.R
  Tmat = matrix(0, nrow = 40, ncol = 40)
  for (i in seq_len(40)){
    bla = getOptions(i, edges)
    for (j in bla){
      Tmat[i,j] = 1/length(bla)
    }
  }
  return (Tmat)
}

getProbs=function(readings, probs){
  #use dnorm(reading, mean, stdev) for each positions salinity, phosphate and nitrogen
  #should return a vector of probabilities the croc is in each hole given the readings
  probVec = matrix(0, nrow = 40, ncol = 1)
  for (i in seq_len(40)){
    probElem = dnorm(readings[1], probs$salinity[i,1], probs$salinity[i,2])
    probElem = probElem * dnorm(readings[2], probs$phosphate[i,1], probs$phosphate[i,2])
    probElem = probElem * dnorm(readings[3], probs$nitrogen[i,1], probs$nitrogen[i,2])
    probVec[i] = probElem
  }
  sumProbs = sum(probVec)
  probVecNormalized = probVec/sumProbs
  return (probVecNormalized)
}