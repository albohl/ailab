bestWC=function(moveInfo,readings,positions,edges,probs) {
  moveInfo$mem = getState(moveInfo, positions, readings, probs)
  crocAt = max(state) #or however you get the index of the highest value in a vector
  direction = search(currentPosition, crocAt);
  moveInfo$moves=c(direction,0)
  return(moveInfo)
}

getState=function(moveInfo, positions, edges, readings, probs){
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
  probs = getProbs(readings, probs)
  state = state * probs
  
}

search=function(currentPosition, crocAt){
  #calculate best path, return a direction to go
  #probably dijsktra, since we have no heuristic for Astar
  #or greedy hill climb or something
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
    sumProbs = sum(probVec)
    probVecNormalized = probVec/sumProbs
  }
  return (probVecNormalized)
}