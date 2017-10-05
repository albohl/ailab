bestWC=function(moveInfo,readings,positions,edges,probs) {
  state = getState()
  crocAt = max(state) #or however you get the index of the highest value in a vector
  direction = search(currentPosition, crocAt);
  moveInfo$moves=c(direction,0)
  return(moveInfo)
}

getState=function(){
}

search=function(currentPosition, crocAt){
  #calculate best path, return a direction to go
  #probably dijsktra, since we have no heuristic for Astar
}

getTmat=function(){
  #should return a 40x40 matrix with equal probability for each possible transition
  #so if the croc is in 40 and can go to 39 and 38, then Tmat[40,39] and Tmat[40,38] should both be 1/2 (Tmat[39,40] and Tmat[38, 40], not sure)
  #could use getOptions from WheresCroc.R
}

getProbs=function(){
  #use dnorm(reading, mean, stdev) for each positions salinity, phosphate and nitrogen
  #should return a vector of probabilities the croc is in each hole given the readings
}