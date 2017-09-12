bestDM = function(roads, car, packages){
  if(car$load == 0){
#    togo = selectPackage(roads, car, packages)
    togo = which(packages[,5] == 0)[1]
    orig = list(x = car$x, y = car$y)
    dest = list(x = packages[togo, 1], y = packages[togo, 2])
    dir = astar(orig, dest, roads)$dir
    car$nextMove = dir
    return (car)
  } else {
    orig = list(x = car$x, y = car$y)
    dest = list(x = packages[car$load, 3], y = packages[car$load, 4])
    dir = astar(orig, dest, roads)$dir
    car$nextMove = dir
    return(car)
  }
}

selectPackage=function(roads,car,packages){
  availablePackagesnr = which(packages[,5] == 0)
  availablePackages = list()
  for (x in availablePackagesnr){
    availablePackages[[i]] = list(nr = x, x1 = packages[x, 1], y1 = packages[x, 2], x2 = packages[x, 3], y2 = packages[x, 4], cost = mDist(list(x = x1, y = y1), list(x = x2, y = y2)))
  }
}

astar=function(orig, dest, roads){
  visited = list()
  frontier = list()
  current = list(x = orig$x, y = orig$y, h = 0, accCost = 0, cameFrom = NULL, dirTo = NULL)
  frontier = lappend(frontier, current)
  while(indexOf(visited, dest)==-1){
    min = 10000
    minIndex = 0
    for(i in seq_len(length(frontier))){
      cost = frontier[[i]]$h + frontier[[i]]$accCost
      if (cost < min){
        min = cost
        minIndex = i
      }
    }
    current = frontier[[minIndex]]
    visited[[length(visited)+1]] = current
    frontier[[minIndex]] = NULL
    frontier = addFrontier(current, frontier, roads, dest)
    print(length(frontier))
  }
  current=visited[[length(visited)]]
  cost = current$accCost
  while(!is.null(current$cameFrom$cameFrom)){
    current = current$cameFrom
  }
  return (list(dir = current$dirTo, cost = cost))
}

addFrontier=function(current, frontier, roads, dest){
  if(current$x>1){
    coords = list(x = current$x - 1, y = current$y)
    heu = mDist(dest, coords)
    accumulatedCost = current$accCost + roads$hroads[current$y, current$x-1]
    toAdd = list(x = coords$x, y = coords$y, h = heu, accCost = accumulatedCost, cameFrom = current, dirTo = 4)
    index=indexOf(frontier, toAdd)
    if (index!=-1){
      if(frontier[[index]]$accCost > toAdd$accCost){
        frontier[[index]] = toAdd
      }
    } else {
      frontier = lappend(frontier, toAdd)
    }
  }
  if(current$x<10){
    coords = list(x = current$x + 1, y = current$y)
    heu = mDist(dest, coords)
    accumulatedCost = current$accCost + roads$hroads[current$y, current$x]
    toAdd = list(x = coords$x, y = coords$y, h = heu, accCost = accumulatedCost, cameFrom = current, dirTo = 6)
    index=indexOf(frontier, toAdd)
    if (index!=-1){
      if(frontier[[index]]$accCost > toAdd$accCost){
        frontier[[index]] = toAdd
      }
    } else {
      frontier = lappend(frontier, toAdd)
    }
  }
  if(current$y>1){
    coords = list(x = current$x, y = current$y - 1)
    heu = mDist(dest, coords)
    accumulatedCost = current$accCost + roads$vroads[current$y-1, current$x]
    toAdd = list(x = coords$x, y = coords$y, h = heu, accCost = accumulatedCost, cameFrom = current, dirTo = 2)
    index=indexOf(frontier, toAdd)
    if (index!=-1){
      if(frontier[[index]]$accCost > toAdd$accCost){
        frontier[[index]] = toAdd
      }
    } else {
      frontier = lappend(frontier, toAdd)
    }
  }
  if(current$y<10){
    coords = list(x = current$x, y = current$y + 1)
    heu = mDist(dest, coords)
    accumulatedCost = current$accCost + roads$vroads[current$y, current$x]
    toAdd = list(x = coords$x, y = coords$y, h = heu, accCost = accumulatedCost, cameFrom = current, dirTo = 8)
    index=indexOf(frontier, toAdd)
    if (index!=-1){
      if(frontier[[index]]$accCost > toAdd$accCost){
        frontier[[index]] = toAdd
      }
    } else {
      frontier = lappend(frontier, toAdd)
    }
  }
  return (frontier)
}

lappend=function(list, node){
  list[[length(list) + 1]] = node
  return (list)
}

#helper function that returns true if node (x,y) is in list
indexOf=function(list, node){
  if(length(list) == 0){
    return (-1)
  }
  for(index in 1:length(list)){
    if ((list[[index]]$x == node$x) && (list[[index]]$y == node$y)){
      return (index)
    }
  }
  return (-1)
}
#returns the manhattan distance between 2 points
mDist=function(orig, dest){
  return (abs (dest$x - orig$x) + abs(dest$y - orig$y))
}
