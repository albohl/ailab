bestDM = function(roads, car, packages){
  if(car$load == 0){
    togo = selectPackage(roads, car, packages)
#    togo = which(packages[,5] == 0)[1]
    orig = list(x = car$x, y = car$y)
    dest = list(x = packages[togo, 1], y = packages[togo, 2])
    dir = astar(orig, dest, roads)
    car$nextMove = dir
    return (car)
  } else {
    orig = list(x = car$x, y = car$y)
    dest = list(x = packages[car$load, 3], y = packages[car$load, 4])
    dir = astar(orig, dest, roads)
    car$nextMove = dir
    return(car)
  }
}

selectPackage=function(roads,car,packages){
  availablePackagesnr = which(packages[,5] == 0)
  availablePackages = list()
  curr = list(end = list(x = car$x, y = car$y), dist = 0, prev = NULL)
  for (i in seq(1,length(availablePackagesnr))){
    availablePackages[[i]] = list(nr = availablePackagesnr[i], start = list(x = packages[i, 1], y = packages[i, 2]), end = list(x = packages[i, 3], y = packages[i, 4]), dist = 0, prev = NULL)
  }
  ends = pathcosts(curr, availablePackages, list(), astardist, roads)
  end
  min = 100000
  for (thing in ends){
    if (thing$dist < min){
      end = thing
      min = thing$dist
    }
  }
  while(!is.null(end$prev$prev)){
    end = end$prev
  }
  return(end$nr)
}

pathcosts=function(curr, left, ends, distfun, roads){
  if (length(left)==1){
    left[[1]]$prev = curr
    left[[1]]$dist = curr$dist + mDist2(curr$end, left[[1]]$start, roads)
    ends[[length(ends)+1]] = left[[1]]
    return (ends)
  }
  else{
    ends = ends
    for (i in seq(1, length(left))){
      lleft = left
      prev = curr
      curr = lleft[[i]]
      curr$prev = prev
      curr$dist = prev$dist + distfun(prev$end, curr$start, roads)
      lleft[[i]] = NULL
      ends = pathcosts(curr, lleft, ends, mDist2, roads)
    }
    return (ends)
  }
}
  
astar=function(orig, dest, roads){
  if (orig$x == dest$x && orig$y == dest$y){
    return (5)
  }
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
    frontier = addFrontier(current, frontier, visited, roads, dest)
  }
  current=visited[[length(visited)]]
  cost = current$accCost
  while(!is.null(current$cameFrom$cameFrom)){
    current = current$cameFrom
  }
  return (current$dirTo)
}

astardist=function(orig, dest, roads){
  if (orig$x == dest$x && orig$y == dest$y){
    return (0)
  }
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
    frontier = addFrontier(current, frontier, visited, roads, dest)
  }
  current=visited[[length(visited)]]
  cost = current$accCost
  while(!is.null(current$cameFrom$cameFrom)){
    current = current$cameFrom
  }
  return (cost)
}

addFrontier=function(current, frontier, visited, roads, dest){
  if(current$x>1){
    coords = list(x = current$x - 1, y = current$y)
    heu = mDist(dest, coords)
    accumulatedCost = current$accCost + roads$hroads[current$y, current$x-1]
    toAdd = list(x = coords$x, y = coords$y, h = heu, accCost = accumulatedCost, cameFrom = current, dirTo = 4)
    index=indexOf(frontier, toAdd)
    index2=indexOf(visited, toAdd)
    if (index!=-1){
      if(frontier[[index]]$accCost > toAdd$accCost){
        frontier[[index]] = toAdd
      }
    } else if (index2==-1){
      frontier = lappend(frontier, toAdd)
    }
  }
  if(current$x<10){
    coords = list(x = current$x + 1, y = current$y)
    heu = mDist(dest, coords)
    accumulatedCost = current$accCost + roads$hroads[current$y, current$x]
    toAdd = list(x = coords$x, y = coords$y, h = heu, accCost = accumulatedCost, cameFrom = current, dirTo = 6)
    index=indexOf(frontier, toAdd)
    index2=indexOf(visited, toAdd)
    if (index!=-1){
      if(frontier[[index]]$accCost > toAdd$accCost){
        frontier[[index]] = toAdd
      }
    } else if (index2==-1){
      frontier = lappend(frontier, toAdd)
    }
  }
  if(current$y>1){
    coords = list(x = current$x, y = current$y - 1)
    heu = mDist(dest, coords)
    accumulatedCost = current$accCost + roads$vroads[current$y-1, current$x]
    toAdd = list(x = coords$x, y = coords$y, h = heu, accCost = accumulatedCost, cameFrom = current, dirTo = 2)
    index=indexOf(frontier, toAdd)
    index2=indexOf(visited, toAdd)
    if (index!=-1){
      if(frontier[[index]]$accCost > toAdd$accCost){
        frontier[[index]] = toAdd
      }
    } else if (index2==-1){
      frontier = lappend(frontier, toAdd)
    }
  }
  if(current$y<10){
    coords = list(x = current$x, y = current$y + 1)
    heu = mDist(dest, coords)
    accumulatedCost = current$accCost + roads$vroads[current$y, current$x]
    toAdd = list(x = coords$x, y = coords$y, h = heu, accCost = accumulatedCost, cameFrom = current, dirTo = 8)
    index=indexOf(frontier, toAdd)
    index2=indexOf(visited, toAdd)
    if (index!=-1 && index2==-1){
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
    tmp = (list[[index]]$x == node$x)
    tmp2 = list[[index]]$y == node$y
    if (tmp && tmp2){
      return (index)
    }
  }
  return (-1)
}
#returns the manhattan distance between 2 points
mDist=function(orig, dest){
  return (abs (dest$x - orig$x) + abs(dest$y - orig$y))
}

mDist2=function(orig, dest, roads){
  return(mDist(orig, dest))
}
