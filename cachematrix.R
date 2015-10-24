## Calculate the inverse of a square matrix

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinverse = function(inverse) inv <<- inverse 
  getinverse = function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## Check if the inverse exists in the local environment(for same input) and return the same. Else recalculate the inverse and retuen the result

cacheSolve <- function(x, ...) {
  inv = x$getinverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinverse(inv)
  return(inv)
}