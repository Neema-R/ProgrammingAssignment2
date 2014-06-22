## Caching the Inverse of a given matrix

makeCacheMatrix <- function (x=matrix())  {
  
  ## makeCacheMatrix creates a list of functions that can cache
  ## the inverse of a given matrix
  
  m <- NULL
  set <- function(y)  {
    x <<- y 
    m <<- NULL
  }
  get <- function () x
  SetInverse <- function (inverse)  m <<- inverse
  getInverse <- function ()  m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function (x=matrix(),...)   {
  
  ## cacheSolve computes the inverse of the given matrix returned
  ## by makeCacheMatrix, unless the inverse has already been calculated,
  ## in which case it reiterates it from the cache
  
  m <- x$getInverse()
  if (!is.null(m))  {
    message ("geting cached data")
    return (m)
  }
  m <- solve(x$get())
  x$setInverse(m)
  m
}
