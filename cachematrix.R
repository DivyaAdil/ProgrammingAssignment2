#The following functions calculate the inverse of the matrix and
#store it in cache to save computation the next time the data is
#required

## This func makes the input matrix in the cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set,get = get, setinv = setinv, getinv = getinv)
  #the list of all the assigned values is returned so that the vals can 
  #be accessed without calling the cachesolve function
}


## This func checks if inv for the mat is available in cache
# if not, calculates and stores in cache

cacheSolve <- function(x, ...) {
  inv <- x$getinv() #getting the data from cache for the inverse
  #showing if cache has the value stored already and using it
  if(!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
  else{
    #since cache is empty, we get data from input
    matr <- x$get()
    inv <- solve(matr, ...)
    x$setinv(inv) #putting in cache
    inv #returning inverse
  }
}
