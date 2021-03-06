##Inverse Matrix
makeCacheMatrix <- function(x = matrix()){
  inverse_m <- NULL
  set <- function(y){
    x <<- y
    inverse_m <<- NULL
  }
  get <- function() x
  setinve_m <- function(inverse) inverse_m <<- inverse
  getinve_m <- function() inverse_m
  list(set = set, get = get, setinve = setinve,
       getinve = getinve)
}


cacheSolve <- function(x, ...) {
  inverse <- x$getinve()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse_m <- solve(data, ...)
  x$setinve(inverse_m)
  inverse_m
}
  