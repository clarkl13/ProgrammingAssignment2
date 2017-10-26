
## creates a object which creates 4 functions and returns the functions into a list

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solv) inv <<- solv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Returns a matrix that is the inverse of the returned makecachematrix object
## if the same returned makecachematrix value is passed into the cacheSolve function 
## returns "getting cached data" text along with actual cached data

cacheSolve <- function(x, ...) 
{
  inv <- x$getinv()
  if(!is.null(inv)) 
  {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
