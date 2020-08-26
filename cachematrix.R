## The following two functions cache the inverse of a matrix.


##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  x_inverse <- NULL
  set <- function(y){
    x <<- y
    x_inverse <<- NULL
  }
  get <- function(){x}
  setInverse <- function(inverse){x_inverse <<- inverse}
  getInverse <- function(){x_inverse}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the inverse
##from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x_inverse <- x$getInverse()
  if(!is.null(x_inverse)){
    if(identical(solve(x_inverse), x$get())){
      message("getting cached data")
      return(x_inverse)
    }
  }
  data <- x$get()
  x_inverse <- solve(data)
  x$setInverse(x_inverse)
  x_inverse
}

##Test
m <- matrix(c(1,1,0,1), nrow = 2, ncol = 2)
special_m <- makeCacheMatrix(m)
special_m$get()
cacheSolve(special_m)
