## Put comments here that give an overall description of what your
## functions do

### These two functions allow to compute the inverse of a matrix
### and allow to save subsequent costly computations by caching the result
### Use case follows:
# m <- c(0, 2, 1, 0, 1, 2, 5, 4, 3)
# dim(m) <- c(3, 3)
# mat <- makeCacheMatrix(m)
# cacheSolve(mat)
# cacheSolve(mat) # This one would just return the cached inverse


## Write a short comment describing this function

### Cache a matrix and its inverse in this function environment
### Both matrices are accessible through set/get and setinverse/getinverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x

  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv

  list(set = set, get = get,
       setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

### Return the inverse of a cached matrix
### Compute it if the inverse is not already in cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    return(inv)
  }

  ### No inverse cached:
  #### Compute the inverse
  mat <- x$get()
  inv <- solve(mat)
  #### An then and cache it
  x$setinverse(inv)
  inv
}
