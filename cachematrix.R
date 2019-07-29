## These functions compute and cache the inverse of a matrix

## This function takes an matrix input (mat) and uses an empty 
## object to set and get the value of a matrix and then
## get and set the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL
    set <- function(y) {
        x <<- y
        mat <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) mat <<- inverse
    getinverse <- function() mat
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function retrieves the computed inverse matrix from the cache

cacheSolve <- function(x, ...) {
     mat <- x$getinverse()
     if (!is.null(mat)) {
          message("getting cached data")
          return(mat)
     }
     data <- x$get()
     mat <- solve(data,...)
     x$setinverse(mat)
     mat
}
