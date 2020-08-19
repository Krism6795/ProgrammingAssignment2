## Cache Inverse of a Matrix
## There are benefits to caching the inverse of a matrix rather than computing 
## it every time. Due to the large amount of computing power matrix inversion 
## takes it may be beneficial to cache and store the inverse. 


## This Function creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
          inv<<- NULL
    }
    get <- function() {x}
    SetInverse <- function(inverse) {inv <<- inverse}
    getInverse <- function() {inv}
    list( set = set, get = get, SetInverse= SetInverse, getInverse = getInverse)
}


## This function computes the inverse of the matrix created by
## makeCacheMatrix. if the matrix has remained the same, and the inverse
## previously calculated, then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null (inv)) {
        message("getting cached data")
        return(inv)
    }
      mat <- x$get()  
      inv <- solve(mat, ...)
      x$SetInverse(inv)
}

