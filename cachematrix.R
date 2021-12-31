## The "makeCacheMatrix" function creates a special matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    invr <- NULL
    set <- function(y) {
        x <<- y
        invr <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) invr <<- solve
    getinverse <- function() invr
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## To calculate the inverse of the special matrix returned by the function above

cacheSolve <- function(x, ...) {
    invr <- x$getinverse()
    if(!is.null(invr)) {
        message("getting cached data")
        return(invr)
    }
    data <- x$get()
    invr <- solve(matrix, ...)
    x$setinverse(invr)
    invr
    
}
