## setinverse(inverse) and getinverse() functions are used to set nad get the inverse of
## the matrix to and from the cache memory respectively


## This function takes a matrix as input
## This function also creates two nested functions setinverse(inverse) and getinverse()
## which will be used to set and get the inverse of a matrix to and from a global variable
## m which is stored in the cache memory

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m                         
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks in the cache whether the inverse of the matrix is already there. 
## If its there in the cache it will return that value otherwise it will calculate the 
## inverse and return that calculated value. This will also keep the inverse of the matrix
## in cache memory by calling setinverse(m) function

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get() 
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
