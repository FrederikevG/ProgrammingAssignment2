## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a matrix that can cache its inverse to avoid recalculating it.

makeCacheMatrix <- function(x = matrix()) {
# Usage of the function:
# m<-makeCacheMatrix() create list with empty matrix x
# m$set() set matrix x in list 
# m$get() get the matrix x from the list
# m$setInverse() cache the matrix inverse
# m$getInverse() return the cached value of the matrix inverse
 
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
             getmean = getinverse)
}


## Write a short comment describing this function
## This function allows to calculate the inverse of a matrix or get it from the cache if
## it has already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
