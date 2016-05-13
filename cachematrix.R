## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function provides the basic methods to manage a cached matrix, namely set and get
## and two methods to set and get the inverse version 

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


## This function receives as a parameter a squared CacheMatrix (as obtained with the makeCacheMatrix) 
## and return its inverse by 
## invoking the getinverse method, 
## checking for the availability of a cached value
## if a cached value doesn't exist, it provides to evaluate the inverse matrix by using the solve R function
## the value obtained is then cached (using the setinverse method) for future calls.. 

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
