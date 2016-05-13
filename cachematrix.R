## These functions provide a mean to cache the result of a matrix inversion 
## They are suitable in algorithms where the inversion process on the same matrix if often performed
##### THIS IS JUST AN EXAMPLE OF USE WITH SOME COMMENTS...
# n<-10 # if would be possible to build a loop on n to check if the advantege in the use of a cached value increases with the matrix size
# k<-matrix(rnorm(n*n),n,n) # k is the original matrix
# M<-makeCacheMatrix(k) # M is the CacheMatrix version of k
# t1<-Sys.time() #..the system time just before the first call
# cacheSolve(M) # the first inversion, the value isn't yet cached
# t2 <-Sys.time() #..the syste time when the inversion is complete
# cacheSolve(M) #..here the cache value is returned
# t3 <- Sys.time() #..the final time stamp
# delta1 <- t2 - t1 #..this is the time required to invert the matrix when the solve function is used
# delta2 <- t3 - t2 #..this is the time required to retrieve the cached value of the same matrix
#######


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
