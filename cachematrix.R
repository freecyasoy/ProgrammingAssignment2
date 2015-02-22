## This programming project consists of a pair of functions which returns the cache of
## the inverse of an input matrix

## The function makeCacheMatrix creates a matrix which basically does the following
    ## 1. set the value of the matrix
    ## 2. get the value of the matrix
    ## 3. set the value of inverse of the matrix
    ## 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    #instantiate the cache
    mat <- NULL 
    #setting the value of the matrix
    set <- function(z) {
        x <<- z 
        mat <<- NULL
    }
    #getting the value of the matrix
    get <- function() x
    #setting the value of the inverse of the matrix
    setinverse <- function(inverse) mat <<- inverse
    #getting the value of the inverse of the matrix
    getinverse <- function() mat
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The cacheSolve function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If True, it gets the cached result and won't
## perform the computation. If False, then it computes the inverse of the matrix
## and sets the value in the cache.

cacheSolve <- function(x, ...) {
    mat <- x$getinverse()
    #check of the inverse already exists and returns it instead.
    if(!is.null(mat)) {
        message("Data available in cache, returning it instead.")
        return(mat)
    }
    #otherwise, take in the matrix and solve for its inverse
    data <- x$get()
    mat <- solve(data)
    x$setinverse(mat)
    mat
}