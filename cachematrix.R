## cachematrix.R - contains two main functions to cache inverse of a given matrix:
### makeCacheMatrix - accepts a matrix and caches its inverse
### cacheSolve - returns inverse of matrix either from cache or using solve function
### cacheSolve assumes that the stored matrix is inversible using solve function. If not,
### it throws a default error

## makeCacheMatrix - accepts a matrix as an argument and has functions 
### default constructor - stores a matrix and sets its inverse to null
### set - to  replace the existing matrix with new one, and reset inverse to null, 
### get - to get (return) an existing matrix
### setInverse - to store inverse of a matrix
### getInverse - to return cached inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    inverseM <- NULL # create a variable and assign null value
    set <- function(y) { # to assign a new matrix
        x <<- y
        inverseM <<- NULL
    }
    get <- function() x  # return matrix
    setInverse <- function(m) inverseM <<- m  #set inverse of matrix
    getInverse <- function() inverseM  # return cached inverse of matrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve - returns an inverse of a matrix either from the cached or by using solve function
### First it checkes whether inverse matrix is cached or not by calling getInverse of 
### makeCacheMatrix function. If it is not cached then it creates an inverse of a given matrix,
### stores in the cache and returns the inverse matrix
### This function assumes that the stored matrix is inversible using solve function. If not,
### it throws a default error

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if (!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    mat <- x$get()  # get matrix
    i <- solve(mat)
    x$setInverse(i)
    i
}
