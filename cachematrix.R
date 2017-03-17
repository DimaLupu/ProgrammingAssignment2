## The following two function work in tandem: 
## 1st function "makeCacheMatrix()" creates a list that has 4 main components:
## a. set the value of a matrix via "set()" function.
## b. get the value of a matrix via "get()" function.
## c. set the value of an inverse matrix via "setInverse()" function
## d. get the value of an inverse matrix via "getInverse()" function

## 2nd function "cacheSolve()" effectively solves for an inverse matrix, should
## a square invertible matrix be provided.

## Note that: "makeCacheMatrix()" is used only to create the initial list, this
## function doesn't do any computations of an inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
    set <- function(y) {x   <<- y 
                        inv <<- NULL}
    get <- function()  {x}
    setInverse <- function(inverse) {inv <<- inverse}
    getInverse <- function() {inv}
    
    list (set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## This function takes as an input the list provided by "makeCacheMatrix()"
## if the "$getInverse" element of the list is not null the function returns 
## a cached inverted matrix.
## if "$getInverse" element is NULL the fucntion proceeds with "solve()" 
## function and stores the result "$setInverse" element of the list created by
## "makeCacheMatrix()" function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("retrieving cached inversed matrix")
        return(inv)
    }
    data <- x$get()
    inv  <- solve(data, ...)
    x$setInverse(inv)
    inv
}

