## ************************************************************************
## My function follow very closely the exemplars supplied in the Assignment
## instructions. The first function (makeCacheMatrix) basically constructs a
## list object that holds data (matrix, and caches the inverse) and behavior
## setters and getters for both the matrix and it's inverse. The second
## function (uses the first) gets and returns the inverse from the object if
## available, otherwise retrives the matrix, calculates the inverse using the
## solve function, stores it in the list object, so next time calculation is
## not required, and returns the inverse.
##
## The first function is really the interesting one. It encapsultes or stores,
## if you will, the matrix and its inverse in variables, as well as methods
## (functions) that remain accessible to the returned list due to lexical
## scoping. Basically, makeCacheMatrix function's body is the defining
## environment for the returned list.
## ************************************************************************


## This function returns a list whose attributes allow access to the function's
## after the function as exited (aka a closure). This way both a matrix and
## its inverse are cacheable.

makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL
    set <- function(m){
        x <<- m
        inverted <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inverted <<- inverse
    get_inverse <- function() inverted
    list(set=set, get=get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## This function gets an object returned from makeCacheMatrix function, tries
## to get the inverse matrix from its cache if available. If the inverse is not
## available, it calculates the inverse using solve, caches it, and returns it.

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$get_inverse()
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix)
    x$set_inverse(inverse)
    inverse
}
