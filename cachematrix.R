## This function is designed to take a matrix and generate the inverse of the
## matrix with caching of the matrix inverse. When cachesolve is called, the 
## matrix inverse will be returned from the cache if available, otherwise it 
## will be calculated.

## This is portion of the function creates a list containg the 
## the values of the following local variables: set, get, setInv and getInv. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() matrix(x)
        setInv<- function(solve) i<<-solve 
        getInv <- function() i
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This is the portion of the function that returns the inverse of the orginal 
## matrix when called on the output of makeCacheMatrix. If the 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
makeCacheMatrix <- function(x = matrix()) {
        