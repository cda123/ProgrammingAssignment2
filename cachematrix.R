## This function is designed to take a matrix and generate the inverse of the
## matrix with caching of the matrix inverse. When cachesolve is called, the 
## matrix inverse will be returned from the cache if available, otherwise it 
## will be calculated.

## Note: the input matrix 'x' must be a square matrix, for 'cacheSolve'
## portion of the code to work.

## This is portion of the function creates a list containg the 
## the values of the following local variables: set, get, setInv and getInv. 

makeCacheMatrix <- function(x = matrix()) { 
        i <- NULL
# set local variable 'set'
        set <- function(y) {
# assign new values to global enviroment variables 'x' and 'i'
                x <<- y
                i <<- NULL
        }
# set values of local variables 'get', 'setInv' and 'getInv'
        
        get <- function() x     # when called returns value of 'x'
        setInv<- function(solve) i<<-solve 
        getInv <- function() i  # when called returns inverse of matrix 'x', if
                                # cached in 'i'or NULL

#create list of containing functions 'set', 'get', 'setInv' and 'getInv'
       
        list(set = set, get = get,setInv = setInv,getInv = getInv)

}


## This is the portion of the function that returns the inverse of the orginal 
## matrix 'x' when called on the output of makeCacheMatrix. If the inverse has 
## already been calculated, the cached value is returned.

## Input for this function is list generated from 'makeCacheMatrix' function.

cacheSolve <- function(x,...) {
                i <- x$getInv()   # gets value of 'i' from list
                
                if(!is.null(i)) { # If 'i' is not "NULL", returns cached data
                
                        message("getting cached data")  
                        return(i)    
                        
                }
                
                data <- x$get()         # If 'i'="NULL", 
                                        # assigns matrix 'x' to 'data'   
                
                i <- solve(data, ...)   # generates inverse of 'data'and assigns
                                        # to 'i'
                
                x$setInv(i)             # 
                
                i                       # prints 'i' - the inverse of matrix 'x' 
        
}