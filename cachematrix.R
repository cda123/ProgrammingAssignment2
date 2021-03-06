## This function is designed to take a matrix and generate the inverse of the
## matrix with caching of the matrix inverse. When 'cacheSolve' is called, the 
## matrix inverse will be returned from the cache if available, otherwise it 
## will be calculated.

## Note: the input matrix 'x' must be a square matrix, for the 'cacheSolve'
## portion of the code to work.


## The 'makeCacheMatrix' function creates a list containg the 
## the values of the following local variables: set, get, setInv and getInv. 

## This output list will be input for the 'cacheSolve' function.

makeCacheMatrix <- function(x = matrix()) { 

# assign value of function variable 'i' to "NULL"
        
        i <- NULL

# assign value of function variable 'set'
 
        set <- function(y) {

# assign values in the parent enviroment to variables 'x' and 'i'
                
                x <<- y
                i <<- NULL
                
        }

# set values of function variables 'get', 'setInv' and 'getInv'
        
        get <- function() x     # when called returns value of 'x'

        setInv<- function(solve) i<<-solve # assigns value of i in parent
                                           # enviroment to output of solve, 
                                           # caching inverse of 'x'

        getInv <- function() i  # when called returns inverse of matrix 'x', if
                                # cached in 'i'or NULL

#create list of containing functions 'set', 'get', 'setInv' and 'getInv'
       
        list(set = set, get = get,setInv = setInv,getInv = getInv)

}

## 'cacheSolve' is a function that reads the value of matrix 'x' from the list
## generated by 'makeCacheMatrix' and returns the inverse of this matrix. 
## If the inverse of matrix 'x' has already been calculated, both a message 
## indicating the cached data is being retrieved and the inverse of matrix 'x' 
## is printed.

## Note: The input for the 'cacheSolve' function is the list generated from 
## 'makeCacheMatrix' function. 


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
                
                x$setInv(i)             # calls the fuction 'setInv' from list
                                        # generated by 'makeCacheMatrix', which
                                        # caches output of solve to 'i'
                
                i                       # prints 'i' - the inverse of matrix 'x' 
        
}