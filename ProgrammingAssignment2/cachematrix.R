######################################################################################################################
## Function makeCacheMatrix creates a special "matrix" which is actually list containing 4 functions to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse 
##
## Notes on useage...
## This function takes as input an 'invertable' matrix. Invoke it as follows:

## eg. output_matrix <- makeCacheMatrix(input_matrix)
#####################################################################################################################
makeCacheMatrix <- function(x = matrix()) {                     ## Function 'makeCacheMatrix' input is an invertible
                                                                ## matrix
        
        inv <- NULL                                             ## initialise the inverse matrix 'inv'
        
        set <- function(y) {                                    ## function to store the input matrix 
                x <<- y 
                inv <<- NULL
        }
        
        get <- function()  {                                    ## function to retrieve the input matrix        
                x
        }        
        
        setinverse <- function(solve) {                         ## function to store the inverse of the input matrix
                inv <<- solve
        }
        
        getinverse <- function() inv                            ## function to retrieve the inverse of the input matrix
        
        list(set = set, get = get,                              ## create the output list of functions
             setinverse = setinverse,
             getinverse = getinverse)
        
}  ## end of function @makeCacheMatrix

####################################################################################################################
## Function cacheSolve calculates the inverse of the special "matrix" created by makeCacheMatrix
## It first checks to see if the inverse has already been calculated.
## If it has, it gets the inverse from the cache and skips the calculation
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the 
## setinverse function
##
## Notes on useage ...
## cacheSolve assumes that the function 'makeCacheMatrix' has already been executed, so input is a matrix previously
## returned from makeCacheMatrix
##
## eg. inverse_matrix <- cacheSolve(output_matrix)
####################################################################################################################
cacheSolve <- function(x, ...) {                                ## Function 'cacheSolve' has matrix 'x' as input        

        inv <- x$getinverse()                                   ## attempt to retrieve the inverse of the input matrix
        
        if(!is.null(inv)) {                                     ## check if inverse was returned
                message("getting cached data")                  ## issue message is inverse found in cache
                return(inv)                                     ## return inverse using version found in cache
                
        }
        
        data <- x$get()                                         ## retrieve matrix for which inverse is required        
        inv <- solve(data, ...)                                 ## use 'solve' to calculate inverse of retrieved matrix
        
        x$setinverse(inv)                                       ## store the calculated inverse 'inv' in cache
        
        inv                                                     ## Return matrix 'inv', that is the inverse of 'x'
        
}  ## end of function @cacheSolve
