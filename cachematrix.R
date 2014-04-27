## This assignment is aimed at solving the problem of repeating complicated and resource consuming operation
## The obvious way to overcome the problem is to use caching of the output and returing values for cache for an already seen problem
## Here we do it using the example of inverse of a matrix

## makeCacheMatrix
## Description: Function with a list of 4 functions viz: set, get, setinverse, getinverse
## Set: Set the value of the incoming matrix
## Get: Getter for the matrix 
## SetInverse: Sets the calculated inverse
## GetInverse: Gets the inverse for the output if it exists in the cache
## Input: A matrix whose inverse can be calculated
## Ouput: Inverse of matrix if exists in the cache

makeCacheMatrix <- function(matrixInput = matrix()) {
	 inverse <- NULL
        # set function sets the input_matrix as x and 
        set <- function(input_matrix) {
            matrixInput <<- input_matrix
            inverse <<- NULL
        }
        get <- function() matrixInput
        setinverse <- function(inverse) {
            inverse <<- inverse
        }
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve
## Description: Checks if the input matrix exists in the cache
##              if true, return the cached inverse
##              if false, calculate the inverse, store in cache and return the inverse  
## Input: Matrix
## Output: Inverse of the given matrix

cacheSolve <- function(matrixInput, ...) {
        inverse <- matrixInput$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- matrixInput$get()
        inverse <- solve(data)
        matrixInput$setinverse(inverse)
        inverse
}
