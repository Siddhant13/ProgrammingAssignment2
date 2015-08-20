## There are two functions in this script file. The first one makeCacheMatrix
## creates a special 'matrix' object which can cache its inverse. The second
## function cacheSolve is used to calculate the inverse of the matrix


## makeCacheMatrix creates the special 'matrix' object and takes a regular
## matrix as input. The output of the function is a list which contains four
## different functions

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL             ## initialize the inverse variable
        
        set <- function(y) {
                ## the function copies the values in the matrix
                x <<- y
                inv<<- NULL
        }
        get <- function(){
                x
        } 
        setinv <- function(inverse){
                inv <<- inverse
        }
        getinv <- function(){
                inv
        }
        
        ## create and return the list of four functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve retrieves the inverse of the matrxi if it is present in the  
## cache or else computes the inverse if the cache is NULL. Thus it is able to
## save a lot of processing time if repeated calculations are required at the
## expense of some extra memory

cacheSolve <- function(x, ...) {
        ## retrieve the cached value of the inverse
        inv <- x$getinv()
        
        ## checking if the inverse is NULL or not
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## computing the inverse as there is no cached data
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
