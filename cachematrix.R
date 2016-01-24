# This function creates a special "matrix" object that can cache its inverse,  
# which is really a list containing a function to  
# set the value of the matrix  
# get the value of the matrix  
# set the value of the inverse of the matrix  
# get the value of the inverse of the matrix  

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL  
        set <- function(y) {  
        x <<- y  
        inv <<- NULL  
 }  
        get <- function() x  
        setInverse <- function(inverse) inv <<- inverse  
        getInverse <- function() inv  
        list(set = set,  
        get = get,  
        setInverse = setInverse,  
        getInverse = getInverse)  

}


 # This function calculates the inverse of the special "matrix"  
 # the special "matrix" which created with the makeCacheMatrix function.  
 # However, it first checks to see if the inverse has already been calculated.  
 # If so, it gets the inverse from the cache and skips the computation.  
 # Otherwise, it calculates the inverse of the matrix and sets the value of the inverse  
 # in the cache via the setinverse function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()  
        if (!is.null(inv)) {  
        message("getting cached data")  
        return(inv)  
        }  
        mat <- x$get()  
        inv <- solve(mat, ...)  
        x$setInverse(inv)  
        inv  
}
