# The following functions return an inverse of the input matrix.
# Additionally, it checks if the inverse has already been calculated(stored in
# cache).  If so, it takes it from the cache and skips the calculation.

## This function creates and returns a list vector of four functions which
## 'cache' the input matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # Set value of matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Get value of matrix
    get <- function() x
    
    # Set value of inverse matrix
    set_inv <- function(solve) {
        inv <<- solve
    }
    
    # Get value of inverse matrix
    get_inv <- function() inv
    
    #Return vector of functions created previously
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
    
}


## This function calculates the inverse of the matrix used in the above function.
## It checks if the inverse has already calculated and if it has, gets the inverse
## from the cache and skips the calculation. If it is not in the cache, it will
## caculate the inverse of the matrix and set the cache value in the "set_inv"
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inv()
    
    # Checks if inverse matrix is in cache and returns it if so.
    if(!is.null(inv)) {
        message("Getting cached data")
        return(inv)
        
    }   
    
    # Otherwise, we put the input matrix in 'data', calculate the inverse,
    # place it in the cache, and return the inverse result.
    else {   
    data <- x$get()
    inv <- solve(data,...)
    x$set_inv(inv)
    inv
    }
}

