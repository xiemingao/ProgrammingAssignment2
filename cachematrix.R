## The first function creates a matrix that can cache its inverse while the second one
## solve its inverse. But it will save a lot of time if the inverse has already been 
## calculated because it can get it from the cache and skips the computation.

## The function below creates a special "matrix" object that cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
 set <- function(y){
     x <<-y
     inv <<- NULL
 }
 get <- function() x
 setinv <- function(inverse) inv <<- inverse
 getinv <- function() inv
 list(set = set, get = get, setinv = setinv,getinv = getinv)
}


## The function below computes the inverse of "matrix" returned by the function above.
## If the inverse has already been calculated and the matrix has not changed, it will 
## retrive the inverse from the cache directly.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    mat.data <- x$get()
    inv <- solve(mat.data,...)
    
    x$setinv(inv)
    
    inv
}
