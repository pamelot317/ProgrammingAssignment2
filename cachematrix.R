## makeCacheMatrix creates a matrix and subfunctions that: set the 
## value of the matrix, get the value of the matrix, 
## set the value of the inverse, and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ## the set function takes an input matrix and stores it in the
    ## variable x; it also resets the value of i
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## the get function retrieves the matrix that was just stored
    ## to x
    get <- function() x
    ## the setinv function takes an inverse matrix as input and 
    ## stores it to the cache variable i 
    setinv <- function(inverse) i <<- inverse
    ## the getinv function retrieves the inverse matrix, if there
    ## is one stored in the cache
    getinv <- function() i
    list(set = set, get = get, 
         setinv = setinv,
         getinv = getinv)
}

## The cacheSolve function first checks to see if there is an inverse
## matrix stored in the cache. If there is one, it returns it and 
## stops executing code. If not, it computes one.

cacheSolve <- function(x, ...) {
    ## these lines are checking to see if there is an inverse
    ## matrix in the cache
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## here, we retrieve the matrix that we want to solve and store 
    ## it as data
    data <- x$get()
    
    ## now we compute the inverse of the matrix and store it 
    ## as the variable i
    i <- solve(data,...)
    ## next we store the new value of i in the cache so we don't 
    ## have to compute it again
    x$setinv(i)
    ## this returns the inverse matrix
    i
}
