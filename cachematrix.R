## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { ## This command line will define the argument with default mode of matrix
inv <- NULL                             ## This command line will initialize inv as a NULL and value of matrix inverse 
    set <- function(y) {                    ## This command line will define the set function 
        x <<- y                             ## to assign new value of matrix (parent environment)
        inv <<- NULL                        ## This command line will set a condition that if there is a new matrix, 
    }					    ##  then it will reset inv to NULL
    get <- function() x                     ## This command line will define the get function and will return value of the matrix
    
    setinverse <- function(inverse) inv <<- inverse  ## This command line will assign value of inv (parent environment)
    getinverse <- function() inv                     ## This command line will get the value of inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}


## Write a short comment describing this function

## The function below computes the inverse of the special "matrix" returned by makeCacheMatrix above, 
## if the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
