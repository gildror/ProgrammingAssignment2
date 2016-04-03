## The following functions take a matrix and create a wrapper vector 
## that can store in a cache the inverted version of that matrix
## in addition, there is a test function that calls the two functions
## to verify they work properly.



## function that takes a square matrix as a parameter and wraps it
## in a vector that allows to store its invesion. The vector acts as
## an object with the following functions
## set - used to set the matrix
## get - user to return the original matrix
## setcache - used to store the inverted matrix in cache
## getcache - used to get the inverted matrix from cache
##
makeCacheMatrix <- function(x = matrix()) {
    c <- NULL
    set <- function(y) {
        x <<- y
        c <<- NULL
    }
    get <- function() x
    setcache <- function(cache) c <<- cache
    getcache <- function() c
    list(set = set, get = get,
         setcache = setcache,
         getcache = getcache)
}



## function that takes a the vector result from makeCacheMatrix
## and checks if the inverted matric has been stroed in its cache
## if it did, then it prints a message and returns the result
## if it doesn't, it calcualtes the inverted matrix and stores it in
## cache.
##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    c <- x$getcache()
    if(!is.null(c)) {
        message("getting cached data")
        return(c)
    }
    data <- x$get()
    c <- solve(data, ...)
    x$setcache(c)
    c    
}


## Function to test the code above by generating a matrix and calling
## the two functions
##
testCacheMatrix <- function(size=5){

    mtrix <- matrix(rnorm(200), size, size) 
    m <- makeCacheMatrix(mtrix)

    ## print original matrix
    retorig <- m$get()
    print(retorig)
    
    ## print first call to function
    retnocache <- cacheSolve(m)
    print(retnocache)
    
    ## print second call to function (should print cache message)
    retcached <- cacheSolve(m)
    print(retcached)
}