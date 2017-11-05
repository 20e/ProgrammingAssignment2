## these two functions cache the inverse of a matrix

## makeCacheMatrix() creates an R object that stores a matrix and its inverse 
## matrix(just store,not calculate)

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get, ##makeCacheMatrix() returns 4 functionsto the
                                   ##parent environment.
             setsolve = setsolve,  ##once an object of type makeCacheMatrix() is
                                   ## created, its value can be changed without 
                                   ## initializing another instance of the object. 
             getsolve = getsolve)  ##Naming this way what allows us to use the $
                                   ##rather than [[]] to get the contents
}

## cacheSolve() retrieves the inverse matrix from the cached value that is stored 
## in the makeCacheMatrix() object's environment.
## cacheSolve() calculated the inverse matrix if the cached value is null.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        matrix <- x$get() ##to retrieve a inverse from the object passed in as 
                          ##the argument
        s <- solve(matrix, ...)
        x$setsolve(s)     ##to set the inverse matrix in the input object
        s
}
