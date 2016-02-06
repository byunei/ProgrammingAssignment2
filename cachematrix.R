## Put comments here that give an overall description of what your
## functions do

# If you need to compute the inverse of a matrix multiple times it may
# be useful to just store the result instead of computing it again.
# Instead of working directly on a matrix, these 2 functions are thought
# for working on an object-kike list that stores a matrix and its inverse,
# if it has already been already computed. 

## Write a short comment describing this function

# makeCacheMatrix takes a matrix and returns a list containing the matrix
# and an "inverse" variable that stores the matrix inverse each time the
# list is passed as an argument to cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    # raise error if x is not a matrix
    if (class(x) != "matrix") stop()

    # initialize inverse
    inv <- NULL

    # sets a new value for the matrix; resets inverse to NULL.
    set <- function(y) {

        # raise error if x is not a matrix
        if (class(x) != "matrix") stop()

        # set new matrix value
        x <<- y

        # when matrix is re-set, inv is reset to NULL
        inv <<- NULL
    }

    # returns matrix
    get <- function() x

    # caches new inverse
    set_inverse <- function(inverse) {
        # The inverse of a matrix must be a matrix.
        # raise error if x is not a matrix.
        if (class(x) != "matrix") stop()

        inv <<- inverse
    }

    # returns cached inverse, if it has been set
    get_inverse <- function() inv

    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## Write a short comment describing this function

# Updates the cached inverse of a matrix-like object built by makeCacheMatrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    thematrix <- x$get()
    inv <- solve(thematrix)
    x$set_inverse(inv)
    inv
}
