## This Programming Assignment takes advantage of the scoping rules of the R language
##   and how they can be manipulated to preserve state inside of an R object.
## Below are two functions that are used to create a special object
##   that stores a matrix and cache's its inverse.


## this function creates a special "matrix", which is really a list containing a function to:
    # 1) set the value of the matrix
    # 2) get the value of the matrix
    # 3) set the value of the inverse
    # 4) set the value of the inverse

makeCacheMatrix <- function(mat = matrix(nrow = 0, ncol = 0)) {
    inv <- NULL
    set <- function(y) {
        mat <<- y
        inv <<- NULL
    }
    get <- function() mat
    setinv <- function(invrs) inv <<- invrs
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The following function calculates the inverse of the special "matrix"
##   which was created with the makeCacheMatrix() function.
## If inverse is already calculated, it is returned;
##   else it is calculated, assigned to inv, and is then returned.

cacheSolve <- function(mat, ...) {
    inv <- mat$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return (inv)
    }
    data <- mat$get()
    inv <- solve(data, ...)
    mat$setinv(inv)
    inv
}
