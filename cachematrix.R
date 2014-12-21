## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        cached.inverse <- NULL
        set <- function(new.x) {
                cached.inverse <<- NULL
                x <<- new.x
        }
        get <- function() { x }
        set.inverse <- function(inverse) cached.inverse <<- inverse
        get.inverse <- function() { cached.inverse }
        list(set = set, get = get,
             set.inverse = set.inverse, get.inverse = get.inverse)
}


# cacheSolve computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above.
#
# This will only yield a benefit with matrices constructed from the function
# above. Specifically, it will be no faster to invert native R matrices compared
# to calling solve() directly.
#
# If the inverse has already been calculated (and the matrix has not changed),
# then cacheSolve should retrieve the inverse from the cache.
# This function assumes that the input matrix is always invertible.
cacheSolve <- function(x, ...) {
        inverse <- x$get.inverse()
        if(is.null(inverse)) {
                # x doesn't have a precomputed inverse. Compute and cache it 
                # before returning the result.
                message("computing uncached inverse")
                matrix <- x$get()
                inverse <- solve(matrix, ...)
                x$set.inverse(inverse)
        } 
        inverse
}
