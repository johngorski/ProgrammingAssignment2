## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        cached.inverse <- NULL
        set <- function(new.x) {
                cached.inverse <- NULL
                x <- new.x
        }
        get <- function() x
        set.inverse <- function(inverse) cached.inverse <<- inverse
        get.inverse <- function() cached.inverse
}


# cacheSolve computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above.
#
# If the inverse has already been calculated (and the matrix has not changed),
# then cacheSolve should retrieve the inverse from the cache.
# This function assumes that the input matrix is always invertible.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        solve(x)
}
