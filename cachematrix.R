# makeCacheMatrix wraps a raw R matrix with a layer that helps us cache 
# previously computed inverses. If the value of the matrix is changed by calling
# $set(), the cached value is reset to NULL to signal to cacheSolve() the need
# to freshly compute an inverse for our matrix.

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
# above.
#
# If the inverse has already been calculated (and the matrix has not changed),
# then cacheSolve should retrieve the inverse from the cache.
# This function assumes that the input matrix is always invertible.
cacheSolve <- function(x, ...) {
        inverse <- x$get.inverse()
        if(is.null(inverse)) {
                # x doesn't have a precomputed inverse. Compute and cache it 
                # before returning the result.
                # This is slightly different from the assignment's cached vector
                # mean example. This allows us to structure this function to
                # have a single exit point, at the end of the function, rather
                # than one early exit point in the middle for the case of a
                # cache hit and one at the end for a cache miss.
                
                # Uncomment this line to see caching behavior when debugging.
                # message("computing uncached inverse")
                
                matrix <- x$get()
                inverse <- solve(matrix, ...)
                x$set.inverse(inverse)
        } 
        inverse
}
