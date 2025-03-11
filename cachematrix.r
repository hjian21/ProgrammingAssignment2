# Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize inverse to NULL (no cached value yet)

    # Function to set the matrix
    set <- function(y) {
        x <<- y  # Set the matrix using <<- 
        inv <<- NULL  # Reset the inverse if the matrix changes
    }

    # Function to get the matrix
    get <- function() x

    # Function to set the inverse (used for caching)
    setInverse <- function(inverse) {
        inv <<- inverse
    }

    # Function to get the inverse (retrieve from cache)
    getInverse <- function() inv

    # Return a list containing these functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# Compute the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    # Check if the inverse has been cached
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data") # Notify that we are retrieving from the cache
        return(inv) # Return the cached inverse
    }

    # Otherwise, retrieve the matrix
    mat <- x$get()

    # Check if the matrix is square and invertible
    if (nrow(mat) != ncol(mat)) {
        stop("The matrix is not square and cannot be inverted.")
    }
    if (det(mat) == 0) {
        stop("The matrix is singular and cannot be inverted.")
    }

    # Calculate the inverse and store it
    inv <- solve(mat, ...)  # solve() calculates the inverse
    x$setInverse(inv)  # Set the inverse in the special matrix
    return(inv)
}