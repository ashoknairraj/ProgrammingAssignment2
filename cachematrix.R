## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## These functions introduce a special matrix object which caches its inverse.
## The create such a special matrix m you execute \code{m <- makeCacheMatrix(x)}
## where x is an ordinary matrix. You can then get the value with \code{m$get()}
## and change the value with \code{m$set(y)} where y is an ordinary matrix.
## You can get the inverse with \code{cacheSolve(m)}.


#' Create a special "matrix" object that can cache its inverse.
#' 
#' @param x A matrix
#' 
#' @return A list containing four functions to set and get the value of the
#'     matrix and to set and get the inverse of the matrix
#'     
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
 
    set <- function(y) {
        x <<- y    # Set the value
        m <<- NULL # Clear the cache
    }
    # Define function to get the value of the matrix
    get <- function() x

    setInverse <- function(inverse) m <<- inverse
    # Define function to get the inverse
    getInverse <- function() m
    
    # Return a list with the above four functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



 
cacheSolve <- function(x) {
    m <- x$getInverse() # This fetches the cached value for the inverse
    if(!is.null(m)) { # If the cache was not empty, we can just return it
        message("getting cached data")
        return(m)
    }
    # The cache was empty. We need to calculate it, cache it, and then return it.
    data <- x$get()  # Get value of matrix
    m <- solve(data) # Calculate inverse
    x$setInverse(m)  # Cache the result
    m                # Return the inverse
}