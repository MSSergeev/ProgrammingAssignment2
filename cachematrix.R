## This pair of functions caches the inverse of a given matrix
## Assumed that the supplied matrix is invertible.

## This function creates a vector, which is a list containing a functions to
## 1. Set the given matrix,
## 2. Get the given matrix,
## 3. Set calculated inverse matrix,
## 4. Get calculate inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    matrix_inverse <- NULL
    set <- function(given_matrix) {
        x <<- given_matrix
        matrix_inverse <<- NULL
    }
    get <- function() x
    put_inverse <- function(counted_inverse) matrix_inverse <<- counted_inverse
    get_inverse <- function() matrix_inverse
    list(set = set, get = get, 
         put_inverse = put_inverse, 
         get_inverse = get_inverse)
}


## The following functions checks if the inverse matrix has already bean calculated.
## And if it has, the function gets the inverse matrix from the cache.
## Otherwise, it calculates the inverse matrix and stores it in cache via the put_inverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    matrix_inverse <- makeCacheMatrix$get_inverse()
    if(!is.null(matrix_inverse)) {
        message("Inverse matrix has been already calculated,
                getting cached matrix")
        return(matrix_inverse)
    }
    
    matrix_inverse <- solve(makeCacheMatrix$get(), ...)
    makeCacheMatrix$put_inverse(matrix_inverse)
    matrix_inverse
}
