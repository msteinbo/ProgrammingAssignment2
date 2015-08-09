## Caching the Inverse of a Matrix is performed in two steps:
##
## makeCacheMatrix: A function creating a special "matrix" object 
##                  that can cache its inverse.
## cacheSolve: Function computing the inverse of the special "matrix" object 
##             returned by makeCacheMatrix. If the inverse has already been 
##             calculated (and the matrix has not changed), then cachesolve 
##             will simply retrieve the inverse from the cache.
##
## Usage example:
## a_Matrix <- makeCacheMatrix(diag(x=3, nrow = 10, ncol=10))
## inv_Matrix <- cacheSolve(a_Matrix)


## Function returning a special matrix object which is able to store it's
## inverse matrix.
## This special matrix is a list of four functions to set and get the matrix
## as well as it's cache (set_inverse_matrix, get_inverse_matrix).
## The cache itself (cached_inverse) is "hidden" from the user

makeCacheMatrix <- function(x = matrix()) {

      cached_inverse <- NULL

      set <- function(y) {
            # Whenever a new matrix is set the cache needs to be emptied
            # as the inverse matrix still needs to be computed for the
            # new matrix.
            x <<- y
            cached_inverse <<- NULL
      }
      
      # Returning the matrix
      get <- function() x
      
      # Function for setting the cache
      set_inverse_matrix <- function(inverse_matrix) {
            cached_inverse <<- inverse_matrix
      }
      
      # Function for returning the cache
      get_inverse_matrix <- function() cached_inverse
      
      # Returning the "special matrix" object (as list)
      list(set = set, 
           get = get,
           set_inverse_matrix = set_inverse_matrix,
           get_inverse_matrix = get_inverse_matrix)
}


## Function returning the cache. If the cache is still empty the inverse
## matrix is computed first and stored in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      cached_inverse <- x$get_inverse_matrix()
      if(is.null(cached_inverse)) {
            # In case the cache is still empty it needs to be set
            # with the inverse of the given matrix

            message("Cache still empy... inverse to be computed and stored")

            cached_inverse <- solve(x$get())
            x$set_inverse_matrix(cached_inverse)
            
      } else {
            # If the cache is not empty nothing needs to be done,
            # but printing a message showing we are simply returing the cache

            message("Inverse matrix already cached")
      }
            
      # Returning the cache (storing the inverse matrix)
      cached_inverse
}

makeVector <- function(x = numeric()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmean <- function(mean) m <<- mean
      getmean <- function() m
      list(set = set, get = get,
           setmean = setmean,
           getmean = getmean)
}


cachemean <- function(x, ...) {
      m <- x$getmean()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- mean(data, ...)
      x$setmean(m)
      m
}