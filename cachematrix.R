
# Author: Hercules N. Hamanaka
# Date: 12.Nov.2017

# This function creates a special "matrix" object that can cache its inverse
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the value of the inverse matrix
# 4.get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      m_inv <- NULL
      set <- function(y) {
            x <<- y
            m_inv <<- NULL
      }
      
      get <- function() x
      set_inverse <- function(inverse) m_inv <<- inverse
      get_inverse <- function() m_inv
      list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      
      # get the inverse of the matrix  
      m_inv <- x$get_inverse()
      
      # checking if it is already cached
      if (!is.null(m_inv))
      {
            message("Returning cached data...")
            return(m_inv)
      }
      
      # get the matrix data
      m_data <- x$get()
      
      # solving the matrix data
      m_inv <- solve(m_data)
      
      # setting the inverse
      x$set_inverse(m_inv)
      m_inv
}
