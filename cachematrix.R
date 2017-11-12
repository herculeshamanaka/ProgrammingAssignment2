
# Author: Hercules N. Hamanaka
# Date: 12.Nov.2017

# This function creates a special "matrix" object that can cache its inverse
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the value of the inverse matrix
# 4.get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      
      # initializing cached inversed matrix
      m_inv <- NULL
      
      # SET function definition
      set <- function(y) {
            x <<- y
            m_inv <<- NULL
      }
      
      # GET function definition
      get <- function() {
            # returning the initial matrix
            x
      }
      
      # SET_INVERSE definition
      set_inverse <- function(inverse) {
            # storing the inverse matrix
            m_inv <<- inverse
      }
      
      # GET_INVERSE definition
      get_inverse <- function() {
            # returning the cached inversed matrix
            m_inv
      }
      
      # returnin the list of availables functions
      list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      
      # get the inverse of the matrix  
      m_inv <- x$get_inverse()
      
      # checking if it is already cached
      # no need to make all the calculations again
      if (!is.null(m_inv))
      {
            message("Returning cached data...")
            return(m_inv)
      }
      
      # IF there is no cached value...
      # get the matrix data
      m_data <- x$get()
      
      # solving the matrix data
      m_inv <- solve(m_data)
      
      # setting the inverse
      x$set_inverse(m_inv)
      
      # returns the result
      m_inv
}
