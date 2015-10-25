## Since Matrix inversion is a costly computation, we define two 
## functions -- one function called makeCacheMatrix() to create 
## a special kind of matrix, and another function called 
## cacheSolve() to compute the inverse of the special matrix and 
## cache its inverse. 
##
## When the special matrix is first created, the inverse is not 
## computed. After the first execution of cacheSolve(), the inverse
## of the special matrix is computed and cached.
## After this step, cacheSolve() can be called numerous times, but 
## the inverse will be retrieved from from the cache, instead of 
## being recomputed each time. 
##
## Note that if you change the special matrix, calling cacheMatrix()
## means the inverse will need to be computed to update the cache inverse.

## Example of usage
#
#    mat <- matrix(c(.5, 0, 0, .5), 2, 2)
#    cacheMat <- makeCacheMatrix(mat)
#    cacheSolve(cacheMat)


###################################################################
# makeCacheMatrix() function is used to create a special "matrix",
# which really is a list containing a funciton to
#  1) set the value of a matrix
#  2) get the value of a matrix
#  3) set the value of the inverse matrix
#  4) get the value of the inverse matrix
#
# Inputs:   - mat (matrix)
#
# Outputs:  - list(set, get, setInverse, getInverse) (list of functions)

makeCacheMatrix <- function(mat = matrix()) {

	 matInv <- NULL
      
      # Used to set the value of 'mat'
      set <- function(newMat) {
            mat <<- newMat
            matInv <<- NULL
      }
      
      # used to get the value of 'mat'
      get <- function() mat
      
      # used by cacheSolve() to set the value of 'matInv'
      setInverse <- function(newInv) matInv <<- newInv
      
      # used to get the value of 'matInv'
      getInverse <- function() matInv
      
      
      # returns a list of functions
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)

}


#########################################################################
# cacheSolve() function calculates the inverse of the special 
# "matrix" created with the above function. It first checks to see 
# if the inverse has already been calculated. If so, it gets the invserse
# from the cache and skips the computation. Otherwise, it calculates the 
# inverse of the matrix and sets the value of the inverse in the cache via 
# the setInverse function.
#
# Inputs:  - cacheMatrix (special matrix created by makeCacheMatrix())
#          - optional arguments to be used by solve() function
#
# Outputs: - matInv (inverse of the matrix)

cacheSolve <- function(cacheMatrix, ...) {
        
        ## Return a matrix that is the inverse of 'x'
      
      matInv <- cacheMatrix$getInverse()
      if(!is.null(matInv)) {
            message("getting cached data")
            return(matInv)
      }
      
      mat <- cacheMatrix$get()
      matInv <- solve(mat, ...)
      cacheMatrix$setInv(matInv)
      matInv

}
