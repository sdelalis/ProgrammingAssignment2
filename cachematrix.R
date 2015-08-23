  
 ########################################################################################################################################
 ## Assignment
 ## Function: cachematrix.R  
 ## Purpose : Caching the Inverse of a Matrix
 ##           This function calls the two functions  makeCacheMatrix & cacheSolve. This will help avoid the computation of the inverse
 ##           a matrix repeatedly as it is known to be usually a costly computation.
 ##
 ## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
 ## cacheSolve: 	 This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
 ##				  	 If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the
 ##				     inverse from the cache.
 ## Date: August 14, 2015.

  
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
			

			#  nitialising minv to null before and after the setter function
			
			minv <- NULL 
		    set <- function(y) {
		    x <<- y
		    minv <<- NULL 
		  }
		  
			  get <- function() x  
			  setInv <- function(inv) minv <<- inv  
			  getInv <- function() minv  
			  list(
			  	   set = set,
			   	   get = get,
			       setInv = setInv,
			       getInv = getInv
			      )

}


## This function computes the inverse of the special "matrix" created by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

         m <- x$getInv() 

        if(!is.null(m)) { 
  		  message("getting cached data")
   			 return(m) 
 		 }
  			data <- x$get() 
  			m <- solve(data)
  			x$setInv(m) 
  			m 
}
