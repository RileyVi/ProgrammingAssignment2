##makeCacheMatrix and cacheSolve will create a cache and then return the cache
##The cache will include a matrix and the inverse of said matrix

## makeCacheMatrix creates an object that stores the basematrix and inverse 
## It also contains 4 functions that maintain the object
## The functions will set and get the base matrix and inverse values
##The return of the function makeCacheMatrix is a list of the internal functions

makeCacheMatrix <- function(base_matrix = matrix()) {
        cached_inverse <- NULL #Need to initialize cache
      set_matrix <- function(newbaseMatrix){
              base_matrix <<- newbaseMatrix
              cached_inverse <<- NULL #Need to reset inverse cache when 
                                        #base number changes
      }
      get_matrix <- function(){
              base_matrix ##returns base matrix out of object
      }
      set_inverse <- function(calculated_inverse){
              cached_inverse <<- calculated_inverse
              ##sets cached inverse to whatever it was passed
      }
      get_inverse <- function(){
              cached_inverse ##returns cached inverse out of object
      }
      list(
              set = set_matrix, 
              get = get_matrix,
              setInverse = set_inverse,
              getInverse = get_inverse
      ) ##returns a list of possible sub-functions to maintain the object
}


##cacheSolve return the cached inverse of a given matrix
##cacheSolve gets passed a special object that's created in the above function
##cacheSolve will reset the inverse in the special object 
        ##if the base matrix is changed

cacheSolve <- function(makeCacheMatrix, ...) {
       temp_inverse <- makeCacheMatrix$getInverse() 
       ## check if the inverse is already cached
       if (is.null(temp_inverse)){ ## if the inverse isn't cached 
               basematrix <- makeCacheMatrix$get ##get the cached matrix
               temp_inverse <- solve(basematrix) ##compute the inverse
               makeCacheMatrix$setInverse(temp_inverse) ##cache the inverse
               return(temp_inverse) #return the cached inverse
           } 
           else { ##if the inverse is already cached
               return(temp_inverse) ##return the cached inverse
           }
}
