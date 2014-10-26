##this program is to create a square matrix and with cachesolve function get the inverse of the matrix.
makeCacheMatrix <- function(M = matrix()) { 
          i  <- NULL 
           set  <- function(Data){ 
                 M <<- Data 
                   i <<- NULL  
           } 
           get  <- function(){
           M 
           }
           setinverse  <- function(inverse) {
           i  <<- inverse 
           }
           getinverse  <- function() {
           i
           }
           ##list of methods
           list(set= set, get = get,  
                setinverse = setinverse,  
                getinverse = getinverse) 
      } 
## this function gives the inverse of the matrix given by 'makeCacheMatrix' function
cacheSolve <- function(M, ...) { 
         i  <- M$getinverse() 
          if (!is.null(i)){ 
                  message("Cached data") 
                 return(i) 
          } 
          data  <- M$get() 
          i  <- solve(data, ...) 
          M$setinverse(i) ## sets the inverse matrix
          i  ## returns the inverse matrix
  
}
 
