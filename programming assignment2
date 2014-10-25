makeCacheMatrix <- function(M = matrix()) { 
          i  <- NULL 
           set  <- function(Data){ 
                 M <<- Data 
                   i <<- NULL  
           } 
           get  <- function() M 
           setinverse  <- function(inverse) i  <<- inverse 
           getinverse  <- function() i 
           list(set= set, get = get,  
                setinverse = setinverse,  
                getinverse = getinverse) 
   
  
   } 

cacheSolve <- function(M, ...) { 
         i  <- M$getinverse() 
          if (!is.null(i)){ 
                  message("Cached data") 
                 return(i) 
          } 
          data  <- M$get() 
          i  <- solve(data, ...) 
          M$setinverse(i) 
          i 
  
}
 
