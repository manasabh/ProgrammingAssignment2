## This assignemnt creates two functions in order to compute inverse of a matrix and cache its value. 
.It uses a function to get the value from its cache hence skip any reductant computation.
If the value is not found in cache then computes the inverse and stores it in cache.
##  

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      set <- function(y){
              x<<-y
              m<<-NULL
      }
      get <- function() x
      setinverse <- function(solve)m<<-solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getinverse()
         if(!is.null(m)){
                  message("getting cached data")
                  return(m)
          }
          data <- x$get()
          m <- solve(data, ...)
          x$setinverse(m)
          m
}
