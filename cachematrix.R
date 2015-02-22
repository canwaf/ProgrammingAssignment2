## Function written for r-prog-011 week 3 programming assignment
## Forked from: https://github.com/rdpeng/ProgrammingAssignment2

## makeCacheMatrix: support routines: setting/retrieving the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
      
      # create the variable in the correct enviroment to check
      # if it's not null, we'll do some stuff later
      m <- NULL
            
      set <- function(y = matrix()) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setcache <- function(solve) m <<- solve
      getcache <- function() m
      list(set = set, get = get,
           setcache = setcache,
           getcache = getcache)
}


## cacheSolve: check if vector already inversed
##   \> if vector, return cached inverse
##   \> if !vector, caculate & return inverse, cache results

cacheSolve <- function(x, ...) {
      m <- x$getcache()                     #retrieve cache
      if(!is.null(m)) {                     #are you there m?
            message("getting cached data")  #rest easy, Bond. M's there.
            return(m)
      }
      data <- x$get()
            
      m <- solve(data, ...)
      
      x$setcache(m)
      
      m 
}