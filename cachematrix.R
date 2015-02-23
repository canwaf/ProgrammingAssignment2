## Function written for r-prog-011 week 3 programming assignment
## Forked from: https://github.com/rdpeng/ProgrammingAssignment2

## makeCacheMatrix: support routines: setting/retrieving the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
## New function means new lexical scope.  By default, the named function parameters become variables in the function scope.
## The variable makeCacheMatrix belongs to the top scope and its value is a function.     
      # create the variable in the correct enviroment to check
      # if it's not null, we'll do some stuff later
      m <- NULL
      # A new variable m is created and initialized
            
      set <- function(y = matrix()) {
            # Hey, a new function means a new lexical scope which we call scope "makeCacheMatrix"
            # As before the variable y is defined.
            # In addition, the variable "set" is defined in the scope "makeCacheMatrix" and its value is a function
            # However the variable y is defined in the scope of the function "set" which is in the scope of the function "makeCacheMatrix"
            x <<- y
            # The variable x has already been created and belongs to the scope "makeCacheMatrix"
            m <<- NULL
            # Like variable x, the variable m belongs to the scope of "makeCacheMatrix"
            # The function "set" returns the value of the last statement which is variable m
      }
      # We have now left the lexical scope of the function "set" and re-entered the scope of "makeCacheMatrix"
      get <- function() x
      # get is a new function that returns the variable x in the scope of "makeCacheMatrix"
      setcache <- function(solve) m <<- solve
      # setcache is a new function that returns the updated value of variable m in the scope of "makeCacheMatrix"
      getcache <- function() m
      # getcache is a new function that returns the value of the variable m in the scope of "makeCacheMatrix"
      list(set = set, get = get,
           setcache = setcache,
           getcache = getcache)
           # This returns a list which contains the functions set, get, setcache and getcache.
           # these functions are returned with the names "set", "get", "setcache" and "getcache"
           # Note none of the variables "m", "x" and "y" are not returned and are invisible outside of the function "makeCacheMatrix" scope
           
}


## cacheSolve: check if vector already inversed
##   \> if vector, return cached inverse
##   \> if !vector, caculate & return inverse, cache results

cacheSolve <- function(x, ...) {
## This creates a new lexical scope and function called cacheSolve in the top level.
## The variable x is created within this new function and assigned the value of what is passed to the function.
      m <- x$getcache()                     #retrieve cache
      # m is a new variable and is assigned the value of getcache called on the variable x
      if(!is.null(m)) {                     #are you there m?
            # We are testing the value of the local variable "m"
            message("getting cached data")  #rest easy, Bond. M's there.
            return(m)
            # Stop now and return the value of "m"
      }
      data <- x$get()
      # data is a new variable and is assigned the value of get called on the variable x
            
      m <- solve(data, ...)
      # The current variable m is assigned a new value by the solve function run on the value of "data"
      x$setcache(m)
      # Now here's the magic.
      # We call the function setcache on variable "x" and pass it the value "m"
            # The value "m" in the current scope is passed to the parameter "solve" of function "setcache"
            # In the scope of the function setcache, the value "m" is set to the value of "solve" 
            # which was set to the value "m" of the current scope.
      m 
      # The current scope value of "m" is returned.
}

# Now, the most important thing to realize is that a function contains a set of instructions to create the output, 
# but it also contains a lookup table of variable names to values for variables defined in the function.
# ALSO it adds variables that are used in the function that refer to the variables in higher scopes.
# Now when you return a function "setcache" within another function "makeCacheMatrix", the lookup table 
# of function "setcache" remembers
# the variable "m" that beloned to the scope of the function called "makeCacheMatrix".
# The function "setcache" has closed over the parent variable "m" and this is why the function "setcache" is called a closure.
# Now when calling the function/closure "setcache", the lexical environment is also restored so the function 
# now has access to the variable "m" of the function "makeCacheMatirx" and actually sets its value.
# If you then call the function getcache, you will enter its lexical environment which also has an entry for the variable "m"
# of the function makeCacheMatrix.
# The value of this variable will be returned.
# Some languages support closures and some do not.
# Read up on "closure" in wikipedia that has a more straight forward example of closures.
