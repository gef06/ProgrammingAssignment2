## The first function makeCacheMatrix() takes a matrix as an input argument and returns an object
## of type makeCacheMatrix. This contains two data objects (x & inv) and four 
## functions (get, set, setinverse & getinverse). These functions can be accessed 
## via a list contained in the makeCacheMatrix environment.

## The second function cacheSolve() takes a makeCacheMatrix oject as its argument. It looks 
## within this to see if there is already a valid inverse matrix cached. If so,
## it will return this along with a message to tell you it has done so. Otherwise,
## it will caculate the inverse of the original matrix and return this, as well
## as storing it within the makeCacheMatrix object for later use.

## Function to create an R object that stores a matrix & its inverse

makeCacheMatrix <- function(x = matrix()) { # initializes x as a function argument
  inv <- NULL # initializes m as an obejct within makeCacheMatrix() environment
  # define some "getters" and "setters"
  set <- function(y) {
    x <<- y # assigns y to x in the parent environment, i.e. makeCacheMatrix() environment
    inv <<- NULL # will clear inv if there is already a valid inverse cached there
  }
  get <- function() x # will retrieve from parent environment due to lexical scoping
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv # as get() above, will retreive from parent environment
  list(set = set, # assigns each function as a element within a list
       get = get, # naming each element means they can later be accessed using $ form of extract operator
       setinverse = setinverse,
       getinverse = getinverse) # this list is returned to parent environment
  
} # retuns an object of type makeCacheMatrix() to be used by cacheSolve


## Function to return inverse matrix

cacheSolve <- function(x, ...) { # takes makeCacheMatrix object as argument
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse() # initializes inv in cacheSolve environment
  if(!is.null(inv)){ # if there is a value in inv
    message("getting cached data")
    return(inv) # return the cached inverse matrix & the above message
  }
  data <- x$get() # if not, ten access get() from makeCacheMatrix object
  inv <- solve(data, ...) # solve for inverse matrix
  x$setinverse(inv) # set this as the inverse matrix in you makeCacheMatrix object
  inv # return inverse matrix
}
