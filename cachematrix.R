#The goal is to avoid recalculating the inverse of the matrix each time,
#it is as if we create a large environment that contains 4 " functions",
#then we call functions already created in order to calculate the inverse of the matrix. 

# makeCacheMatrix creates a list containing a 4 functions :
#set the value of the matrix
#get the value of the matrix
# set the value of inverse of the matrix
#get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL    # initialisation
  
  set <- function(y) {
    x <<- y      
    inv <<- NULL   
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) # we creat 4 objects
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
cacheSolve <- function(x) {
  inv <- x$getinverse() 
  if(!is.null(inv)) { #if isn't null
    message("we cache the matrix")
    return(inv) 
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}