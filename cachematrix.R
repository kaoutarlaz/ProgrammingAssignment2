#The goal is to avoid recalculating the inverse of the matrix each time,
#it is as if we create a large environment that contains 4 " functions",
#then we call functions already created in order to calculate the inverse of the matrix. 

# makeCacheMatrix creates a list containing a 4 functions :
#set the value of the matrix
#get the value of the matrix
# set the value of inverse of the matrix
#get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  Iv <- NULL    # initialisation
  
  set <- function(y) {
    x <<- y      
    Iv <<- NULL   
  }
  get <- function() x
  setinverse_matrice <- function(inverse_matrice) Iv <<- inverse_matrice
  getinverse_matrice <- function() Iv
  list(set=set, get=get, setinverse_matrice=setinverse_matrice, getinverse_matrice=getinverse_matrice) 
}


#this function returns the inverse of the matrix ( if it's not null), 
#if it is null , it calls the above mentioned functions ( it avoids us to recalculate)
cacheSolve <- function(x) {
  inv <- x$getinverse_matrice() 
  if(!is.null(inv)) { #if isn't null
    message("we cache the matrix")
    return(Iv) 
  }
  matr <- x$get()
  Iv <- solve(matr)
  x$setinverse_matrice(Iv)
  Iv
}
