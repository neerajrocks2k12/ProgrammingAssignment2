## Put comments here that give an overall description of what your
## functions do

## function creates a matrix and stores its inverse 

makeCacheMatrix <- function(x = matrix()) 
{
  inv<-NULL
  set<-function(y)
  {
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function()inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## this function checks for cached inverse, if not found then it computes it and stores it

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv))
  {
    message("retrieving cached data")
    return (inv)
  }
  
  mat<-x$get()
  inv<-solve(mat,...)
  x$setinverse(inv)
  
  inv
}
