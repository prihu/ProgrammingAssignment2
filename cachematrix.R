## Below are two functions that are used to create a
## special object that stores a matrix and caches its inverse.

## Function to create the special "matrix" object to store matrix

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x <<-y
    i <<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) i <<-inverse
  getinverse<-function() i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Function to compute and return inverse and if cached, return cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data, ...)
  x$setinverse(i)
  i
}
