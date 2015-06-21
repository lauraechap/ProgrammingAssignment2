## Computes the inverse of a matrix unless the inverse is
## available in the cache

## Sets the value of the matrix, gets the value of the matrix,
## sets the value of the inverse, gets the value of the inverse

makeCacheMatrix <- function(x=matrix) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinv <- function(solve) m <<- solve(x)
  getinv <- function() m
  list(set=set, get=get,setinv=setinv,getinv=getinv)
}


## Checks if the inverse is calculated in the cache; uses 
# the cached value if available; calculates the inverse if it is not

cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data)
  x$setinv(m)
  m
}
