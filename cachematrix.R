## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
###This makeCacheMatrix function create a inverse function, by setting a matrix, getting a matrix, creating function to inverse the matrix and getting the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function()x
    setinverse<-function(inverse) m<<-solve 
    getinverse<-function() m
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
### Here we create another function to first check if the inverse function is in the cache. If not, then use the previous defined function to get the inverse function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m<-x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setinverse(m)
    m
}