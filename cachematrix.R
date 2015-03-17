## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
            print(x)
            m <- NULL        
            set <- function(y= matrix()) {
                  x <<- y
                  m <<- NULL
            }
            get <<- function() x ##assigns x to get for cacheSolve to call later 
            setinv <- function(inv=matrix()) m <<- inv ##allows for user to set the inverse matrix
            getinv <- function() m ##assigns getinv the value of m from the above function
            list(get = get, setinv=setinv, getinv=getinv, set=set) 

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getinv() ## calls getinv from makeCacheMatrix
         if(!is.null(m)) { ## if getinv is NOT empty, then the program will return the inverse matrix from the cache
          message("getting cached data")
           return(m) ##this is the inverse matrix from the cache. return ends function
          }  
         data <- x$get() #if getinv is empty then function solves input matrix
         m <-solve(data,...)
        x$setinv(m) #solved input matrix is defined to makeCacheMatrix, and stored in cache to m
        print(m)

}
