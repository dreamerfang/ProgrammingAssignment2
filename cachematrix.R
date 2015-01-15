## Put comments here that give an overall description of what your
## functions do
## The cachematrix functions here are creating a closure here to cache the inverse 
## of certain matrix (assuming the matrix is inversible)

## Write a short comment describing this function
## The makeCacheMatrix function here is to create a special matrix that is actually 
##  to set a value to a matrix, get values of a matrix, set the inverse of the matrix
## and get the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
        m<- NULL
        
        set<-function(y){
                x<<- y
                m<<- NULL
        }
        get<-function() x
        setMatrix<-function(solve) m<<-solve
        getMatrix<-function() m
        
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
}


## Write a short comment describing this function
## The function here is calculation the inverse of the the matrix created before.
## Tf there is already cache value of the matrix, then it just print the inverse 
## result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getMatrix()
        if(!is.null(m)){
                message("getting cached matrix")
                return(m)     
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setMatrix(m)
        m
}