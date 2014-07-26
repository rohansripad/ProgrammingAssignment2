## The file contains two functions/objects that essentially takes advantage of scoping rules in 
## R language and caches the results of time consuming calculation of inverse of a matrix, and
## returns the cached value of the inverse when required if the original matrix hasn't chanched

## makeCacheMatrix takes input of a matrix, for which we would need an inverse, and return a list
## containing four functions, that sets the matrix, gets the matrix, solves for inverse of matrix
## and gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        #m stores inverse of matrix
        m<-NULL
        
        #set is used to set the matrix in x through input y, and initialize inverse (m) to NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        
        #get is used to retrieve original matrix
        get<-function() x
        
        #solveInverse calculates the inverse of the matrix and assigns it to m
        setInverse <- function(inverse) m <<- inverse
        
        #getInverse retrieves the inverse of the matrix
        getInverse <- function() m
        
        #the object returns a list of four functions
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve, essentially returns the inverse of matrix either cached or un cached, by taking
## input of a list of functions returned by makeCache Matrix.

cacheSolve <- function(x, ...) {
        ## m gets the inverse of a matrix through x, which is output of makeCacheMatrix
        ## which is a list of functions
        m<-x$getInverse()
        
        #returns cached inverse if avaialble
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        ## if cache unavailable(either due to change in matrix or no cache), we calculate/solve
        ## for the inverse, store in cache and return the value
        data<-x$get()
        m<-solve(data)
        x$setInverse(m)
        m
}
