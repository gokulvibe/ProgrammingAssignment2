#The functions find the inverse of a given matrix. If the inverse already exists, it gets the inverse from the cache.
#Else, it calculates the inverse.



#This function has the functions to:
#Set the value of the matrix, Get the value of the matrix, Set the value of the inverse, Get the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
        j <- NULL
        set <- function(y){
                x <<- y
                j <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse) j <<- inverse
        getInverse <- function() j 
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


#This function calculates the inverse of the matrix.
#It first checks whether the mean has already been calculated. If so, it gets the mean from the cache and skips the computation.
#Else it calculates the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        j <- x$getInverse()
        if(!is.null(j)){
                message("getting cached data")
                return(j)
        }
        mat <- x$get()
        j <- solve(mat,...)
        x$setInverse(j)
        j
}