## Programming assingment 2
## this code caches the inverse of a matrix in order to 
## avoid computing it repeatedly  when doing for example
## a loop.



## this function makes a list 4 elemtns, the first set the 
## value of the matrix, the secodn get the value of the matrix,
## the third set the value of the inverse and the last get the 
## value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
                }
        get<-function()x
        setinverse<-function(solve)i<<-solve
        getinverse<-function()i
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## this function takes the list created above and calculates
## the inverse of the matrix. But before calculating it
## look for the solution in the cache memory. The solution
## will be in the cache if it was calculated before.

cacheSolve <- function(x, ...) {
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



###############################################################
## Example
z<-matrix(c(4, 2, 7, 6), 2, 2) #to create a matrix
z
solve(z) #to calculate the inverse of the matrix directly

y<-makeCacheMatrix(z) #creates the list
y
cacheSolve(y) #to calculate the inverse using the function

cacheSolve(y) #run the function again, notice the message
#"getting cached data" because the solution was pulled from
#the cache memory rather than calculate it again.



##THE END
#######
