makeCacheMatrix <- function(x = matrix()) {
    m<- NULL
    set <-function(y) {
        x<<-y
        m<<-NULL
    }
    get <- function() x
    getInverse <- solve(x)
    setInverse <- function(inverse)
    m<<-inverse
    list(set = set, get = get,
    setInverse = setInverse,getInverse = getInverse)
}

cacheSolve <- function(x,...) {
    m<-makeCacheMatrix(x)$getInverse
    m<-NULL
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data<-makeCacheMatrix(x)$get()
    m <-solve(data)
    makeCacheMatrix(x)$setInverse(m)
    m
    
    
}
