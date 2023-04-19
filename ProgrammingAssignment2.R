############################ Load Packages ############################
library(magrittr)
library(dplyr)  
library(tidyverse)
library(haven)

############################ Answer ############################

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve #mean was changed to solve
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) #mean was changed to solve
  x$setInv(m)
  m
}

#confirmation
mat2_2 <- makeCacheMatrix(matrix(1:4,2,2))
mat2_2$get()
cacheSolve(mat2_2)
cacheSolve(mat2_2) #printed "getting cached data" as this is 2nd time 




##Please ignore following programs as these are the sample programs 
##shown in the explanation of this task. (only for reference)

############################ example (makevector, cachemean) #############

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

#confirmation
mean1_10 <- makeVector((1:10))
mean1_10$get()
cachemean(mean1_10)