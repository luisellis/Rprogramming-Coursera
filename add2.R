add2 <- function(x, y) {
  x + y
}

above10 <- function(x) {
  use <- x >10
  x[use]
}

above <- function(x,n = 10) {
  use1 <- x>n
  x[use1]
}
