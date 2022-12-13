#trial functions

output <- vector("integer", 0)
for (i in seq_along(x)) {
  output <- c(output, lengths(x[[i]]))
}
output


#col clounts which does the summary count only for appropriate columns
colcounts <- function(df, fun) {
  # create an empty vector which will store whether each
  # column is character or logical - i.e. "suitable"
  suitable_cols <- vector("logical", length(df))
  # test whether each column is suitable
  for (i in seq_along(df)) {
    suitable_cols[[i]] <- (is.character(df[[i]]) || is.logical(df[[i]]))
  }
  # find the indexes of the suitable columns
  idxs <- which(suitable_cols)
  # find the number of suitable columns
  n <- sum(suitable_cols)
  # create a vector to hold the results
  out <- vector("double", n)
  
  # apply the function only to suitable vectors
  for (i in seq_along(idxs)) {
    out[[i]] <- fun(df[[idxs[[i]]]])
  }
  # name the vector
  names(out) <- names(df)[idxs]
  out
}





col_sum2 <- function(df, f, ...) {
  map(keep(df, is.numeric), f, ...)
}
