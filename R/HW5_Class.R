## HW5 Class/Methods

#' @import methods
#' @importFrom graphics abline legend points
NULL

#' Sparse Vector class
#'
#'An S4 class that represents sparse numeric vectors, or vectors which contain mostly zeros
#'
#'@slot value a numeric vector containing the non-zero elements of the vector
#'@slot pos an integer vector with the positions of the non-zero elements
#'@slot length a single integer that indicates the full length of the vector
#'@export
setClass(
    Class = "sparse_numeric",
    slots = c(
        value = "numeric",
        pos = "integer",
        length = "integer"
    )
)

setValidity(
  Class = 'sparse_numeric',
  method = function(object) {
    msgs <- character() # vector to store error messages

    # check 1: value and pos must have the same length
    if (length(object@value) != length(object@pos)) {
      msgs <- c(msgs, "slots 'value' and 'pos' must have the same length")
    }

    # check 2: length must be a single integer
    if (length(object@length) != 1) {
      msgs <- c(msgs, "slot 'length' must be a single integer")
    }

    # check 3: length must be non-negative
    else if (object@length < 0) {
      msgs <- c(msgs, "slot 'length' must be non-negative")
    }

    # check 4: all pos values must be >= 1
    if (any(object@pos < 1)) {
      msgs <- c(msgs, "all positions in 'pos' must be 1 or greater")
    }

    # check 5: all pos values must be <= length
    if (any(object@pos > object@length)) {
      msgs <- c(msgs, "all positions in 'pos' must be less than or equal to 'length'")
    }

    # check 6: pos values must be unique
    if (anyDuplicated(object@pos) != 0) {
      msgs <- c(msgs, "all positions in 'pos' must be unique")
    }

    # check 7: pos values in order
    if (!is.na(is.unsorted(object@pos)) && is.unsorted(object@pos)) {
      msgs <- c(msgs, "positions in 'pos' must be sorted in increasing order")
    }

    # check 8: value slot should not contain explicit zeros
    if (any(object@value == 0)) {
      msgs <- c(msgs, "slot 'value' should not contain zeros (use coercion to fix)")
    }

    # return messages or TRUE if all tests passed
    if (length(msgs) == 0) {
      TRUE
    } else {
      msgs
    }
  }
)


#' Sparse Addition
#'
#' Adds two sparse_numeric vectors
#'
#' @param x a sparse_numeric object
#' @param y a sparse_numeric object
#' @param ... additional arguments
#' @return A new sparse_numeric object that represents the sum of x and y
#' @export
setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))

#' @describeIn sparse_add Add two sparse_numeric vectors
setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"), function(x, y) {

  if (x@length != y@length) {
    stop("sparse vectors are not the same length")
  }

  len_x <- length(x@pos)
  len_y <- length(y@pos)

  # pre-allocate result vectors for efficiency (max possible size)
  max_len <- len_x + len_y
  res_val <- numeric(max_len)
  res_pos <- integer(max_len)

  i <- 1 # index for x
  j <- 1 # index for y
  k <- 1 # index for result

  # iterate through each position
  while (i <= len_x && j <= len_y) {
    if (x@pos[i] < y@pos[j]) {
      res_val[k] <- x@value[i]
      res_pos[k] <- x@pos[i]
      i <- i + 1
      k <- k + 1
    } else if (x@pos[i] > y@pos[j]) {
      res_val[k] <- y@value[j]
      res_pos[k] <- y@pos[j]
      j <- j + 1
      k <- k + 1
    } else { # x@pos[i] == y@pos[j]
      sum_val <- x@value[i] + y@value[j]
      if (sum_val != 0) {
        res_val[k] <- sum_val
        res_pos[k] <- x@pos[i]
        k <- k + 1
      }
      i <- i + 1
      j <- j + 1
    }
  }

  # add remaining elements from x
  while (i <= len_x) {
    res_val[k] <- x@value[i]
    res_pos[k] <- x@pos[i]
    i <- i + 1L
    k <- k + 1L
  }

  # add remaining elements from y
  while (j <= len_y) {
    res_val[k] <- y@value[j]
    res_pos[k] <- y@pos[j]
    j <- j + 1L
    k <- k + 1L
  }

  # trim the pre-allocated vectors
  k_final <- k - 1

  new("sparse_numeric",
      value = res_val[1L:k_final],
      pos = res_pos[1L:k_final],
      length = x@length
  )
})

#' Sparse Multiplication
#'
#' Multiplies two sparse_numeric vectors element-wise.
#'
#' @param x A sparse_numeric object.
#' @param y A sparse_numeric object.
#' @param ... Additional arguments.
#' @return A new sparse_numeric object representing the product.
#' @export
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))

#' @describeIn sparse_mult Multiply two sparse_numeric vectors
setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"), function(x, y) {
  if (x@length != y@length) {
    stop("sparse vectors are not the same length")
  }

  len_x <- length(x@pos)
  len_y <- length(y@pos)

  # pre-allocate (max size is the shorter of the two)
  max_len <- min(len_x, len_y)

  # Handle empty inputs
  if (max_len == 0) {
    return(new("sparse_numeric", length = x@length))
  }

  res_val <- numeric(max_len)
  res_pos <- integer(max_len)

  i <- 1
  j <- 1
  k <- 1

  while (i <= len_x && j <= len_y) {
    if (x@pos[i] < y@pos[j]) {
      i <- i + 1
    } else if (x@pos[i] > y@pos[j]) {
      j <- j + 1
    } else { # x@pos[i] == y@pos[j]
      prod_val <- x@value[i] * y@value[j]
      if (prod_val != 0) {
        res_val[k] <- prod_val
        res_pos[k] <- x@pos[i]
        k <- k + 1
      }
      i <- i + 1
      j <- j + 1
    }
  }

  # no remaining elements to add

  # trim
  k_final <- k - 1L

  if (k_final == 0) {
    return(new("sparse_numeric", length = x@length))
  }

  new("sparse_numeric",
      value = res_val[1L:k_final],
      pos = res_pos[1L:k_final],
      length = x@length
  )
})

#' Sparse Subtraction
#'
#' Subtracts one sparse_numeric vector from another.
#'
#' @param x A sparse_numeric object.
#' @param y A sparse_numeric object.
#' @param ... Additional arguments.
#' @return A new sparse_numeric object representing the difference.
#' @export
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))

#' @describeIn sparse_sub Subtract two sparse_numeric vectors
setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"), function(x, y) {
  if (x@length != y@length) {
    stop("sparse vectors are not the same length")
  }

  len_x <- length(x@pos)
  len_y <- length(y@pos)

  # Pre-allocate result vectors
  max_len <- len_x + len_y
  res_val <- numeric(max_len)
  res_pos <- integer(max_len)

  i <- 1
  j <- 1
  k <- 1

  while (i <= len_x && j <= len_y) {
    if (x@pos[i] < y@pos[j]) {
      res_val[k] <- x@value[i]
      res_pos[k] <- x@pos[i]
      i <- i + 1
      k <- k + 1
    } else if (x@pos[i] > y@pos[j]) {
      res_val[k] <- -y@value[j] # Negate y's value
      res_pos[k] <- y@pos[j]
      j <- j + 1
      k <- k + 1
    } else { # x@pos[i] == y@pos[j]
      diff_val <- x@value[i] - y@value[j] # Subtract
      if (diff_val != 0) {
        res_val[k] <- diff_val
        res_pos[k] <- x@pos[i]
        k <- k + 1
      }
      i <- i + 1
      j <- j + 1
    }
  }

  # add remaining elements from x
  while (i <= len_x) {
    res_val[k] <- x@value[i]
    res_pos[k] <- x@pos[i]
    i <- i + 1
    k <- k + 1
  }

  # add remaining (negated) elements from y
  while (j <= len_y) {
    res_val[k] <- -y@value[j] # Negate
    res_pos[k] <- y@pos[j]
    j <- j + 1
    k <- k + 1
  }

  # trim
  k_final <- k - 1L

  new("sparse_numeric",
      value = res_val[1L:k_final],
      pos = res_pos[1L:k_final],
      length = x@length
  )
})

#' Sparse Cross Product
#'
#' Calculates the dot product of two sparse_numeric vectors.
#'
#' @param x A sparse_numeric object.
#' @param y A sparse_numeric object.
#' @param ... Additional arguments.
#' @return A single numeric value.
#' @export
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

#' @describeIn sparse_crossprod Calculate dot product
setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"), function(x, y) {
  if (x@length != y@length) {
    stop("sparse vectors are not the same length")
  }

  len_x <- length(x@pos)
  len_y <- length(y@pos)

  i <- 1
  j <- 1
  total_sum <- 0

  while (i <= len_x && j <= len_y) {
    if (x@pos[i] < y@pos[j]) {
      i <- i + 1
    } else if (x@pos[i] > y@pos[j]) {
      j <- j + 1
    } else { # x@pos[i] == y@pos[j]
      total_sum <- total_sum + (x@value[i] * y@value[j])
      i <- i + 1
      j <- j + 1
    }
  }

  return(total_sum)
})


# change operators to use defined methods

#' @rdname sparse_add
#' @aliases +,sparse_numeric,sparse_numeric-method
#' @param e1 a sparse numeric object
#' @param e2 a sparse numeric object
#' @export
setMethod("+", c("sparse_numeric", "sparse_numeric"), function(e1, e2) {
  sparse_add(e1, e2)
})

#' @rdname sparse_sub
#' @aliases -,sparse_numeric,sparse_numeric-method
#' @param e1 a sparse numeric object
#' @param e2 a sparse numeric object
#' @export
setMethod("-", c("sparse_numeric", "sparse_numeric"), function(e1, e2) {
  sparse_sub(e1, e2)
})

#' @rdname sparse_mult
#' @aliases *,sparse_numeric,sparse_numeric-method
#' @param e1 a sparse numeric object
#' @param e2 a sparse numeric object
#' @export
setMethod("*", c("sparse_numeric", "sparse_numeric"), function(e1, e2) {
  sparse_mult(e1, e2)
})


# coercion


#' Coerce numeric to sparse_numeric
#' @name as.sparse_numeric
#' @family coercion
setAs("numeric", "sparse_numeric", function(from) {

  # find indices of non-zero elements
  pos <- which(from != 0)

  # get the non-zero values
  value <- from[pos]

  # get the total length
  len <- length(from)

  # create the new object
  new("sparse_numeric",
      value = value,
      pos = as.integer(pos),
      length = as.integer(len)
  )
})


#' Coerce sparse_numeric to numeric
#' @name as.numeric
#' @family coercion
setAs("sparse_numeric", "numeric", function(from) {

  # create a zero vector of the correct length
  vec <- numeric(from@length)

  # fill in the non-zero values at their positions
  vec[from@pos] <- from@value

  # return the dense vector
  return(vec)
})


#' Show sparse_numeric
#'
#' Prints a summary and the head of the vector.
#' @param object A sparse_numeric object.
#' @export
setMethod("show", "sparse_numeric",
          function(object) {

            cat("sparse_numeric vector of length", object@length, "\n")
            cat("with", length(object@value), "non-zero entries.\n")

            # determine how many elements to show (like default R print)
            n_show <- min(object@length, getOption("max.print", 6L))

            if (n_show == 0) {
              cat("numeric(0)\n")
              return(invisible(NULL))
            }

            # reconstruct the head of the dense vector
            dense_head <- numeric(n_show)

            # find which sparse elements fall into this head
            head_indices <- which(object@pos <= n_show)

            if (length(head_indices) > 0) {
              # get their positions and values
              pos_in_head <- object@pos[head_indices]
              val_in_head <- object@value[head_indices]

              # fill them into the dense head vector
              dense_head[pos_in_head] <- val_in_head
            }

            # print the reconstructed head using R's default numeric printing
            print.default(dense_head, quote = FALSE)

            # indicate if there are more elements not shown
            if (object@length > n_show) {
              cat("... (", object@length - n_show, " more elements)\n", sep = "")
            }
          }
)

#' Plot Overlapping Elements
#'
#' Plots the overlapping non-zero elements of two sparse_numeric vectors.
#' X-axis represents position, Y-axis represents value.
#'
#' @param x A sparse_numeric object.
#' @param y A sparse_numeric object.
#' @param ... Additional graphical parameters.
#' @export
setMethod("plot", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) {

            # check for length equality
            if (x@length != y@length) {
              stop("sparse vectors are not the same length")
            }

            len_x <- length(x@pos)
            len_y <- length(y@pos)

            # pre-allocate (max size is the shorter of the two)
            max_len <- min(len_x, len_y)

            # handle empty inputs
            if (max_len == 0) {
              warning("No overlapping non-zero elements to plot.")
              plot(NULL, xlim = c(1, x@length), ylim = c(-1, 1),
                   xlab = "Position", ylab = "Value",
                   main = "Overlapping Non-Zero Elements (None)", ...)
              return(invisible(NULL))
            }

            common_pos <- integer(max_len)
            val_x_overlap <- numeric(max_len)
            val_y_overlap <- numeric(max_len)

            i <- 1
            j <- 1
            k <- 1

            # find overlaps
            while (i <= len_x && j <= len_y) {
              if (x@pos[i] < y@pos[j]) {
                i <- i + 1L
              } else if (x@pos[i] > y@pos[j]) {
                j <- j + 1L
              } else { # x@pos[i] == y@pos[j] (Found overlap)
                common_pos[k] <- x@pos[i]
                val_x_overlap[k] <- x@value[i]
                val_y_overlap[k] <- y@value[j]
                k <- k + 1L
                i <- i + 1L
                j <- j + 1L
              }
            }

            # trim the vectors to the actual number of overlaps
            k_final <- k - 1L
            if (k_final == 0) {
              warning("No overlapping non-zero elements to plot.")
              plot(NULL, xlim = c(1, x@length), ylim = c(-1, 1),
                   xlab = "Position", ylab = "Value",
                   main = "Overlapping Non-Zero Elements (None)", ...)
              return(invisible(NULL))
            }

            common_pos <- common_pos[1L:k_final]
            val_x_overlap <- val_x_overlap[1L:k_final]
            val_y_overlap <- val_y_overlap[1L:k_final]


            # determine plot limits
            xlim <- c(1, x@length)

            # y-axis spans all overlapping values, plus 0
            all_overlap_values <- c(0, val_x_overlap, val_y_overlap)
            ylim <- range(all_overlap_values, na.rm = TRUE)

            # set up the empty plot
            plot(NULL,
                 type = "n",
                 xlim = xlim,
                 ylim = ylim,
                 xlab = "Position",
                 ylab = "Value",
                 main = "Overlapping Non-Zero Elements",
                 ...)

            # draw the zero line for reference
            abline(h = 0, lty = 2, col = "gray")

            # plot points for vector x and y at overlap positions
            points(common_pos, val_x_overlap,
                   pch = 19, # circle
                   col = "blue",
                   cex = 1.2)

            points(common_pos, val_y_overlap,
                   pch = 17, # triangle
                   col = "red",
                   cex = 1.2)

            # add a legend
            legend("topright",
                   legend = c("Vector x", "Vector y"),
                   col = c("blue", "red"),
                   pch = c(19, 17),
                   bty = "n")

            return(invisible(NULL))
          }
)


#' Norm of a sparse_numeric Object
#'
#' Operates on sparse vectors and computes the norm of the vector, which in this case is the square root of the sum of the squared individual elements of a vector.
#'
#' @param x A sparse_numeric vector
#' @export
setMethod("norm", "sparse_numeric", function(x) {
  return(sqrt(sum(x@value^2)))
})


#' Calculate Mean of Sparse Vector
#'
#' Calculates the arithmetic mean of the sparse vector, including zeros.
#'
#' @param x A sparse_numeric object.
#' @param ... Additional arguments passed to sum().
#' @return A numeric value.
#' @export
setMethod("mean", "sparse_numeric", function(x, ...) {
  if (x@length == 0L) return(NaN)
  # Sum of all elements is just sum of non-zero elements
  total_sum <- sum(x@value, ...)
  return(total_sum / x@length)
})

#' Standardize Sparse Vector
#'
#' Standardizes the vector (subtract mean, divide by SD).
#' Returns a dense numeric vector because standardization usually destroys sparsity.
#'
#' @param x A sparse_numeric object.
#' @return A dense numeric vector where each element is (x_i - mean) / sd.
#' @export
setGeneric("standardize", function(x) standardGeneric("standardize"))

#' @describeIn standardize Standardize a sparse_numeric vector
setMethod("standardize", "sparse_numeric", function(x) {
  if (x@length == 0L) return(numeric(0))
  if (x@length == 1L) return(NaN) # SD is undefined for length 1

  n <- x@length

  # Sum of sparse values is sum of all values
  sum_x <- sum(x@value)
  mean_x <- sum_x / n

  # Formula: sum((xi - mean)^2) = sum(xi^2) - 2*mean*sum(xi) + n*mean^2
  # sum(xi^2) is just sum(x@value^2) because 0^2 is 0
  sum_sq_x <- sum(x@value^2)

  # Variance = (sum_sq - (sum^2 / n)) / (n - 1)
  ss <- sum_sq_x - (sum_x^2 / n)
  if (ss < 0) ss <- 0

  sd_x <- sqrt(ss / (n - 1))

  if (sd_x == 0) {
    # If SD is 0, all values are identical
    return(rep(NaN, n))
  }

  # Default value for "zero" elements: (0 - mean) / sd
  default_val <- (0 - mean_x) / sd_x

  # Create a dense vector filled with the default standardized value
  res <- rep(default_val, n)

  # Update the positions that were originally non-zero
  # New value: (old_value - mean) / sd
  res[x@pos] <- (x@value - mean_x) / sd_x

  return(res)
})


