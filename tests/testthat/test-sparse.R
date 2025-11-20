test_that("check validity method exists", {
  expect_false(is.null(getValidity(getClassDef("sparse_numeric"))))
})

test_that("check validity method accepts valid objects", {
  expect_true({
    x <- new("sparse_numeric", value = c(1, 2), pos = c(1L, 5L), length = 10L)
    validObject(x)
  })
})

test_that("validity catches length mismatches", {
  x <- new("sparse_numeric", value = c(1, 2), pos = c(1L, 5L), length = 10L)

  # Test: value and pos length mismatch
  x@value <- c(1, 2, 3)
  expect_error(validObject(x), "same length")

  # Test: length is not length 1
  x@value <- c(1, 2); x@length <- c(10L, 10L)
  expect_error(validObject(x), "single integer")
})



test_that("validity catches invalid positions and lengths", {
  x <- new("sparse_numeric", value = c(1), pos = c(1L), length = 10L)

  # Test: Negative length
  x@length <- -5L
  expect_error(validObject(x), "non-negative")

  # Reset
  x@length <- 5L

  # Test: Pos out of bounds (> length)
  x@pos <- c(6L)
  expect_error(validObject(x), "less than or equal")

  # Test: Pos < 1
  x@pos <- c(0L)
  expect_error(validObject(x), "1 or greater")
})

test_that("validity catches sorting and uniqueness issues", {
  # 1. Start with a valid object
  x <- new("sparse_numeric", value = c(1, 2), pos = c(2L, 5L), length = 10L)

  # 2. Manually break it (Unsorted)
  x@pos <- c(5L, 2L)
  expect_error(validObject(x), "sorted")

  # 3. Manually break it (Duplicates)
  x@pos <- c(2L, 2L)
  expect_error(validObject(x), "unique")

  # 4. Manually break it (Explicit zeros in value)
  x@pos <- c(1L, 2L) # Reset pos to valid
  x@value <- c(1, 0)
  expect_error(validObject(x), "contain zeros")
})

test_that("check coercion numeric -> sparse", {
  vec <- c(0, 0, 5, 0, 2)
  sp <- as(vec, "sparse_numeric")
  expect_s4_class(sp, "sparse_numeric")
  expect_equal(sp@value, c(5, 2))
  expect_equal(sp@pos, c(3L, 5L))
})

test_that("check coercion sparse -> numeric", {
  # This covers the as(x, "numeric") method
  vec <- c(0, 0, 5, 0, 2)
  sp <- as(vec, "sparse_numeric")
  back_to_vec <- as(sp, "numeric")
  expect_equal(vec, back_to_vec)
})


test_that("methods exist", {
  expect_no_error(getMethod("show", "sparse_numeric"))
  expect_no_error(getMethod("plot", c("sparse_numeric", "sparse_numeric")))
  expect_no_error(getMethod("+", c("sparse_numeric", "sparse_numeric")))
  expect_no_error(getMethod("-", c("sparse_numeric", "sparse_numeric")))
  expect_no_error(getMethod("*", c("sparse_numeric", "sparse_numeric")))
  expect_no_error(getMethod("mean", "sparse_numeric"))
})

test_that("generics exist", {
  expect_true(isGeneric("sparse_add"))
  expect_true(isGeneric("sparse_mult"))
  expect_true(isGeneric("sparse_sub"))
  expect_true(isGeneric("sparse_crossprod"))
  expect_true(isGeneric("standardize"))
})

test_that("formals are correct", {
  expect_true(length(formals(sparse_add)) >= 2L)
  expect_true(length(formals(sparse_mult)) >= 2L)
  expect_true(length(formals(sparse_sub)) >= 2L)
  expect_true(length(formals(sparse_crossprod)) >= 2L)
})


test_that("sparse_add logic covers all branches", {
  # Case 1: Overlap (i == j), Left (i < j), Right (i > j)
  v1 <- c(0, 1, 0, 2, 0)
  v2 <- c(0, 1, 3, 0, 5)
  # Sum:  0, 2, 3, 2, 5

  x <- as(v1, "sparse_numeric")
  y <- as(v2, "sparse_numeric")

  res <- sparse_add(x, y)
  expect_equal(as(res, "numeric"), v1 + v2)

  # Test Operator Alias
  expect_equal(as(x + y, "numeric"), v1 + v2)
})


test_that("sparse_add error handling", {
  x <- as(c(1, 2), "sparse_numeric")
  y <- as(c(1, 2, 3), "sparse_numeric")
  expect_error(sparse_add(x, y), "same length")
})


test_that("sparse_sub logic covers all branches", {
  v1 <- c(0, 10, 0, 5, 0, 1, 0)
  v2 <- c(0, 2,  3, 5, 0, 0, 1) # Note: 5-5=0 (cancellation)
  # Sub:  0, 8, -3, 0, 0

  x <- as(v1, "sparse_numeric")
  y <- as(v2, "sparse_numeric")

  res <- sparse_sub(x, y)

  expect_equal(as(res, "numeric"), v1 - v2)
  expect_false(4 %in% res@pos) # Index 4 should be gone because 5-5=0

  # Test Operator Alias
  expect_equal(as(x - y, "numeric"), v1 - v2)
})

test_that("sparse_sub error handling", {
  x <- new("sparse_numeric", length=5L)
  y <- new("sparse_numeric", length=6L)
  expect_error(sparse_sub(x, y), "same length")
})


test_that("sparse_mult logic covers all branches", {
  v1 <- c(1, 0, 2, 4, 0)
  v2 <- c(5, 3, 0, 2, 0)
  # Mult: 5, 0, 0, 8, 0

  x <- as(v1, "sparse_numeric")
  y <- as(v2, "sparse_numeric")

  res <- sparse_mult(x, y)
  expect_equal(as(res, "numeric"), v1 * v2)

  # Test Operator Alias
  expect_equal(as(x * y, "numeric"), v1 * v2)
})

test_that("sparse_mult handles empty overlap", {
  # No common positions
  v1 <- c(1, 0)
  v2 <- c(0, 1)

  x <- as(v1, "sparse_numeric")
  y <- as(v2, "sparse_numeric")

  res <- sparse_mult(x, y)
  expect_equal(length(res@value), 0)
})

test_that("sparse_mult error handling", {
  x <- new("sparse_numeric", length=5L)
  y <- new("sparse_numeric", length=6L)
  expect_error(sparse_mult(x, y), "same length")
})


test_that("sparse_crossprod logic", {
  v1 <- c(1, 2, 0, 4)
  v2 <- c(2, 0, 3, 1)
  # 1*2 + 2*0 + 0*3 + 4*1 = 2 + 0 + 0 + 4 = 6

  x <- as(v1, "sparse_numeric")
  y <- as(v2, "sparse_numeric")

  res <- sparse_crossprod(x, y)
  expect_equal(res, 6)
})

test_that("sparse_crossprod error handling", {
  x <- new("sparse_numeric", length=2L)
  y <- new("sparse_numeric", length=3L)
  expect_error(sparse_crossprod(x, y), "same length")
})


test_that("mean works correctly", {
  v <- c(0, 0, 3, 9) # Sum=12, Len=4, Mean=3
  x <- as(v, "sparse_numeric")

  expect_equal(mean(x), 3)

  # Test empty/zero length (NaN case)
  x_empty <- new("sparse_numeric", length=0L)
  expect_true(is.nan(mean(x_empty)))
})

test_that("standardize works correctly", {
  v <- c(1, 2, 3, 4, 5)
  x <- as(v, "sparse_numeric")

  # Should return a dense numeric vector
  std_x <- standardize(x)
  expect_true(is.numeric(std_x))
  expect_false(is(std_x, "sparse_numeric"))

  # Compare with R's built-in scale (returns matrix, so drop attributes)
  expected <- as.vector(scale(v))
  expect_equal(std_x, expected)
})

test_that("standardize edge cases", {
  # Length 0
  x0 <- new("sparse_numeric", length=0L)
  expect_equal(length(standardize(x0)), 0L)

  # Length 1 (SD undefined)
  x1 <- as(c(5), "sparse_numeric")
  expect_true(is.nan(standardize(x1)))

  # Constant vector (SD = 0)
  v_const <- c(2, 2, 2)
  x_const <- as(v_const, "sparse_numeric")
  # Division by zero SD usually results in NaNs
  expect_true(all(is.nan(standardize(x_const))))
})

test_that("show method runs", {
  # We just want to ensure no errors occur during printing
  x <- as(c(1, 0, 2, 0, 0, 3, 4, 5, 6, 7, 8), "sparse_numeric")

  # Capture output to prevent clogging console, but ensure code runs
  expect_output(show(x), "sparse_numeric vector")

  # Test empty
  x_empty <- new("sparse_numeric", length=0L)
  expect_output(show(x_empty), "numeric\\(0\\)")
})

test_that("plot method runs", {
  # 1. Overlap case
  x <- as(c(1, 0, 2), "sparse_numeric")
  y <- as(c(2, 0, 1), "sparse_numeric")

  pdf(NULL) # Prevent plot window from opening
  expect_no_error(plot(x, y))
  dev.off()

  # 2. No overlap case (triggers warning)
  x_no <- as(c(1, 0), "sparse_numeric")
  y_no <- as(c(0, 1), "sparse_numeric")

  pdf(NULL)
  expect_warning(plot(x_no, y_no), "No overlapping")
  dev.off()

  # 3. Error case
  expect_error(plot(x, as(c(1,2,3,4), "sparse_numeric")), "same length")
})
