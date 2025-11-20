# Sparse Multiplication

Multiplies two sparse_numeric vectors element-wise.

## Usage

``` r
sparse_mult(x, y, ...)

# S4 method for class 'sparse_numeric,sparse_numeric'
sparse_mult(x, y)

# S4 method for class 'sparse_numeric,sparse_numeric'
e1 * e2
```

## Arguments

- x:

  A sparse_numeric object.

- y:

  A sparse_numeric object.

- ...:

  Additional arguments.

- e1:

  a sparse numeric object

- e2:

  a sparse numeric object

## Value

A new sparse_numeric object representing the product.

## Methods (by class)

- `sparse_mult(x = sparse_numeric, y = sparse_numeric)`: Multiply two
  sparse_numeric vectors
