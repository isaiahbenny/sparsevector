# Standardize Sparse Vector

Standardizes the vector (subtract mean, divide by SD). Returns a dense
numeric vector because standardization usually destroys sparsity.

## Usage

``` r
standardize(x)

# S4 method for class 'sparse_numeric'
standardize(x)
```

## Arguments

- x:

  A sparse_numeric object.

## Value

A dense numeric vector where each element is (x_i - mean) / sd.

## Methods (by class)

- `standardize(sparse_numeric)`: Standardize a sparse_numeric vector
