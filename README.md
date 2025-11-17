
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SpraseVectorsHW6

<!-- badges: start -->

<!-- badges: end -->

The goal of SpraseVectorsHW6 is to create a new object type called
**sparse vector** that reduces memory usage for vector that have large
amounts of 0 values. Along with a new object, the package also has a
variety of functions to manipluate sparse vectors.

## Installation

You can install the development version of SpraseVectorsHW6 like so:

``` r
remotes::install_github("MadelineRBoss/SparseVectorHW6")
```

## Examples

Creating a Sparse Vector

``` r
library(SpraseVectorsHW6)
#> 
#> Attaching package: 'SpraseVectorsHW6'
#> The following objects are masked from 'package:base':
#> 
#>     mean, norm

x <- new("sparse_numeric",
                 value = c(1, 2, 3, 1),
                 pos = c(1L, 2L, 3L, 5L),
                 length = 10L)
y <- new("sparse_numeric",
                 value = c(4, 6, 1, 2),
                 pos = c(1L, 3L, 5L, 7L),
                 length = 10L)
```

Plot two vectors to see where they overlap

``` r
plot(x, y)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

Add, Multiply, Subtract, and Cross Product Vectors

``` r
sparse_add(x,y)
#> The sparse vector of size 10 has the following values: 
#> position: 1 , value: 5 
#> position: 2 , value: 2 
#> position: 3 , value: 9 
#> position: 5 , value: 2 
#> position: 7 , value: 2
x + y
#> The sparse vector of size 10 has the following values: 
#> position: 1 , value: 5 
#> position: 2 , value: 2 
#> position: 3 , value: 9 
#> position: 5 , value: 2 
#> position: 7 , value: 2
```

``` r
sparse_mult(x,y)
#> The sparse vector of size 10 has the following values: 
#> position: 1 , value: 4 
#> position: 3 , value: 18 
#> position: 5 , value: 1
x * y
#> The sparse vector of size 10 has the following values: 
#> position: 1 , value: 4 
#> position: 3 , value: 18 
#> position: 5 , value: 1
```

``` r
print(sparse_sub(x,y))
#> The sparse vector of size 10 has the following values: 
#> position: 1 , value: -3 
#> position: 3 , value: -3 
#> position: 2 , value: 2 
#> position: 7 , value: -2
x - y
#> The sparse vector of size 10 has the following values: 
#> position: 1 , value: -3 
#> position: 3 , value: -3 
#> position: 2 , value: 2 
#> position: 7 , value: -2
```

``` r
print(sparse_crossprod(x,y))
#> [1] 23
```

Get the sum, mean, norm, and standardize vector

``` r
sparse_sum(x)
#> [1] 7
```

``` r
mean(x)
#> [1] 0.7
```

``` r
norm(x)
#> [1] 3.872983
```

``` r
standardize(x)
#> The sparse vector of size 10 has the following values: 
#> position: 1 , value: 0.2581989 
#> position: 2 , value: 0.5163978 
#> position: 3 , value: 0.7745967 
#> position: 5 , value: 0.2581989
```
