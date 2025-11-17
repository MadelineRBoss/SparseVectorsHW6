#' @import methods
#' @import graphics
#' @import stats
#' @import utils
NULL

#' Sparse Numeric Class
#'
#' Represents a sparse numeric vector with values, positions, and total length.
#'
#' @slot value Numeric vector of nonzero entries.
#' @slot pos Integer vector indicating positions of nonzero entries.
#' @slot length Integer specifying the overall vector length.
#'
#' @return A `sparse_numeric` object.
#' @name sparse_numeric-class
#' @rdname sparse_numeric-class
#' @export

setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

#-------------------------------------------------------------------------------
#' Validate a sparse_numeric object
#'
#' Checks if the sparse numeric object meets the basic requirements of being valid.
#'
#' @param object A `sparse_numeric` object.
#' @return TRUE if valid; otherwise returns an error message.
#' @name validation
#' @rdname validation

setValidity(Class = "sparse_numeric",
            method = function(object){
              if(length(object@value) != length(object@pos))
                return("Amount of values and positions are unequal")
              if(length(object@pos) > object@length)
                return("Too many values for length of sprase vector")
              if(any(object@pos > object@length))
                return("A number has a greater position value than the actual space")
              if(any(object@pos < 1))
                return("A number has an invalid position")
              if(any(object@value == 0))
                return("A value position is 0")
              TRUE
            })
#-------------------------------------------------------------------------------
#' Convert dense to sparse numeric
#'
#' Converts a dense numeric vector to a `sparse_numeric` object.
#'
#' @param from A dense numeric vector.
#' @return A `sparse_numeric` object.
#' @name setAs
#' @rdname setAs

setAs(from = "numeric",
      to = "sparse_numeric",
      def = function(from) {
        pos <- which(from != 0)
        value <- from[pos]            # keep as numeric
        vec_length <- as.integer(length(from))
        new("sparse_numeric", value = value, pos = pos, length = vec_length)
      })

#' Convert sparse to dense numeric
#'
#' Converts a `sparse_numeric` object to a regular numeric vector.
#'
#' @param from A `sparse_numeric` object.
#' @return A numeric vector.
#' @name setAs
#' @rdname setAs

setAs(from = "sparse_numeric",
      to = "numeric",
      def = function(from){
        value <- from@value
        pos   <- from@pos
        len   <- from@length

        # initialize vector of zeros
        vec <- numeric(len)

        # fill in non-zero values
        for (i in seq_along(value)){
          vec[pos[i]] <- value[i]
        }

        vec   # return numeric vector
      })

#-------------------------------------------------------------------------------
#' Sort sparse vector
#'
#' Sorts values in the sparse vector by position.
#'
#' @param x A `sparse_numeric` object.
#' @param decreasing Logical; if TRUE, sort in decreasing order.
#' @param ... additional arguments passed to or from other methods
#' @return A sorted `sparse_numeric` object.
#' @name sort
#' @rdname sort
#' @aliases sort,sparse_numeric-method
#' @export

setMethod("sort", signature(x = "sparse_numeric"),
          function(x, decreasing = FALSE, ...) {
            ord <- order(x@pos, decreasing = decreasing)

            new("sparse_numeric",
                value = x@value[ord],
                pos = x@pos[ord],
                length = x@length)
          })


#-------------------------------------------------------------------------------
#' Add two sparse vectors
#'
#' Generic and method to add two `sparse_numeric` vectors element-wise.
#'
#' @param x First sparse vector.
#' @param y Second sparse vector.
#' @return A `sparse_numeric` object representing the sum.
#' @name sparse_add
#' @rdname sparse_add
#' @export


# Define Generic for parse_add
setGeneric("sparse_add", function(x, y) standardGeneric("sparse_add"))

#' @describeIn sparse_add Compute the addtion vector for two sparse_numeric vectors.
setMethod("sparse_add",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y) {
            if(x@length != y@length){
              stop("Length of vectors do not match")
            }
            double_pos <- intersect(x@pos, y@pos)

            x_vals_match <- x@value[x@pos %in% double_pos]
            y_vals_match <- y@value[y@pos %in% double_pos]

            x_values_no_match <- x@value[!(x@pos %in% double_pos)]
            y_values_no_match <- y@value[!(y@pos %in% double_pos)]
            x_pos_no_match <- x@pos[!(x@pos %in% double_pos)]
            y_pos_no_match <- y@pos[!(y@pos %in% double_pos)]

            add_vals <- x_vals_match + y_vals_match
            add_vals <- c(add_vals, x_values_no_match, y_values_no_match)
            new_pos <- c(double_pos, x_pos_no_match, y_pos_no_match)

            new_sparse <- new("sparse_numeric",
                              value = add_vals,
                              pos = new_pos,
                              length = max(x@length, y@length))
            sort(new_sparse)
          })
#-------------------------------------------------------------------------------
#' Multiply two sparse vectors
#'
#' Generic and method to multiply two `sparse_numeric` vectors element-wise.
#'
#' @param x First sparse vector.
#' @param y Second sparse vector.
#' @return A `sparse_numeric` object.
#' @name sparse_mult
#' @rdname sparse_mult
#' @export

# Define Generic for parse_mult
setGeneric("sparse_mult", function(x, y) standardGeneric("sparse_mult"))

#' @describeIn sparse_mult Compute the multplication vector for two sparse_numeric vectors.
# Define Method for parse_mult
setMethod("sparse_mult",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y){
            if(x@length != y@length){
              stop("Length of vectors do not match")
            }
            else{
              double_pos <- intersect(x@pos, y@pos)

              # Find matching indices in each object
              x_vals <- x@value[x@pos %in% double_pos]
              y_vals <- y@value[y@pos %in% double_pos]

              # Multiply the values where positions match
              prod_vals <- x_vals * y_vals

              # Construct new sparse_numeric
              new_sparse <- new("sparse_numeric",
                  value = prod_vals,
                  pos = double_pos,
                  length = max(x@length, y@length))
              sort(new_sparse)
            }})

#-------------------------------------------------------------------------------
#' Subtract two sparse vectors
#'
#' Generic and method to subtract one `sparse_numeric` vector from another.
#'
#' @param x First sparse vector.
#' @param y Second sparse vector.
#' @return A `sparse_numeric` object.
#' @name sparse_sub
#' @rdname sparse_sub
#' @export

# Define Generic for parse_add
setGeneric("sparse_sub", function(x, y) standardGeneric("sparse_sub"))

#' @describeIn sparse_sub Compute the subtraction vector of two sparse_numeric vectors.
# Define Method for parse_add
setMethod("sparse_sub",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y){
            double_pos <- intersect(x@pos, y@pos)

            # Find matching indices in each object
            x_vals_match <- x@value[x@pos %in% double_pos]
            y_vals_match <- y@value[y@pos %in% double_pos]

            # Find non matching indices
            x_values_no_match = x@value[!(x@pos %in% double_pos)]
            y_values_no_match = y@value[!(y@pos %in% double_pos)] * -1
            x_pos_no_match = x@pos[!(x@pos %in% double_pos)]
            y_pos_no_match = y@pos[!(y@pos %in% double_pos)]

            # dd the values where positions match
            sub_vals <- x_vals_match - y_vals_match
            invalid_vals = c()
            if(length(sub_vals) != 0){
              for (val in 1:length(sub_vals)){
                if (sub_vals[val] == 0){
                  invalid_vals = append(invalid_vals, val)
                }
              }

            }
            if(length(invalid_vals) != 0){
              sub_vals = sub_vals[-invalid_vals]
              double_pos = double_pos[-invalid_vals]
            }

            #append values that don't have matches
            sub_vals <- c(sub_vals, x_values_no_match, y_values_no_match)
            new_pos <- c(double_pos, x_pos_no_match, y_pos_no_match)


            # Construct new sparse_numeric
            new_sparse <- new("sparse_numeric",
                value = sub_vals,
                pos = new_pos,
                length = max(x@length, y@length))
            sort(new_sparse)
          })

#-------------------------------------------------------------------------------
#' Cross product of two sparse vectors
#'
#' Computes the cross product (dot product) of two `sparse_numeric` vectors.
#'
#' @param x First sparse vector.
#' @param y Second sparse vector.
#' @return A numeric value.
#' @name sparse_crossprod
#' @rdname sparse_crossprod
#' @export

# Define Generic for sparse_crossprod
setGeneric("sparse_crossprod", function(x, y) standardGeneric("sparse_crossprod"))

#' @describeIn sparse_crossprod Compute the cross product for two sparse_numeric vectors.
# Define Method for sparse_crossprod
setMethod("sparse_crossprod",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x,y){

            double_pos <- intersect(x@pos, y@pos)

            # Find matching indices in each object
            x_vals <- x@value[x@pos %in% double_pos]
            y_vals <- y@value[y@pos %in% double_pos]

            # Multiply the values where positions match
            prod_vals <- x_vals * y_vals

            final_value <- 0

            for (v in prod_vals){
              final_value = final_value + v
            }

            # Construct new sparse_numeric
            new("numeric", c(final_value))
          })

#-------------------------------------------------------------------------------
#make alts
#' Arithmetic Operations for Sparse Numeric Vectors
#'
#' Defines element-wise arithmetic operations for `sparse_numeric` objects.
#' Supports addition (`+`), subtraction (`-`), and multiplication (`*`).
#'
#' @param e1 A `sparse_numeric` object.
#' @param e2 A `sparse_numeric` object.
#' @return A `sparse_numeric` object resulting from the arithmetic operation.


# Addition
#' @describeIn sparse_numeric-arithmetic Element-wise addition of two sparse_numeric vectors.
setMethod("+", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))

# Subtraction
#' @describeIn sparse_numeric-arithmetic Element-wise subtraction of two sparse_numeric vectors.
setMethod("-", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))

# Multiplication
#' @describeIn sparse_numeric-arithmetic Element-wise multiplication of two sparse_numeric vectors.
setMethod("*", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))

#-------------------------------------------------------------------------------
#' Show a sparse vector
#'
#' Prints a readable representation of a `sparse_numeric` object.
#'
#' @param object A `sparse_numeric` object.
#' @return Printed output to console.
#' @name show
#' @rdname show
#' @aliases show,sparse_numeric-method
#' @export

setMethod("show", "sparse_numeric",
          function(object){
            cat("The sparse vector of size", object@length, "has the following values: \n")
            for(v in 1:length(object@value)){
              cat("position:", object@pos[v],
                  ", value:", object@value[v], "\n")

            }
          })

#-------------------------------------------------------------------------------
#' Plot sparse vectors
#'
#' Creates a dot plot showing where two sparse vectors overlap.
#'
#' @param x First sparse vector.
#' @param y Second sparse vector.
#' @return A plot object.
#' @name plot
#' @rdname plot
#' @aliases plot,sparse_numeric,sparse_numeric-method
#' @export

setMethod("plot",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y) {

            double_pos <- intersect(x@pos, y@pos)
            plot_value = c()
            for(l in 1:x@length){
              if (l %in% double_pos){
                plot_value = c(plot_value, 1)
              }
              else{
                plot_value = c(plot_value, 0)
              }
            }
            plot(x = 1:x@length, y = plot_value)
          })

#-------------------------------------------------------------------------------
#' Sum of sparse vector
#'
#' Computes the sum of all nonzero values in a `sparse_numeric` object.
#'
#' @param object A `sparse_numeric` object.
#' @return A numeric value.
#' @name sparse_sum
#' @rdname sparse_sum
#' @export

setGeneric("sparse_sum", function(object) standardGeneric("sparse_sum"))

#' @describeIn sparse_sum Compute the sum of values for sparse_numeric.
setMethod("sparse_sum", "sparse_numeric",
          function(object){
            sum = 0
            for(v in object@value){
              sum = sum + v
            }
            sum
          })

#-------------------------------------------------------------------------------
#' Mean of sparse vector
#'
#' Generic and method to compute the mean of a `sparse_numeric` vector.
#'
#' @param object A `sparse_numeric` object.
#' @return A numeric mean value.
#' @name mean
#' @rdname mean
#' @export

setGeneric("mean", function(object) standardGeneric("mean"))

#' @describeIn mean Compute mean for sparse_numeric.
setMethod("mean", "sparse_numeric",
          function(object){
            sum = 0
            for(i in object@value){
              sum = sum + i
            }
            sum/object@length
          })

#' Norm of sparse vector
#'
#' Generic and method to compute the Euclidean norm of a `sparse_numeric` vector.
#'
#' @param object A `sparse_numeric` object.
#' @return A numeric norm value.
#' @name norm
#' @rdname norm
#' @export
setGeneric("norm", function(object) standardGeneric("norm"))

#' @describeIn norm Compute norm for sparse_numeric.
setMethod("norm", "sparse_numeric",
          function(object){
            sum = 0
            for(i in object@value){
              sum = sum + (i**2)
            }
            sum**(1/2)
          })

#' Standardize sparse vector
#'
#' Generic and method to normalize a `sparse_numeric` vector.
#'
#' @param object A `sparse_numeric` object.
#' @return A normalized `sparse_numeric` object.
#' @name standardize
#' @rdname standardize
#' @export
setGeneric("standardize", function(object) standardGeneric("standardize"))

#' @describeIn standardize Compute the standardize sparse vector for sparse_numeric.
setMethod("standardize", "sparse_numeric",
          function(object){
            my_norm = norm(object)
            new_value = object@value / my_norm

            new("sparse_numeric",
                value = new_value,
                pos = object@pos,
                length = object@length)
          })


