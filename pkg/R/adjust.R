#' Adjust records to satisfy linear (in)equality constraints 
#'
#' @name validate.adjust
#'
#' @docType package
#' @import methods
#' @import validate
#' @import supral
#'
{}


#' Adjust records to satisfy linear (in)equality constraints
#'
#' @param dat An R object carrying data
#' @param rules An R object carrying linear (in)equation restriction rules
#'
#' @rdname adjust 
#' @export
setGeneric("adjust",function(dat, rules,...) standardGeneric('adjust'))

#' @param adjust \code{[logical array]} indicating what fields to adjust. If left unspecified,
#' all variables occuring in linear rules may be altered.
#' @param weight The adjustment weight assigned to each variable. Variables with a higher
#'   weight will be altered less. The following values are allowed:
#'   \itemize{
#'   \item{\code{"unity"} If weights are not set, all variables are treated equal (with weight 1).}
#'   \item{\code{"ratio"} Weights are set so that the ratio between adjusted variables remain
#'    approximately constant.}
#'   \item{\code{numeric} vector with the same number of variables as \code{dat}. Custom weights
#'   applied to each record.}
#'   \item{\code{numeric array} of the same size as \code{dat}. Custom weights for each record}
#'   }
#' @param tol Convergence criterion: maximum contribution of a single value to the final deviation
#' from the rules.
#' @param maxiter Maximum number of iterations for the optimization algorithm per record
#' (see \code{supral::\link[supral]{project}})
#' @param ... Currently unused.
#' @rdname adjust
setMethod("adjust", c("data.frame","validator")
      , function(dat, rules, adjust, weight="uniform", tol=0.01, maxiter=1000L, ...){
  i <- rules$is_linear()
  v <- rules[i]
  if (sum(i) < length(rules)){
    message(sprintf("Ignoring %d non-linear rules",length(rules)-sum(i)))
  }
  
  if (missing(adjust)){
    adjust <- array(TRUE,dim=dim(dat),dimnames=list(NULL,names(dat)))
    adjust <- adjust[,variables(v),drop=FALSE]
  }
  
  arr <- t(dat[names(dat) %in% variables(v)])
  if ( !is.numeric(arr) ){
    stop("Linear rules applied to non-numeric data")
  }
 
  # Organize the linear coeff's before passing them to supral::project 
  coef <- v$linear_coefficients()
  eqs <- coef$operators == "=="
  A <- coef$A[c(which(eqs),which(!eqs)),,drop=FALSE]
  b <- coef$b[c(which(eqs),which(!eqs))]
  
  # generate a function that returns the correct weight for each record.
  get_weight_fun <- function(w,nvar){
    if ( identical(w,"uniform") ){
      w <- rep(1,nvar)
      function(...) w
    } else if ( identical(w,"ratio") ){
      function(r,...){
        w <- abs(1/r)
        w[w==Inf] <- median(w[w!=Inf])
        w
      }
    } else if ( is.numeric(w) && length(w)==nvar ) {
      function(...) w
    } else if (is.numeric(w) && is.array(w) ){
      function(i,...) w[i,]
    } else {
      stop("Unrecognized weight specification")
    }
  }
  
  get_weight <- get_weight_fun(weight, length(variables(v)))
   
  for ( i in seq_len(ncol(arr)) ) {
    arr[,i] <- adjust_vector(
      arr[,i]
      , adjust[i,]
      , A = A
      , b = b
      , neq = sum(eqs)
      , w = get_weight(i=i, r=arr[,i])
      , tol = tol
      , maxiter = maxiter
      )
  }
  dat[variables(v)] <- t(arr)
  dat
})



adjust_vector <- function(x, adjust, A, b, neq, w, tol, maxiter ){
  # Substitute fixed values in restrictions
  if ( any(!adjust) ){
    b <- b - A[,!adjust,drop=FALSE] * x[!adjust]
    A <- A[,adjust,drop=FALSE]
    # TODO: redundancy removal
  }
  out <- supral::project(x[adjust], A=A, b=b, neq=neq, w=w[adjust], tol=tol, maxiter = maxiter)
  x[adjust] <- out$x
  x
}
























