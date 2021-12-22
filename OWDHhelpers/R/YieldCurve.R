
#' Yield curve Object
#' @description An R class defining a yield curve object with corresponding
#' functionalities
#' @docType class
#' @author Daniel Hohmann
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object

YieldCurve = R6Class(
  
  classname = "YieldCurve",
  
  public = list(
    
    #' @description Create a new yield curve object
    #' @param terms An integer vector providing the input terms
    #' @param spotrates A double vector providing the spot rates corresponding
    #' to the input terms
    #' @param zcb A double vector providing the zero coupon bond prices 
    #' corresponding to the input terms (either spotrates or zcb must be
    #' provided)
    
    initialize = function(terms, spotrates = NULL, zcb = NULL) {
      
      private$terms = terms
      
      if(is.null(spotrates) & is.null(zcb))
        stop("Neither spot rates nor zero coupon bond prices provided")
      
      if(is.null(zcb)) {
        private$spotrates = spotrates
        private$zcb = (1 + spotrates) ^ (-terms)
      } else {
        private$zcb = zcb
        private$spotrates = zcb ^ (-1 / terms) - 1
      }
    },
    
    
    #' @description Print the yield curve object
    
    print = function() {
      output = private$zcb
      names(output) = private$terms
      print(output)
    },
    
    
    #' @description Calibrate the Smith-Wilson zero coupon bond proxy
    #' price function
    #' @param alpha A double specifying the convergence rate (does not need to
    #' be provided in case alpha_search = TRUE)
    #' @param UFR A double specifying the ultimate forward rate
    #' @param UFR_correction A Boolean specifying if the given UFR is annually
    #' compounded (TRUE) or continuously compounded (FALSE)
    #' @param alpha_search A Boolean specifying if the optimal convergence
    #' rate according to the EIOPA methodology is to be derived
    #' @param alpha_range A double vector specifying the lower and upper
    #' bound for the alpha search
    #' @param UFR_convergence_point An integer specifying the term at which
    #' the UFR is to be approximated
    #' @param UFR_precision A double specifying the precision up to which the
    #' UFR is to be approximated
    #' @param alpha_precision A double specifying the precision up to which
    #' alpha is to be optimized
    
    calibrate_price_function = function(alpha = 0.2, 
                                        UFR = 0.042, 
                                        UFR_correction = TRUE, 
                                        alpha_search = FALSE, 
                                        alpha_range = c(0.05, 0.8), 
                                        UFR_convergence_point = 60, 
                                        UFR_precision = 1E-4, 
                                        alpha_precision = 1E-6) {
      
      zcb = private$zcb
      terms = private$terms
      if (UFR_correction) UFR = log(1 + UFR)
      
      N = length(zcb)
      mu = zcb - exp(-UFR * terms)
      
      if (!alpha_search) {
        
        X = array(dim = c(N, N))
        for (k in 1:N) {
          X[, k] = private$Wil(terms, terms[k], alpha, UFR)
        }
        zeta = solve(X, mu)
        
      } else {
        
        alpha_old = alpha_range[1]
        
        repeat {
          alpha_new = (alpha_range[2] + alpha_range[1]) / 2
          
          X = array(dim = c(N, N))
          for (k in 1:N) {
            X[, k] = private$Wil(terms, terms[k], alpha_new, UFR)
          }
          zeta = solve(X, mu)
          kappa = (1 + alpha_new * sum((terms) * zeta * exp(-UFR * terms))) / 
            sum((exp(alpha_new * terms) - exp(-alpha_new * terms)) / 
                  2 * zeta * exp(-UFR * terms))
          g = alpha_new / abs(1 - kappa * 
                                exp(alpha_new * UFR_convergence_point))
          
          if (g < UFR_precision) {
            alpha_range = c(alpha_range[1], alpha_new)
          } else {
            alpha_range = c(alpha_new, alpha_range[2])
          }
          
          if (abs(alpha_new - alpha_old) < alpha_precision) {
            alpha = alpha_new
            break
          } else {
            alpha_old = alpha_new
          }
        }
      }
      
      private$UFR = UFR
      private$alpha = alpha
      private$zeta = zeta
    },
    
    
    
    #' @description Calculate zero coupon bond prices for a given vector of 
    #' terms given the price function calibration
    #' @param terms An integer vector specifying the terms
    #' @return A double vector
    
    ZCB = function(terms) {
      
      if(is.null(private$zeta))
        stop("Price function not calibrated yet")
      
      N = length(private$terms)
      Y = array(dim = c(length(terms), N))
      for (k in 1:N) {
        Y[, k] = private$Wil(terms, 
                             private$terms[k], 
                             private$alpha, 
                             private$UFR)
      }
      zcb_out = as.numeric(exp(-private$UFR * terms) + Y %*% private$zeta)
      return(zcb_out)
    },
    
    
    
    #' @description Calculate spot rates for a given vector of terms given
    #' the price function calibration
    #' @param terms An integer vector specifying the terms
    #' @return A double vector
    
    SpotRates = function(terms) {
      
      if(is.null(private$zeta))
        stop("Price function not calibrated yet")
      
      zcb_out = self$ZCB(terms)
      spot_out = zcb_out ^ (-1 / terms) - 1
      return(spot_out)
    },
    
    
    
    #' @description Return the current alpha parameter
    #' @return A double value
    
    get_alpha = function() {
      if(is.na(private$alpha)) warning("Price function not calibrated yet")
      return(private$alpha)
    }
    
  ),
  
  
  private = list(
    
    terms = NULL,
    spotrates = NULL,
    zcb = NULL,
    UFR = NA,
    alpha = NA,
    zeta = NULL,
    
    Wil = function(t, u, alpha = 0.2, UFR = 0.042) {
      res = sapply(t, function(y) exp(-UFR * (y + u)) * 
                     (alpha * min(y, u) - 0.5 * exp(-alpha * max(y, u)) * 
                        (exp(alpha * min(y, u)) - exp(-alpha * min(y, u)))))
      return(res)
    }
    
  )
)
