
#' Cluster Object
#' @description An R6 class defining a cluster object for parallel computations
#' @docType class
#' @author Daniel Hohmann
#' @importFrom R6 R6Class
#' @importFrom parallel makeCluster clusterEvalQ clusterExport clusterApplyLB stopCluster
#' @importFrom rlang expr
#' @export
#' @format An \code{\link{R6Class}} generator object

Cluster = R6Class(
  
  classname = "Cluster",
  
  public = list(
    
    #' @description Create new cluster object
    #' @param numberOfCores integer; the number of cores to be used
    
    initialize = function(numberOfCores) {
      
      private$cluster = makeCluster(numberOfCores)
    },
    
    
    #' @description Export objects to the cluster
    #' @param objects list; objects to be exported
    
    export = function(objects) {
      clusterExport(cl = private$cluster, 
                    varlist = objects,
                    envir = .GlobalEnv)
    },
    
    
    #' @description Load packages for the cluster
    #' @param packages character vector; the packages to be loaded
    
    loadPackages = function(packages) {
      
      for(pk in packages) {
        library(pk, character.only = TRUE, quietly = TRUE)
      }
    },
    
    
    #' @description Load balanced parallel execution
    #' @param args vector of list of arguments to be passed to fun
    #' @param fun function; the function to be executed
    #' @param ... additional arguments to be passed to fun
    
    apply = function(args, fun, ...) {
      return(
        clusterApplyLB(private$cluster,
                       args,
                       fun,
                       ...)
      )
    },
    
    
    #' @description Stop the cluster connection
    
    stop = function() {
      stopCluster(private$cluster)
    }    
    
  ),
  
  
  private = list(
    
    cluster = NULL
  )
  
)