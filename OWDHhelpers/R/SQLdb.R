
#' SQLite database Object
#' @description SQLite database Object
#' @docType class
#' @author Daniel Hohmann
#' @importFrom R6 R6Class
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbConnect dbDisconnect dbListTables
#' @export
#' @format An \code{\link{R6Class}} generator object

SQLdb = R6Class(
  
  classname = "SQLdb",
  
  public = list(
    
    #' @description Open the database connection. A new data will be created in case dbpath is not an existing one
    #' @param dbpath A string defining the path and file name of the database
    
    initialize = function(dbpath) {
      
      private$database = dbConnect(
        SQLite(),
        dbname = dbpath
      )
    },
    
    
    #' @description List all tables in the database
    #' @return A string giving the table names
    
    listTables = function() {
      dbListTables(private$database)
    },
    
    #' @description Disconnect the database
    
    disconnect = function() {
      dbDisconnect(private$database)
    }
  ),
  
  private = list(
    
    database = NULL
  )
)
