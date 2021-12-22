
#' SQLite database Object
#' @description SQLite database Object
#' @docType class
#' @author Daniel Hohmann
#' @importFrom R6 R6Class
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbConnect dbDisconnect dbListTables dbWriteTable dbReadTable
#' dbRemoveTable dbExecute dbGetQuery
#' @importFrom dplyr "%>%" tbl
#' @importFrom tibble tibble
#' @export
#' @format An \code{\link{R6Class}} generator object

SQLdb = R6Class(
  
  classname = "SQLdb",
  
  public = list(
    
    #' @description Open the database connection. A new database will be created
    #' in case dbpath is not an existing one
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
    
    
    
    #' @description Write a new table to the database
    #' @param tableName A string specifying the name of the new or existing
    #' table
    #' @param data A \code{\link[tibble:tbl_df-class]{tibble}} specifying the
    #' data to be written to the database
    #' @param append A boolean specifying whether the new data should be
    #' appended in case the table already exists
    
    writeTable = function(tableName, data, append = TRUE) {
      private$database %>% 
        dbWriteTable(
          tableName,
          data,
          append = append
        )
    },
    
    
    
    #' @description Read a table from the database
    #' @param tableName A string specifying the name of the table
    #' @return A \code{\link[tibble:tbl_df-class]{tibble}}
    
    readTable = function(tableName) {
      
      return(private$database %>% 
               dbReadTable(tableName) %>% 
               tibble())
    },
    
    
    
    #' @description Return a reference to an SQL table
    #' @param tableName A string specifying the name of the table
    #' @return A \code{tbl_SQLiteConnection} object
    
    getTableReference = function(tableName) {
      return(private$database %>% tbl(tableName))
    },
    
    
    
    #' @description Remove a table from the database
    #' @param tableName A string specifying the name of the table
    
    removeTable = function(tableName) {
      
      private$database %>% dbRemoveTable(tableName)
    },
    
    
    
    #' @description Execute an SQL statement in the database
    #' @param sqlStatement A string specifying the statement
    #' @param silent A boolean specifying if the result should be printed
    #' @return An integer giving the number of affected rows
    
    execute = function(sqlStatement, silent = TRUE) {
      
      rowsaffected = private$database %>% 
        dbExecute(sqlStatement)
      
      if(!silent) return(rowsaffected)
    },
    
    
    
    #' @description Execute an SQL query in the database and retrieve the
    #' resulting table
    #' @param sqlStatement A string specifying the query
    #' @return A \code{\link[tibble:tbl_df-class]{tibble}}
    
    getQuery = function(sqlStatement) {
      
      private$database %>% 
        dbGetQuery(sqlStatement) %>% 
        tibble()
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
