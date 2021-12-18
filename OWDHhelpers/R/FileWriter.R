
#' File Writer Object
#' @description AAM Credit ESG File Writer Object
#' @docType class
#' @author Daniel Hohmann
#' @importFrom R6 R6Class
#' @importFrom openxlsx write.xlsx setColWidths saveWorkbook createWorkbook addWorksheet writeData
#' @importFrom readr write_csv write_csv2
#' @export
#' @format An \code{\link{R6Class}} generator object


FileWriter = R6Class(
  
  classname = "FileWriter",
  
  
  ## PUBLIC FIELDS AND METHODS
  
  public = list(
    
    
    #' @description Create a new FileWriter instance
    #' @param type a string (csv or xlsx) specifying the file type
    #' @param csv_locale a string (US or DE) specifying the csv file seperator and decimal convention
    #' @param folder an optional string which can pre-define the output folder
    
    initialize = function(type = "csv", csv_locale = "US", folder = NA) {
      
      private$folder = folder
      
      if (type %in% c("csv", "xlsx")) {
        private$type = type
      } else {
        stop("Unknown file type")
      }
      
      if (csv_locale %in% c("US", "DE")) {
        private$csv_locale = csv_locale
      } else {
        stop("Unknown csv locale")
      }
    },
    
    
    
    #' @description Write a data.frame to a csv or xlsx file based on the pre-defined file type
    #' @param data a [tbl_df-class] object to be written to the file
    #' @param filename a character string specifying the filename of the file to be written
    #' @param folder a character string specifying the folder of the file to be written (if NA no additional folder is added to the output path)
    #' @param add_filename_extension boolean specifying if the filename extension should be added in case it is missing
    
    write = function(data, filename, folder = NA, add_filename_extension = TRUE) {
      
      data = as.data.frame(data)
      
      if (!is.na(folder)) {
        path = file.path(folder, filename)
      } else {
        if (!is.na(private$folder)) {
          path = file.path(private$folder, filename)
        } else {
          path = filename
        }
      }
      
      if (private$type == "xlsx") {
        if (add_filename_extension & !grepl(".xlsx", path)) {
          path = paste0(path, ".xlsx")
        }
        write.xlsx(data, path)
      }
      
      if (private$type == "csv") {
        if (add_filename_extension & !grepl(".csv", path)) {
          path = paste0(path, ".csv")
        }
        if (private$csv_locale == "US") {
          write_csv(data, path)
        }
        if (private$csv_locale == "DE") {
          write_csv2(data, path)
        }
      }
    },


    #' @description Write a data.frame to a csv of xlsx file based on the pre-defined file type
    #' @param data_portfolio a [tbl_df-class] object to be written in the first sheet with the information on portfolio level
    #' @param data_asset a [tbl_df-class] object to be written in the second sheet with the information on asset level
    #' @param filename a character string specifying the filename of the file to be written
    #' @param folder a character string specifying the folder of the file to be written (if NA no additional folder is added to the output path)
    
    write_final_output = function(data_portfolio, data_asset, filename, folder = NA) {
      
      list_of_datasets <- list("Portfolio_Information" = as.data.frame(data_portfolio), "Asset_Information" = as.data.frame(data_asset))
      
      if (!is.na(folder)) {
        path = file.path(folder, filename)
      } else {
        if (!is.na(private$folder)) {
          path = file.path(private$folder, filename)
        } else {
          path = filename
        }
      }
      
      path = paste0(path, ".xlsx")
      
      # write.xlsx(list_of_datasets, file = path)
      wb = createWorkbook()
      
      addWorksheet(wb, names(list_of_datasets)[1])
      writeData(wb, sheet = 1, x = list_of_datasets[[1]])
      setColWidths(wb, sheet = 1, cols = 1:ncol(list_of_datasets[[1]]), widths = "auto")
      
      addWorksheet(wb, names(list_of_datasets)[2])
      writeData(wb, sheet = 2, x = list_of_datasets[[2]])
      setColWidths(wb, sheet = 2, cols = 1:ncol(list_of_datasets[[2]]), widths = "auto")
      
      saveWorkbook(wb, path, overwrite = TRUE)
    }
    
  ),
  
  
  ## PRIVATE FIELDS AND METHODS
  
  private = list(
    
    type = NULL,
    csv_locale = NULL,
    folder = NA
    
  )
  
  
)