
#' EIOPA RFR
#' @description Read and use EIOPA risk free rates
#' @docType class
#' @author Daniel Hohmann
#' @importFrom R6 R6Class
#' @importFrom openxlsx read.xlsx
#' @importFrom dplyr "%>%" select pull group_by mutate summarize matches
#' @importFrom tibble as_tibble add_column
#' @importFrom data.table as.data.table
#' @format An \code{\link{R6Class}} generator object
#' @export

EIOPA_RFR = R6Class(

  classname = "EIOPA_RFR",


  ## PUBLIC FIELDS AND METHODS

  public = list(

    #' @description Create a new EIOPA RFR object
    #' @param path The path to the input excel file

    initialize = function(path) {

      private$RFR_input = read.xlsx(path, sheet = "RFR_spot_no_VA", startRow = 10) %>% as_tibble()
      header = colnames(read.xlsx(path, sheet = "RFR_spot_no_VA", rows = 3))
      colnames(private$RFR_input) = c("TERM", header)
      private$names = colnames(read.xlsx(path, sheet = "RFR_spot_no_VA", rows = 2, sep.names = " "))[-1]
      private$ZCB_interpolated = tibble(TERM = 1:(max(private$RFR_input$TERM) * 12))
    },


    #' @description Get list of available EIOPA RFRs
    #' @return [tbl_df-class] object

    available_RFRs = function() {

      RFRs = tibble(Name = private$names, Information = colnames(private$RFR_input)[-1])
      return(RFRs)
    },


    #' @description Export the complete RFR table
    #' @return [tbl_df-class] object

    get_RFR_table = function() {
      return(private$RFR_input)
    },


    #' @description Export a specific RFR by country code
    #' @param country a string specifying the Country code
    #' @return [tbl_df-class] object

    get_RFR_by_country_code = function(country) {

      if (!any(grepl(country, names(private$RFR_input)))) {
        country = private$default_currency
      }

      RFR_by_country_code = private$RFR_input %>% select(matches(country))
      return(RFR_by_country_code)
    },



    #' @description Calculate monthly forward rates from EIOPA ZSKs using Smith-Wilson-Interpolation
    #' @param country a string specifying the Country code
    #' @param months an integer vector specifying the relevant forward terms in months (optional)

    forward_rates = function(country, months = NULL) {

      if (!any(grepl(country, names(private$RFR_input)))) {
        country = private$default_currency
      }

      if (!(country %in% names(private$ZCB_interpolated))) {
        private$do_interpolation(country)
      }

      zcb = private$ZCB_interpolated %>% pull(country)

      if (!is.null(months)) {
        zcb = zcb[months]
      } else {
        months = 1:length(zcb)
      }
      
      forwards = c(1, zcb[-length(zcb)]) / zcb - 1
      Forwards = tibble(Month = months, Forward = forwards)
      return(as.data.table(Forwards))
    },
    
    
    
    #' @description Provide an overview of currency availabilities
    #' @param assets the [Asset_Portfolio] providing all relevant asset information
    #' @return a [tbl_df-class] object
    
    availability_overview = function(assets) {
      
      available_currencies = names(private$RFR_input)
      
      accomplishments = assets$get_asset_table(only_relevant_assets = TRUE) %>%
        select(InstrumentID, MarketValue = DirtValueExposure, Currency) %>%
        mutate(`Currency available` = sapply(Currency, function(x) any(grepl(x, available_currencies)))) %>%
        group_by(`Currency available`) %>%
        summarize(MarketValue = sum(MarketValue), .groups = "drop") %>%
        mutate(Proportion = MarketValue / sum(MarketValue)) %>%
        select(`Currency available`, Proportion)
      
      return(accomplishments)
    }
      

    ),



  ## PRIVATE FIELDS AND METHODS

  private = list(

    RFR_input = NULL,
    ZCB_interpolated = NULL,
    names = NULL,
    default_currency = 'EUR',


    do_interpolation = function(country) {

      max_term = nrow(private$RFR_input)
      terms_in = 1:max_term * 12
      months = 1:(max_term * 12)
      
      spot_rates = private$RFR_input %>% select(matches(country)) %>% pull(1)
      ufr = (1 + spot_rates[max_term]) ^ (max_term) / (1 + spot_rates[max_term - 1]) ^ (max_term - 1) - 1
      zcb = private$SW_extrapolation(spot = spot_rates, terms = terms_in, monthly = TRUE, UFR = ufr)$zcb
      
      private$ZCB_interpolated = private$ZCB_interpolated %>% add_column(!!(country) := zcb)
    },


    # Smith-Wilson Inter- and extrapolation

    Wil = function(t, u, alpha = 0.2, UFR = 0.042) {
      res = sapply(t, function(y) exp(-UFR * (y + u)) * (alpha * min(y, u) - 0.5 * exp(-alpha * max(y, u)) * (exp(alpha * min(y, u)) - exp(-alpha * min(y, u)))))
      return(res)
    },

    SW_extrapolation = function(zcb = NA, spot = NA, terms = NA, monthly = FALSE, max_term_extrapolation = max(terms), alpha = 0.2, UFR = 0.042, UFR_correction = TRUE, alpha_search = FALSE, alpha_range = c(0.05, 0.8), UFR_convergence_point = 60, UFR_precision = 1E-4, alpha_precision = 1E-6) {

      scaling_terms = ifelse(monthly, 12, 1)

      # catch possible input errors and compute zcb's if necessary
      if (anyNA(zcb) & anyNA(spot) | anyNA(terms)) stop("Smith-Wilson-Interpolation: input error")
      if (anyNA(zcb)) {
        if (length(spot) != length(terms)) stop("Smith-Wilson-Interpolation: number of given terms incorrect")
        zcb = (1 + spot) ^ (-terms / scaling_terms)
      }
      if (length(zcb) != length(terms)) stop("Smith-Wilson-Interpolation: number of given terms incorrect")

      # UFR correction: if TRUE then the discrete UFR is approached
      if (UFR_correction) UFR = log(1 + UFR)


      # calibrate proxy price function
      N = length(zcb)
      mu = zcb - exp(-UFR * terms / scaling_terms)

      if (!alpha_search) {

        X = array(dim = c(N, N))
        for (k in 1:N) {
          X[, k] = private$Wil(terms / scaling_terms, terms[k] / scaling_terms, alpha, UFR)
        }
        zeta = solve(X, mu)

      } else {

        alpha_old = alpha_range[1]

        repeat {
          alpha_new = (alpha_range[2] + alpha_range[1]) / 2

          X = array(dim = c(N, N))
          for (k in 1:N) {
            X[, k] = Wil(terms / scaling_terms, terms[k] / scaling_terms, alpha_new, UFR)
          }
          zeta = solve(X, mu)

          kappa = (1 + alpha_new * sum((terms / scaling_terms) * zeta * exp(-UFR * terms / scaling_terms))) / sum((exp(alpha_new * terms / scaling_terms) - exp(-alpha_new * terms / scaling_terms)) / 2 * zeta * exp(-UFR * terms / scaling_terms))
          g = alpha_new / abs(1 - kappa * exp(alpha_new * UFR_convergence_point / scaling_terms))

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


      # inter-/extrapolation
      t = 1:max_term_extrapolation
      Y = array(dim = c(max_term_extrapolation, N))
      for (k in 1:N) {
        Y[, k] = private$Wil(t / scaling_terms, terms[k] / scaling_terms, alpha, UFR)
      }
      zcb_out = as.numeric(exp(-UFR * t / scaling_terms) + Y %*% zeta)
      spot_out = zcb_out ^ (-scaling_terms / t) - 1

      if (!alpha_search) {
        return(list(zcb = zcb_out, spot = spot_out))
      } else {
        return(list(alpha_opt = alpha, zcb = zcb_out, spot = spot_out))
      }
    }


    )
  )
