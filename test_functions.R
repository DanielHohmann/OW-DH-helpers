

# Test functions

#library(OWDHhelpers)

devtools::load_all("OWDHhelpers")


# YieldCurve ----

library(readr)

input = read_csv("01_Input\\spot_input.csv")
terms = c(1:10, 12, 15, 20)
spots = input$SPOT[terms]
UFR = 0.036
alpha =  0.131942

yc = YieldCurve$new(terms = terms,
                    spotrates = spots)

yc$get_alpha()
yc$calibrate_price_function(alpha = alpha,
                            UFR = UFR)
yc$calibrate_price_function(alpha = 0.5,
                            UFR = UFR,
                            alpha_search = TRUE)

yc$SpotRates(1:150)
yc$ZCB(1:150)


# SQLdb ----

db = SQLdb$new("test.db")

db2$listTables()

db_ = db$.__enclos_env__$private$database

x = data.frame(a = rnorm(1:10), b = rnorm(1:10))
DBI::dbWriteTable(db_, "normalRNs", x)

DBI::dbListTables(db2$.__enclos_env__$private$database)
DBI::dbDisconnect(db_)
dbplyr::tbl
db
library(dbplyr)
tbl_sql("normalRNs", db_)
c = dplyr::tbl(db_, "normalRNs")
c %>% mutate(c = a+b)
library(dplyr)
