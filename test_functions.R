

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

library(dplyr)
library(dbplyr)
library(DBI)
library(tibble)

db = SQLdb$new("test.db")

db$listTables()

db_ = db$.__enclos_env__$private$database


class(dbReadTable(db_, "normalRNs"))
for(k in 1:50) {
  db_ %>% dbWriteTable(
    "X",
    tibble(x = rnorm(10000000)),
    append = TRUE
  )
}

db_ %>% dbReadTable("X")
X = db_ %>% tbl("X")
f = function(y) y < 0
X %>% mutate(y = x < 0) %>% summarize(Sum = sum(y)) %>% show_query()
X %>% summarize(n()) %>% collect()
db_ %>% dbGetQuery(
  "SELECT x, x < 0 as y FROM X LIMIT 10"
)
f(2)
X %>% summarize(n())

