

# Test functions

library(OWDHhelpers)



# YieldCurve ----

yc = YieldCurve$new(terms = c(5, 10, 20),
                    spotrates = c(.008, .01, .012))
yc$calibrate_price_function(alpha_search = TRUE)
yc$.__enclos_env__$private$alpha
yc$.__enclos_env__$private$zeta
yc$zcb_out(1:30)



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

