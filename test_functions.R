

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
