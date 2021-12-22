

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
library(tidyr)
library(DBI)
library(tibble)

db = SQLdb$new("02_Output\\test.db")

db$listTables()
db$getQuery("select SIMULATION, ASSET from (select SIMULATION, ASSET from simulations left join assets on SIMULATION = ASSET) where ASSET is null limit 5")
x = db$getQuery("select SIMULATION, ASSET from simulations left join assets on SIMULATION = ASSET")


x %>% filter(is.na(ASSET))

for(k in 1:5) {
  db$writeTable(
    "X",
    tibble(x = rnorm(10000000))
  )
}


X = db$getTableReference("Assets")
X %>% summarize(n())


db_ %>% dbWriteTable(
  "Simulations",
  tibble(SIMULATION = 1:100000)
)

db_ %>% dbWriteTable(
  "Assets",
  tibble(ASSET = 1:4000)
)

db$disconnect()

db_ %>% dbExecute("CREATE TABLE Combinations AS SELECT * FROM Simulations CROSS JOIN Assets")

db$execute("
           SELECT * 
           FROM Assets
           ")
C = db$getTableReference("Combinations")
dplyr::filter(C, ASSET == 2)

