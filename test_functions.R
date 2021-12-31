

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

db = SQLdb$new(file.path("02_Output", "test.db"))

db$listTables()


for(k in 1:2) {
  db$writeTable(
    "X",
    tibble(x = rnorm(2)),
    append = FALSE
  )
}
db$readTable("X")

X = db$getTableReference("Assets")
X %>% summarize(n())


db$writeTable(
  "Simulations",
  tibble(SIMULATION = 1:100000)
)

db$writeTable(
  "Assets",
  tibble(ASSET = 1:4000)
)

db$disconnect()

db$execute("CREATE TABLE Combinations AS SELECT * FROM Simulations CROSS JOIN Assets")

db$execute("
           SELECT * 
           FROM Assets
           ")
C = db$getTableReference("Combinations")
dplyr::filter(C, ASSET == 2)

assets = db$getTableReference("Assets")
sims = db$getTableReference("Simulations")
combis = db$getTableReference("CombisShort")
qu = combis %>% mutate(Val = 3) %>% 
  pivot_wider(names_from = "ASSET", values_from = "Val") %>% 
  dplyr::show_query()


db$execute("CREATE VIEW Combi3 AS SELECT * FROM Combinations WHERE SIMULATION = 3")
db$execute("DROP VIEW Combi3")
db$listTables()

# Glue ----

library(glue)

?glue_sql
name = "Daniel"

glue_sql("SELECT * from data where Name = {name}")

glue(
  "SELECT {name} FROM {name}"
)

db_ = db$.__enclos_env__$private$database
fr = "Combinations"
sel = c("SIMULATION", "ASSET")
choice = 5
wh = "SIMULATION = 3"

ss = glue_sql_collapse(glue_sql("{`sel`}", .con = db_), sep = ", ")
ss
qu = glue_sql("SELECT {sel} FROM {fr} WHERE `SIMULATION` = {choice}", .con = db_)
qu = glue_sql("SELECT {ss} FROM {fr} WHERE {wh} LIMIT 3", .con = db_)
qu

db$getQuery(qu)
db$getQuery(glue("SELECT {sel} FROM {fr}"))
db$getSelectQuery(sel, "Combinations")
