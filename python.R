


if(!require("shiny")) {install.packages("shiny")}
if(!require("reticulate")) {install.packages("reticulate")}

library(shiny)
library(reticulate)
sklearn <- import(sklearn)

# # scourcing Python --------------------------------------------------------
# import pandas
# def read_flights(file):
#   flights = pandas.read_csv(file)
# flights = flights[flights['dest'] == "ORD"]
# flights = flights[['carrier', 'dep_delay', 'arr_delay']]
# flights = flights.dropna()
# return flights
# ---------------------------------
# source_python("flights.py")
# flights <- read_flights("flights.csv")
# ------alternatively diectly call ----------------------------
# library(reticulate)
# os <- import("os")
# os$listdir(".")


log_reg = reactive({

  
  
  })