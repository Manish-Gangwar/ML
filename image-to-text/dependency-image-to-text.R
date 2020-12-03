# update.packages(ask = FALSE)
try(require("devtools")||install.packages("devtools"))
library("devtools")

try(require("shiny")||install.packages("shiny"))
try(require("tesseract")||install.packages("tesseract"))
try(require("magick")||devtools::install_github("ropensci/magick"))
try(require("magrittr")||install.packages("magrittr"))

library("shiny")
library("tesseract")
library("magick")
library("magrittr")

# now install magick to sharpen contrast & clarify input text images
