
if(!require("shiny")) {install.packages("shiny")}
if(!require("pastecs")){install.packages("pastecs")}
if(!require("RColorBrewer")){install.packages("RColorBrewer")}
if(!require("Hmisc")){install.packages("Hmisc")}
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("reshape2")){install.packages("reshape2")}
if (!require("shiny")) {install.packages("shiny")}
if (!require("nFactors")) {install.packages("nFactors")}
if (!require("qgraph")) {install.packages("qgraph")}
if (!require("corrplot")) {install.packages("corrplot")}

try(require("caret")||install.packages("caret"))
try(require("rpart")||install.packages("rpart"))
try(require("rpart.plot")||install.packages("rpart.plot"))
try(require("randomForest")||install.packages("randomForest"))
try(require("dplyr")||install.packages("dplyr"))
try(require("hydroGOF")||install.packages("hydroGOF"))
try(require("party")||install.packages("party"))
try(require("partykit")||install.packages("partykit"))
try(require("mlogit")||install.packages("mlogit"))

if (!require("Rfast")) {install.packages("Rfast")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("multiROC")) {install.packages("multiROC")}
if (!require("PerformanceAnalytics")) {install.packages("PerformanceAnalytics")}