#******************************************************************************
#
# global.R
# Loads packages used in server.R and ui.R
#
#******************************************************************************
require(shiny) # Required for the Shiny application
# devtools::install_github("shiny-incubator", "rstudio")
require(shinyIncubator)
# if (!require("devtools"))
#   install.packages("devtools")
# if (!require("rCharts"))
#   devtools::install_github('rCharts', 'ramnathv', ref = "dev")
require(rCharts)
require(igraph)
require(WriteXLS)

options(shiny.maxRequestSize=10*1024^2)