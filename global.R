library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(plotly)
library(bslib)
library(knitr)
# library(toOrdinal)
library(shinyWidgets)
library(shinyjs)
library(markdown)

source('othercode/functions.R')

# #Update / do in conda if in env
# install.packages(c(
#   "shiny",
#   "tidyverse",
#   "sf",
#   "leaflet",
#   "plotly",
#   "bslib",
#   "knitr",
#   # "toOrdinal",
#   "shinyWidgets",
#   "shinyjs",
#   "markdown"
# ), dependencies = T)