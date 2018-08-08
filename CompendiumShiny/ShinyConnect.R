setwd("~/Box Sync/R_Scripts&Data/20171218JAP_iceberg/Shiny/")
library(rsconnect)
library(shiny)
library(plotly)
library(DT)

rsconnect::setAccountInfo(name='mcleanresearchgroup',
                          token='4D74565AAE9F5C1136D01D06EDB21925',
                          secret='UaOUOUHM4btjQkktWGiETJzapeqB1FGVHVDTdHsJ')

rsconnect::deployApp("~/Box Sync/R_Scripts&Data/20171218JAP_iceberg/Shiny/")
