suppressPackageStartupMessages({
  if (!require('shiny')){install.packages('shiny')}; library(shiny)
  if (!require('magrittr')){install.packages('magrittr')}; library(magrittr)
  if (!require('ggplot2')){install.packages('ggplot2')}; library(ggplot2)
  if (!require('dplyr')){install.packages('dplyr')}; library(dplyr)
  if (!require('h2o')){install.packages('h2o')}; library(h2o)
  if (!require('DT')){install.packages('DT')};library(DT)
  if (!require('markdown')){install.packages('markdown')};library(markdown)
  if (!require('Amelia')){install.packages('Amelia')};library(Amelia)
  
})