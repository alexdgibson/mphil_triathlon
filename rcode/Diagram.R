### Diagram Flow chart
library(diagram)
library(DiagrammeR)
library(tidyverse)
library(timevis)

grViz("digraph {
  graph[layout = dot, 
        rankdir = TB,
        overlap = true,
        fontsize = 10]
  node [shape = rectangle,
  fixedsize = false,
  width = 1.5]
  
  'Raw Data'
  'Subset Sex'
  'Standard Distance'
  'Final Standard Distance'
  'Clean Standard Distance Segments'
  
  'Raw Data' -> 'Subset Sex' [label = '   Subsetting elite men & elite women']
  'Subset Sex' -> 'Standard Distance' [label = '    Removing races shorter than standard distance']
  'Standard Distance' -> 'Final Standard Distance' [label = '   Flagging races with erroneous timing']
  'Final Standard Distance' -> 'Clean Standard Distance Segments' [label = '    Selecting races with only complete segment times']
}")


grViz("digraph {

  graph[layout = dot, 
        rankdir = TB,
        overlap = TRUE,
        fontsize = 10,
        fontname = 'Times New Roman']
  node [shape = rectangle,
  fixedsize = false,
  width = 2]
  
  
  'Raw Data \n(n = 97,374)' -> 'Elite Men \n(n = 61,099)'
  'Raw Data \n(n = 97,374)' -> 'Elite Women \n(n = 36,275)'
  'Elite Men \n(n = 61,099)' -> 'Not Standard Distance \n(n = 4,600)'
  'Elite Men \n(n = 61,099)' -> 'Standard Distance Elite Men \n(n = 57,929)' 
  'Elite Women \n(n = 36,275)' -> 'Not Standard Distance \n(n = 4,600)'
  'Elite Women \n(n = 36,275)' -> 'Standard Distance Elite Women \n(n = 34,845)'
}")

timevis(data.frame(
  id = 1:8,
  content = c("International Triathlon Union Formed",
              "World Cup Series Started",
              "World Championship Serires Started",
              "Introduction of Drafting",
              "Triathlon Olympic Debut",
              "Paratriathlon Paralympic Debut",
              "Mixed Relay Olympic Debut",
              "ITU Reformed to The World Triathlon Organization"),
  start = c("1989-01-01", 
            "1991-01-01",
            "2009-01-01",
            "1995-01-01",
            "2000-01-01",
            "2016-01-01",
            "2020-01-01",
            "2020-01-01"),
  end = c(NA,NA,NA,NA,NA,NA,NA,NA)
))
