library(shiny)
library(plotly)
library(leaflet)
source("global.r")
# Define UI for dataset viewer application
shinyUI(fluidPage(  

titlePanel("HDD分析網"),

sidebarLayout(
sidebarPanel(
   #select brand
   uiOutput("brandui"),
   selectInput("class", "Choose a class:",
                choices = unique(allstorage$Class)),
   uiOutput("sizeui"),
   radioButtons("warranty", "Choose Warranty:",
                 choices = c("None" = 0,"2Year" = 2, "3Year" = 3, "5Year" = 5), inline=T),
   uiOutput("priceui")
 ),  
 
mainPanel(
   tabsetPanel(type = "tabs", 
               tabPanel("比價王", plotlyOutput("plot")),
               tabPanel("價格趨勢", plotlyOutput("line")),
               tabPanel("各牌占比", plotlyOutput("pie")),
               tabPanel("原價屋據點", leafletOutput("mpgPlot"))
              )
 )
 )
))