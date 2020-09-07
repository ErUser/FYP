#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(lubridate)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(prophet)
library(plyr)
library(dplyr)
library(tidyverse)
library(GGally)
library(cowplot)
library(mice)
library(VIM)
library(keras)
library(caret)
library(caTools)
library(datetime)
library(DT)



#sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Past Year", icon = icon("th"),tabName = "past"),
    menuItem("Input Dataset", icon = icon("list-alt"),tabName = "input")
  )
)

#body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            fluidPage(
              theme = "default.css",
              fluidRow(
                
                h4(textOutput("currentTime")),
                column(4, 
                       h1(textOutput("currentDay")),
                       h1(htmlOutput("display_temp")) %>% tagAppendAttributes(class="temp-font"),
                       textOutput("degree") %>% tagAppendAttributes(class="degree-font")
                       
                ),
                column(4,
                       htmlOutput("current_datetime") %>% tagAppendAttributes(class="attribute")),
                
                
              ),
              fluidRow(uiOutput("btn_future_day")),
              #fluidRow(htmlOutput("current_datetime")),
            ),
    ),
    tabItem(tabName = "past",
            h2("Mean Temperature per day"),
            fluidRow(
              box(
                title = "Data Range (Start Year)", status = "primary",
                uiOutput("year_slider")
                
             
              ),
            ),
            hr(),
            fluidRow(plotOutput("weatherSg"))
            
    ),
    tabItem(tabName = "input",
            fluidRow(
              fileInput("dataset_file", "Choose CSV File",
                        accept = c("test/csv", 
                                   "text/comma-separated-values,text/plain", 
                                   ".csv")
                        ),
              actionButton("btn_file", "Upload")
            ),
            fluidRow(
              mainPanel(DT::dataTableOutput("contents", height = 500))
            )
          
    )
  )
)

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
  dashboardHeader(title = "SG Weather"),
  sidebar,
  body
))
