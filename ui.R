
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(ggmap)
library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)
library(reshape2)
library(ggmap)
library(gridExtra)
library(markdown)
library(shiny)

d.sfpd <- fread('d_sfpd.csv')

shinyUI(navbarPage(

  # Application title
  title = "SF Crime Data", theme = 'bootstrap.css',
  
  tabPanel(title = 'Home',
           fluidRow(
                   includeMarkdown('home.md')
                   )
           ),
  
  tabPanel(title = 'Time Series', 
           sidebarLayout(
                   sidebarPanel(
                           radioButtons(inputId = 'category',
                                        label = 'Type of Incident:',
                                        choices = c('All Incidents' = 'all',
                                                    'Violent Crime' = 'violent',
                                                    'Non Violent Crimes' = 'nonviolent')),
                           
                           checkboxGroupInput(inputId = 'district',
                                              label = 'District:',
                                              choices = unique(d.sfpd$PdDistrict),
                                              selected = unique(d.sfpd$PdDistrict)),
                           
                           sliderInput( inputId = 'timeperiod',
                                        label = 'Time Period:',
                                        min = 2003,
                                        max = 2014, 
                                        value = c(2003, 2014),
                                        step = 1, format = '####'),
                           
                           submitButton(text = "Apply Changes", icon = NULL)
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel(
                           includeMarkdown('timeseries.md'),
                           plotOutput("timeSeries")
                   )
           )
           ),

  tabPanel(title = 'Weekly Patterns',
           sidebarLayout(
                   sidebarPanel(
                           radioButtons(inputId = 'tempcategory',
                                        label = 'Type of Incident:',
                                        choices = c('All Incidents' = 'all',
                                                    'Violent Crime' = 'violent',
                                                    'Non Violent Crimes' = 'nonviolent')),
                           
                           checkboxGroupInput(inputId = 'tempdistrict',
                                              label = 'District:',
                                              choices = unique(d.sfpd$PdDistrict),
                                              selected = unique(d.sfpd$PdDistrict)),
                           
                           sliderInput( inputId = 'temptimeperiod',
                                        label = 'Time Period:',
                                        min = 2003,
                                        max = 2014, 
                                        value = c(2003, 2014),
                                        step = 1, format = '####'),
                           
                           submitButton(text = "Apply Changes", icon = NULL)
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel(
                           includeMarkdown('dofw.md'),
                           plotOutput("dofw")
                   ) 
                   )
           ),

  tabPanel(title = 'Daily Patterns',
           sidebarLayout(
                   sidebarPanel(
                           radioButtons(inputId = 'daycategory',
                                        label = 'Type of Incident:',
                                        choices = c('All Incidents' = 'all',
                                                    'Violent Crime' = 'violent',
                                                    'Non Violent Crimes' = 'nonviolent')),
                           
                           checkboxGroupInput(inputId = 'daydistrict',
                                              label = 'District:',
                                              choices = unique(d.sfpd$PdDistrict),
                                              selected = unique(d.sfpd$PdDistrict)),
                           
                           sliderInput( inputId = 'daytimeperiod',
                                        label = 'Time Period:',
                                        min = 2003,
                                        max = 2014, 
                                        value = c(2003, 2014),
                                        step = 1, format = '####'),
                           
                           submitButton(text = "Apply Changes", icon = NULL)
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel(
                           includeMarkdown('daily.md'),
                           plotOutput("daypattern")
                   ) 
           )
  ),
  
  tabPanel(title = 'Spatial Patterns',
           sidebarLayout(
                   sidebarPanel(
                           radioButtons(inputId = 'spatcategory',
                                        label = 'Type of Incident:',
                                        choices = c('All Incidents' = 'all',
                                                    'Violent Crime' = 'violent',
                                                    'Non Violent Crimes' = 'nonviolent')),
                           
                           checkboxGroupInput(inputId = 'spatdistrict',
                                              label = 'District:',
                                              choices = unique(d.sfpd$PdDistrict),
                                              selected = unique(d.sfpd$PdDistrict)),
                           
                           sliderInput( inputId = 'spattimeperiod',
                                        label = 'Time Period:',
                                        min = 2003,
                                        max = 2014, 
                                        value = 2010,
                                        step = 1, format = '####'),
                           
                           submitButton(text = "Apply Changes", icon = NULL)
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel(
                           includeMarkdown('map.md'),
                           plotOutput("map")
                   ) 
           )
  )
  
))
