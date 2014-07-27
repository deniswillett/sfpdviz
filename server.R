
# This is the server logic for a Shiny web application.
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

d_sfpd <- fread('d_sfpd.csv')

d_sfpd <- as.data.frame(d_sfpd)

crimes = unique(d_sfpd$Category)
violent_crimes = crimes[c(1,2,21, 25)]
nonviolent_crimes = crimes[-c(1,2,21, 25)]

sfmap <- get_map('San Francisco', zoom = 12, maptype = 'toner', source = 'stamen')


shinyServer(function(input, output) {
        sf_map <- reactive({
                d_sfpd %>%
                        mutate(date = mdy(Date),
                               lat = as.numeric(Lat),
                               lon = as.numeric(Lon)) %>%
                        filter(year(date) == 2013) %>%
                        select(lat, lon) %>%
                        na.omit(.)
                
                if (input$spatcategory == 'all') {
                        d_sfpd %>%
                                mutate(date = mdy(Date),
                                       lat = as.numeric(Lat),
                                       lon = as.numeric(Lon)) %>%
                                filter(year(date) == input$spattimeperiod,
                                       PdDistrict %in% input$spatdistrict) %>%
                                select(lat, lon) %>%
                                na.omit(.)
                } else {
                        if (input$spatcategory == 'violent') {
                                d_sfpd %>%
                                        mutate(date = mdy(Date),
                                               lat = as.numeric(Lat),
                                               lon = as.numeric(Lon)) %>%
                                        filter(year(date) == input$spattimeperiod,
                                               Category %in% violent_crimes,
                                               PdDistrict %in% input$spatdistrict) %>%
                                        select(lat, lon) %>%
                                        na.omit(.)
                        } else {
                                d_sfpd %>%
                                        mutate(date = mdy(Date),
                                               lat = as.numeric(Lat),
                                               lon = as.numeric(Lon)) %>%
                                        filter(year(date) == input$spattimeperiod,
                                               Category %in% nonviolent_crimes,
                                               PdDistrict %in% input$spatdistrict) %>%
                                        select(lat, lon) %>%
                                        na.omit(.)
                        }
                }
        })
        
        
        daily  <- reactive({
                if (input$daycategory == 'all') {
                        d_sfpd %>%
                                mutate(date = mdy(Date),
                                       time = mdy_hm(paste(Date,Time))) %>%
                                filter(year(date) >= input$daytimeperiod[1],
                                       year(date) <= input$daytimeperiod[2],
                                       PdDistrict %in% input$daydistrict) %>%
                                group_by(date, hr = hour(time)) %>%
                                summarize(n = n()) %>%
                                group_by(hr) %>%
                                summarize(avg = mean(n),
                                          sd = sd(n))
                } else {
                        if (input$daycategory == 'violent') {
                                d_sfpd %>%
                                        mutate(date = mdy(Date),
                                               time = mdy_hm(paste(Date,Time))) %>%
                                        filter(year(date) >= input$daytimeperiod[1],
                                               year(date) <= input$daytimeperiod[2],
                                               Category %in% violent_crimes,
                                               PdDistrict %in% input$daydistrict) %>%
                                        group_by(date, hr = hour(time)) %>%
                                        summarize(n = n()) %>%
                                        group_by(hr) %>%
                                        summarize(avg = mean(n),
                                                  sd = sd(n))
                        } else {
                                d_sfpd %>%
                                        mutate(date = mdy(Date),
                                               time = mdy_hm(paste(Date,Time))) %>%
                                        filter(year(date) >= input$daytimeperiod[1],
                                               year(date) <= input$daytimeperiod[2],
                                               Category %in% nonviolent_crimes,
                                               PdDistrict %in% input$daydistrict) %>%
                                        group_by(date, hr = hour(time)) %>%
                                        summarize(n = n()) %>%
                                        group_by(hr) %>%
                                        summarize(avg = mean(n),
                                                  sd = sd(n))
                        }
                }
        })
        
        
        dofwdata <- reactive({
                if (input$tempcategory == 'all') {
                        d_sfpd %>%
                                mutate(date = mdy(Date)) %>%
                                filter(year(date) >= input$temptimeperiod[1],
                                       year(date) <= input$temptimeperiod[2],
                                       PdDistrict %in% input$tempdistrict) %>%
                                group_by(date) %>%
                                summarize(n = n(),
                                          dofw = factor(first(DayOfWeek), 
                                                        ordered = TRUE,
                                                        levels = c('Monday', 'Tuesday', 
                                                                  'Wednesday','Thursday',
                                                                  'Friday', 'Saturday',
                                                                  'Sunday')))%>%
                                group_by(dofw) %>%
                                summarize(avg = mean(n),
                                          sd = sd(n))
                } else {
                        if (input$tempcategory == 'violent') {
                                d_sfpd %>%
                                        mutate(date = mdy(Date)) %>%
                                        filter(year(date) >= input$temptimeperiod[1],
                                               year(date) <= input$temptimeperiod[2],
                                               Category %in% violent_crimes,
                                               PdDistrict %in% input$tempdistrict) %>%
                                        group_by(date) %>%
                                        summarize(n = n(),
                                                  dofw = factor(first(DayOfWeek), 
                                                                ordered = TRUE,
                                                                levels = c('Monday', 'Tuesday', 
                                                                           'Wednesday','Thursday',
                                                                           'Friday', 'Saturday',
                                                                           'Sunday'))) %>%
                                        group_by(dofw) %>%
                                        summarize(avg = mean(n),
                                                  sd = sd(n))
                        } else {
                                d_sfpd %>%
                                        mutate(date = mdy(Date)) %>%
                                        filter(year(date) >= input$temptimeperiod[1],
                                               year(date) <= input$temptimeperiod[2],
                                               Category %in% nonviolent_crimes,
                                               PdDistrict %in% input$tempdistrict) %>%
                                        group_by(date) %>%
                                        summarize(n = n(),
                                                  dofw = factor(first(DayOfWeek), 
                                                                ordered = TRUE,
                                                                levels = c('Monday', 'Tuesday', 
                                                                           'Wednesday','Thursday',
                                                                           'Friday', 'Saturday',
                                                                           'Sunday')))%>%
                                        group_by(dofw) %>%
                                        summarize(avg = mean(n),
                                                  sd = sd(n))
                        }
                }
        })
        
        tseries  <- reactive({
                if (input$category == 'all') {
                        d_sfpd %>% 
                                mutate(date = mdy(Date)) %>%
                                filter(year(date) >= input$timeperiod[1],
                                       year(date) <= input$timeperiod[2],
                                       PdDistrict %in% input$district) %>%
                                group_by(date) %>%
                                summarize(n = n())
                } else {
                        if (input$category == 'violent') {
                                d_sfpd %>% 
                                        mutate(date = mdy(Date)) %>%
                                        filter(year(date) >= input$timeperiod[1],
                                               year(date) <= input$timeperiod[2],
                                               Category %in% violent_crimes,
                                               PdDistrict %in% input$district) %>%
                                        group_by(date) %>%
                                        summarize(n = n())
                        } else {
                                d_sfpd %>% 
                                        mutate(date = mdy(Date)) %>%
                                        filter(year(date) >= input$timeperiod[1],
                                               year(date) <= input$timeperiod[2],
                                               Category %in% nonviolent_crimes,
                                               PdDistrict %in% input$district) %>%
                                        group_by(date) %>%
                                        summarize(n = n())
                        }
                }
               
        })
        
        dec  <- reactive({
                decompose(ts(tseries()$n, frequency = 365))
        })
        
        plot_tseries  <- reactive({
                data.frame(Date = tseries()$date,
                           Incidents = as.numeric(dec()$x),
                           Trend = as.numeric(dec()$trend),
                           Seasonal = as.numeric(dec()$seasonal),
                           Random = as.numeric(dec()$random))
        })
        
        m_plot <- reactive({
                melt(plot_tseries(), id = 'Date', variable.name = 'Type', 
                     value.name = 'n')
        })
        
        output$timeSeries <- renderPlot({
                ggplot(m_plot(), aes(x = Date, y = n)) +
                        geom_line(color = 'blue') +
                        theme_bw(14) +
                        facet_grid(Type ~ . , scales = 'free') +
                        labs(x = '', y = '                               Number of Incidents')
        })
        
       output$dofw <- renderPlot({
               ggplot(dofwdata(), aes(x = dofw, y = avg)) +
                       geom_point() +
                       geom_pointrange(aes(x = dofw, y = avg, ymax = avg + sd, ymin = avg - sd), 
                                     color = 'green') +
                       theme_bw(14) +
                       labs(x = '', y = "Average Number of Incidents per Day")
       })
       
       output$daypattern  <- renderPlot({
               ggplot(daily(), aes(x = hr, y = avg)) +
                       geom_line() +
                       geom_ribbon(aes(x = hr, ymax = avg + sd, ymin = avg - sd), fill = 'grey50',
                                   alpha = 0.2)+
                       theme_bw(14) + stat_smooth(method = 'loess', se = FALSE) +
                       scale_x_continuous(breaks = c(0,6,12,18,23), 
                                          labels = c('Midnight', '6 AM', 'Noon', '6 PM', 'Midnight')) +
                       labs(x = '', y = 'Average Number of Incidents')
       })
       
       output$map <- renderPlot({
               ggmap(sfmap) + 
                       stat_density2d(data = sf_map(), 
                                      aes(x = lon, y = lat, fill = ..level..), 
                                      geom = 'polygon', bins = 64, alpha = 0.24)+
                       theme(axis.ticks = element_blank(),
                             axis.text = element_blank(),
                             axis.title = element_blank())+
                       scale_fill_gradient(name = 'Incidents')
       })
                 



})
