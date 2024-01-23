# Load packages -----------------------------------------------------

if(!require("shiny")) install.packages("shiny", dependencies = TRUE)
if(!require("tools")) install.packages("tools", dependencies = TRUE)
if(!require("stringr")) install.packages("stringr", dependencies = TRUE)
if(!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)
if(!require("ggthemes")) install.packages("ggthemes", dependencies = TRUE)
if(!require("ggpubr")) install.packages("ggpubr", dependencies = TRUE)
if(!require("shinythemes")) install.packages("shinythemes", dependencies = TRUE)
if(!require("rsconnect")) install.packages("rsconnect", dependencies = TRUE)
if(!require("shinyWidgets")) install.packages("shinyWidgets", dependencies = TRUE)
if(!require("shinydashboard")) install.packages("shinydashboard", dependencies = TRUE)
if(!require("shinyalert")) install.packages("shinyalert", dependencies = TRUE)
if(!require("leaflet")) install.packages("leaflet", dependencies = TRUE)
if(!require("devtools")) install.packages("devtools", dependencies = TRUE)
if(!require("dashboardthemes")) install_github("nik01010/dashboardthemes", dependencies = TRUE)
if(!require("markdown")) install.packages("markdown", dependencies = TRUE)


rm(list=ls())
biota <- read_csv("table_biota_all_variables_YSOUZA_10-02-2020.csv")
biota_seedlings <- read_csv("table_biota_seedlings_YSOUZA_08-02-2021.csv")

shinyServer(function(input,output){
  
  
  #shinyalert(
  #  title = "Info message",
  # text = "The filter applyed in the Data exploration will also be applyed in the Data table tab",
  #  size = "l", 
  # closeOnEsc = TRUE,
  #  closeOnClickOutside = FALSE,
  #  html = TRUE,
  #  type = "info",
  #  showConfirmButton = TRUE,
  #  showCancelButton = FALSE,
  #  confirmButtonText = "OK",
  #  confirmButtonCol = "AEDEF4",
  #  timer = 0,
  #  animation = FALSE
  #)
  
  
  output$mymap <- renderLeaflet({
    leaflet(biota) %>% 
      setView(lat = -24.106261, lng = -46.5814557, zoom = 8)  %>% 
      addProviderTiles(providers$OpenStreetMap, group = "Open Street Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "National Geographic World Map") %>%
      addProviderTiles(providers$OpenTopoMap, group = "Open Topo Map") %>%
      addProviderTiles(providers$Esri.WorldPhysical, group = "World Physical") %>%
      #addProviderTiles(providers$Stamen, group = "Stamen") %>%
      addLayersControl(baseGroups = c("Open Street Map", "Esri World Imagery", 
                                      "National Geographic World Map", "Open Topo Map", 
                                      "World Physical"), 
                       options = layersControlOptions(collapsed = TRUE)) %>% 
      addTiles(group = "National Geographic World Map") %>% 
      addMiniMap(zoomLevelOffset = -4) %>%
      addScaleBar() %>% 
      addCircles(data = biota, lat = ~ Latitude, lng = ~ Longitude, 
                 group = "Treatment" , weight = 15, color = c("blue", "red"), label = "Plots (red is closed and blue is open)") %>% 
      addMarkers(lng=-47.9535872, lat=-25.1297196, popup="Cardoso Island State Park") %>% 
      addMarkers(lng=-47.949167, lat=-24.131389, popup="Carlos Botelho State Park") %>% 
      addMarkers(lng=-45.1465229, lat=-23.3367648, popup="Serra do Mar State Park (Itamambuca base)") %>% 
      addMarkers(lng=-45.2449083, lat=-23.4379185, popup="Serra do Mar State Park (Santa Virginia base)")
  })
  


  # Print data table if checked
  output$biotatable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = biota,
                    filter = "top",
                    selection = "none",
                    class = 'cell-border stripe',
                    extensions = c("Buttons", "KeyTable", 'Scroller'),
                    options = list(
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#66CDAA', 'color': '1c1b1b'});",
                        "}"),
                      order = list(63, 'desc'), #reorder by modifying data
                      keys = TRUE,
                      autoWidth = TRUE,
                      columnDefs = list(
                        #list(targets = -1, orderable = TRUE),
                        list(className = 'dt-center', targets = "_all"),
                        list(targets = c(1:217), width = '200px')),
                      scrollX = TRUE,
                      dom = 'Blfrtip',
                      deferRender = TRUE,
                      scrollY = 450,
                      scroller = TRUE,
                      buttons = list(
                        I('colvis'),
                        list(
                          extend = "csv",
                          charset = 'UTF-8',
                          bom = TRUE,
                          text = "Download",
                          title = paste0("biota_all_variables-", Sys.Date())
                    )
                  )
                )
              )
    }
  )
  
  # download tables
#  output$downloadData <- downloadHandler(
#    filename = function(){
#      paste("biota", "csv", sep = ".")
#    },
#    content = function(file){
#      write_csv(biota_subset2(), file)
#    }
#  )
  

  output$downloadData1 <- downloadHandler(
    filename = function(){
      paste("biota_seedlings", "csv", sep = ".")
    },
    content = function(file){
      write_csv(biota_seedlings, file)
    }
  )
  

  # filter datatable
  biota_subset <- reactive({
    req(input$Location) # ensure availability of value before proceeding
    filter(biota, Location %in% input$Location & Date %in% input$Date)
  })
  
  # plot LM
  output$scatterplot <- renderPlot({
    
    ggplot(data = biota_subset(), aes_string(x = input$x, y = input$y,
                                             colour = input$z), fill = input$Date) +
      geom_smooth(method='loess', formula= y~x,  size=3) + 
      geom_point(alpha = input$alpha, size = 10) + 
      theme_gdocs() +
      #stat_cor(label.y = 1, label.x = 0) + 
      #stat_regline_equation(label.y = 1, label.x = 10) +
      scale_colour_manual(values = c("tomato4","darkgreen", "gold4", "darkorange3")) + 
      theme(axis.title.x = element_text(size=20, face="bold"),
            axis.title.y = element_text(size=20, face="bold"),
            axis.text.x = element_text(size=14, face="bold"),
            axis.text.y = element_text(size=14, face="bold"),
            legend.title = element_text(size=22, face="bold"),
            legend.text = element_text(size=14, face="bold"),
            plot.background = element_rect(fill = "#f4fafb",colour = NA),
            panel.grid.minor = element_blank(),
            legend.background = element_rect(fill="transparent",colour = NA)) +
      labs(
        color = toTitleCase(str_replace_all(input$z, "_", " ")),
        x=input$x_axis,
        y=input$y_axis,
        # With isolate() only update plot title when other inputs that
        # go into the plot change, not when the title is updated
        #title = isolate({ toTitleCase(input$plot_title) })
        title = input$plot_title
      )
  })

  output$counter <- 
    renderText({
      if (!file.exists("counter.Rdata")) 
        counter <- 0
      else
        load(file="counter.Rdata")
      counter  <- counter + 1
      save(counter, file="counter.Rdata")     
      paste("Hits: ", counter)
    })
  
  })