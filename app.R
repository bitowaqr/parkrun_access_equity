# parkrun, access, equity
# version 1.0

# load packages
library(ggplot2)
library(shiny)
library(leaflet)
library(cowplot)

# load data
centroid_lsoa = read.csv("./centroids.csv")
data = raster::shapefile("./data")
parkrun_marker = raster::shapefile("./marker")
vars1 <- c("Absolute" = "absolute","Relative (UNSTABLE!)" = "relative")   

# UI
ui <- fluidPage(
  
  # leaflet map init
   leafletOutput("mymap", width = "100%", height = 750),
   
   # user panel
   absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, 
                 top = "auto", left = "auto", right = 0, bottom = 0,
                 width = 300, height = "auto",
                 # title
                 h4("parkrun, access, equity"),
                 # color scaling adjuster
                 selectInput("color", "Scaling", vars1),
                 # plot bivariate relationships
                 plotOutput("p1_dist", height = 600)
                 )
   ) # end of UI
   


# ##   SERVER   #  ### #### ### #  ### #### ### #  

server <- function(input, output) {
  
  # Define base-map
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner Map") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner Map") %>%
      addTiles(group = "OSM Map") %>%
      addProviderTiles("CartoDB.Positron",group= "Carto Map", options= providerTileOptions(opacity = 0.99)) %>%
      setView(lng = -1.43, lat = 53.36, zoom = 7
      ) 
  })
  
  # Plot bivariate relationships
  output$p1_dist <- renderPlot({
    
    # plot for area within scope
    areasInBounds <- reactive({
      if (is.null(input$mymap_bounds)){return(data)}
      bounds <- input$mymap_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      subset.data = subset(data,
                           data$lat >= latRng[1] & data$lat <= latRng[2] &
                             data$lon >= lngRng[1] & data$lon <= lngRng[2]
                           )
      return(subset.data)
    })
    # data within scope
    plot_data = areasInBounds()@data
    
      # plot distance ~population density
        p1 = ggplot(plot_data[sample(1:length(plot_data[,1]),
                                      ifelse(length(plot_data[,1])>1000,1000,length(plot_data[,1]))),]) +
          geom_point(aes(x=log(pp_dnst), y=mn_dstn)) +
          geom_smooth(aes(x=log(pp_dnst),y=mn_dstn),method='lm',formula=y~x) +
          xlab("Population Density (log)") +
          ylab("Distance to nearest event") +
          ggtitle(paste( ifelse(length(plot_data[,1])>1000,1000,length(plot_data[,1])),
                  "sample data points"))
    
        # plot distance ~ deprivation
          p2 = ggplot(plot_data[sample(1:length(plot_data[,1]),
                                  ifelse(length(plot_data[,1])>1000,1000,length(plot_data[,1]))),]) +
            geom_point(aes(x=a, y=mn_dstn)) +
            geom_smooth(aes(x=a,y=mn_dstn),method='lm',formula=y~x) +
            xlab("Deprivation Score") +
            ylab("Distance to nearest event") +
            ggtitle(paste( ifelse(length(plot_data[,1])>1000,1000,length(plot_data[,1])),
                           "sample data points"))
          
          # combine plots
          p3 = cowplot::plot_grid(p1,p2,nrow=2)
          return(p3)
  })
  
  
  
  observe({
    
    # Area within scope for relative colors
    areasInBounds <- reactive({
      if (is.null(input$mymap_bounds)){return(data)}
      bounds <- input$mymap_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      subset.data = subset(data,
                           data$lat >= latRng[1] & data$lat <= latRng[2] &
                             data$lon >= lngRng[1] & data$lon <= lngRng[2]
      )
      return(subset.data)
    })
    
    # Relative or absolute scalang? 
    colorBy <- input$color
    if(colorBy=="absolute"){data_select = data}  
    if(colorBy =="relative"){data_select = areasInBounds()}
     
    # Color palete functions
    q_dists = as.numeric(quantile(data_select$mn_dstn,probs = c(seq(0,1,by=0.2))))
    pal_dist <- colorBin("RdYlGn", domain = data_select$mn_dstn, bins = q_dists,reverse =T)

    q_imd_a = as.numeric(quantile(data_select$a,probs = c(seq(0,1,by=0.2))))
    pal_imd_a <- colorBin("RdYlGn", domain = data_select$a, bins = q_imd_a,reverse = T)
    
    q_dens = as.numeric(quantile(data_select$pp_dnst,probs = c(seq(0,1,by=0.2))))
    pal_dens <- colorBin("RdYlGn", domain = data_select$pp_dnst, bins = q_dens,reverse = T)
    
    
    # UPDATE BASE MAP
      leafletProxy("mymap") %>%
        
        # clear everything if relative scaling selected
        clearShapes() %>%
        clearControls() %>%
        
        # control layers (add/remove)
        addLayersControl(
          baseGroups = c("Toner Map","OSM Map", "Carto Map"),
          overlayGroups = c("Event", "Distance","IMD","Density"),
          options = layersControlOptions(collapsed = T)
        ) %>%
        
        # ADD POLYGON LAYERS
        # layer 1
        addPolygons(data = data_select, group = "Distance",
                    color = "gray", smoothFactor = 0,stroke = T,
                    opacity = 0.5,weight = 0.1, 
                    fillOpacity = 0.5,
                    fillColor = ~pal_dist(mn_dstn),
                    highlight = highlightOptions(
                      weight = 1,
                      color = "white",
                      opacity = 0.5,
                      bringToFront = TRUE,
                      sendToBack = TRUE),
                    popup = paste(data_select$name,"<br>",
                                  "Event:", data_select$nrst_vn,"<br>",
                                  "Distance: ",data_select$mn_dstn," km <br>",
                                  "SIMD score:", data_select$a,"<br>",
                                  "Pop density:", data_select$pp_dnst,"<br>",
                                  "Population:", data_select$pop)
        ) %>%
        # layer 2
        addPolygons(data = data_select, group = "Density",
                    color = "gray", smoothFactor = 0,stroke = T,
                    opacity = 0.5,weight = 0.1, 
                    fillOpacity = 0.5,
                    fillColor = ~pal_dens(pp_dnst),
                    highlight = highlightOptions(
                      weight = 1,
                      color = "white",
                      opacity = 0.5,
                      bringToFront = TRUE,
                      sendToBack = TRUE),
                    popup = paste(data_select$name,"<br>",
                                  "Event:", data_select$nrst_vn,"<br>",
                                  "Distance: ",data_select$mn_dstn," km <br>",
                                  "SIMD score:", data_select$a,"<br>",
                                  "Pop density:", data_select$pp_dnst,"<br>",
                                  "Population:", data_select$pop)
        ) %>%
        # layer 3
        addPolygons(data = data_select, group = "IMD",
                    color = "gray", smoothFactor = 0,stroke = T,
                    opacity = 0.5,weight = 0.1, 
                    fillOpacity = 0.5,
                    fillColor = ~pal_imd_a(a),
                    highlight = highlightOptions(
                      weight = 1,
                      color = "white",
                      opacity = 0.5,
                      bringToFront = TRUE,
                      sendToBack = TRUE),
                    popup = paste(data_select$name,"<br>",
                                  "Event:", data_select$nrst_vn,"<br>",
                                  "Distance: ",data_select$mn_dstn," km <br>",
                                  "SIMD score:", data_select$a,"<br>",
                                  "Pop density:", data_select$pp_dnst,"<br>",
                                  "Population:", data_select$pop)
        ) %>%
        # ADD LEGENDS
        # legend 1
        addLegend("bottomleft", pal = pal_dist, values = data_select$mn_dstn,
                  opacity = 0.7, title="Distance quintiles",group = "Distance",
                  labFormat = labelFormat(
                    suffix  = "km")
        ) %>%
        # legend 2
        addLegend("bottomleft", pal = pal_imd_a, values = data_select$a,
                  opacity = 0.7, title="Deprivation quintiles",group = "IMD"
        ) %>% 
        # legend 3
        addLegend("bottomleft", pal = pal_dens, values = data_select$pp_dnst,
                  opacity = 0.7, title="Density quintiles",group = "Density",
                  labFormat = labelFormat(
                    suffix  = "km")
        ) %>%
        
        # ADD PARKRUN EVENT MARKERS
        addCircleMarkers(
          group = "Event",
          data = parkrun_marker,
          radius = 5,
          fillColor = "blue",
          stroke = FALSE, fillOpacity = 0.9,
          popup = paste("Course:",parkrun_marker$Club,"<br>",
                        "Established:", parkrun_marker$Estblsh,"<br>",
                        "Age in years:", parkrun_marker$Age_yrs,"<br>",
                        "Mean participants:", round(parkrun_marker$Mn_prtc),"<br>",
                        "Mean volunteers:", round(parkrun_marker$Mn_vlnt),"<br>")
        ) %>% 
        
        # hide groups after init
        hideGroup("IMD") %>% 
        hideGroup("Distance") %>% 
        hideGroup("Density")
      
      }) # end of 'observe' 
  
  
} # end of server
  
  
# Run the map
shinyApp(ui = ui, server = server)

