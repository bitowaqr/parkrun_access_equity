# SRC 5: create interactive map

# Load pkgs -------
library(leaflet)
library(htmlwidgets)
library(shiny)


# load data
# event_sp # existing event locations
candidates.coords = cbind(lng=top_consecutive_parks$lon,lat = top_consecutive_parks$lat) # recommended new locations


ref = tags$div(id="ref",
               style="padding-left: 10px;
                    padding-right: 10px;
                    background: rgba(245,245,245,0.5);
                    font-size: 12px;",
               'Schneider et al. Multiple deprivation and geographic distance to community physical activity events. Available at:',
               tags$a(href="", "tbc",target="_blank"))


# build leaflet map
   map <- 
    leaflet(options = leafletOptions(minZoom = 7, maxZoom = 15),
                 width = "100%",height = "800px") %>% 
    # provider tiles
    # addTiles(group="OSM") %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik, group="OSM Mapnik<br>") %>% 
    addProviderTiles(providers$CartoDB.Positron, group="CartoDB") %>%
    
    # add reference tag
    addControl(ref, position = "bottomleft", className="map-ref") %>% 
  
    # add CONTROLS           
    
    addLayersControl(
      baseGroups = c("CartoDB","OSM Mapnik<br>"),
      overlayGroups = c("parkrun events (12 Dec 2018)","Recommended locations<br>&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp(Numbers indicate rank)"),
      options = layersControlOptions(collapsed = F,autoZIndex=F,opacity=0) 
    ) %>%
    
    addCircles(
      group = "parkrun events (12 Dec 2018)",
      data = event_sp,
      radius = 100,fillColor = "blue",
      stroke = F, fillOpacity = 1,
      opacity = 1,color="blue"
    ) %>%
    addCircles(
      group = "parkrun events (12 Dec 2018)",
      data = event_sp,
      radius = 2500,fillColor = "blue",
      stroke = F, fillOpacity = 0.4,
      opacity = 1,color="blue",weight = 1,
      popup = paste("parkrun event:",event_sp$course,"<br>",
                    "Established:", event_sp$Estblsh)
    ) %>%
    addCircles(
      data = top_consecutive_parks,
      group = "Recommended locations<br>&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp(Numbers indicate rank)",
      radius = 100,fillColor = "orange",
      stroke = F, fillOpacity = 1,
      lng = ~lon, lat = ~lat
    ) %>%
    addCircles(
      data = top_consecutive_parks,
      group = "Recommended locations<br>&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp(Numbers indicate rank)",
      radius = 2500,fillColor = "orange",weight = 1,
      stroke = T, fillOpacity = 0.4,color = "orange",
      popup = paste("Name: ",top_consecutive_parks$distName1,"<br>",
                    "Rank: ", top_consecutive_parks$rank,"<br>",
                    "Access improvement: ",top_consecutive_parks$change, "%<br>",
                    "Coordinates: ", round(top_consecutive_parks$lat,5), "; ",round(top_consecutive_parks$lon,5),"<br>",
                    "<a href='https://www.google.com/maps/@",top_consecutive_parks$lat,",",top_consecutive_parks$lon,",2515m/data=!3m1!1e3' target='_blank'>Show on Google Maps</a>",
                    sep=""),
      lng = ~lon, lat = ~lat,
      label=as.character(top_consecutive_parks$rank),
      labelOptions = labelOptions(noHide = T, textsize = "9px",textOnly = T, direction ="center")
    ) 
  
  
  
    saveWidget(map, file="index.html")
    