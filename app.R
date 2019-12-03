library(leaflet)
library(dplyr)
library(htmltools)
library(shiny)
library(shinydashboard)
library(highcharter)
library(stringr)


geodata <- read.csv("geocoded.csv")
geodata$HospitalName <- as.character(geodata$HospitalName)
NN_Color <- c("#0299F4","#01387B","#0299F4")



## Labels for popups

labs <- lapply(seq(nrow(geodata)), function(i) {
  paste0(
    '<p>',
    "<b> Site Name: </b>",
    geodata[i, "HospitalName"],
    '<p></p> ',
    #"<b> Title: </b>", geodata[i, "Title"], '<p></p> ',
    "<b> Conditions: </b>",
    geodata[i, "Conditions"],
    '</p><p>',
    "<b> Phases: </b>",
    geodata[i, "Phases"],
    '</p>' ,
    "<b> Enrollment: </b>",
    geodata[i, "Enrollment"],
    '</p>',
    "<b> URL: </b>",
    "<a href='",
    geodata[i, "URL"],
    "'>Click Here for Link</a>",
    '</p>'
  )
})


##Colors for data
getColor <- function(geocolor) {
  sapply(geodata$Phases, function(Phases) {
    if (Phases == levels(geodata$Phases)[1]) {
      "green"
    } else if (Phases == levels(geodata$Phases)[2]) {
      "orange"
    } else {
      "red"
    }
  })
}
#Icons for map
icons1 <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(geodata)
)

# #create a grouping variable -- this can be whatever you want to filter by
# geodata <- geodata %>%
#   mutate(groups =  case_when(Phases == "Not Applicable" ~ 1,
#                              Phases == "Phase 4" ~ 2))
# geodata$groups <- as.numeric(geodata$groups)
# ##Gør biget ned grupe så det vurjer...
# k <- n_distinct(geodata$Phases)
# map <- leaflet() %>% addTiles()
# geodata.df <- split(geodata, geodata$groups)
# names(geodata.df) %>%
#   purrr::walk(function(df) {
#     map <<- map %>%
#       addMarkers(
#         data = geodata.df[[df]],
#         lng =  ~ lon,
#         lat =  ~ lat,
#         label =  ~ as.character(HospitalName),
#         popup =  ~ lapply(labs, htmltools::HTML),
#         group = df,
#         clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
#         labelOptions = labelOptions(noHide = F,
#                                     direction = 'auto')
#       )
#   })
# 
# map %>%
#   addLayersControl(
#     overlayGroups = names(geodata.df),
#     options = layersControlOptions(collapsed = FALSE)
#   )



ui <- dashboardPage(
  dashboardHeader(title = "Clinical trial overview"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
      ),
    menuItem(
      "Geomap",
      icon = icon("map-marker"),
      tabName = "geomap"
      
    )
  )),
  dashboardBody(tabItems(tabItem(
    tabName = "dashboard",
    box(
      status = "primary",
      width = 12,
      solidHeader = TRUE,
      collapsible = TRUE,
      fluidRow(
        column(width = 6,
               
               highchartOutput("trialsbyPhases")),
        column(width = 6,
               highchartOutput("enrollbyPhases"))
      )
    ),
    fluidRow(
      box(
        status = "warning",
        width = 12,
        solidHeader = TRUE,
        collapsible = TRUE,
        column(width = 6,
               highchartOutput("diabeteschart")),
        column(width = 6,
               highchartOutput("piechart"))
      )
    )
  ), 
    tabItem(tabName = "geomap",
            fluidRow(leafletOutput("geomap")))
  ))
)

server <- function(input, output,session) {
  
output$piechart <- renderHighchart({
  geodata %>% count(Funded.Bys) %>% arrange(n) %>% hchart(type="pie",hcaes(x=Funded.Bys, y=n), name = "Value") %>%
    hc_tooltip(pointformat=paste("<b>{point.percentage:.1f}%</b>")) %>% hc_title(text="Study funding sources")
})  
  
output$diabeteschart <- renderHighchart({
  
  geodata %>% count (IsDiabetes) %>% arrange (n) %>% filter(n>1) %>%
    hchart(type="column", hcaes (x=IsDiabetes, y=n), name="Studies", dataLabels = list(
      enabled = TRUE,
      formatter = JS(
        "function() {
                  return this.point.y;
                  }" ))) %>%
    hc_title(text="Distribution of current studies in the US") %>%
    hc_xAxis(title=list(text="Condition Types")) %>%
    hc_yAxis(title=list(text="Log Scaled Number of studies in category"),type="logarithmic") %>%
    hc_colors(NN_Color)
  
})  
output$trialsbyPhases <- renderHighchart({
  geodata %>% count(Phases) %>% arrange(n) %>%
    hchart(type="column", hcaes (x=Phases, y=n),name="Trials",dataLabels = list(
      enabled = TRUE,
      formatter = JS(
        "function() {
      return this.point.y  ;
    }"))) %>% hc_title(text="Number of trials in different phases") %>%
    hc_yAxis(title=list(text="Number of Studies"))
})  
output$enrollbyPhases <- renderHighchart({
  #Chart of enrollment by phases.
  geodata %>% group_by(Phases) %>%
    summarize(Enroll=sum(Enrollment,na.rm=TRUE)) %>% arrange(Enroll) %>% 
    hchart(type="column", hcaes(x=Phases,y=Enroll),name="Enrollment",dataLabels = list(
      enabled = TRUE,
      formatter = JS(
        "function() {
                  return this.point.y;
                  }" ))) %>% hc_colors(NN_Color) %>% 
    hc_title(text="Enrollment of patients in different phases") %>%
    hc_add_series(Name="Enrollment",ShowInLegend=FALSE)
})
 output$geomap <- renderLeaflet({
   ## geomap
   geodata %>% leaflet() %>% addTiles() %>%
     addAwesomeMarkers(
       ~ lon,
       ~ lat,
       clusterOptions = markerClusterOptions(),
       clusterId = "Phasecluster",
       popup = lapply(labs, htmltools::HTML),
       label =  ~ as.character(HospitalName),
       icon = icons1
     ) %>%
     addLegend(colors = c("Green","Orange"),labels= ~levels(Phases), opacity = 1) %>%
     addEasyButton(easyButton(
       states = list(
         easyButtonState(
           stateName="unfrozen-markers",
           icon="ion-toggle",
           title="Freeze Clusters",
           onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'Phasecluster');
            clusterManager.freezeAtZoom();
            btn.state('frozen-markers');
          }")
         ),
         easyButtonState(
           stateName="frozen-markers",
           icon="ion-toggle-filled",
           title="UnFreeze Clusters",
           onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'Phasecluster');
            clusterManager.unfreeze();
            btn.state('unfrozen-markers');
          }")
         )
       )
     ))
 }) 
}

shinyApp(ui, server)





  