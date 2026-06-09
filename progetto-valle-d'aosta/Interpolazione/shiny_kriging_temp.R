library(shiny)
library(sf)
library(dplyr)
library(leaflet)
library(ggplot2)
library(viridisLite)
library(bslib)

# ------------------------------------------------------------
# File paths
# ------------------------------------------------------------
pred_path <- "~/Desktop/Research/UNVDA/data/predictions_grid_sf_rolling.rds"
muni_path <- "~/Desktop/Research/UNVDA/data/municipality_polygons_rolling.rds"

# ------------------------------------------------------------
# Read prediction data
# ------------------------------------------------------------
pred_sf <- readRDS(pred_path)

if (!inherits(pred_sf, "sf")) {
  stop("predictions_grid_sf_rolling.rds must be an sf object.")
}

pred_sf$date <- as.Date(pred_sf$date)

required_cols <- c("variable", "date", "pred_value")
missing_cols <- setdiff(required_cols, names(pred_sf))
if (length(missing_cols) > 0) {
  stop("pred_sf is missing: ", paste(missing_cols, collapse = ", "))
}

if (!"municipality_key" %in% names(pred_sf)) {
  pred_sf$municipality_key <- "unknown"
}

pred_sf <- pred_sf %>%
  filter(
    !is.na(variable),
    !is.na(date),
    is.finite(pred_value)
  )

if (nrow(pred_sf) == 0) {
  stop("No usable rows in prediction object.")
}

# ------------------------------------------------------------
# Transform to leaflet CRS and EXTRACT fresh lon/lat
# ------------------------------------------------------------
pred_sf_4326 <- st_transform(pred_sf, 4326)

coords_4326 <- st_coordinates(pred_sf_4326)
pred_sf_4326$lon <- coords_4326[, 1]
pred_sf_4326$lat <- coords_4326[, 2]

pred_df <- pred_sf_4326 %>%
  st_drop_geometry() %>%
  mutate(
    variable = as.character(variable),
    municipality_key = as.character(municipality_key)
  )

all_variables <- sort(unique(pred_df$variable))
all_dates <- sort(unique(pred_df$date))
all_munis <- sort(unique(pred_df$municipality_key))

# ------------------------------------------------------------
# Optional municipality borders
# ------------------------------------------------------------
muni_sf <- NULL
if (file.exists(muni_path)) {
  muni_sf <- readRDS(muni_path)
  if (inherits(muni_sf, "sf")) {
    muni_sf <- st_transform(muni_sf, 4326)
  } else {
    muni_sf <- NULL
  }
}

bbox_map <- st_bbox(pred_sf_4326)

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  
  titlePanel("Valle d'Aosta Meteo Predictions"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "var",
        "Variable",
        choices = all_variables,
        selected = all_variables[1]
      ),
      
      sliderInput(
        "date",
        "Day",
        min = min(all_dates),
        max = max(all_dates),
        value = min(all_dates),
        timeFormat = "%Y-%m-%d",
        animate = animationOptions(interval = 800, loop = FALSE)
      ),
      
      selectInput(
        "muni_ts",
        "Time series average",
        choices = c("All Valle d'Aosta", all_munis),
        selected = "All Valle d'Aosta"
      ),
      
      checkboxInput(
        "show_borders",
        "Show municipality borders",
        value = TRUE
      ),
      
      sliderInput(
        "pt_size",
        "Point size",
        min = 1,
        max = 8,
        value = 4,
        step = 1
      )
    ),
    
    mainPanel(
      leafletOutput("map", height = 700),
      br(),
      plotOutput("ts_plot", height = 260)
    )
  )
)

# ------------------------------------------------------------
# Server
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  current_date <- reactive({
    as.Date(input$date, origin = "1970-01-01")
  })
  
  current_points <- reactive({
    pred_df %>%
      filter(
        variable == input$var,
        date == current_date()
      )
  })
  
  color_domain <- reactive({
    vals <- pred_df %>%
      filter(variable == input$var) %>%
      pull(pred_value)
    
    rng <- range(vals, na.rm = TRUE)
    
    if (!all(is.finite(rng))) rng <- c(0, 1)
    if (rng[1] == rng[2]) rng <- rng + c(-0.5, 0.5)
    
    rng
  })
  
  pal <- reactive({
    colorNumeric(
      palette = viridisLite::viridis(256),
      domain = unname(color_domain()),
      na.color = "transparent"
    )
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      fitBounds(
        lng1 = unname(bbox_map["xmin"]),
        lat1 = unname(bbox_map["ymin"]),
        lng2 = unname(bbox_map["xmax"]),
        lat2 = unname(bbox_map["ymax"])
      )
  })
  
  observe({
    df_now <- current_points()
    req(nrow(df_now) > 0)
    
    m <- leafletProxy("map")
    m %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls()
    
    if (!is.null(muni_sf) && isTRUE(input$show_borders)) {
      m <- m %>%
        addPolygons(
          data = muni_sf,
          color = "#333333",
          weight = 1,
          fill = FALSE,
          opacity = 0.8
        )
    }
    
    popup_txt <- paste0(
      "<b>Variable:</b> ", df_now$variable,
      "<br><b>Date:</b> ", df_now$date,
      "<br><b>Prediction:</b> ", round(df_now$pred_value, 3),
      "<br><b>Municipality:</b> ", df_now$municipality_key
    )
    
    m %>%
      addCircleMarkers(
        lng = df_now$lon,
        lat = df_now$lat,
        radius = input$pt_size,
        stroke = FALSE,
        fillOpacity = 0.9,
        color = pal()(df_now$pred_value),
        popup = popup_txt
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal(),
        values = unname(color_domain()),
        title = paste0(input$var, "<br>", current_date()),
        opacity = 1
      )
  })
  
  ts_data <- reactive({
    df_var <- pred_df %>%
      filter(variable == input$var)
    
    if (input$muni_ts == "All Valle d'Aosta") {
      df_var %>%
        group_by(date) %>%
        summarise(
          mean_value = mean(pred_value, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      df_var %>%
        filter(municipality_key == input$muni_ts) %>%
        group_by(date) %>%
        summarise(
          mean_value = mean(pred_value, na.rm = TRUE),
          .groups = "drop"
        )
    }
  })
  
  output$ts_plot <- renderPlot({
    df_ts <- ts_data()
    req(nrow(df_ts) > 0)
    
    subtitle_txt <- if (input$muni_ts == "All Valle d'Aosta") {
      "Average over all prediction points"
    } else {
      paste("Average over points in", input$muni_ts)
    }
    
    ggplot(df_ts, aes(x = date, y = mean_value)) +
      geom_line(linewidth = 0.7) +
      geom_point(
        data = df_ts %>% filter(date == current_date()),
        size = 2.5
      ) +
      labs(
        title = paste("Time series of", input$var),
        subtitle = subtitle_txt,
        x = NULL,
        y = "Average predicted value"
      ) +
      theme_minimal(base_size = 13)
  })
}

shinyApp(ui, server)
