setwd("~/R_project/Shinyapp_spatial")

library(shiny)
library(sf)
library(ggplot2)
library(leaflet)
library(DT)
library(bslib)
library(tidyverse)
library(shinybusy)
library(zip)

library("INLA")
library("spdep")
library("tmap")

ui = page_fluid(
  titlePanel("BCC Spatial Smoothing App"),     
  navset_card_pill(
    nav_panel("Step 1: Add you files", 
              sidebarLayout(
                sidebarPanel(
                  width = 3,
                  #Upload shape file
                  fileInput("shp_zip", 
                            "Upload zipped shapefile", 
                            accept = ".zip"),
                  #status message of shape 
                  tags$div(
                    textOutput("upload_status_shp"),
                    style = "font-size: 12px; color: red; font-weight: bold;"),
                  
                  helpText("Note: Please select the correct area id and area name"),

                  
                  selectInput("area_id", "Select area id", choices = NULL, selected = NULL),
                  selectInput("area_name", "Select area name", choices = NULL, selected = NULL),
                  
                  hr(),
                  
                  #Upload csv file 
                  fileInput("csv_file",
                            "Upload a CSV file",
                            accept = ".csv"),
                  #status message of csv
                  tags$div(
                    textOutput("upload_status_csv"),
                    style = "font-size: 12px; color: red; font-weight: bold;"
                  ),
                  
                  helpText("Note: Please select the correct column of the count of cases and population"),
                  
                  selectInput("area_id2", "Select area id", choices = NULL, selected = NULL),
                  selectInput("cases", "Select column of cases", choices = NULL, selected = NULL),
                  selectInput("pop", "Select column of population", choices = NULL, selected = NULL),
                  hr(),
                  selectInput("age", "Select column of age", choices = NULL, selected = NULL),
                  selectInput("year", "Select column of year", choices = NULL, selected = NULL)
                ),
                
                mainPanel(
                  fluidRow(
                    column(
                      width = 6,
                      leafletOutput("map_plot", height = "600px")
                    ),
                    column(
                      width = 6,
                      DTOutput("tbl_shp")
                    )),
                  fluidRow(
                    column(
                      width = 12,
                      DTOutput("tbl_csv")
                    )),
                  fluidRow(
                    column(
                      width = 12,
                      DTOutput("tbl_joined")
                    ))
              )
            )
          ),
    nav_panel("Step 2: Run the analysis",
              sidebarLayout(
                sidebarPanel(
                  width = 3,
                  h4("Model specification"),
                  tags$div(
                    "It is recommended to keep the default prior settings unless you have strong prior knowledge.",
                    style = "color: red; font-size: 14px;"
                  ),
                  
                  br(),
                  tags$strong("Precision prior"),
                  helpText("PC prior for overall variability in the BYM2 random effect."),
                  numericInput("prec_u", "Upper bound for SD (U)", value = 1, min = 0.001),
                  numericInput("prec_alpha", "Probability alph a", value = 0.01, min = 0.0001, max = 1, step = 0.01),
                  
                  hr(),
                  
                  tags$strong("Phi prior"),
                  helpText("PC prior for the proportion of spatially structured variation."),
                  numericInput("phi_u", "Phi threshold (U)", value = 0.5, min = 0, max = 1, step = 0.05),
                  numericInput("phi_alpha", "Probability alpha", value = 0.5, min = 0.0001, max = 1, step = 0.05),
                  
                  br(),
                  
                  actionButton("run_model", "Run spatial smoothing"),
                  hr(),
                  
                  h4("Adjust the outputs"),
                  
                  selectInput(
                    "map_to_edit", "Choose map to adjust", choices = c("Unsmoothed map" = "unsmoothed",
                                                                       "Smoothed median" = "median",
                                                                       "Lower limit" = "lower",
                                                                       "Upper limit" = "upper"),
                                                          selected = "unsmoothed"),
                  
                  
                  textInput("map_title", "Enter the title", value = ""),
                  textInput("map_unit", "Enter the unit", value = ""),
                  selectInput(
                    "map_colour", "Choose colour palette", 
                    choices = c(
                      "Blues",   "BuGn",   "BuPu",
                      "GnBu",    "Greens", "Greys",
                      "Oranges", "OrRd",   "PuBu",
                      "PuBuGn",  "PuRd",   "Purples",
                      "RdPu",    "Reds",   "YlGn",
                      "YlGnBu",  "YlOrBr", "YlOrRd"
                    ),
                    selected = "Blues"
                  ),
                  actionButton("apply_map_settings", "Apply settings"),
                  
                  hr()
                  
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel(
                      "Interactive",
                      fluidRow(
                        column(
                          width = 6,
                          tmapOutput("map_nonsmoothed", height = "600px" )
                        )
                    )
                    ),
                    tabPanel(
                      "Maps",
                  fluidRow(
                    column(
                      width = 6,
                      tmapOutput("map_nonsmoothed", height = "600px" )
                    ),
                    column(
                      width = 6,
                      tmapOutput("map_smoothed_median", height = "600px")
                    )),
                  fluidRow(
                    column(
                      width = 6,
                      tmapOutput("map_smoothed_LL", height = "600px")
                    ),
                    column(
                      width = 6,
                      tmapOutput("map_smoothed_UL", height = "600px")
                    ))
                  ),
                  
                  tabPanel(
                    "Table",
                    fluidRow(
                      column(
                        width = 12,
                        DTOutput("smoothed_table")
                      )
                    )
                  ),
                  
                  tabPanel(
                    "Download",
                    br(),
                    fluidRow(
                      column(
                        width = 2,
                        downloadButton("download_output", "Download")
                      )
                    ),
                    br(),
                    helpText("Tick the boxes to select what you want to download"),
                    hr(),
                    h3("Maps"),
                    br(),
                    checkboxGroupInput(
                      "checkbox_maps",
                      label = NULL,
                      choices = c("Unsmoothed map" = "unsmoothed",
                                  "Smoothed median" = "median",
                                  "Lower limit" = "lower",
                                  "Upper limit" = "upper"),
                      selected = c("unsmoothed", "median", "lower", "upper"),
                      inline = TRUE
                    ),
                    br(),
                    helpText("Choose file format"),
                    radioButtons(
                      "download_format",
                      label = NULL,
                      choices = c("PNG" = "png", "PDF"="pdf"),
                      selected = "png",
                      inline = TRUE
                    ),
                    hr()
                    

                    
                    
                  )
                  )
                  
                  
                  )
              )
 
  )
)
)

server = function(input, output, session) {
  
  #  Show loading screen when button is clicked
  observeEvent(input$run_model, {
    show_modal_spinner(
      spin = "fading-circle",
      text = "Be patient! Running spatial smoothing..."
    )
  })
  
  observe({
    req(data_smoothed())
    remove_modal_spinner()
  })
  
  #shape file
  shp_data = reactive({
    req(input$shp_zip)
    
    # create a unique temporary extraction folder
    unzip_dir = file.path(tempdir(), paste0("shp_", as.integer(Sys.time())))
    dir.create(unzip_dir, showWarnings = FALSE)
    
    unzip(input$shp_zip$datapath, exdir = unzip_dir)
    
    shp_file = list.files(unzip_dir, pattern = "\\.shp$", full.names = TRUE)
    
    validate(
      need(length(shp_file) > 0, "No .shp file found inside the zip.")
    )
    
    st_read(shp_file[1], quiet = TRUE) %>%
      st_transform(4326)
    
  })
  
  #csv data
  
  csv_data = reactive({
    req(input$csv_file)
    
    read.csv(input$csv_file$datapath)
    })
  
  
  
  #mesage of successful upload
    
  output$upload_status_shp = renderText({
    paste("Uploaded successfully. Rows:", nrow(shp_data()))
  })
  
  #mesage of successful upload
  
  output$upload_status_csv = renderText({
    paste("Uploaded successfully. Rows:", nrow(csv_data()))
  })
  
  #map output to visualise shp file 
  
  output$map_plot = renderLeaflet({
    leaflet(shp_data() )%>% 
      addProviderTiles("CartoDB.Positron") %>% 
      addPolygons(
        color = "black",        # outline colour
        weight = 2,           # outline thickness
        fillColor = "grey60",   # fill colour
        fillOpacity = 0.5,    # transparency
        popup = paste0("test")
      )
})
  
  
  #datatable rendering for shape 
  
  output$tbl_shp = renderDT({
    datatable(st_drop_geometry(shp_data()))
  })
  
  #datatable rendering for csv
  
  output$tbl_csv = renderDT({
    datatable(csv_data())
  })
  
  
  #user selected area id for later analysis use from shp 
  
  observeEvent(shp_data(), {
    updateSelectInput(session, "area_id", 
                      choices = names(st_drop_geometry(shp_data())),
                      selected = NULL)
    })
  
  observeEvent(shp_data(), {
    updateSelectInput(session, "area_name", 
                      choices = names(st_drop_geometry(shp_data())),
                      selected = NULL)
  })
  
  observeEvent(csv_data(), {
    updateSelectInput(session, "cases", 
                      choices = names(csv_data()),
                      selected = NULL)
  })
  
  observeEvent(csv_data(), {
    updateSelectInput(session, "pop", 
                      choices = names(csv_data()),
                      selected = NULL)
  })
  
  observeEvent(csv_data(), {
    updateSelectInput(session, "area_id2", 
                      choices = names(csv_data()),
                      selected = NULL)
  })
  
  observeEvent(csv_data(), {
    updateSelectInput(session, "age", 
                      choices = names(csv_data()),
                      selected = NULL)
  })
  
  observeEvent(csv_data(), {
    updateSelectInput(session, "year", 
                      choices = names(csv_data()),
                      selected = NULL)
  })
  
  ##############################################################################
  #create joined dataset 
  
  joined_data = reactive({
    req(shp_data(), csv_data(), input$area_id, input$area_id2, input$cases, input$pop)
    
    shp = shp_data()
    csv = csv_data()
    
    shp$id_join = as.character(shp[[input$area_id]])
    csv$id_join = as.character(csv[[input$area_id2]])
    
    merged = shp %>% 
      left_join(csv, by = "id_join")
    
    merged$Proportion_unsmoothed = (merged[[input$cases]] / merged[[input$pop]]) * 100
    
    merged
    
  })
  
  
  output$tbl_joined = renderDT({
    req(joined_data())
    datatable(st_drop_geometry(joined_data()))
  })
  
  
  
  ##############################################################################
  # Define spatial neighbourhood structure (LSOA adjacency)
  ##############################################################################
  analysis_data  = reactive({
    req(joined_data())
    req(input$run_model > 0)
    
    shp = joined_data()
    
    # Create a stable numeric ID for each areaID
    shp$new_id = 1:nrow(shp)
    
    # Reproject to British National Grid (EPSG:27700)
    # Using metres avoids angular distortion and improves geometric operations
    shp = st_transform(shp, 27700)
    
    # Ensure geometries are valid (fix if necessary)
    shp = st_make_valid(shp)
    which(!st_is_valid(shp))
    
    shp
  })
  
  
  g = reactive({
    req(analysis_data())
    
    shp = analysis_data()
    
    # Create adjacency list
    mcnty_nb = poly2nb(shp, row.names = as.character(shp$new_id), queen = TRUE)
    
    # Convert the nb object into an adjacency file required by R-INLA
    adj_path = tempfile(fileext = ".adj")
    nb2INLA(adj_path, mcnty_nb)
    
    
    # Read the adjacency file back into R as an INLA graph object
    inla.read.graph(adj_path)
    
  })
  
  
  
  # ----------------------------------------------------------
  # Fit a Bayesian spatial Poisson model (BYM2) using R-INLA
  # ----------------------------------------------------------
  
  modelresult = reactive({
    req(input$run_model >0)
    req(analysis_data(), g(), input$area_id, input$area_id2, input$cases, input$pop)
    
    shp = analysis_data()
    shp$y = shp[[input$cases]]
    shp$pop = shp[[input$pop]]
    
    #make sure the y has no decimal places
    shp$y = round(shp$y)
    
    formula = y ~ 1 + f(new_id,
                            model = "bym2",
                            graph = g(),
                            hyper = list(
                              # PC prior for precision (overall variability / smoothing strength)
                              prec = list(prior = "pc.prec", param = c(input$prec_u, input$prec_alpha)),
                              # PC prior for phi (mix between spatially structured vs unstructured variation)
                              phi = list(prior = "pc", param = c(input$phi_u, input$phi_alpha))
                            ))
    
    inla(
      formula,
      data = st_drop_geometry(shp),
      family = "poisson",
      offset = log(pop),
      control.predictor = list(compute = TRUE),
      verbose = TRUE
    )
    

  })
  
  # ----------------------------------------------------------
  # Extract posterior smoothed estimates from the INLA model
  # ----------------------------------------------------------
 
  data_smoothed = reactive({
    req(input$run_model >0)
    req(modelresult(), analysis_data())
    
    
    shp = analysis_data()
    fitmod = modelresult()
    
    
    data = shp %>%
      cbind(
        fitmod$summary.fitted.values[, c("mean", "0.025quant","0.5quant", "0.975quant")]
      ) %>%
      rename(
        lower025 = X0.025quant,
        median = X0.5quant,
        upper975 = X0.975quant
      ) %>% 
      mutate(
        Proportion = median/pop*100,
        Proportion_LL = lower025/ pop*100,
        Proportion_UL = upper975 / pop*100
      )
      
    data
  })
    

  
  
  output$smoothed_table = renderDT({
    datatable(st_drop_geometry(data_smoothed()))
    
  })
  
  ##################################################
  #map output
  
  
  map_settings = reactiveValues(
    unsmoothed = list(
      title = "Unsmoothed map",
      unit = "%",
      colour = "Blues"
    ),
    median = list(
      title = "Smoothed median",
      unit = "%",
      colour = "Blues"
    ),
    lower = list(
      title = "Lower limit",
      unit = "%",
      colour = "Blues"
    ),
    upper = list(
      title = "Upper limit",
      unit = "%",
      colour = "Blues"
    )
  )
  
  observeEvent(input$map_to_edit, {
    selected = input$map_to_edit
    
    updateTextInput(session, "map_title",
                    value = map_settings[[selected]]$title)
    
    updateTextInput(session, "map_unit",
                    value = map_settings[[selected]]$unit)
    
    updateSelectInput(session, "map_colour",
                      selected = map_settings[[selected]]$colour)

  })
  
  observeEvent(input$apply_map_settings, {
    selected = input$map_to_edit
    
    map_settings[[selected]] = list(
      title = input$map_title,
      unit = input$map_unit,
      colour = input$map_colour
    )
  })
  
  make_tmap = function(data, fill_var, title, unit, colour) {
    tm_shape(data) +
      tm_polygons(
        fill = fill_var,
        fill.scale = tm_scale_continuous(values = colour),
        fill.legend = tm_legend(unit, group_id = "top", frame = FALSE)
      ) +
      tm_layout(
        frame = FALSE,
        legend.position = c("left", "top"),
        legend.frame = FALSE,
        inner.margins = c(0.07, 0, 0.01, 0)
      ) +
      tm_title(stringr::str_wrap(title, width = 50)) +
      tm_compass(
        type = "8star",
        size = 4,
        position = c("RIGHT", "bottom"),
        color.light = "white"
      ) +
      tm_credits(
        text = paste(
          "Contains OS data \u00A9 Crown copyright and database right",
          format(Sys.Date(), "%Y"),
          ". Source:\nOffice for National Statistics licensed under the Open Government Licence v.3.0."
        ),
        position = c("LEFT", "BOTTOM")
      )
  }
  
  
  map_list = reactive({
    req(input$run_model > 0)
    req(joined_data(), data_smoothed())
    
    list(
      unsmoothed = make_tmap(
        data = joined_data(),
        fill_var = "Proportion_unsmoothed",
        title = map_settings$unsmoothed$title,
        unit = map_settings$unsmoothed$unit,
        colour = map_settings$unsmoothed$colour
      ),
      
      median = make_tmap(
        data = data_smoothed(),
        fill_var = "Proportion",
        title = map_settings$median$title,
        unit = map_settings$median$unit,
        colour = map_settings$median$colour
      ),
      
      lower = make_tmap(
        data = data_smoothed(),
        fill_var = "Proportion_LL",
        title = map_settings$lower$title,
        unit = map_settings$lower$unit,
        colour = map_settings$lower$colour
      ),
      
      upper = make_tmap(
        data = data_smoothed(),
        fill_var = "Proportion_UL",
        title = map_settings$upper$title,
        unit = map_settings$upper$unit,
        colour = map_settings$upper$colour
      )
    )
  })
  
  
  output$map_nonsmoothed = renderTmap({
    map_list()$unsmoothed
  })
  
  output$map_smoothed_median = renderTmap({
    map_list()$median
  })
  
  output$map_smoothed_LL = renderTmap({
    map_list()$lower
  })
  
  output$map_smoothed_UL = renderTmap({
    map_list()$upper
  })
  
  
          
  ##################################################################################
  #download output
  
  output$download_output <- downloadHandler(
    filename = function() {
      paste0("BCC_spatial_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(input$checkbox_maps)
      req(length(input$checkbox_maps) > 0)
      req(input$download_format)
      
      tmp_dir <- file.path(tempdir(), paste0("maps_", as.integer(Sys.time())))
      dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
      
      ext <- input$download_format
      files_to_zip <- character(0)
      
      for (map_name in input$checkbox_maps) {
        map_obj <- map_list()[[map_name]]
        out_name <- paste0(map_name, ".", ext)
        out_file <- file.path(tmp_dir, out_name)
        
        if (ext == "png") {
          tmap_save(
            tm = map_obj,
            filename = out_file,
            dpi = 800
          )
        } else if (ext == "pdf") {
          tmap_save(
            tm = map_obj,
            filename = out_file
          )
        }
        
        files_to_zip <- c(files_to_zip, out_name)
      }
      
      zip::zipr(
        zipfile = file,
        files = files_to_zip,
        root = tmp_dir
      )
    }
  )
  
}

shinyApp(ui, server)


