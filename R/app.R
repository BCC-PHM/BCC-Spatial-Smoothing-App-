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
library(shinyWidgets)
library(shinythemes)

library("INLA")
library("spdep")
library("tmap")


####################################
# Load built-in population data

lsoa11pop = readRDS("data/lsoa11pop.rds")
lsoa21pop = readRDS("data/lsoa21pop.rds")

ward21cd_pop = readRDS("data/ward21pop_2011_2022.rds")

####################################


ui = page_fluid(
  theme = shinytheme("flatly"),
  titlePanel("BCC Spatial Smoothing App"),     
  navset_card_pill(
    nav_panel(
      "Step 1: Add your files",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          
          h4("Analysis setup"),
          
          selectInput(
            "analysis_type",
            "Choose analysis type",
            choices = c(
              "Spatial model" = "spatial",
              "Spatial age-standardised model" = "spatial_age_standardised",
              "Spatio-temporal model" = "spatiotemporal",
              "Spatio-temporal age-standardised model" = "spatiotemporal_age_standardised"
            )
          ),
          
          hr(),
          
          fileInput(
            "shp_zip",
            "Upload zipped shapefile",
            accept = ".zip"
          ),
          
          tags$div(
            textOutput("upload_status_shp"),
            style = "font-size: 12px; color: red; font-weight: bold;"
          ),
          
          helpText("Note: Please select the correct area id and area name"),
          
          selectInput("area_id", "Select area id", choices = NULL, selected = NULL),
          selectInput("area_name", "Select area name", choices = NULL, selected = NULL),
          
          hr(),
          
          fileInput(
            "csv_file",
            "Upload a CSV file",
            accept = ".csv"
          ),
          
          tags$div(
            textOutput("upload_status_csv"),
            style = "font-size: 12px; color: red; font-weight: bold;"
          ),
          
          helpText("Note: Please select the correct column of the count of cases and population"),
          
          selectInput("area_id2", "Select area id", choices = NULL, selected = NULL),
          selectInput("cases", "Select column of cases", choices = NULL, selected = NULL),
          
          # -------------------------------------------------------------------
          # Population denominator setup
          # -------------------------------------------------------------------
          
          checkboxInput(
            "use_builtin_pop",
            "Use built-in population denominator",
            value = FALSE
          ),
          
          # Built-in population options
          conditionalPanel(
            condition = "input.use_builtin_pop == true",
            
            selectInput(
              "builtin_pop_geo",
              "Select population geography",
              choices = c(
                "LSOA 2011" = "lsoa11",
                "LSOA 2021" = "lsoa21",
                "Ward 2021" = "ward21"
              )
            ),
            
            conditionalPanel(
              condition = "input.analysis_type == 'spatial' || input.analysis_type == 'spatial_age_standardised'",
              
              tags$p(
                "If population data is not available for the selected financial year, the latest available population denominator will be used.",
                style = "color: red; font-weight: bold; font-size: 14px; margin-bottom: 5px;"
              ),
              
              selectInput(
                "builtin_pop_year",
                "Select financial year of your data",
                choices = NULL
              )
              
              
            )
          ),
          
          # User-supplied population column
          conditionalPanel(
            condition = "input.use_builtin_pop == false",
            
            selectInput(
              "pop",
              "Select column of population",
              choices = NULL,
              selected = NULL
            )
          ),
          
          # Year column for temporal models
          conditionalPanel(
            condition = "input.analysis_type == 'spatiotemporal' || input.analysis_type == 'spatiotemporal_age_standardised'",
            
            tags$p(
              "Please ensure the financial year is formatted as 'YYYY' (e.g., FY 2024/25 to 2425).",
              style = "color: red; font-weight: bold; font-size: 14px; margin-bottom: 5px;"
            ),
            
            selectInput(
              "year",
              "Select financial year column in your data",
              choices = NULL,
              selected = NULL
            )
          ),
          
          # Age column for age-standardised models
          conditionalPanel(
            condition = "input.analysis_type == 'spatial_age_standardised' || input.analysis_type == 'spatiotemporal_age_standardised'",
            
            helpText("Age column is only needed for age-standardised models."),
            
            selectInput(
              "age",
              "Select column of age group",
              choices = NULL,
              selected = NULL
            )
       
          ),
          
          # Age groups to select for every models
          # default will opt-in every age groups 
          conditionalPanel(
            condition = "input.use_builtin_pop == true",
            
            tags$p(
              "Default includes all age groups. For custom age groups not shown here, please provide your own population column.",
              style = "color: red; font-weight: bold; font-size: 14px; margin-bottom: 5px;"
            ),
            
            pickerInput(
              inputId = "age_group_picker",
              label = "Number of observation:",
              choices =  c(
                "UNDER 1", "1-4",  "5-9",   "10-14", "15-19", "20-24",
                "25-29",   "30-34","35-39", "40-44", "45-49", "50-54",
                "55-59",   "60-64","65-69", "70-74", "75-79", "80-84",
                "85-89",   "90+"
              ),
              multiple = TRUE,
              selected =  c(
                "UNDER 1", "1-4",  "5-9",   "10-14", "15-19", "20-24",
                "25-29",   "30-34","35-39", "40-44", "45-49", "50-54",
                "55-59",   "60-64","65-69", "70-74", "75-79", "80-84",
                "85-89",   "90+"
              )
            )
            
            
          )
          
          
          
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
            )
          ),
          fluidRow(
            column(
              width = 12,
              DTOutput("tbl_csv")
            )
          ),
          fluidRow(
            column(
              width = 12,
              DTOutput("tbl_joined")
            )
          )
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
  
  
  # --- Update Shapefile Selectors ---
  
  observeEvent(shp_data(), {
    cols = names(st_drop_geometry(shp_data()))
    
    # Add an empty string at the start of the choices
    choices_with_blank = c("Please select..." = "", cols)
    
    updateSelectInput(session, "area_id", 
                      choices = choices_with_blank,
                      selected = "")
  })
  
  observeEvent(shp_data(), {
    cols = names(st_drop_geometry(shp_data()))
    
    # Add an empty string at the start of the choices
    choices_with_blank = c("Please select..." = "", cols)
    
    updateSelectInput(session, "area_name", 
                      choices = choices_with_blank,
                      selected = "")
  })
  
  
  
  #===========================================================
  #the built-in population logic
  #-----------------------------------------------------------
  observe({
    req(input$builtin_pop_geo)
    
    pop_data = switch(
      input$builtin_pop_geo,
      "lsoa11" = lsoa11pop,
      "lsoa21" = lsoa21pop,
      "ward21" = ward21cd_pop
    )
    
    updateSelectInput(
      session,
      "builtin_pop_year",
      choices = sort(unique(pop_data$fin_year)),
      selected = max(sort(unique(pop_data$fin_year)))
    )
    
  })
  
  # --- Update CSV Selectors ---
  observeEvent(csv_data(), {
    cols = names(csv_data())
    
    # Add an empty string at the start of the choices
    choices_with_blank = c("Please select..." = "", cols)
    
    updateSelectInput(session, "cases", choices = choices_with_blank, selected = "")
    updateSelectInput(session, "pop", choices = choices_with_blank, selected = "")
    updateSelectInput(session, "area_id2", choices = choices_with_blank, selected = "")
    updateSelectInput(session, "age", choices = choices_with_blank, selected = "")
    updateSelectInput(session, "year", choices = choices_with_blank, selected = "")
  })
  
  #create built-in poulation reactive 
  builtin_pop_selected = reactive({
    req(input$use_builtin_pop)
    req(input$builtin_pop_geo)
    
    pop_data = switch(
      input$builtin_pop_geo,
      "lsoa11" = lsoa11pop %>%
        rename(geo_id = LSOA11CD, geo_name = LSOA11NM, lad_name = LAD17NM),
      
      "lsoa21" = lsoa21pop %>%
        rename(geo_id = LSOA21CD, geo_name = LSOA21NM, lad_name = LAD21NM),
      
      "ward21" = ward21cd_pop %>%
        rename(geo_id = WD21CD, geo_name = WD21NM, lad_name = LAD21NM)
    )
    
    pop_data
  })
  
  

  
  
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
  

  ##############################################################################
  #create joined dataset 
  
  joined_data = reactive({
    req(shp_data(), csv_data(), input$area_id, input$area_id2, input$cases)
    
    shp = shp_data()
    csv = csv_data()
    
    # Create join ID in both spatial file and uploaded CSV
    shp$id_join = as.character(shp[[input$area_id]])
    csv$id_join = as.character(csv[[input$area_id2]])
    
    if (isTRUE(input$use_builtin_pop)) {
      
      req(input$age_group_picker)
      
      # to prevent left_join from creating "pop.x" and "pop.y"
      csv = csv %>% select(-any_of(c("pop", "standard_pop")))
      
      
    
    # ------------------------------------------------------------
    # 1) Spatial model
    # Join by area only; one selected financial year; population summed over selected age groups
    # ------------------------------------------------------------
    
    if (input$analysis_type =="spatial") {
      
      req(input$builtin_pop_year)
      
      pop_lookup = builtin_pop_selected() %>%
        filter(fin_year == input$builtin_pop_year,
               age_band %in% input$age_group_picker)%>%
        group_by(geo_id) %>%
        summarise(
          pop = sum(count, na.rm = TRUE),
          standard_pop = sum(standard_pop, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(id_join = as.character(geo_id)) %>% 
        select(id_join, pop, standard_pop)
      
      csv = csv %>%
        left_join(pop_lookup, by = "id_join")
      
      pop_col = "pop"
     }
     
    # ------------------------------------------------------------
    # 2) Spatial age-standardised model
    # Join by area + age band; one selected financial year; population kept by age band
    # ------------------------------------------------------------
    
    if (input$analysis_type == "spatial_age_standardised") {
    
      
      req(input$builtin_pop_year, input$age)
      
      pop_lookup = builtin_pop_selected() %>%
        filter(
          fin_year == input$builtin_pop_year,
          age_band %in% input$age_group_picker
        ) %>%
        mutate(
          id_join = as.character(geo_id),
          age_join = as.character(age_band)
        ) %>%
        select(id_join, age_join, pop = count, standard_pop)
      
      csv = csv %>%
        mutate(age_join = as.character(.data[[input$age]])) %>%
        left_join(pop_lookup, by = c("id_join", "age_join"))
      
      pop_col = "pop"
    }

    # ------------------------------------------------------------
    # 3) Spatio-temporal model
    # Join by area + financial year; population summed over selected age groups   
    # ------------------------------------------------------------
    if (input$analysis_type == "spatiotemporal") {
      req(input$year)
    
      pop_lookup = builtin_pop_selected() %>%
        filter(age_band %in% input$age_group_picker) %>%
        group_by(geo_id, fin_year) %>%
        summarise(
          pop = sum(count, na.rm = TRUE),
          standard_pop = sum(standard_pop, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          id_join = as.character(geo_id),
          year_join = as.character(fin_year)
        ) %>%
        select(id_join, year_join, pop, standard_pop)
      
      csv = csv %>%
        mutate(year_join = as.character(.data[[input$year]])) %>%
        left_join(pop_lookup, by = c("id_join", "year_join"))
      pop_col = "pop"
    }
    
    # ------------------------------------------------------------
    # 4) Spatio-temporal age-standardised model
    # Join by area + age band + financial year; population kept by age band
    # ------------------------------------------------------------
    if (input$analysis_type == "spatiotemporal_age_standardised") {
      req(input$year, input$age)
      
      
      pop_lookup = builtin_pop_selected() %>%
        filter(age_band %in% input$age_group_picker) %>%
        mutate(
          id_join = as.character(geo_id),
          age_join = as.character(age_band),
          year_join = as.character(fin_year)
        ) %>%
        select(id_join, age_join, year_join, pop = count, standard_pop)
      
      csv = csv %>%
        mutate(
          age_join = as.character(.data[[input$age]]),
          year_join = as.character(.data[[input$year]])
        ) %>%
        left_join(pop_lookup, by = c("id_join", "age_join", "year_join"))
      pop_col = "pop"
    }
    
  } else {
    
    req(input$pop)
    pop_col = input$pop
    
    if (input$analysis_type %in% c("spatiotemporal", "spatiotemporal_age_standardised")) {
      req(input$year)
      csv = csv %>%
        mutate(year_join = as.character(.data[[input$year]]))
    
    }
  }
    
  # -------------------------------------------------------------------------
  # Create final joined dataset depending on analysis type
  # -------------------------------------------------------------------------
    

    
  if (input$analysis_type == "spatial"){
    
    merged = shp %>% 
      left_join(csv, by = "id_join")
    
    merged$Proportion_unsmoothed =
      (merged[[input$cases]] / merged[[pop_col]]) * 100
    

  } 
    
  if (input$analysis_type == "spatial_age_standardised"){  
    
    csv_asr = csv %>%
      mutate(
        cases = as.numeric(.data[[input$cases]]),
        pop = as.numeric(.data[[pop_col]]),
        age_specific_rate = cases / pop,
        weighted_rate = age_specific_rate * standard_pop
      ) %>%
      group_by(id_join) %>%
      summarise(
        cases = sum(cases, na.rm = TRUE),
        pop = sum(pop, na.rm = TRUE),
        standard_pop_total = sum(standard_pop, na.rm = TRUE),
        ASR_unsmoothed = (sum(weighted_rate, na.rm = TRUE) / standard_pop_total) * 100000,
        .groups = "drop"
      )
    
    merged = shp %>% 
      left_join(csv_asr, by = "id_join")
    
  }
    
  if (input$analysis_type == "spatiotemporal"){ 
    csv_rate = csv %>%
      mutate(
        cases = as.numeric(.data[[input$cases]]),
        pop = as.numeric(.data[[pop_col]])
      ) %>%
      group_by(id_join, year_join) %>%
      summarise(
        cases = sum(cases, na.rm = TRUE),
        pop = sum(pop, na.rm = TRUE),
        Proportion_unsmoothed = (cases / pop) * 100,
        .groups = "drop"
      )
    
    
    merged = shp %>%
      left_join(csv_rate, by = "id_join")
  }
  
    if (input$analysis_type == "spatiotemporal_age_standardised") {
      csv_asr_time = csv %>%
        mutate(
          cases = as.numeric(.data[[input$cases]]),
          pop = as.numeric(.data[[pop_col]]),
          age_specific_rate = cases / pop,
          weighted_rate = age_specific_rate * standard_pop
        ) %>%
        group_by(id_join, year_join) %>%
        summarise(
          cases = sum(cases, na.rm = TRUE),
          pop = sum(pop, na.rm = TRUE),
          standard_pop_total = sum(standard_pop, na.rm = TRUE),
          ASR_unsmoothed = (sum(weighted_rate, na.rm = TRUE) / standard_pop_total) * 100000,
          .groups = "drop"
        )
      
      merged = shp %>%
        left_join(csv_asr_time, by = "id_join")
      
  }
  
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


