library(sf)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(htmlwidgets)
library(plotly)
library(DT)


## For testing only
source('Functions.R')

SA2_Tables <- readRDS('SA2_Tables.rds')
LHD_Tables <-  readRDS('LHD_Tables.rds')
LHD_Cropped <- sf::st_read('LHD_Cropped/')
SA2_Cropped <- sf::st_read('SA2_2021_Cropped/')

names(SA2_Cropped) <- gsub('_2', '', names(SA2_Cropped))
names(SA2_Cropped) <- gsub('\\..*', '', names(SA2_Cropped))

### Filter for Valid SA2s
ValidSA2s <- SA2_Tables$URP %>% 
  dplyr::filter(SEXP_lab == 'Persons') %>% 
  dplyr::filter(Value >= quantile(Value, 0.05),
                .by = TIME_PERIOD) %>% 
  dplyr::filter(dplyr::n() == 3,
                .by = REGION) %>% 
  dplyr::pull(REGION) %>% 
  unique()


SA2_Tables <-  purrr::map(SA2_Tables, 
                          dplyr::filter,
                          REGION %in% ValidSA2s)

## Format CoB
SA2_Tables$CoB <- SA2_Tables$CoB %>% 
  dplyr::mutate(BPLP_lab = gsub('(, | \\().*', '', BPLP_lab),
                BPLP_lab = gsub('Korea', 'South Korea', BPLP_lab)) 

LHD_Tables$CoB <- LHD_Tables$CoB %>% 
  dplyr::mutate(BPLP_lab = gsub('(, | \\().*', '', BPLP_lab),
                BPLP_lab = gsub('Korea', 'South Korea', BPLP_lab)) 


## Format Lang
SA2_Tables$Lang <- SA2_Tables$Lang %>% 
  dplyr::mutate(LANP_lab = gsub('Uses ', '', LANP_lab),
                LANP_lab = gsub('Chinese: (.*)', 'Chinese (\\1)', LANP_lab)) 

LHD_Tables$Lang <- LHD_Tables$Lang %>% 
  dplyr::mutate(LANP_lab = gsub('Uses ', '', LANP_lab),
                LANP_lab = gsub('Chinese: (.*)', 'Chinese (\\1)', LANP_lab)) 


# LHD_Cropped <- sf::st_crop(sf::st_make_valid(LHD_Cropped), SA2_Cropped)
Lang_Top10_SLHD <- LHD_Tables$Lang %>%
  dplyr::filter(Name == "Sydney",
                SEXP == '3') %>% 
  dplyr::summarise(Value = mean(Value),
                   .by = c(LANP, LANP_lab)) %>% 
  dplyr::arrange(-Value) %>% 
  head(11) 

CoB_Top10_SLHD <- LHD_Tables$CoB %>%
  dplyr::filter(Name == "Sydney",
                SEXP == '3') %>% 
  dplyr::summarise(Value = mean(Value),
                   .by = c(BPLP, BPLP_lab)) %>% 
  dplyr::arrange(-Value) %>% 
  head(11) 


AllVars <- setNames(c("Usual Resident Population",
                      "Median weekly personal income",
                      "Median Age",
                      "Country of birth",
                      "Main language spoken at home",
                      "Needs assistance with core activities",
                      "Mental health condition (including depression or anxiety)",
                      "Diabetes (excluding gestational diabetes)",
                      "Asthma",
                      "Arthritis",
                      "Stroke",
                      "Cancer (including remission)",
                      "Dementia (including Alzheimer's)",
                      "Heart disease (including heart attack or angina)",
                      "Kidney disease",
                      "Lung condition (including COPD or emphysema)",
                      'Index of Relative Socio-Economic Disadvantage (IRSD)',
                      'Index of Relative Socio-Economic Advantage and Disadvantage (IRSAD)',
                      'Index of Education and Occupation (IEO)',
                      'Index of Economic Resources (IER)'),
                    c('URP',
                      'MedIncome',
                      'Med_Age',
                      'BornOS',
                      'LOTE',
                      'Assist',
                      'MH',
                      'Diabetes',
                      "Asthma",
                      "Arthritis",
                      "Stroke",
                      "Cancer",
                      "Dementia",
                      "Heart",
                      "Kidney",
                      "Lung",
                      "IRSD", 
                      "IRSAD", 
                      "IEO", 
                      "IER"))

SexLookup <- setNames(c('Males', 'Females', 'Persons'),
                      as.character(1:3))

Variable_Types <- setNames(c('URP',
                             'Med_Age',
                             'MedIncome',
                             'BornOS',
                             'CoB',
                             'LOTE',
                             'Lang',
                             'Assist',
                             'MH',
                             'Diabetes',
                             "Asthma",
                             "Arthritis",
                             "Stroke",
                             "Cancer",
                             "Dementia",
                             "Heart",
                             "Kidney",
                             "Lung",
                             "IRSD", 
                             "IRSAD", 
                             "IEO", 
                             "IER"),
                           c(rep('num', 2),
                             'money',
                             rep('pct', 4),
                             rep('asr', 11),
                             rep('seifa', 4)))

info_types <- setNames(c('num', 'num', 'cald', 'asr', 'seifa'),
                       unique(names(Variable_Types)))


LHD_Cropped_Labels <- cbind(
  sf::st_drop_geometry(LHD_Cropped),
  sf::st_coordinates(sf::st_centroid(LHD_Cropped)))

### Classify each of the Values by year and sex
### Could make it consistent across year for better comparison? 
### Easy to change if need be

SA2_Tables <- purrr::imap(
  SA2_Tables,
  function(df, variable){
    type_in <- names(Variable_Types[Variable_Types == variable])
    
    if(grepl('CoB|Lang', variable)){
      dplyr::mutate(df,
                    Value_Cat = Classify_Quantiles(Value, type_in),
                    .by = dplyr::matches('TIME_PERIOD|SEXP|BPLP|LANP')) 
    } else {
      dplyr::mutate(df,
                    Value_Cat = Classify_Quantiles(Value, type_in),
                    .by = dplyr::matches('TIME_PERIOD|SEXP')) 
    }
  })


header <- dashboardHeader(title = "Census Local Health District Visualisation - PHRAME", 
                          titleWidth = '1000')

body <- dashboardBody(
  tags$style(HTML(paste(
    '.dataTables_wrapper',
    '.dataTables_filter {float: left; padding-left: 50px;}',
    '.dataTables_wrapper',
    '.dataTables_filter input{width: 500px;}'))),
  
  fluidRow(
    tabBox(
      title = NULL, width = 12, height = 0.2,  
      tabPanel(
        'Dashboard',
        fluidRow(
          column(
            width = 6,
            tabItem(tabName = 'dashboard',
                    box(width = NULL, 
                        solidHeader = TRUE,
                        leafletOutput(outputId = "map", 
                                      height = 500)),
                    box(width = NULL,
                        plotlyOutput("barchart", 
                                     height = 250)))
          ), 
          column(width = 6, 
                 height = 814, 
                 tabItem(tabName = 'dashboard',
                         box(width = NULL, 
                             height = '100%',
                             downloadButton('download',
                                            "Download SA2 data"),
                             DTOutput("datatable"))))
        )
      ),
      tabPanel('Instructions',
               fluidRow(
                 column(3, 
                        box(
                          width = NULL, 
                          # title = "Instructions for using the platform", 
                          HTML('<iframe width="1000" height="600" src="https://www.youtube.com/embed/4LXR21rCs74" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                        )))
      ),
      tabPanel('Metadata',
               box(width = 700,
                   (div(style='height:700px;overflow-y: scroll;',
                        htmlOutput('markdown', height = 700))))
      )
    ),
  )
)


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", 
             selected = TRUE, 
             startExpanded = TRUE, 
             tabName = "dashboard",
             icon = icon("dashboard"),
             selectInput(inputId = "Type",
                         label = h4("Variable"),
                         choices = unname(AllVars)),
             selectInput(inputId = "Year",
                         label =  h4("Census year"),
                         choices = '2021'),
             selectInput(inputId = "Sex",
                         label =  h4("Gender"),
                         choices = 'Persons'), 
             uiOutput("COB"),
             uiOutput("Lang")
    )
  )
)


# input <- data.frame(Type = AllVars[1],
#                     Sex = SexLookup[3],
#                     Year = '2021')
# input <- data.frame(Type = AllVars[18],
#                     Sex = SexLookup[1],
#                     Year = '2011')
# input <- data.frame(Type = AllVars[2],
#                     Sex = SexLookup[3],
#                     Year = '2021')
# input <- data.frame(Type = AllVars[6],
#                     Sex = SexLookup[3],
#                     Year = '2011')
# input <- data.frame(Type = AllVars[2],
#                     Sex = SexLookup[3],
#                     COB = 'China',
#                     Year = '2011')
# input <- data.frame(Type = AllVars[3],
#                     Sex = SexLookup[3],
#                     Lang = 'Korean',
#                     Year = '2011')


server <- function(input, output, session) {
  
  ## Render sidebar options
  output$COB <- renderUI({
    if("Country of birth" %in% input$Type) {
      selectInput("COB",
                  "Specify country",
                  choices = c("Overseas",
                              paste0('\u2014', CoB_Top10_SLHD$BPLP_lab)),
                  selected = "Overseas")
    } else { }
  })

  output$Lang <- renderUI({
    if(grepl("Main language", input$Type)) {
      selectInput("Lang",
                  "Specify language",
                  choices = c("Languages other than English",
                              paste0('\u2014', Lang_Top10_SLHD$LANP_lab)),
                  selected = "Languages other than English")
    } else { }

  })
  
  ## Interactive Map ###########################################
  # Create the map
  output$map <- renderLeaflet({
    leaflet(data = SA2_Cropped) %>%
      setView(lng = 151.1258,
              lat = -33.89312,
              zoom = 11) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMapPane("Maps", zIndex = 410) %>%
      addMapPane("SLHD", zIndex = 420) %>%
      addPolygons(data = LHD_Cropped,
                  group = 'Base',
                  weight = 2,
                  options = pathOptions(pane = "SLHD"),
                  color = 'rgba(0, 0, 0, 0.5)',
                  fill = NA) 
  })
  # 
  
  observeEvent(
    eventExpr = input$map_zoom, {
      # print(input$map_zoom)
      leafletProxy(
        mapId = "map", 
        session = session
      ) %>% 
        clearMarkers() %>%
        addLabelOnlyMarkers(
          data = LHD_Cropped_Labels,
          lng = ~X,
          lat = ~Y,
          label = ~paste(Name, "LHD"),
          options = pathOptions(pane = "SLHD"),
          labelOptions = labelOptions(
            noHide = TRUE,
            direction = 'top',
            textOnly = TRUE,
            style = list(
              'color' = 'black',
              'font-size' = paste0(input$map_zoom/2 + 10, 
                                   'px'),
              'font-weight' = 'bolder',
              'font-family' = '"Gill Sans Extrabold", sans-serif'
              # 'text-shadow' = '0px 0px white' 
              # '-webkit-text-stroke-width' = '1px',
              # '-webkit-text-stroke-color' = 'rgba(0, 38, 100, 1)'
              )))    
    }
  )
  

  ## Observe the events on the sidebar to update content
  observe({
    Input_Type <- names(AllVars[AllVars == input$Type])
    input_COB <- ifelse(is.null(input$COB) | Input_Type != 'BornOS', 
                        '', 
                        input$COB)
    input_COB <- ifelse(Input_Type == 'BornOS' & input_COB == '',
                        'Overseas', 
                        gsub('\u2014', '', input_COB))
    
    input_Lang <- ifelse(is.null(input$Lang) | Input_Type != 'LOTE', 
                         '', 
                         input$Lang)
    input_Lang <- ifelse(Input_Type == 'LOTE' & Input_Type == '',
                         'Languages other than English', 
                         gsub('\u2014', '', input_Lang))
    
    Input_Legend <- dplyr::case_when(
      input_COB != '' ~ paste('Country of birth:', input_COB),
      input_Lang != '' ~ stringr::str_wrap(
        paste('Speaks', gsub('Language', 'language', input_Lang),
              'at home'),
        width = 25),
      grepl('Index', input$Type) ~ Input_Type,
      TRUE ~ gsub("( \\(.*)|( with core.*)", 
                  "", 
                  input$Type))
    
    if(input_COB %in% CoB_Top10_SLHD$BPLP_lab){
        SA2data <- SA2_Tables$CoB %>% 
          dplyr::filter(BPLP_lab == input_COB)
        LHDdata <- LHD_Tables$CoB %>% 
          dplyr::filter(BPLP_lab == input_COB)
      
    } else if (input_Lang %in% Lang_Top10_SLHD$LANP_lab){
        SA2data <- SA2_Tables$Lang %>% 
          dplyr::filter(LANP_lab == input_Lang)
        LHDdata <- LHD_Tables$Lang %>% 
          dplyr::filter(LANP_lab == input_Lang)
    } else {
      SA2data <- SA2_Tables[[Input_Type]]
      LHDdata <- LHD_Tables[[Input_Type]]
    }
    
    SexChoices <- unname(SexLookup[sort(unique(SA2data$SEXP))])
    SexChoice <- ifelse(input$Sex %in% SexChoices, 
                        input$Sex,
                        'Persons')
    
    updateSelectInput(session, "Sex",
                      selected = SexChoice,
                      choices = SexChoices)
    
    YearChoices <- sort(unique(SA2data$TIME_PERIOD))
    YearChoice <- ifelse(input$Year %in% YearChoices,
                         input$Year,
                         YearChoices[1])
    
    updateSelectInput(session, "Year",
                      selected = YearChoice,
                      choices = YearChoices)
    
    Input_Sex <- names(SexLookup[SexLookup == SexChoice])
    
    output$YearChoice <- renderText(YearChoice)
    
    SA2data <- dplyr::filter(SA2data,
                             TIME_PERIOD == YearChoice) %>% 
      dplyr::mutate(Name = gsub(" \\(.*", "", Name))
    
    
    ## Assign type of variable
    vartype <- names(Variable_Types[Variable_Types == Input_Type])
    
    ## Keep all years if it's SEIFA
    if(vartype != 'seifa'){
      LHDdata <- dplyr::filter(LHDdata,
                               TIME_PERIOD == YearChoice)
    }
    
    LHDdata$LHD <- forcats::fct_relevel(LHDdata$Name, 
                                        'Sydney',
                                        after = 0L)
    
    ### Choose variable type for the markdown info
    # info_type <- ifelse(vartype %in% c('num', 'money', 'pct'),
    #                     'num', vartype)
    
    info_type <- info_types[vartype] 
    
    output$markdown <- renderUI({
      includeHTML(paste0(info_type, '.html'))
    })
    
    SA2data <- tidyr::drop_na(SA2data)
    
    if(vartype == "asr"){
      SA2data$Value_Label <-  asr_ci(SA2data$Value,
                                     SA2data$lci,
                                     SA2data$uci)
      LHDdata$Value_Label <- asr_ci(LHDdata$Value,
                                    LHDdata$lci,
                                    LHDdata$uci)
      
    } else if(vartype == 'pct'){
      SA2data$Value_Label <-  scales::percent(SA2data$Value, 
                                              accuracy = 0.1)
      
      LHDdata$Value_Label <-  scales::percent(LHDdata$Value, 
                                              accuracy = 0.1)
      
    } else if(vartype == 'num'){
      SA2data$Value_Label <-  scales::number(SA2data$Value, 
                                             accuracy = 1, 
                                             big.mark = ',')
      LHDdata$Value_Label <-  scales::number(LHDdata$Value, 
                                             accuracy = 1,
                                             big.mark = ',')
      
    } else if(vartype == 'money'){
      SA2data$Value_Label <-  scales::dollar(SA2data$Value, 
                                             accuracy = 1, 
                                             big.mark = ',')
      LHDdata$Value_Label <-  scales::dollar(LHDdata$Value, 
                                             accuracy = 1,
                                             big.mark = ',')
      
    } else if(vartype == 'seifa'){
      SA2data$Value_Label <-  SA2data$Value
      LHDdata$Value_Label <-  scales::percent(LHDdata$Value, 
                                              accuracy = 0.1)
      
    }      
    
    SA2Map <- dplyr::left_join(SA2_Cropped,
                               SA2data[SA2data$SEXP == Input_Sex,],
                               by = c('sa2_code' = 'REGION'))
    
    SA2Map <- dplyr::mutate(SA2Map,
                            Value_Cat = forcats::fct_reorder(Value_Cat,
                                                             Value, 
                                                             .na_rm = TRUE))
    SA2Map <- sf::st_as_sf(SA2Map)
    
    ### Format data for download
    SA2_Download <- SA2data %>% 
      dplyr::rename(SA2_Code21 = REGION,
                    SA2_Name21 = sa2_name_2021,
                    LHD = Name,
                    Year = TIME_PERIOD,
                    Sex = SEXP)
    
    ### UP TO HERE -----------
    ## Select variables based on indicator type
    if(vartype == 'asr'){
      SA2_Download <- SA2_Download %>% 
        dplyr::mutate(Sex_lab = SexLookup[Sex]) %>% 
        dplyr::select(Year, SA2_Code21, SA2_Name21, 
                      LHD, 
                      dplyr::matches('_lab$'),
                      ASR = Value,
                      ASR_LCI = lci,
                      ASR_UCI = uci)
      
    } else if(vartype == 'seifa'){
      SA2_Download <- SA2_Download %>% 
        dplyr::mutate(Index = AllVars[INDEX_TYPE]) %>% 
        dplyr::select(Index, Year, SA2_Code21, SA2_Name21, LHD, 
                      Decile = Value)  
      
    } else if(vartype == 'num'){
      SA2_Download <- SA2_Download %>% 
        dplyr::select(Year, SA2_Code21, SA2_Name21, LHD, 
                      dplyr::matches('_lab$'), 
                      Count = Value)
    } else if(vartype == 'pct'){
      SA2_Download <- SA2_Download %>% 
        dplyr::select(Year, SA2_Code21, SA2_Name21, LHD, 
                      dplyr::matches('_lab$'), 
                      Proportion = Value)
    }
    
    output$download <- downloadHandler(
      filename = function(){paste0(Input_Type, "_", input$Sex, "_", 
                                   input$Year, ".csv")}, 
      content = function(fname){
        write.csv(SA2_Download, fname)
      }
    )
    
    pal <- colorFactor(
      pals::brewer.blues(n = length(levels(SA2Map$Value_Cat))),
      domain = SA2Map$Value_Cat,
      na.color = NA)
    
    sketch <- if(vartype == 'seifa'){
      htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(rowspan = 2, 'SA2 Name'),
            th(rowspan = 2, 'Local Health District'),
            th(colspan = 1, 'SEIFA Decile'),
          ),
          tr(
            lapply(SexChoices, th)
          )
        )
      ))
    } else {
      htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(rowspan = 2, 'SA2 Name'),
            th(rowspan = 2, 'Local Health District'),
            th(colspan = length(SexChoices), 'Gender'),
          ),
          tr(
            lapply(SexChoices, th)
          )
        )
      ))
    }
    
    ##### Render the Data Table #####
    Table_Variable <- ifelse(vartype == 'asr', 'Value_Label', 'Value')
    
    datatab <- DT::datatable(
      tidyr::spread(
        dplyr::mutate(SA2data[, c('sa2_name_2021', 'Name', 'SEXP', Table_Variable)],
                      Name = factor(Name),
                      SEXP = SexLookup[SEXP]),
        SEXP, 
        get(Table_Variable)),
      rownames = FALSE,
      filter = 'top',
      options = list(
        pageLength = 13,
        columnDefs = list(list(targets = c(0,2:(1 + length(SexChoices))),
                               searchable = FALSE)),
        dom = 'tip'),
      style = 'bootstrap4',
      container = sketch) %>% 
      DT::formatStyle(columns = c(0:(1 + length(SexChoices))),
                      fontSize = '10pt')
    
    if(vartype == 'asr'){
      output$datatable <- renderDT({datatab})
    } else if(vartype == 'pct') {
      output$datatable <- renderDT({DT::formatPercentage(
        datatab,
        columns = 3:(2 + length(SexChoices)), 1)
      })
    } else if(vartype == 'num') {
      output$datatable <- renderDT({DT::formatRound(
        datatab,
        columns = 3:(2 + length(SexChoices)), 
        digits = 0)
      })
    } else if(vartype == 'money') {
      output$datatable <- renderDT({DT::formatCurrency(
        datatab,
        columns = 3:(2 + length(SexChoices)), 
        digits = 0)
      })
    } else if(vartype == 'seifa') {
      output$datatable <- renderDT({datatab})
    }
    
    textsize <- 8
    textangle <- 45
    
    
    ##### Render the Bar Chart #####
    output$barchart <-
      renderPlotly({
        if(vartype == 'asr'){
          baseplot <- ggplot(dplyr::mutate(LHDdata,
                                           SEXP = factor(SEXP,
                                                         levels = 1:3,
                                                         labels = SexLookup),
                                           SEXP = forcats::fct_rev(SEXP)),
                             aes(y = Value,
                                 ymin = lci,
                                 ymax = uci,
                                 x = LHD,
                                 fill = SEXP,
                                 text = paste(
                                   'Name:', LHD,
                                   "<br>Sex:", SEXP,
                                   "<br>ASR [95% CI]:", Value_Label))) +
            scale_y_continuous(paste0(Input_Legend,
                                      "\n(ASR by 10,000 population)"))
              
        }
        else if(vartype == 'pct')
        {
          baseplot <- ggplot(dplyr::mutate(LHDdata,
                                           SEXP = factor(SEXP,
                                                         levels = 1:3,
                                                         labels = SexLookup),
                                           SEXP = forcats::fct_rev(SEXP)),
                             aes(x = LHD,
                                 y = Value,
                                 fill = SEXP,
                                 text = paste(
                                   "Name:", LHD,
                                   "<br>Sex:", SEXP,
                                   "<br>Proportion:", 
                                   scales::percent(Value, 
                                                   accuracy = 0.1)))) +
            scale_y_continuous(stringr::str_wrap(paste0(Input_Legend, "(%)"),
                                                 width = 20),
                               labels = scales::percent)
          
        }
        else if(vartype == 'num')
        {
          baseplot <- ggplot(dplyr::mutate(LHDdata,
                                           SEXP = factor(SEXP,
                                                         levels = 1:3,
                                                         labels = SexLookup),
                                           SEXP = forcats::fct_rev(SEXP)),
                             aes(y = Value,
                                 x = LHD,
                                 fill = SEXP,
                                 text = paste0(
                                   'Name:', LHD,
                                   "<br>Sex:", SEXP,
                                   ifelse(grepl('Age', input$Type), 
                                          "<br>Median Age:", 
                                          "<br>Count:"),
                                   scales::comma(Value, 
                                                 accuracy = 1)))) +
            scale_y_continuous(stringr::str_wrap(paste(
              input$Type,
              ifelse(grepl('Age', input$Type), "(Years)", "(N)")), 
              width = 20),
              labels = scales::comma) 
        }
        else if(vartype == 'money')
        {
          baseplot <- ggplot(dplyr::mutate(LHDdata,
                                           SEXP = factor(SEXP,
                                                         levels = 1:3,
                                                         labels = SexLookup),
                                           SEXP = forcats::fct_rev(SEXP)),
                             aes(y = Value,
                                 x = LHD,
                                 fill = SEXP,
                                 text = paste(
                                   'Name:', Name,
                                   "<br>Sex:", SEXP,
                                   "<br>Weekly income:", scales::dollar(Value, 
                                                                accuracy = 1)))) +
            scale_y_continuous(stringr::str_wrap(input$Type, 
                                                 width = 20),
                               labels = scales::dollar) 
        }
        else if(vartype == 'seifa')
        {
          baseplot <- ggplot(dplyr::mutate(LHDdata,
                                           SEXP = factor(SEXP,
                                                         levels = 1:3,
                                                         labels = SexLookup),
                                           SEXP = forcats::fct_rev(SEXP)),
                             aes(y = Value,
                                 x = LHD,
                                 fill = TIME_PERIOD,
                                 text = paste('Name:', Name,
                                              '<br>Year:', TIME_PERIOD,
                                              "<br>Proportion:", scales::percent(Value)))) +
            scale_y_continuous(paste0(
              stringr::str_wrap(gsub('(Index of | \\(.*)', '', input$Type), width = 30),
              "\n(% in highest two quintiles)"),
              labels = scales::percent) 
          
        }
        
        baseplot <- baseplot +
          geom_col(position = position_dodge(width = 0.8),
                   width = 0.8,
                   colour = 'grey60') +
          scale_fill_brewer(NULL,
                            palette = 'Blues',
                            direction = -1,
                            breaks = rev) +
          scale_x_discrete("Local Health District") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = textangle,
                                           hjust = 1),
                text = element_text(size = textsize))
        
        plotly::ggplotly(p = baseplot, 
                         tooltip = c('text'))  
      })
    
    ##### Render the Leaflet
    if(vartype == 'asr'){
      leafletProxy(mapId = "map",
                   data = SA2Map) %>%
        clearGroup(unname(AllVars)) %>%
        clearControls() %>%
        addPolygons(weight = 1,
                    color = 'grey',
                    smoothFactor = 0.3,
                    fillOpacity = 1,
                    group = 'Total',
                    fillColor = ~pal(Value_Cat),
                    options = pathOptions(pane = "Maps"),
                    label = ~lapply(glue::glue(
                      .sep = "<br/>",
                      "<b>{sa2_name}</b>",
                      "Gender: {input$Sex}",
                      "{Input_Legend}: {ifelse(is.na(Value), 'Insufficient data', Value_Label)}"),
                      htmltools::HTML)) %>%
        addLegend(title = stringr::str_wrap(gsub("(\\(.*)|( with core.*)", "", 
                                                 Input_Legend), width = 30),
                  pal = pal,
                  position = 'bottomleft',
                  group = 'Total',
                  values = ~Value_Cat,
                  na.label = "N/A",
                  opacity = 1.0)
    } else if(vartype == "pct") {
      leafletProxy(mapId = "map",
                   data = SA2Map) %>%
        clearGroup(unname(AllVars)) %>%
        clearControls() %>%
        addPolygons(weight = 1,
                    color = 'grey',
                    smoothFactor = 0.3,
                    fillOpacity = 1,
                    group = 'Total',
                    fillColor = ~pal(Value_Cat),
                    options = pathOptions(pane = "Maps"),
                    label = ~lapply(glue::glue(
                      .sep = "<br/>",
                      "<b>{sa2_name}</b>",
                      "Gender: {input$Sex}",
                      "{Input_Legend}: {ifelse(is.na(Value), 'Insufficient data', scales::percent(Value, a = 0.1))}"),
                      htmltools::HTML)) %>%
        addLegend(title = stringr::str_wrap(paste(Input_Legend, "(%)"),
                                            width = 30),
          pal = pal,
          position = 'bottomleft',
          group = 'Total',
          values = ~Value_Cat,
          na.label = "N/A",
          opacity = 1.0)
    } else if(vartype == "num") {
      leafletProxy(mapId = "map",
                   data = SA2Map) %>%
        clearGroup(unname(AllVars)) %>%
        clearControls() %>%
        addPolygons(weight = 1,
                    color = 'grey',
                    smoothFactor = 0.3,
                    fillOpacity = 1,
                    group = 'Total',
                    fillColor = ~pal(Value_Cat),
                    options = pathOptions(pane = "Maps"),
                    label = ~lapply(glue::glue(
                      .sep = "<br/>",
                      "<b>{sa2_name}</b>",
                      "Gender: {input$Sex}",
                      "{Input_Legend}: {ifelse(is.na(Value), 'Insufficient data', scales::comma(Value))}"),
                      htmltools::HTML)) %>%
        addLegend(title = stringr::str_wrap(Input_Legend,
                                            width = 30),
                  pal = pal,
                  position = 'bottomleft',
                  group = 'Total',
                  values = ~Value_Cat,
                  na.label = "N/A",
                  opacity = 1.0)
    } else if(vartype == "money") {
      leafletProxy(mapId = "map",
                   data = SA2Map) %>%
        clearGroup(unname(AllVars)) %>%
        clearControls() %>%
        addPolygons(weight = 1,
                    color = 'grey',
                    smoothFactor = 0.3,
                    fillOpacity = 1,
                    group = 'Total',
                    fillColor = ~pal(Value_Cat),
                    options = pathOptions(pane = "Maps"),
                    label = ~lapply(glue::glue(
                      .sep = "<br/>",
                      "<b>{sa2_name}</b>",
                      "Gender: {input$Sex}",
                      "{Input_Legend}: {ifelse(is.na(Value), 'Insufficient data', scales::dollar(Value))}"),
                      htmltools::HTML)) %>%
        addLegend(title = stringr::str_wrap(paste(Input_Legend, "($)"),
                                            width = 30),
                  pal = pal,
                  position = 'bottomleft',
                  group = 'Total',
                  values = ~Value_Cat,
                  na.label = "N/A",
                  opacity = 1.0)
    } else if(vartype == "seifa") {
      leafletProxy(mapId = "map",
                   data = SA2Map) %>%
        clearGroup(unname(AllVars)) %>%
        clearControls() %>%
        addPolygons(weight = 1,
                    color = 'grey',
                    smoothFactor = 0.3,
                    fillOpacity = 1,
                    group = 'Total',
                    fillColor = ~pal(Value_Cat),
                    options = pathOptions(pane = "Maps"),
                    label = ~lapply(glue::glue(
                      .sep = "<br/>",
                      "<b>{sa2_name}</b>",
                      "Year: {input$Year}",
                      "{Input_Legend} Decile: {ifelse(is.na(Value), 'Insufficient data', Value_Label)}"),
                      htmltools::HTML)) %>%
        addLegend(title = stringr::str_wrap(Input_Legend,
                                            width = 30),
                  pal = pal,
                  position = 'bottomleft',
                  group = 'Total',
                  values = ~Value_Cat,
                  na.label = "N/A",
                  opacity = 1.0)
    }
    
  })
  
  
}


shinyApp(ui = dashboardPage(header, sidebar, body), server = server)










