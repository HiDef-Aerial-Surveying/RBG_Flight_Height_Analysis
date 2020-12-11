###############################
## Shiny application for running flight height analysis
## Works with spreadsheets with measured bird lengths that are merged to a single sheet
## Grant Humphries
## April 16, 2020
###############################
header <- dashboardHeader(
    
    titleWidth =270,
    title = "HiDef Bootstrap module",
    
    tags$li(class = "dropdown", actionLink("howto", label = 'User guide', style = "font-size: 19px")),
    tags$li(class = "dropdown", a(img(src = "HiDefLogo.PNG", height = "40px"), href='https://hidef.bioconsult-sh.de/',
                                  style = "padding-top: 5px; padding-bottom: 5px;", target='_blank', id="lbl_hiDefLogoLink"))
    
)

#################################################################
# Dashboard sidebar
#################################################################

sidebar <- dashboardSidebar(
    width = 270,
    sidebarMenu(
        id = "tabs",
        
        
        menuItem(startExpanded = TRUE,
                 "Working directory", tabName = "tab_simulation", icon = shiny::icon("folder"),
                 shinyDirButton('folder', 'Select folder', 'Select folder where survey files stored', FALSE)
        ),
        
        menuItem(startExpanded = TRUE,
                 "Output directory", tabName = "tab_simulation", icon = shiny::icon("folder"),
                 shinyDirButton('outdir', 'Select folder', 'Please select output directory', FALSE)
        ),
        
        menuItem(startExpanded = TRUE,
                 "Species", tabName = "tab_simulation", icon = shiny::icon("paw"),
                 selectInput('species', 'Select species',choices=c("Kittiwake","Gannet",
                                                                   "Herring Gull","Lesser black-backed Gull") ,multiple=FALSE)
        ),
        menuItem(startExpanded = TRUE,
                 "Run flight height", tabName = "tab_simulation", icon = shiny::icon("play-circle"),
                 bsButton('run',label='Run',icon=shiny::icon('play-circle'),style='info',type='action')
        ),
        
        hr(),
        
        br(),
        bsAlert(anchorId = "alert")
        
        
    )
)


#################################################################
# Dashboard body
#################################################################

body <- dashboardBody(
    fluidRow(
        
        box(
            title='Directory information',
            width = 12,
            class='boxbox',
            status= 'success',
            solidHeader = TRUE,
            collapsible = TRUE,
            column(12,
                   p('Working directory:'),
                   verbatimTextOutput('rawInputValue',placeholder = FALSE),
                   p('Output directory'),
                   verbatimTextOutput('outputfolder',placeholder = FALSE)
            )
            
        ),
        
        box(
            title='Output',
            width = 12,
            class='boxbox',
            status= 'info',
            solidHeader = TRUE,
            collapsible = TRUE,
            column(12,
                   uiOutput('cons')
                   #textInput("cons",""),     
                   #pre(id="console")
            )
            
        )
        
        
    )
    
) ## End PAR dashboard Body

bootstrapPage(
    includeCSS("style.css"),
    tags$head(
        tags$link(rel="stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Montserrat:100,300,500|Open+Sans:100,300,500|Roboto:100,300,500")
    ),
    dashboardPage(skin='black',header,sidebar,body)
    
)