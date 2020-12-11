#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  wkd <- reactive({parseDirPath(volumes, input$folder)})
  od <- reactive({parseDirPath(volumes, input$outdir)})
  spp <- reactive(input$species)
  
  shinyDirChoose(input, id='folder', roots = volumes,filetypes=c(''))
  shinyDirChoose(input, id='outdir', roots = volumes, filetypes = FALSE)
  
  output$rawInputValue <- renderPrint({parseDirPath(volumes, input$folder)})
  output$outputfolder <- renderPrint({parseDirPath(volumes, input$outdir)})
  
  
  #########
  
  observeEvent(input$run,{
    
    wkd <- wkd()
    od <- od()
    spp <- spp()
    
    output$cons <- renderUI({
      input$run

      withProgress(message = 'Merging datasheets', value = 0, {
        flist <- list.files(path=wkd,full.names=TRUE)
        n <- length(flist)
        dat <- foreach(k=flist,.combine='rbind')%do%{
          incProgress(1/n, detail = paste("Merging", k))
          Sys.sleep(0.02)
          v <- readxl::read_xlsx(k)
          names(v)[grep("Flight height",names(v))] <- "Plane Height"
          names(v)[grep("Plane height",names(v))] <- "Plane Height"
          if(length(grep("Plane Height",names(v)))>0){
            v <- v %>%
              dplyr::select(Date,Camera,`Reel Name`,Frame,`Marker Number`, Species, `Plane Height`, `Frame 1`:`Frame 8`) 
            return(v)
          }
          
        }
      })
      
      Run_Flight_Height(dat,od,spp)
      
      tagList(
        h4("Flight height data generated! Celebrate with prosecco!")
      )
      
    })
  })
            

  
  
  ############################################

  observeEvent(input$howto,{
    showModal(
      modalDialog(
        h2('User guide'),
        hr(),
        h3('STEP 1:'),
        p('Put all spreadsheets with measured birds for the entire project into a folder:'),
        h3('STEP 2:'),
        p('Select that directory under "Working directory" on the left panel'),
        h3('STEP 3:'),
        p('Select a directory under "Output directory" where two CSV files of output will be generated'),
        h3('STEP 4:'),
        p('Select the target species under the "Species" tab'),
        h3('STEP 5:'),
        p('Click "Run"'),
        h3('STEP 6:'),
        p('If successful, celebrate with prosecco!! If not... drown your sorrows with prosecco.. 
          and then get a hold of Grant... grant.humphries@hidefsurveying.co.uk')
        )
        )
  })
  
})
