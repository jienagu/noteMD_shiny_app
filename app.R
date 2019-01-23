#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# devtools::install_github("jienagu/noteMD")
library(noteMD)
library(knitr)  
library(rmarkdown)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30),
         downloadButton('describe_download',"Download Report",class="butt" ),br(),
         tags$head(tags$style(".butt{background-color:#230682;} .butt{color: #e6ebef;}")),
         radioButtons('format', 'Document format', c('PDF', 'Word'),
                      inline = TRUE)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         fluidRow(
           column(12,
                  helpText("Note: Any comments made in the box will be printed if you download the summary report.") ),
           column(12,
                  tags$textarea(
                    "Please using any **markdown** syntax!",
                    id    = 'markdowninput',
                    rows  = 3,
                    style = 'width:100%;')) ),
         helpText("Preview:"),
         htmlOutput('htmlmarkdown')
         
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   output$htmlmarkdown = reactive({
     note_in_html(input$markdowninput)
   })
   
   output$describe_download = downloadHandler(
     filename<- function(){
       paste("Summary",Sys.Date(),switch(
         input$format, PDF = '.pdf', Word = '.docx'
       ),sep = "")
     },
     
     content = function(file) {
       if (input$format=="PDF"){
         #### Progressing indicator
         withProgress(message = 'Download in progress',
                      detail = 'This may take a while...', value = 0, {
                        for (i in 1:15) {
                          incProgress(1/15)
                          Sys.sleep(0.01)
                        }
                        
                        ## End of progression
                        src <- normalizePath('summary_report.Rmd')
                        
                        # temporarily switch to the temp dir, in case you do not have write
                        # permission to the current working directory
                        owd <- setwd(tempdir())
                        on.exit(setwd(owd))
                        file.copy(src, 'summary_report.Rmd', overwrite = TRUE)
                        
                        library(rmarkdown)
                        out <- render('summary_report.Rmd', pdf_document())
                        file.rename(out, file)
                        
                      })
         ### below is the end of pdf content
       }else{
         withProgress(message = 'Download in progress',
                      detail = 'This may take a while...', value = 0, {
                        for (i in 1:15) {
                          incProgress(1/15)
                          Sys.sleep(0.01)
                        }
                        
                        ## End of progression
                        src <- normalizePath('summary_report_word.Rmd')
                        
                        # temporarily switch to the temp dir, in case you do not have write
                        # permission to the current working directory
                        owd <- setwd(tempdir())
                        on.exit(setwd(owd))
                        file.copy(src, 'summary_report_word.Rmd', overwrite = TRUE)
                        
                        library(rmarkdown)
                        out <- render('summary_report_word.Rmd', word_document())
                        file.rename(out, file)
                      })
       }
       
     })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

