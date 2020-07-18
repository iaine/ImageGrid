#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
    
    textInput("firstCase", "First filter...",NULL),
    
    textInput("secondCase", "Second filter...",NULL),
    
    # App title ----
    titlePanel("Uploading Files"),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all", Count = "count"),
                         selected = "head")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Data file ----
            tableOutput("contents")
            
        )
        
    )
)
            
# Define server logic to read selected file ----
server <- function(input, output) {
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        
        df1 = data.frame(text=df$text, media_urls=df$media_urls)

        d <- input$firstCase
        if (d != '') {
            if (grepl(',', d, fixed = TRUE)) {
                d1 = paste(strsplit(d, sep=','), sep='|')
            } else {
                d1 = d
            }
            filtered <- df1[grep(d1, df1$text, ignore.case = TRUE),]

        } else {
            filtered <- df1
        }       
        
        p <- input$secondCase
        if (p != '') {
            if (grepl(',', p, fixed = TRUE)) {
                p1 = paste(strsplit(p, sep=','), sep='|')
            } else {
                p1 = p
            }
            
            issues <- filtered[grep(p1, filtered$text, ignore.case = TRUE),]
        }
        else {
            issues <- filtered
        }
        
        if(input$disp == "head") {
            return(head(issues))
        }
        else if (input$disp == "all"){
            return(issues)
        } else {
          split_media<- lapply(issues$media_urls, function (x) strsplit(as.character(x),';',fixed=TRUE))
          #tidy up. For now the URL is needed to map back
          split_file<- lapply(split_media, function (x) {
            if (x != 'character(0)') {
              f = strsplit(as.character(x),'/',fixed=TRUE); 
              g = tail(as.character(f[[1]]), n=1);
              gsub('")', '', g)
            }
          })
          
          count = table(unlist(split_media))
          imagegrid = rbind(as.data.frame(count))
          return(imagegrid)
        }
  
    })
    
}
# Run the app ----
shinyApp(ui, server)