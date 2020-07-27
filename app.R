#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(DT)

# Define UI for data upload app ----
ui <- fluidPage(
  
    downloadLink("downloadData", "Download Data"),
    actionButton("downloadImages", "Download Images"),
    
    textInput("firstCase", "First issue filter",NULL),
    
    textInput("secondCase", "Second issue filter...",NULL),
    
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
                         choices = c(#Head = "head",
                                     #All = "all", 
                                     Count = "count"),
                         selected = "count")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Data file ----
          #tableOutput("contents")
            DT::dataTableOutput("contents")
            
        )
        
    )
)
            
# Define server logic to read selected file ----
server <- function(input, output) {
    
    output$contents <- DT::renderDataTable({
        
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
          d1=''
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
          p1 = ''
            issues <- filtered
        }
        
        if(input$disp == "head") {
            return(head(issues))
        }
        else if (input$disp == "all"){
            cooccur <- as.data.frame.table(issues$text, issues$media_url)
            return(cooccur)
        } else {
          split_media<- lapply(issues$media_urls, function (x) strsplit(as.character(x),';',fixed=TRUE))
          #tidy up. For now the URL is needed to map back
          split_file<- lapply(unlist(split_media), function (x) {
            #slight hack to make sure that any empty lines are removed.
            if (x != 'character(0)') {
              gsub('")', '', x)
              HTML(paste('<img src="',as.character(x),'" height="50"></img>', sep=''))
            }
          })
          
          count = table(unlist(split_file))
          imagegrid = rbind(as.data.frame(count))
          
          clean_column <- lapply(imagegrid$Var1, function(x) {
            x1 <- gsub('<img src="', '', x[1])
            gsub('" height="50"></img>', '', x1[[1]])
          })
          
          downloadFiles <- function(fileList) {
            imageDir = paste(getcwd(),'imagegrid',Sys.Date(), sep='/')
            if (!dir.exists(imageDir)) {
              dir.create(imageDir, showWarnings = FALSE)
            }
            
            for (x in fileList) {
              skip_to_next <- FALSE
              
              f = strsplit(as.character(x),'/',fixed=TRUE); 
              y = paste(imageDir, tail(as.character(f[[1]]), n=1), sep='/');
              if (!file.exists(y)) {
                tryCatch(download.file(trimws(x, which="both"), destfile = y, mode = 'wb')
                         , error = function(e) { skip_to_next <<- TRUE}) 
              }
              if(skip_to_next) { next } 
            }
          }
          
          imagegrid$Name = unlist(clean_column)
          
          output$downloadData <- downloadHandler(
            filename = function() {
              paste(p1,"_",d1,"_grid_", Sys.Date(), ".csv", sep="")
            },
            content = function(file) {
              write.csv(data.frame(Name=imagegrid$Name, Frequency=imagegrid$Freq ), file)
            }, 

          )
          
          observeEvent(input$downloadImages, {
            downloadFiles(imagegrid$Name)
          })
          
          return(imagegrid)
        }
  
    }, escape = FALSE)
    
}
# Run the app ----
shinyApp(ui, server)