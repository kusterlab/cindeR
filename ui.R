require(shiny)
require(shinyjs)


shinyUI(
  fluidPage(
    
    tags$script('
              $(document).on("keydown", function (e) {
              Shiny.onInputChange("decision", [e.which,e.timeStamp]);
              });
              '),  
    tags$style(type="text/css", "
                                       #loadmessage {
                              position: fixed;
                              top: 50px;
                              left: 0px;
                              width: 100%;
                              padding: 5px 0px 5px 0px;
                              text-align: center;
                              font-weight: bold;
                              font-size: 100%;
                              color: #000000;
                              background-color: #FF0000;
                              z-index: 105;
                              }
                              "),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     
                     tags$div("Loading...",id="loadmessage")),
    #fluidRow( column(2 , div(h1("cindeR" )) , style="color:red;text-align:center") ),#, column(11 , img(src='logo.png', align = "right" , height = "100px" , width = "80px"))),
    #useShinyjs(),
    
    
    navbarPage(div("cindeR" , style="color:red;text-align:center;font-style: italic;font-weight: bold;font-size: 150%"),
               tabPanel("Evaluation",
                        sidebarPanel(
                          
                          wellPanel(
                          
                          shiny::fileInput(inputId = "file" , label = "Upload a csv file to judge") ,
                          checkboxInput("import.header", "Header", TRUE),
                          
                          selectInput("import.sep", "Separator", selected = ",",
                                      
                                      choices = c(Comma = ",", Semicolon = ";", Tab = "\t")),
                          
                          selectInput("import.quote", "Quote", selected = '"',
                                      
                                      choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"))
                          
                          
                          
                        ),
                          
                          wellPanel(
                            h4("Instructions"),
                            icon("toggle-left") , ": negative" ,
                            
                            icon("toggle-right") , ": positive"),
                          br(),
                          
                          wellPanel(
                            h4("Progress"),
                            uiOutput("progress")
                          ),
                          
                          downloadButton("Save" , "Download current progress")
                          
                          
                          
                        ),
                        
                        mainPanel(
                          
                          shiny::plotOutput("plotout" ,height = "600px")
                            
                        )
                        
               ),
               tabPanel("Plot function",
                        sidebarPanel( width = 3 ,
                                      fileInput("plotScript" , label = "Select a plot script")
                        ),
                        mainPanel(width = 9 ,
                                  h4("Plot function"),
                                  verbatimTextOutput("plotfuncode")))
               
               
               
    )
    
    
  )
)
