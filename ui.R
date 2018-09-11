require(shiny)
require(shinysense)


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
               top: 35%;
               left: 64%;
               margin: auto;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background: #FFFFFF;
               opacity: 0.7;
               z-index:1;
               }
               "),
    tags$head(
      tags$title("cindeR"),
      tags$link(rel = "icon" , href="logo.png"),
      tags$script(src="LoadingBar.js")),
    div(div(class = "busy",
            img(src="Loading_icon.gif")) , id = "loadmessage"),

    
    navbarPage(div("cindeR" , style="color:#C4071B;text-align:center;font-style: italic;font-weight: bold;font-size: 150%"),
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
                          shinyswiprUI("plot_swiper" , 
                                       shiny::plotOutput("plotout" ,height = "600px")),
                          br(),
                          fluidRow(column(6 , align="center", offset = 3,  actionButton("back" , label = "back" , icon = icon("mail-reply" , lib = "font-awesome")) , tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")))
                          
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
