#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for random distribution app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Milestone 3"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select the random distribution type ----
            # radioButtons("dist", "Distribution type:",
            #              c("Normal" = "norm",
            #                "Uniform" = "unif",
            #                "Log-normal" = "lnorm",
            #                "Exponential" = "exp")),
            
            # br() element to introduce extra vertical spacing ----
            br(),
            
            # Input: Slider for the number of observations to generate ----
            # sliderInput("n",
            #             "Number of observations:",
            #             value = 500,
            #             min = 1,
            #             max = 1000)
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("About", 
                                 "Here is the progress I have made this week in pursuit of my final project goal (with the support of 
                                 Beau!). I successfully created an Ipums account, selected the variables and years I plan to analyze
                                 and downloaded the data. I unzipped, and created an rds version of my data set. 
                                 I have started to examine and interrogate the data. In my Data tab, you can see a summary of the data 
                                 I'm working with at this stage. 
                                 
                                 Here's my repo, for reference: https://github.com/t-cobb/milestone3.git ",
                                 plotOutput("about")),
                        tabPanel("Data", " YEAR          PERNUM            DUID             PID           
 Min.   :2010   Min.   : 1.000   Min.   :  10001   Length:311371     
 1st Qu.:2012   1st Qu.: 1.000   1st Qu.:  23347   Class :character  
 Median :2014   Median : 2.000   Median :  42463   Mode  :character  
 Mean   :2014   Mean   : 2.402   Mean   : 264730                     
 3rd Qu.:2016   3rd Qu.: 3.000   3rd Qu.:  71369                     
 Max.   :2018   Max.   :20.000   Max.   :2329687                     
                                                                     
    MEPSID              PANEL           PSUANN         STRATANN   
 Length:311371      Min.   :14.00   Min.   :1.000   Min.   :1001  
 Class :character   1st Qu.:16.00   1st Qu.:1.000   1st Qu.:1050  
 Mode  :character   Median :18.00   Median :2.000   Median :1100  
                    Mean   :18.41   Mean   :1.632   Mean   :1229  
                    3rd Qu.:21.00   3rd Qu.:2.000   3rd Qu.:1146  
                    Max.   :23.00   Max.   :3.000   Max.   :2117  ",
                                 verbatimTextOutput("data"))
                        
            )
            
        )
    )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
    
    # Reactive expression to generate the requested distribution ----
    # This is called whenever the inputs change. The output functions
    # defined below then use the value computed from this expression
    d <- reactive({
        dist <- switch(input$dist,
                       norm = rnorm,
                       unif = runif,
                       lnorm = rlnorm,
                       exp = rexp,
                       rnorm)
        
        dist(input$n)
    })
    
    # Generate a plot of the data ----
    # Also uses the inputs to build the plot label. Note that the
    # dependencies on the inputs and the data reactive expression are
    # both tracked, and all expressions are called in the sequence
    # implied by the dependency graph.
    output$plot <- renderPlot({
        dist <- input$dist
        n <- input$n
        
        hist(d(),
             main = paste("r", dist, "(", n, ")", sep = ""),
             col = "#75AADB", border = "white")
    })
    
    # Generate a summary of the data ----
    output$summary <- renderPrint({
        summary(d())
    })
    
    # Generate an HTML table view of the data ----
    output$table <- renderTable({
        d()
    })
    
}

# Create Shiny app ----
shinyApp(ui, server)

