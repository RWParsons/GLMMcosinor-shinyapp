#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(GLMMcosinor)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("GLMMcosinor"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("file1",
                      "Choose CSV File",
                      accept = c(
                        "text/csv", 
                        "text/comma-seperated-values,text/plain", 
                        ".csv"
                      )
          ),
          tags$br(),
          uiOutput("time_selector"),
          uiOutput("group_selector"),
          uiOutput("outcome_selector"),
          uiOutput("family_selector"),
          numericInput("component_num", "Number of Components:", 
                       value = 1, 
                       min = 1, 
                       step = 1)
        ,
        uiOutput("period_inputs"),
        
          tags$br(),
          uiOutput("ui.action")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot"), 
           tableOutput("table"))
    )
)

#Define server logic 
server <- function(input, output) {
  filedata <- reactive({
    infile <- input$file1
    if(is.null(infile)) {
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  cols <- reactive({
    df <- filedata()
    if(is.null(df)) {
      return(NULL)
      
    } else {
      return(names(df))
    }
    
  })
  
  output$time_selector <- renderUI({
    if(is.null(cols())) {
      return(NULL)
    }
    selectInput("time", "Select the TIME (INDEPENDENT) variable from:", cols())
  })

  output$group_selector <- renderUI({
    if(is.null(cols())) {
      return(NULL)
    }
    selectInput("group", "Select the GROUPING variable from:", c("None (default)",cols()))
  })
  
  output$family_selector <- renderUI({
    if(is.null(cols())){
      return(NULL)
    }
    selectInput("family", "Select the data distribution:", c(
      "gaussian",
      'Gamma(link = "log")',
      "poisson"
    ))
  })
  
  
  output$outcome_selector <- renderUI({
    if(is.null(cols())) {
      return(NULL)
    }
    selectInput("outcome", "Select the OUTCOME (DEPENDENT) variable from:", cols())
  })
  
  output$component_num <- renderText({
    component_num <- input$component_num
  })

  period_values <- reactiveValues(values = NULL)
  
    observe({
      component_num <- input$component_num

    period_inputs <- lapply(1:component_num, function(i) {
      numericInput(paste0("period_input_", i), label = paste("Period for Component", i), value = 1, min = 1, step = 1)  
    })
  
    output$period_inputs <- renderUI({
      period_inputs
    })
  })

    
    
  output$ui.action <- renderUI({
    if (is.null(input$file1)) {
      return()
    }
    actionButton("action", "Run")
  })
  
  observeEvent(input$action, {
    isolate({
      df <- filedata()
      if (is.null(df)) {
        return(NULL)
      }
      set.seed(42)
      

      # Collect the values entered in period_inputs
      component_num <- input$component_num
      values <- sapply(1:component_num, function(i) {
        input[[paste0("period_input_", i)]]
      })
      period_values <- values
      #family <- noquote(paste0("stats::",input$family))
      family <- input$family
      family <- eval(parse(text = family))
      
      # Define the formula as a string to be evaluated 
      form_obj <- paste0(input$outcome,"~",input$group, "+", "amp_acro(time_col = ",input$time, ", n_components =", input$component_num ,", group = '",input$group,"', period =c(",paste(period_values, collapse = ", "), "))")
      formula <- as.formula(form_obj)
      
      #generate the cglmm object 
      cc_obj <- 
          GLMMcosinor::cglmm(
            data = df, 
            formula = eval(formula),
            family = family
          )
      
      # ensure that autoplot() has access to the formula 
      cc_obj$cglmm.calls$cglmm$formula = formula 
    
    })
    
    output$contents <- renderText({
      if (inherits(cc_obj, "error")) {
        return(cc_obj$message)
      }
     cc_obj
    })
    
    output$plot <- renderPlot({
      # if (!inherits(cc_obj, "list")) {
      #   return(NULL)
      # }
      autoplot(cc_obj,
               superimpose.data = TRUE,
               predict.ribbon = FALSE
      )
    })
    
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)
