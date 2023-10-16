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
          uiOutput("outcome_selector"),
          uiOutput("time_selector"),
          uiOutput("group_selector"),
          uiOutput("add_interaction_selector"),
          uiOutput("family_selector"),
          uiOutput("component_selector"),
        uiOutput("period_inputs"),
        
          tags$br(),
          uiOutput("ui.action")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot"), 
           textOutput("formula_text"),
           tags$hr(),
           uiOutput("plot_toggles")
           )
    )
)


#Define server logic. Generates a plot using autoplot()
server <- function(input, output, session) {
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
  
  group_col <- reactive({
    group_check <- input$group
    if(is.null(group_check)) {
      return(NULL)
    } else {
      return(group_check)
    }
  })
    output$add_interaction_selector <- renderUI({
      if(is.null(group_col()) || group_col() == "None (default)") {
        return(NULL)
      } 
    checkboxInput("add_interaction","Add as interaction Term", value = TRUE)
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
  
  
  # plot_col <- reactive ({
  # if(!is.null(output$plot)){
  #   return(NULL) 
  # } else {
  #   return(plot_check)
  # }
  # })
  
  

  
  
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
  
  output$component_selector <- renderUI({
    if(is.null(cols())) {
      return(NULL)
    }
    numericInput("component_num", "Number of Components:", 
                 value = 1, 
                 min = 1, 
                 step = 1)
  })
  
  component_col <- reactive({
    component_check <- input$component_num
    if(is.null(component_check)) {
      return(NULL)
    } else {
      return(component_check)
    }
  })
  
  output$outcome_selector <- renderUI({
    if(is.null(cols())) {
      return(NULL)
    }
    selectInput("outcome", "Select the OUTCOME (DEPENDENT) variable from:", cols())
  })
  
  # output$component_num <- renderText({
  #   if(is.null(cols())) {
  #     return(NULL)
  #   }
  #   component_num <- input$component_num
  # })
  # 

  

  period_values <- reactiveValues(values = NULL)
    observe({
      
      if(is.null(component_col())){
        return(NULL)
      } 
      component_num <- input$component_num

    period_inputs <- lapply(1:component_num, function(i) {
      numericInput(paste0("period_input_", i), label = paste("Period for Component", i), value = 1, min = 1, step = 1)  
    })
  
    output$period_inputs <- renderUI({
      if(is.null(cols())) {
        return(NULL)
      }
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
      
  
      if(input$group == "None (default)") {
        group <- NULL 
      } else {
      group <- input$group 
      }
      if(!is.null(input$add_interaction)){
        group_label_1 <- paste0(group)
        group_label_2 <- paste0("group ='",group,"',")
      } else {
        group_label_1 <- NULL 
        group_label_2 <- paste0("group ='",group,"',")
      }
      
      if(input$group == "None (default)") {
        group_label_2 <- NULL 
      }

      # Define the formula as a string to be evaluated 
      form_obj <- paste0(input$outcome,"~",group_label_1, "+", "amp_acro(time_col = ",input$time, ", n_components =", input$component_num ,",",group_label_2,"period =c(",paste(period_values, collapse = ", "), "))")
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
      

      if (is.null(input$plot_variables) || length(input$plot_variables) == 0) {
        superimpose_arg <- FALSE  # Default value when no toggles are selected
        predict_arg <- FALSE
      } else {
        # Determine values based on selected toggles
        superimpose_arg <- "superimpose data" %in% input$plot_variables
        predict_arg <- "predict ribbon" %in% input$plot_variables
      }
      
      autoplot(cc_obj,
               superimpose.data = superimpose_arg,
               predict.ribbon = predict_arg
      )
    })
    
    output$plot_toggles <- renderUI({
      checkboxGroupInput('plot_variables', 'Plot options:',
                         c("superimpose data",
                           "predict ribbon"))
    })

  })
  
  
  
  
    # Reactive expression to generate the formula based on user inputs
  formula_text <- reactive({
    
    if(is.null(input$outcome)) {
      outcome <- "Y" 
    } else {
      outcome <- input$outcome
    }
    
    if(is.null(input$group)|| input$group == "None (default)") {
      group <- NULL 
      group_label_1 <- NULL 
      group_label_2 <- paste('group = NULL')
    } else {
      #since 'group' appears at two different points in the formula, the 
      #formatting is slightly different: 
      group_label_1 <- paste(input$group,"+") 
      group_label_2 <- paste("group ='",input$group,"'")
    }
    
    
    
 
    group <- input$group 
    if(!is.null(input$add_interaction)) {
    if(!input$add_interaction){
      group_label_1 <- NULL 
    } 
    }
    
    if(is.null(input$component_num)) {
      component_num <- 1
      period_values <- 1
    } else {
      component_num <- input$component_num
      period_inputs <- lapply(1:component_num, function(i) {
        numericInput(paste0("period_input_", i), label = paste("Period for Component", i), value = 1, min = 1, step = 1)  
      })
      values <- sapply(1:component_num, function(i) {
        input[[paste0("period_input_", i)]]
      })
      
      period_values <- values
    }
    
    if(length(period_values)>1) {
      period_values <- paste0("c(",paste(period_values, collapse = ", "),")")
    }
    
    if(is.null(input$time)) {
      time <- "time"
    } else  {
      time <- input$time
    }
    
    if(is.null(input$family)) {
      family <- "gaussian"
    } else {
      family <- input$family
    }
    family <- eval(parse(text = family))
    
    # Define the formula as a string to be evaluated 
    form_obj <- paste(outcome,"~",group_label_1, "amp_acro(time_col = ",time, ", n_components =", component_num ,",",group_label_2,", period =",period_values, ")")
    #formula <- as.formula(form_obj)
    
    # You can customize the formula creation based on your specific requirements
    
    return(form_obj)
  })
  
  # Render the formula text in the UI
  output$formula_text <- renderText({
    formula_text()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
