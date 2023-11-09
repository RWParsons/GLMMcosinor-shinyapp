#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyFiles)
library(GLMMcosinor)
library(shinyjs)
source("get_formula.R")




ui <- fluidPage(

    # Application title
    titlePanel("GLMMcosinor"),

    useShinyjs(),
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
          uiOutput("mixed_model"),
          uiOutput("random_effect_inputs"),
          uiOutput("random_effect_intercept_inputs"),
          uiOutput("categorical_var_mixed_mod"),
          uiOutput("add_ranef"),
          tags$br(),
          uiOutput("ui.action")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          # uiOutput("show_formula_toggle"),
           textOutput("formula_text"),
           tags$hr(),
           plotOutput("plot"), 
           tags$hr(),
           uiOutput("plot_toggles"),
           uiOutput("plot_variables_ranef"),
           tags$hr(),
           tableOutput("table"), 
           plotOutput("polar_plot")
           )
    )
)

# Initialize a reactiveValues to store the sets of options
options_list <- reactiveValues(options = list())
counter <- reactiveVal(0)  # Initialize a counter
counter_values <- reactiveVal(1)
counter_seq <- reactiveVal(0)


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

  random_effect_values <- reactiveValues(values = NULL)


  period_values <- reactiveValues(values = NULL)
  ranef_values <- reactiveValues(values = NULL)
    observe({
      
      if(is.null(component_col())){
        return(NULL)
      } 
      component_num <- input$component_num
      group <- input$group 
   
    period_inputs <- lapply(1:component_num, function(i) {
      numericInput(paste0("period_input_", i), label = paste0("Period for Component ", i,":"), value = 1, min = 1, step = 1) 
    })
    random_effect_inputs <- lapply(1:component_num,function(i){
      checkboxInput(paste0("amp_acro",i),label = paste("Add component",i, "as random effect"))
    })
    
    random_effect_intercept_inputs <- checkboxInput("group_ranef",label = paste("Add group:",group, "as random effect"))

  
    output$period_inputs <- renderUI({
      if(is.null(cols())) {
        return(NULL)
      }
      
      period_inputs
    })
  
  })


  # callModule(randomEffectModule, "module1")
    
    
  output$ui.action <- renderUI({
    if (is.null(input$file1)) {
      return()
    }
    actionButton("action", "Run")
  })
  
  
  output$add_ranef <- renderUI({
    if (is.null(input$file1)) {
      return()
    }
    checkboxInput("add_ranef", label= "Add random effect term", FALSE)
  })
  
  


  options_list$options <- list()

  #Work in progress to try to make ranef spec more modular
  observeEvent(input$add_ranef, {
    if(is.null(input$component_num)) {
      return(NULL)
    }
    
    component_num <- input$component_num
    

    #create the initial set of options 
    current_options <- options_list$options


    get_new_options <- function (cols, component_num,group) {
      new_options <- list(
        
       selectInput(paste0("mixed_mod_var"), paste("Select a random variable", ":"), cols())
        ,
        lapply(1:(component_num), function(i) {
          checkboxInput(paste0("amp_acro", i), label = paste("Add component", i, "as random effect"))
        }))
      
      if(!is.null(group)){
        random_effect_intercept_inputs <- checkboxInput("group_ranef",label = paste("Add group:",group, "as random effect"))
        new_options[["int_term"]] <- random_effect_intercept_inputs
      }
      
      return(new_options)
    }
    # Create a new set of random effect and categorical variable options together
    
    if(input$group == "None (default)") {
      group = NULL
    } else {
      group = input$group 
    }
    new_options <- get_new_options(cols(), component_num, group)
    

    #names(new_options) <- rep(paste0("ranef_part",(counter())), length(new_options))
    options_list$options[["ranef"]] <- new_options


    if(input$add_ranef) {
    output$random_effect_inputs <- renderUI({
      tagList(
      HTML("<hr>"),  # Add a horizontal line as a separator
      options_list$options,
      HTML("<hr>")  # Add a horizontal line as a separator
      )
    })
    ranef_components <- input$random_effect_inputs
    } else {
      ranef_components <- NULL
      output$random_effect_inputs <- renderUI({
        return()
      })
      
    }
    
    
  })

  
  
  observeEvent(input$action, {
    isolate({
      df <- filedata()
      if (is.null(df)) {
        return(NULL)
      }
      set.seed(42)
      
      if(input$group == "None (default)") {
        group <- NULL 
      } else {
        group <- input$group 
      }

      component_num <- input$component_num
      
      
      
      #Get the period values for each component
      period_values <- sapply(1:component_num, function(i) {
        input[[paste0("period_input_", i)]]
      })


      if(input$add_ranef){
      k = 1
      random_effect_values <- NULL
      for (i in 1:component_num) {
        if (input[[paste0("amp_acro",i)]]) {
          random_effect_values[[k]] <- paste0("amp_acro",i)
          k = k+1
        } else {
          random_effect_values[[i]] <- NULL
        }
      }} else {
        random_effect_values <- NULL
      }
      

      
      #store a vector of ccomponents with random effects selected
      
      ranef_components <- random_effect_values
      
      ranef_int <- NULL
      if(input$add_ranef){
      categorical_var <- input$mixed_mod_var
      if(!is.null(input$group_ranef)) {
        if(input$group_ranef) {
          ranef_int <- input$group
        }}} else {
      categorical_var <- NULL
      ranef_int <- NULL
      }
      
    cc_obj <- get_formula(
      component_num = component_num, 
      df = df,
      family = input$family, 
      group = input$group, 
      add_interaction = input$add_interaction,
      outcome = input$outcome, 
      time = input$time, 
      period_values = period_values, 
      ranef_components = ranef_components, 
      categorical_var = categorical_var, 
      ranef_int = ranef_int
      )
    
    })
    output$contents <- renderText({
      if (inherits(cc_obj, "error")) {
        return(cc_obj$message)
      }
     cc_obj
    })
    
    output$plot <- renderPlot({


      if (is.null(input$plot_variables) || length(input$plot_variables) == 0) {
        superimpose_arg <- FALSE  # Default value when no toggles are selected
        predict_arg <- FALSE
      } else {
        # Determine values based on selected toggles
        superimpose_arg <- "superimpose data" %in% input$plot_variables
        predict_arg <- "predict ribbon" %in% input$plot_variables
      }

      if(input$add_ranef) {
        
        if(is.null(input$plot_variables_ranef) || length(input$plot_variables_ranef) == 0){
        ranef_bit <- NULL
        } else {
          if ("Plot distinct random effects" %in% input$plot_variables_ranef) {
          ranef_bit <- categorical_var
          } else {
            ranef_bit <- NULL 
          }
        }
      } else {
        ranef_bit <- NULL
      }
      
      autoplot(cc_obj,
               superimpose.data = superimpose_arg,
               predict.ribbon = predict_arg, 
               ranef_plot = ranef_bit
      )
    })
    output$table <- renderTable({
      sum_obj <- summary(cc_obj)
      sum_obj[["transformed.table"]]
    }, rownames = TRUE, digits = 5)
    output$polar_plot <- renderPlot({
      component_num <- input$component_num
      if(component_num>1) {
      polar_plot(cc_obj, component_index = 1:component_num)
      } else {
      polar_plot(cc_obj)
        
      }

    })
    
    output$plot_toggles <- renderUI({
      checkboxGroupInput('plot_variables', 'Plot options:',
                         c("superimpose data",
                           "predict ribbon"))
    })
    output$plot_variables_ranef <- renderUI({
      if (input$add_ranef) {
      checkboxGroupInput('plot_variables_ranef', "", "Plot distinct random effects")
      } else {
        return(NULL)
      }
    })
    
    

  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
