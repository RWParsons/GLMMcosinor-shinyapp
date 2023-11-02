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



# Initialize a counter_seq reactiveVal
# counter_seq <- reactiveVal(NULL)
# 
# # Function to update counter_seq
# updateCounterSeq <- function(value, add = TRUE) {
#   current_seq <- counter_seq()
#   if (add) {
#     counter_seq(c(current_seq, value))
#   } else {
#     counter_seq(current_seq[current_seq != value])
#   }
# }

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
          uiOutput("categorical_var_mixed_mod"),
          uiOutput("ui.add_ranef"),
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
           uiOutput("plot_toggles")
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
  
  # output$component_num <- renderText({
  #   if(is.null(cols())) {
  #     return(NULL)
  #   }
  #   component_num <- input$component_num
  # })
  # 

  random_effect_values <- reactiveValues(values = NULL)


  period_values <- reactiveValues(values = NULL)
  ranef_values <- reactiveValues(values = NULL)
    observe({
      
      if(is.null(component_col())){
        return(NULL)
      } 
      component_num <- input$component_num

   
    period_inputs <- lapply(1:component_num, function(i) {
      numericInput(paste0("period_input_", i), label = paste0("Period for Component ", i,":"), value = 1, min = 1, step = 1) 
    })
    random_effect_inputs <- lapply(1:component_num,function(i){
      checkboxInput(paste0("amp_acro",i),label = paste("Add component",i, "as random effect"))
    })

    # Initialize a new vector to store the combined values
    # combined_vector <- character(length(period_inputs) + length(random_effect_inputs))
    # 
    # # Interleave the elements
    # for (i in 1:length(period_inputs)) {
    #   combined_vector[(i - 1) * 2 + 1] <- period_inputs[i]
    #   combined_vector[i * 2] <- random_effect_inputs[i]
    # }
    # 
  
    output$period_inputs <- renderUI({
      if(is.null(cols())) {
        return(NULL)
      }
      
      period_inputs
    })
  
    # output$random_effect_toggle <- renderUI({
    #   if(is.null(cols())) {
    #     return(NULL)
    #   }
    #   random_effect_values
    # })
    
    # output$show_formula_toggle <- renderUI({
    #   if(is.null(cols())) {
    #     return(NULL)
    #   }
    #   checkboxInput("show_formula", "Show formula", FALSE)
    #   
    # })
  })


  # callModule(randomEffectModule, "module1")
    
    
  output$ui.action <- renderUI({
    if (is.null(input$file1)) {
      return()
    }
    actionButton("action", "Run")
  })
  
  
  output$ui.add_ranef <- renderUI({
    if (is.null(input$file1)) {
      return()
    }
    actionButton("add_ranef", "Add random effect term")
  })
  
  


  options_list$options <- list()

  #Work in progress to try to make ranef spec more modular
  observeEvent(input$add_ranef, {
    if(is.null(input$component_num)) {
      return(NULL)
    }
    # Get the current set of options
    current_options <- options_list$options
    counter(counter() + 1)
    # updateCounterSeq(counter(), TRUE) 
    
    #create the initial set of options 
    current_options <- options_list$options

    component_num <- input$component_num

    get_new_options <- function (cols, component_num) {
      new_options <- list(
        
       selectInput(paste0("mixed_mod_var"), paste("Select a random variable", ":"), cols())
        ,
        lapply(1:(component_num), function(i) {
          checkboxInput(paste0("amp_acro", i), label = paste("Add component", i, "as random effect"))
        }), 
        actionButton(paste0("remove_button", counter()), "Remove")
      )
      return(new_options)
    }
    # Create a new set of random effect and categorical variable options together
    new_options <- get_new_options(cols(), component_num)
    

    #names(new_options) <- rep(paste0("ranef_part",(counter())), length(new_options))
    options_list$options[[paste0("ranef_",counter())]] <- new_options


    output$random_effect_inputs <- renderUI({
      tagList(
      HTML("<hr>"),  # Add a horizontal line as a separator
      options_list$options,
      HTML("<hr>")  # Add a horizontal line as a separator
      )
    })


    if(!is.null(input$random_effect_inputs)) {
      ranef_components <- input$random_effect_inputs

    } else {
      ranef_components <- NULL
    }
    
    
    #Remove the button after activation 
    output$ui.add_ranef <- renderUI({
        return()
    })
    
    
  })

  observe({
    for (i in counter())
      observeEvent(input[[paste0("remove_button",i)]],{
        # options_list$options <- options_list$options[-((3*(i-1)+1):(3*(i)))]
        options_list$options[[paste0("ranef_",i)]] <- NULL
        
        #Reshow the add_ranef button 
        output$ui.add_ranef <- renderUI({
          if (is.null(input$file1)) {
            return()
          }
          actionButton("add_ranef", "Add random effect term")
        })
        
      })
    
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

      k = 1
      random_effect_values <- NULL
      for (i in 1:component_num) {
        if (input[[paste0("amp_acro",i)]]) {
          random_effect_values[[k]] <- paste0("amp_acro",i)
          k = k+1
        } else {
          random_effect_values[[i]] <- NULL
        }
      }
      

      
      #store a vector of ccomponents with random effects selected
      ranef_components <- random_effect_values
      categorical_var <- input$mixed_mod_var
      
      
    cc_obj <- get_formula(
      component_num = component_num, 
      df = df,
      family = input$family, 
      group = input$group, 
      add_interaction = input$add_interaction,
      outcome = input$outcome, 
      time = input$time, 
      period_values <- period_values, 
      ranef_components <- ranef_components, 
      categorical_var = categorical_var
      )
    
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
  
  

  #   # Reactive expression to generate the formula based on user inputs
  # formula_text <- reactive({
  #   if(is.null(cols()) || is.null(input$show_formula)) {
  #     return(NULL)
  #   } else {
  #     if(!input$show_formula) {
  #       return(NULL)
  #     }
  #   
  #   if(is.null(input$component_num)) {
  #     component_num <- 1
  #     period_values <- 1
  #   } else {
  #     component_num <- input$component_num
  #     period_inputs <- lapply(1:component_num, function(i) {
  #       numericInput(paste0("period_input_", i), label = paste("Period for Component", i), value = 1, min = 1, step = 1)  
  #     })
  #     values <- sapply(1:component_num, function(i) {
  #       input[[paste0("period_input_", i)]]
  #     })
  #     
  #     period_values <- values
  #   }
  #   
  #   if (!is.null(input$file1$name)) {
  #     file_name <- sub(".csv$", "", input$file1$name)
  #   } else {
  #     file_name <- "NULL"
  #   }
  #     
  #     
  #     browser()
  #     if(!is.null(categorical_var)){
  #       categorical_var <- input$mixed_mod_var
  #     }
  #   
  #   form_obj <- get_UI_formula(
  #     component_num = component_num, 
  #     df = df,
  #     family_arg = input$family, 
  #     group = input$group, 
  #     add_interaction = input$add_interaction,
  #     outcome = input$outcome, 
  #     time = input$time, 
  #     period_values <- period_values, 
  #     file_name <- file_name, 
  #     categorical_var <- categorical_var, 
  #     input <- input
  #   )
  #   
  #   
  #   
  #   return(form_obj)
  #   }
  # })
  # 
  # # Render the formula text in the UI
  # output$formula_text <- renderText({
  #   formula_text()
  # })
  # 
  # 
  
}

# Run the application 
shinyApp(ui = ui, server = server)
