#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#devtools::install_github("https://github.com/RWParsons/GLMMcosinor")

library(shiny)
library(GLMMcosinor)
source("get_formula.R")
source("get_pred_length_out.R")




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
          uiOutput("mixed_model"),
          uiOutput("add_ranef"),
          uiOutput("random_effect_inputs"),
          uiOutput("random_effect_intercept_inputs"),
          uiOutput("categorical_var_mixed_mod"),
          tags$br(),
          uiOutput("ui.action")
        ),

        mainPanel(
          
          #Output as tabs
          tabsetPanel(type = "tabs", 
                      tabPanel("Plots", 
                               plotOutput("plot"),
                               tags$hr(),
                               uiOutput("plot_toggles"),
                               uiOutput("xbounds"),
                               uiOutput("prediction_length"),
                               uiOutput("plot_variables_ranef"), 
                               tags$hr(),
                               plotOutput("polar_plot")
                               ),
                      tabPanel("Summary",
                               tableOutput("table")
                               ),
                      tabPanel("Comparison",
                               uiOutput("choose_comparison"),
                               uiOutput("config_comparison"),
                               tags$hr(),
                               uiOutput("comparison_title"),
                               uiOutput("comparison_text"),
                               tags$hr(),
                               uiOutput("comparison_title2"),
                               uiOutput("comparison_text2")),
                      tabPanel("Data",
                               uiOutput("dataframe"))
                      )



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
  
  
  
  output$dataframe <- renderTable({
    if(is.null(input$file1)) {
      return()
    }
    df <- filedata()
    df
  })
  
  
  cols <- reactive({
    df <- filedata()
    if(is.null(df)) {
      return(NULL)
      
    } else {
      return(names(df))
    }
    
  })
  
  coef_names <- function(cc_obj,param, type){

    if(type == "group") {
      filtered_coef_names <- cc_obj$group_stats[[input$group]]
    }
    
    if(type == "component") {
      filtered_coef_names <- seq(1:input$component_num)
    }

    return(filtered_coef_names)
  }
  
  
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
    
    random_effect_intercept_inputs <- checkboxInput("group_ranef",label = paste("Add group:",paste(group), "as random effect"))

  
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
    
    output$choose_comparison <- renderUI({
      if (inherits(cc_obj, "error")) {
        return(cc_obj$message)
      }
      
      comp_choices <- NULL
      
      #get the number of levels within the selected group 
      group_levels <- cc_obj$group_stats[[input$group]]
      
      #get the number of components 
      component_number <- input$component_num
      
      if(length(group_levels)>1) {
        comp_choices <- append(comp_choices, "group")
      }
      if(component_number > 1) {
        comp_choices <- append(comp_choices, "component")
      }
      
      if (length(group_levels)>1 && component_number > 1){
        comp_choices <- append(comp_choices, "both group and component")
      }
      
      if(is.null(comp_choices)){
        return(NULL)
      }
      
      selectInput("choose_comparison", label = "Choose the type of comparison:", 
                  choices = comp_choices)
    })

    output$config_comparison <- renderUI({
      if (inherits(cc_obj, "error")) {
        return(cc_obj$message)
      }

      if (is.null(input$choose_comparison)) {
        return(NULL)
      }

      # selectInput("config_comparison", label= "Compare amplitudes", choices = coef_names(cc_obj))
      if(input$choose_comparison == "group") {
        input_selector <- list(selectInput("config_comparison1", 
                                           label = "choose a reference group:", 
                                           choices = coef_names(cc_obj, "amp", "group")),
                               actionButton("run_comparison","compare"))        
      }
      
      if(input$choose_comparison == "component") {
        input_selector <- list(selectInput("config_comparison2", 
                                           label = "choose a reference component:", 
                                           choices = coef_names(cc_obj, "amp", "component")),
                               actionButton("run_comparison","compare"))        
      }
      
      if(input$choose_comparison == "both group and component") {
        input_selector <- list(selectInput("config_comparison1", 
                                           label = "choose reference group:", 
                                           choices = coef_names(cc_obj, "amp", "group")),
                               selectInput("config_comparison2", 
                                           label = "choose reference component:", 
                                           choices = coef_names(cc_obj, "amp", "component")),
                               actionButton("run_comparison","compare"))        
      }
      
     
      
      
# 
#         if(input$add_comparison == "amplitude") {
#           input_selector <- list(selectInput("config_comparison1", 
#                                              label= "Choose reference amplitude:", 
#                                              choices = coef_names(cc_obj, "amp", comparison_type)),
#                                  actionButton("run_comparison", "compare"))
#       
#           
#         }
# 
#         if(input$add_comparison == "acrophase") {
#           input_selector <- list(selectInput("config_comparison1", 
#                                              label= "Choose reference acrophase:", 
#                                              choices = coef_names(cc_obj, "acr", comparison_type)),
#                                  actionButton("run_comparison", "Compare"))
#           
#         }
# 
#         if(input$add_comparison == "MESOR") {
#           input_selector <- list(selectInput("config_comparison1", 
#                                              label= "Choose reference MESOR:", 
#                                              choices = coef_names(cc_obj, "MESOR", comparison_type)),
#                                  actionButton("run_comparison", "Compare"))
#           
#         }
      input_selector
    })
    
    observeEvent(input$run_comparison,{
      ref_level <- input$config_comparison1
      ref_comp <- input$config_comparison2
      group_name <- input$group
      if(group_name == "None (default)"){
        group_name <- NULL
      }
      components <- input$component_num
      

      comp_table <- data.frame()
      comp_table2 <- data.frame()
      levels <- cc_obj$group_stats[[group_name]]
      if(!is.null(group_name)){
        comparison_levels <- levels[levels != ref_level]
      }
      counter = 0
      counter2 = 0
      for (param in c("amp","acr")) {
      ##
      # comp_table <- data.frame()
      # levels <- cc_obj$group_stats[[group_name]]
      # comparison_levels <- levels[levels != ref]
      # counter = 0
      if(input$choose_comparison %in% c("group", "both group and component")) {  
      for (i in 1:components) {
        
        for (j in 1:length(comparison_levels)) {
          counter = counter + 1
          comp_output_param<- test_cosinor_levels(cc_obj,group_name, param = param,
                                             comparison_A = ref_level,
                                             comparison_B = comparison_levels[j], 
                                             component_index = i)
          
          #If n_components = 1 in cc_obj, then the coefficients output will have no component suffix, so
          #so, capturing the reference value from this output must be adjusted 
          if(component_num == 1){
            ref_full <- round(cc_obj$coefficients[paste0(group_name,ref_level,":",param)], digits = 5)
          } else {
            ref_full <- round(cc_obj$coefficients[paste0(group_name,ref_level,":",param,i)], digits = 5)
          }
          comp_table[counter,1] <- paste0("[",paste0(group_name,ref_level,":",param,i),"] = ",ref_full)
          comp_table[counter,2] <- paste0("[",group_name,"=",comparison_levels[j],"]:", param,i," = ",round(comp_output_param$ind.test$conf.int[1], digits = 5))
          comp_table[counter,3:5] <- comp_output_param$ind.test$conf.int
            
          comp_table[counter,6] <- comp_output_param$ind.test$p.value

          colnames(comp_table) = c("reference est.",'comparison est.',"difference","lower CI", "upper CI", "p-value")
        }
      }
      }
        
      if(components > 1 && input$choose_comparison %in% c("component", "both group and component")) {
        #for this, should just be able to append to existing table
      ref_comp <- as.numeric(ref_comp)
      levels <- cc_obj$group_stats[[group_name]]
      comparison_components <- seq(components)[-ref_comp]

      for (i in 1:length(levels)) {
        for (j in 1:length(comparison_components)) {
          counter2 = counter2 + 1
          comp_output_param <-test_cosinor_components(cc_obj,group_name, param = param,
                                                  comparison_A = ref_comp,
                                                  comparison_B = comparison_components[j],
                                                  level_index = as.integer(levels[i]))
          if(is.null(group_name)){
            ref_full <- round(cc_obj$coefficients[paste0(param,ref_comp)], digits = 5)
            comp_table2[counter2,1] <- paste0("[",paste0(param,ref_comp),"] = ",ref_full)
            comp_table2[counter2,2] <- paste0(param,comparison_components[j]," = ",round(comp_output_param$ind.test$conf.int[1], digits = 5))
          } else {
            ref_full <- round(cc_obj$coefficients[paste0(group_name,levels[i],":",param,ref_comp)], digits = 5)
            comp_table2[counter2,1] <- paste0("[",paste0(group_name,levels[i],":",param,ref_comp),"] = ",ref_full)
            comp_table2[counter2,2] <- paste0("[",group_name,"=",levels[i],"]:", param,comparison_components[j]," = ",round(comp_output_param$ind.test$conf.int[1], digits = 5))
          }

          comp_table2[counter2,3:5] <- comp_output_param$ind.test$conf.int
          comp_table2[counter2,6] <- comp_output_param$ind.test$p.value
          colnames(comp_table2) = c("reference est.",'comparison est.',"difference","lower CI", "upper CI", "p-value")
          
          
        }
      }
      }
      
      } 
      ##
      
      # if(comparison_type == "group") {
      #   comp_output <- test_cosinor_levels(cc_obj,group_name, param = param,
      #                       comparison_A = ref,
      #                       comparison_B = comp, 
      #                       component_index = 1)
      # }
      
      if(input$choose_comparison %in% c("group", "both group and component")) {
        output$comparison_title <- renderText({
          # Wrap the text in HTML tags for bold formatting
          bold_text <- "<b>Group comparison</b>"
          HTML(bold_text)
        })
        
      output$comparison_text <- renderTable({
        comp_table
      }, digits = 5)
      } else {
        output$comparison_title <- NULL
        output$comparison_text <- NULL
      }
      
      if(components > 1 && input$choose_comparison %in% c("component", "both group and component")) {
      output$comparison_title2 <- renderText({
        # Wrap the text in HTML tags for bold formatting
        bold_text <- "<b>Component comparison</b>"
        HTML(bold_text)
        })
        
      output$comparison_text2 <- renderTable({
        comp_table2
      }, digits = 5)
      } else {
        output$comparison_title2 <- NULL
        output$comparison_text2 <- NULL
      }
      
    })
    
    # output$comparison_analysis <- renderText({
    #   
    # })
    
    

      

    
    # selectInput("add_comparison", label= "Comparison test", choices = names(cc_obj$coefficients))
    
    output$plot <- renderPlot({


      if (is.null(input$plot_variables) || length(input$plot_variables) == 0) {
        superimpose_arg <- FALSE  # Default value when no toggles are selected
        predict_arg <- FALSE
      } else {
        # Determine values based on selected toggles
        superimpose_arg <- "Overlay original data" %in% input$plot_variables
        predict_arg <- "Show prediction interval " %in% input$plot_variables
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
      
      #Set xlims based on user input
      if((is.null(input$xmin) || is.null(input$xmax)) || !("Specify plot window " %in% input$plot_variables)){
      xmin <- min(cc_obj$newdata[cc_obj$time_name])
      xmax <- max(cc_obj$newdata[cc_obj$time_name])
      } else {
        xmin <- input$xmin
        xmax <- input$xmax
      }
      
      #Fitted model resolution
      if((is.null(input$prediction_length)) || !("Fitted model resolution" %in% input$plot_variables)){
        prediction_length_arg <- get_pred_length_out(cc_obj)
      } else {
        prediction_length_arg <- input$prediction_length
      }
      
      
      autoplot(cc_obj,
               superimpose.data = superimpose_arg,
               predict.ribbon = predict_arg, 
               ranef_plot = ranef_bit,
               xlims = c(xmin,xmax), 
               pred.length.out = prediction_length_arg
      )
    })
    
    output$table <- renderTable({
      sum_obj <- summary(cc_obj)
      sum_obj[["transformed.table"]]
    }, rownames = TRUE, digits = 5)
    
    
    
    output$polar_plot <- renderPlot({
      component_num <- input$component_num
      polar_plot_list <- list()
      if(component_num>1) {
      
      for (i in 1:component_num){
        polar_plot_list[[i]] <- polar_plot(cc_obj, component_index = i)
      }
      polar_plot_list[[2]] 
      #polar_plot(cc_obj, component_index = 1:component_num)
      } else {
      polar_plot(cc_obj)
        
      }

    })
    
    output$plot_toggles <- renderUI({
    
      checkboxGroupInput('plot_variables', 'Plot options:',
                         c("Overlay original data",
                           "Show prediction interval ", 
                           "Specify plot window ", 
                           "Fitted model resolution"))
      
    })
    
    output$xbounds <- renderUI({
      if(!("Specify plot window " %in% input$plot_variables)){
        return(NULL)
      } else{
        xmin <- round(min(cc_obj$newdata[cc_obj$time_name]),digits = 5)
        xmax <- round(max(cc_obj$newdata[cc_obj$time_name]),digits = 5)
        xbound_list <- list(
          numericInput('xmin', label = "x-min", value = xmin, width = "250px"),
          numericInput('xmax', label = "x-max", value = xmax, width = "250px") 
        )
        xbound_list 
      }

    })
    
    output$prediction_length <- renderUI({
      if(!("Fitted model resolution" %in% input$plot_variables)){
        return(NULL)
      } else{
        default_pred.length.out <- get_pred_length_out(cc_obj)
        numericInput('prediction_length', label = 'Prediction length:', value = default_pred.length.out, width = "250px")
         
      }    
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
