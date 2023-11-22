#Shiny app for the GLMMcosinor R package 
#devtools::install_github("https://github.com/RWParsons/GLMMcosinor")

library(shiny)
library(GLMMcosinor)
lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source)


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
          uiOutput("ci_level"),
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
                               plotOutput("polar_plot"), 
                               # Action buttons in a single row
                               uiOutput("polar_plot_selector"), 
                               uiOutput("polar_plot_toggles"),
                               tags$hr(),
                               tags$hr(),
                               tags$hr()
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
  
  
    output$add_interaction_selector <- renderUI({
      if(is.null(input$group) || input$group == "None (default)") {
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
  
  
  output$family_selector <- renderUI({
    if(is.null(cols())){
      return(NULL)
    }
    
    #These arguments are from the family() function to be passed to glmmTMB 
    selectInput("family", "Select the data distribution:", c(
      'gaussian(link = "identity")',
      'binomial(link = "logit")',
      'Gamma(link = "log")',
      'inverse.gaussian(link = "1/mu^2")',
      'poisson(link = "log")',
      'quasi(link = "identity", variance = "constant")',
      'quasibinomial(link = "logit")',
      'quasipoisson(link = "log")'
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
      
      if(is.null(input$component_num) || is.na(input$component_num) || input$component_num == 0){
        return(NULL)
      }
      
      component_num <- input$component_num
      group <- input$group 
   
    period_inputs <- lapply(seq_len(component_num), function(i) {
      numericInput(paste0("period_input_", i), label = paste0("Period for Component ", i,":"), value = 1, min = 1, step = 1) 
    })
    random_effect_inputs <- lapply(seq_len(component_num),function(i){
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
  
  output$ci_level <- renderUI({
    if (is.null(input$file1)) {
      return()
    }
    numericInput("ci_level", "confidence level:", value = 0.95, min = 0, max = 1)
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
      choose_comparison <- input$choose_comparison
      comparison_table <- get_comparison_table(ref_level = ref_level,
                                               ref_comp = ref_comp,
                                               group_name = group_name,
                                               components = components,
                                               cc_obj = cc_obj,
                                               choose_comparison = choose_comparison, 
                                               ci_level = ci_level)
      comparison_table_group <- comparison_table[1]
      comparison_table_components <- comparison_table[2]
      
      
      if(input$choose_comparison %in% c("group", "both group and component")) {
        output$comparison_title <- renderText({
          # Wrap the text in HTML tags for bold formatting
          bold_text <- "<b>Group comparison</b>"
          HTML(bold_text)
        })
        
      output$comparison_text <- renderTable({
        comparison_table_group
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
        comparison_table_components
      }, digits = 5)
      } else {
        output$comparison_title2 <- NULL
        output$comparison_text2 <- NULL
      }
      
    })
    

    
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
               pred.length.out = prediction_length_arg, 
               ci_level = ci_level
      )
    })
    
    output$table <- renderTable({
      sum_obj <- summary(cc_obj, ci_level = ci_level)
      sum_obj[["transformed.table"]]
    }, rownames = TRUE, digits = 5)
    
    # Update plot based on user interaction
    polar_plot_index <- reactiveVal(1)
    plotGenerated <- reactiveVal(FALSE)
    
    output$polar_plot_selector <- renderUI({
      if(!plotGenerated() || input$component_num == 1){
        return(NULL)
      }
      
      tagList(
        actionButton("prevButton", "Previous", icon("arrow-left"), style = 'display: inline-block; margin-left: 15px;'),
        actionButton("nextButton", "Next", icon("arrow-right"), style = 'display: inline-block;')
      )
      

    })
    
    
    observeEvent(input$nextButton, {
      polar_plot_index(min(polar_plot_index() + 1, input$component_num))
    })
    
    observeEvent(input$prevButton, {
      polar_plot_index(max(polar_plot_index() - 1, 1))
    })
    
    observe({
      polar_plot_index()
      polar_plot_toggle()
      polar_plot_overlay_parameter_info()
      polar_plot_ellipse_opacity()
      polar_plot_clockwise()
      output$polar_plot <- renderPlot({
        component_num <- input$component_num
        if(is.null(input$polar_plot_view_toggle)){
          view = "full"
        } else {
          view = input$polar_plot_view_toggle
        }
        if(component_num>1) {
          show_component_labels  = TRUE
        } else {
          show_component_labels  = FALSE        
        }
        
         if(is.null(input$overlay_parameter_info)){
           polar_plot_overlay_parameter_info <- FALSE
         } else {
           polar_plot_overlay_parameter_info <-input$overlay_parameter_info
         }
        if(is.null(input$ellipse_opacity)){
          ellipse_opacity <- 0.3
        } else {
          ellipse_opacity <- input$ellipse_opacity
        }
        
        if(is.null(input$clockwise) || !input$clockwise){
          clockwise <- FALSE
          start = "right"
          radial_units = c("radians")
          grid_angle_segments = 8
        } else {
          clockwise <- TRUE
          start = "top"
          radial_units = c("radians")
          grid_angle_segments = 12
        }
        
        
        polar_plot_object <- polar_plot(cc_obj, 
                                        component_index = polar_plot_index(), 
                                        show_component_labels = show_component_labels,
                                        view = view, 
                                        overlay_parameter_info = polar_plot_overlay_parameter_info, 
                                        ci_level = ci_level, 
                                        ellipse_opacity = ellipse_opacity, 
                                        clockwise = clockwise, 
                                        start = start,
                                        radial_units = radial_units,
                                        grid_angle_segments = grid_angle_segments)
        
        plotGenerated(TRUE)
        return(polar_plot_object)

      })
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
        return(xbound_list)
      }
      
    })
    
    output$prediction_length <- renderUI({
      if(!("Fitted model resolution" %in% input$plot_variables)){
        return(NULL)
      } else{
        default_pred.length.out <- get_pred_length_out(cc_obj)
        output <- numericInput('prediction_length', label = 'Prediction length:', value = default_pred.length.out, width = "250px")
        return(output)
      }    
    })
    output$plot_variables_ranef <- renderUI({
      if (input$add_ranef) {
        checkboxGroupInput('plot_variables_ranef', "", "Plot distinct random effects")
      } else {
        return(NULL)
      }
    })
    
    
    
    output$polar_plot_toggles <- renderUI({
      if(!plotGenerated()){
        return(NULL)
      }
      
      outputs <- list(selectInput('polar_plot_view_toggle', "Select view:",
                  c("full", "zoom","zoom_origin")),
      checkboxInput('overlay_parameter_info', 'show parameter info', FALSE), 
      sliderInput("ellipse_opacity", "Confidence Ellipse opacity:", min = 0, max = 1, value = 0.3), 
      checkboxInput("clockwise","24 hour clock view:", FALSE)
      )
      outputs
    })
    
    #might be able to wrap these under one reactive value? 
    polar_plot_toggle <- reactiveVal(input$polar_plot_toggles)
    polar_plot_overlay_parameter_info <- reactiveVal(input$overlay_parameter_info)
    polar_plot_ellipse_opacity <- reactiveVal(input$ellipse_opacity)
    polar_plot_clockwise <- reactiveVal(input$clockwise)
    
    ci_level <- reactiveVal({
      if(is.null(input$ci_level)){
        ci_level <- 0.95
      } else {
        ci_level <-  input$ci_level
        if(is.na(ci_level)){
          ci_level <-0.95 
        }
        
        if(ci_level <= 0) {
          ci_level <- 0
        }
        
        if(ci_level >= 1) {
          ci_level <- 0
        }
        
      }
      return(ci_level)
    }
    )
    
    
    

  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
