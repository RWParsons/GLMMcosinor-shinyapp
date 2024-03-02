# Shiny app for the GLMMcosinor R package
# devtools::install_github("https://github.com/RWParsons/GLMMcosinor")
# devtools::install_github("https://github.com/RWParsons/GLMMcosinor.git", ref = "dev")

library(shiny)
library(shinyjs)
library(DT)
library(GLMMcosinor)
library(shinythemes)
library(ggplot2)
library(glmmTMB)
library(shinycssloaders) # for the loading animation
library(readxl)
lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source)


ui <- fluidPage(
  
  # Application title
  titlePanel("GLMMcosinor"),
  
  # Application theme
  theme = shinytheme("cerulean"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput("file1",
                "Choose CSV File",
                accept = c(
                  "text/csv",
                  ".xlsx",
                  "text/comma-seperated-values,text/plain",
                  ".csv"
                )
      ),
      tags$br(),
      uiOutput("outcome_selector"),
      uiOutput("time_selector"),
      uiOutput("add_interaction_selector_time"),
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
    conditionalPanel(
      condition = "input.action > 0",
      mainPanel(
        
        # Output as tabs
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Time series plot",
            # uiOutput("plot"),
            shinycssloaders::withSpinner(plotOutput("plot")),
            # plotOutput("plot"),
            uiOutput("saveBtn"),
            tags$hr(),
            # downloadButton("saveBtn", "Save Image"),
            uiOutput("plot_toggles"),
            uiOutput("xbounds"),
            uiOutput("prediction_length"),
            uiOutput("plot_variables_ranef"),
            tags$hr()
          ),
          tabPanel(
            "Polar plot",
            plotOutput("polar_plot"),
            uiOutput("saveBtn_polar_plot"),
            tags$hr(),
            # Action buttons in a single row
            uiOutput("polar_plot_selector"),
            uiOutput("polar_plot_toggles"),
            tags$hr(),
            tags$hr(),
            tags$hr()
          ),
          tabPanel(
            "Summary",
            tableOutput("table"),
            uiOutput("saveBtn_data")
          ),
          tabPanel(
            "Comparison",
            uiOutput("choose_comparison"),
            uiOutput("config_comparison"),
            tags$hr(),
            uiOutput("comparison_title"),
            uiOutput("comparison_text"),
            uiOutput("saveBtn_comparison_table_group"),
            tags$hr(),
            uiOutput("comparison_title2"),
            uiOutput("comparison_text2"),
            uiOutput("saveBtn_comparison_table_component")
          ),
          tabPanel(
            "Data",
            DTOutput("dataframe")
          )
        )
      )
    ) # closes the conditional panel
  )
)

# Initialize a reactiveValues to store the sets of options
options_list <- reactiveValues(options = list())

# Define server logic. Generates a plot using autoplot()
server <- function(input, output, session) {
  filedata <- reactive({
    infile <- input$file1
    if (is.null(infile)) {
      return(NULL)
    }
    # check if file is an excel (xlsx) file
    if (tolower(substr(infile$datapath,
                       start = nchar(infile$datapath) - 4,
                       stop = nchar(infile$datapath)
    )) == ".xlsx") {
      return(read_excel(infile$datapath))
    } else {
      return(read.csv(infile$datapath))
    }
  })
  
  # show the dataset
  output$dataframe <- renderDT(
    {
      if (is.null(input$file1)) {
        return()
      }
      df <- filedata()
      return(df)
    },
    options = list(pageLength = 15)
  )
  
  
  # a reactive function that gets the column names from the dataset
  cols <- reactive({
    df <- filedata()
    if (is.null(df)) {
      return(NULL)
    } else {
      return(names(df))
    }
  })
  
  # gets the names of the coefficients from the cglmm object
  coef_names <- function(cc_obj, param, type) {
    if (type == "group") {
      filtered_coef_names <- cc_obj$group_stats[[input$group]]
    }
    
    if (type == "component") {
      filtered_coef_names <- seq(1:input$component_num)
    }
    
    return(filtered_coef_names)
  }
  
  
  # a checkbox that determines whether the group will be included as an interaction term
  output$add_interaction_selector <- renderUI({
    if (is.null(input$group) || input$group == "None (default)") {
      return(NULL)
    }
    checkboxInput("add_interaction", "Add as interaction term", value = FALSE)
  })
  
  # choose the time variable
  output$time_selector <- renderUI({
    if (is.null(cols())) {
      return(NULL)
    }
    selectInput("time", "Select the TIME (INDEPENDENT) variable from:", cols())
  })
  
  # a checkbox that determines whether the time variable will be included as an interaction term
  output$add_interaction_selector_time <- renderUI({
    if (is.null(input$time)) {
      return(NULL)
    }
    checkboxInput("add_interaction_time", "Add as interaction term", value = FALSE)
  })
  
  
  # choose the grouping variable.
  output$group_selector <- renderUI({
    if (is.null(cols())) {
      return(NULL)
    }
    selectInput("group", "Select the GROUPING variable from:", c("None (default)", cols()))
  })
  
  # choose the family distribution from family(). TODO: add all families supported by glmmTMB
  output$family_selector <- renderUI({
    if (is.null(cols())) {
      return(NULL)
    }
    
    # These arguments are from the family() function to be passed to glmmTMB
    selectInput("family", "Select the data distribution:", c(
      'gaussian(link = "identity")',
      'binomial(link = "logit")',
      'Gamma(link = "log")',
      'inverse.gaussian(link = "1/mu^2")',
      'poisson(link = "log")',
      'nbinom2(link = "log")',
      'nbinom1(link = "log")',
      'compois(link = "log")',
      'truncated_compois(link = "log")',
      'genpois(link = "log")',
      'truncated_genpois(link = "log")',
      'truncated_poisson(link = "log")',
      'truncated_nbinom2(link = "log")',
      'truncated_nbinom1(link = "log")',
      'beta_family(link = "logit")',
      'betabinomial(link = "logit")',
      'tweedie(link = "log")',
      'lognormal(link = "log")',
      'ziGamma(link = "inverse")',
      't_family(link = "identity")',
      'ordbeta(link = "logit")'
    ))
  })
  
  # select the number of components in the model
  output$component_selector <- renderUI({
    if (is.null(cols())) {
      return(NULL)
    }
    numericInput("component_num", "Number of Components:",
                 value = 1,
                 min = 1,
                 step = 1
    )
  })
  
  # select the response variable
  output$outcome_selector <- renderUI({
    if (is.null(cols())) {
      return(NULL)
    }
    selectInput("outcome", "Select the OUTCOME (DEPENDENT) variable from:", cols())
  })
  
  # define some reactive values to be accessed throughout the app
  random_effect_values <- reactiveValues(values = NULL)
  period_values <- reactiveValues(values = NULL)
  ranef_values <- reactiveValues(values = NULL)
  
  # based on the number of components specified, get the user to input a period value
  # for each of the components. This depends on 'component_num', hence the observe() function
  observe({
    if (is.null(input$component_num) || is.na(input$component_num) || input$component_num == 0) {
      return(NULL)
    }
    
    component_num <- input$component_num
    group <- input$group
    
    # get the periods for each component
    period_inputs <- lapply(seq_len(component_num), function(i) {
      numericInput(paste0("period_input_", i), label = paste0("Period for Component ", i, ":"), value = 1, min = 1, step = 1)
    })
    
    # based on the number of components, allow the user to select which components to add as
    # # random effects
    # random_effect_inputs <- lapply(seq_len(component_num),function(i){
    #   checkboxInput(paste0("amp_acro",i),label = paste("Add component",i, "as random effect"))
    # })
    
    # if there is a group, allow the user to specify as a random effect term
    random_effect_intercept_inputs <- checkboxInput("group_ranef", label = paste("Add group:", paste(group), "as random effect"))
    
    
    # ask the user to input periods for each component
    output$period_inputs <- renderUI({
      if (is.null(cols())) {
        return(NULL)
      }
      period_inputs
    })
  })
  
  
  
  # toggle to enable mixed model specification
  output$add_ranef <- renderUI({
    if (is.null(input$file1)) {
      return()
    }
    checkboxInput("add_ranef", label = "Add random effect term", FALSE)
  })
  
  # set the confidence level. This will be used across the entire app
  output$ci_level <- renderUI({
    if (is.null(input$file1)) {
      return()
    }
    numericInput("ci_level", "confidence level:", value = 0.95, min = 0, max = 1)
  })
  
  # display button to run the cglmm() analysis
  output$ui.action <- renderUI({
    if (is.null(input$file1)) {
      return()
    }
    actionButton("action", "Run")
  })
  
  
  # get the mixed model specification details
  options_list$options <- list()
  observeEvent(input$add_ranef, {
    if (is.null(input$component_num)) {
      return(NULL)
    }
    
    
    # get the component number
    component_num <- input$component_num
    # get the group argument
    if (input$group == "None (default)") {
      group <- NULL
    } else {
      group <- input$group
    }
    
    # create the initial set of options
    current_options <- options_list$options
    # this function takes the column names, number of components, and groups from the
    # cglmm model and returns a list of potential variables that could be assigned
    # random effects in the mixed model.
    get_new_options <- function(cols, component_num, group) {
      new_options <- list(
        selectInput(paste0("mixed_mod_var"), paste("Select a random variable", ":"), cols()),
        lapply(1:(component_num), function(i) {
          checkboxInput(paste0("amp_acro", i), label = paste("Add component", i, "as random effect"))
        })
      )
      
      if (!is.null(group)) {
        random_effect_intercept_inputs <- checkboxInput("group_ranef", label = paste("Add group:", group, "as random effect"))
        new_options[["int_term"]] <- random_effect_intercept_inputs
      }
      
      return(new_options)
    }
    
    new_options <- get_new_options(cols(), component_num, group)
    options_list$options[["ranef"]] <- new_options
    
    # present the mixed model spec options to the user
    if (input$add_ranef) {
      output$random_effect_inputs <- renderUI({
        tagList(
          HTML("<hr>"), # Add a horizontal line as a separator
          options_list$options,
          HTML("<hr>") # Add a horizontal line as a separator
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
  
  
  # analyse the data using cglmm() once the user selects 'run'
  observeEvent(input$action, {
    isolate({
      df <- filedata()
      if (is.null(df)) {
        return(NULL)
      }
      set.seed(42)
      
      if (input$group == "None (default)") {
        group <- NULL
      } else {
        group <- input$group
      }
      
      # get the component number
      component_num <- input$component_num
      
      # get the period values for each component
      period_values <- sapply(1:component_num, function(i) {
        input[[paste0("period_input_", i)]]
      })
      
      # get the components which have random effects
      if (input$add_ranef) {
        k <- 1
        random_effect_values <- NULL
        for (i in 1:component_num) {
          if (input[[paste0("amp_acro", i)]]) {
            random_effect_values[[k]] <- paste0("amp_acro", i)
            k <- k + 1
          } else {
            random_effect_values[[i]] <- NULL
          }
        }
      } else {
        random_effect_values <- NULL
      }
      
      # store a vector of components with random effects selected
      ranef_components <- random_effect_values
      
      # get the categorical variable from the mixed model and the intercept (if applicable)
      # that has a random variable designation
      ranef_int <- NULL
      if (input$add_ranef) {
        categorical_var <- input$mixed_mod_var
        if (!is.null(input$group_ranef)) {
          if (input$group_ranef) {
            ranef_int <- input$group
          }
        }
      } else {
        categorical_var <- NULL
        ranef_int <- NULL
      }
      
      # get the cglmm() object
      cc_obj <- tryCatch(
        {
          get_formula(
            component_num = component_num,
            df = df,
            family = input$family,
            group = input$group,
            add_interaction = input$add_interaction,
            add_interaction_time = input$add_interaction_time,
            outcome = input$outcome,
            time = input$time,
            period_values = period_values,
            ranef_components = ranef_components,
            categorical_var = categorical_var,
            ranef_int = ranef_int
          )
        },
        error = function(e) {
          showNotification(paste("Error: ", e$message), type = "error")
          return(NULL) # Return NULL or any default value
        },
        warning = function(w) {
          # Handle the warning here
          showNotification(paste("Warning: ", w$message), type = "warning")
          # Continue with the calculation despite the warning
          get_formula(
            component_num = component_num,
            df = df,
            family = input$family,
            group = input$group,
            add_interaction = input$add_interaction,
            add_interaction_time = input$add_interaction_time,
            outcome = input$outcome,
            time = input$time,
            period_values = period_values,
            ranef_components = ranef_components,
            categorical_var = categorical_var,
            ranef_int = ranef_int
          )
        }
      )
    })
    
    # store the cc_obj as output
    output$contents <- renderText({
      if (inherits(cc_obj, "error")) {
        return(cc_obj$message)
      }
      cc_obj
    })
    
    # summary tab
    # generate and present the summary statistics of the cglmm() object
    # this includes the parameter estimates
    output$table <- renderTable(
      {
        sum_obj <- summary(cc_obj, ci_level = ci_level)
        sum_obj[["transformed.table"]]
      },
      rownames = TRUE,
      digits = 5
    )
    
    output$saveBtn_data <- renderUI({
      if (is.null(cc_obj)) {
        return(NULL)
      }
      downloadButton("saveBtn_data", "Save table")
      downloadHandler(
        filename = function() {
          paste(input$file1, "_summary.csv", sep = "")
        },
        content = function(file) {
          sum_obj <- summary(cc_obj, ci_level = ci_level)
          write.csv(sum_obj[["transformed.table"]], file, row.names = TRUE)
        }
      )
    })
    
    
    
    # comparison tab
    # present options for the comparison table (table comparing parameter estimates)
    output$choose_comparison <- renderUI({
      if (inherits(cc_obj, "error")) {
        return(cc_obj$message)
      }
      
      # comp_choices will be the comparison type (group, component, or both)
      comp_choices <- NULL
      
      # get the number of levels within the selected group
      group_levels <- cc_obj$group_stats[[input$group]]
      
      # get the number of components
      component_number <- input$component_num
      
      # if there are more than one group_levels, then a group comparison is possible:
      if (length(group_levels) > 1) {
        comp_choices <- append(comp_choices, "group")
      }
      
      # if there is more than one components, then a component comparison is possible:
      if (component_number > 1) {
        comp_choices <- append(comp_choices, "component")
      }
      
      # if there are multiple group levels AND components, then both comparisons are possible:
      if (length(group_levels) > 1 && component_number > 1) {
        comp_choices <- append(comp_choices, "both group and component")
      }
      
      # if none of the conditions above are met, then no comparison can be made
      if (is.null(comp_choices)) {
        return(NULL)
      }
      
      # choose the type of comparison (group, component, or both)
      selectInput("choose_comparison",
                  label = "Choose the type of comparison:",
                  choices = comp_choices
      )
    })
    
    # configure the comparison by choosing the reference level (within a group), and
    # the reference component where applicable. These reference selections will be
    # compared against all other groups/component estimates for each parameter
    output$config_comparison <- renderUI({
      if (inherits(cc_obj, "error")) {
        return(cc_obj$message)
      }
      
      if (is.null(input$choose_comparison)) {
        return(NULL)
      }
      
      if (input$choose_comparison == "group") {
        input_selector <- list(
          selectInput("config_comparison1",
                      label = "choose a reference group:",
                      choices = coef_names(cc_obj, "amp", "group")
          ),
          actionButton("run_comparison", "compare")
        )
      }
      
      if (input$choose_comparison == "component") {
        input_selector <- list(
          selectInput("config_comparison2",
                      label = "choose a reference component:",
                      choices = coef_names(cc_obj, "amp", "component")
          ),
          actionButton("run_comparison", "compare")
        )
      }
      
      if (input$choose_comparison == "both group and component") {
        input_selector <- list(
          selectInput("config_comparison1",
                      label = "choose reference group:",
                      choices = coef_names(cc_obj, "amp", "group")
          ),
          selectInput("config_comparison2",
                      label = "choose reference component:",
                      choices = coef_names(cc_obj, "amp", "component")
          ),
          actionButton("run_comparison", "compare")
        )
      }
      input_selector
    })
    
    # generate the comparison table
    observeEvent(input$run_comparison, {
      ref_level <- input$config_comparison1 # reference level
      ref_comp <- input$config_comparison2 # reference component
      group_name <- input$group
      if (group_name == "None (default)") {
        group_name <- NULL
      }
      components <- input$component_num
      choose_comparison <- input$choose_comparison
      
      # pass arguments to function that returns the comparison table(s)
      comparison_table <- get_comparison_table(
        ref_level = ref_level,
        ref_comp = ref_comp,
        group_name = group_name,
        components = components,
        cc_obj = cc_obj,
        choose_comparison = choose_comparison,
        ci_level = ci_level
      )
      comparison_table_group <- comparison_table[1] # group comparison table
      comparison_table_components <- comparison_table[2] # component comparison table
      
      # format title and present the group comparison table
      if (input$choose_comparison %in% c("group", "both group and component")) {
        output$comparison_title <- renderText({
          # Wrap the text in HTML tags for bold formatting
          bold_text <- "<b>Group comparison</b>"
          HTML(bold_text)
        })
        
        output$comparison_text <- renderTable(
          {
            comparison_table_group
          },
          digits = 5
        )
        
        
        # download button for comparison table
        output$saveBtn_comparison_table_group <- renderUI({
          downloadButton("saveBtn_comparison_table_group", "Save table")
          downloadHandler(
            filename = function() {
              paste(input$file1, "_group_comparison.csv", sep = "")
            },
            content = function(file) {
              group_name <- input$group
              if (group_name == "None (default)") {
                group_name <- NULL
              }
              comparison_table_group <- get_comparison_table(
                ref_level = input$config_comparison1,
                ref_comp = input$config_comparison2,
                group_name = group_name,
                components = input$component_num,
                cc_obj = cc_obj,
                choose_comparison = input$choose_comparison,
                ci_level = ci_level
              )
              write.csv(comparison_table_group[1], file, row.names = TRUE)
            }
          )
        })
      } else {
        output$comparison_title <- NULL
        output$comparison_text <- NULL
        output$saveBtn_comparison_table_group <- NULL
      }
      
      # format title and present the component comparison table
      if (components > 1 && input$choose_comparison %in% c("component", "both group and component")) {
        output$comparison_title2 <- renderText({
          # Wrap the text in HTML tags for bold formatting
          bold_text <- "<b>Component comparison</b>"
          HTML(bold_text)
        })
        
        output$comparison_text2 <- renderTable(
          {
            comparison_table_components
          },
          digits = 5
        )
        
        
        # download button for comparison table
        output$saveBtn_comparison_table_component <- renderUI({
          downloadButton("saveBtn_comparison_table_component", "Save table")
          downloadHandler(
            filename = function() {
              paste(input$file1, "_component_comparison.csv", sep = "")
            },
            content = function(file) {
              group_name <- input$group
              if (group_name == "None (default)") {
                group_name <- NULL
              }
              comparison_table_component <- get_comparison_table(
                ref_level = input$config_comparison1,
                ref_comp = input$config_comparison2,
                group_name = group_name,
                components = input$component_num,
                cc_obj = cc_obj,
                choose_comparison = input$choose_comparison,
                ci_level = ci_level
              )
              write.csv(comparison_table_component[2], file, row.names = TRUE)
            }
          )
        })
      } else {
        output$comparison_title2 <- NULL
        output$comparison_text2 <- NULL
        output$saveBtn_comparison_table_component <- NULL
      }
    })
    
    
    # plots
    # create a reactive value that is TRUE if a time-plot has been generated
    time_plotGenerated <- reactiveVal(FALSE)
    
    observe({
      # withProgress(message = 'Making plot', value = 0, {
      # generate the time plot
      output$plot <- renderPlot({
        # if(is.null(cc_obj)){
        #   return(NULL)
        # }
        
        # these correspond to reactive inputs. If any of them change, the plots will
        # be generated again to reflect updated inputs
        detect_superimpose.data()
        detect_predict.ribbon()
        detect_xmin()
        detect_xmax()
        detect_prediction_length()
        detect_add_ranef_plot()
        detect_xlabel()
        detect_ylabel()
        
        # get the time plot
        time_plot_object <- get_time_plot_inputs(
          superimpose.data = input$superimpose.data,
          predict.ribbon = input$predict.ribbon,
          xmin = input$xmin,
          xmax = input$xmax,
          prediction_length = input$prediction_length,
          add_ranef = input$add_ranef_plot,
          categorical_var = input$mixed_mod_var,
          ci_level = input$ci_level,
          cc_obj = cc_obj
        )
        # Add x and y labels to the plot
        if (!is.null(input$xlabel)) {
          time_plot_object <- time_plot_object + ggplot2::xlab(input$xlabel)
        }
        
        if (!is.null(input$ylabel)) {
          time_plot_object <- time_plot_object + ggplot2::ylab(input$ylabel)
        }
        time_plotGenerated(TRUE)
        return(time_plot_object)
      }) # closes output$plot
    }) # closes  observe()
    ##
    output$saveBtn <- renderUI({ ##
      if (!time_plotGenerated()) {
        return(NULL)
      } else {
        downloadButton("saveBtn", "Save Image")
        downloadHandler(
          filename = function() {
            paste("time_plot.png")
          },
          content = function(file) {
            time_plot_object <- get_time_plot_inputs(
              superimpose.data = input$superimpose.data,
              predict.ribbon = input$predict.ribbon,
              xmin = input$xmin,
              xmax = input$xmax,
              prediction_length = input$prediction_length,
              add_ranef = input$add_ranef_plot,
              categorical_var = input$mixed_mod_var,
              ci_level = input$ci_level,
              cc_obj = cc_obj
            )
            
            # Add x and y labels to the plot
            if (!is.null(input$xlabel)) {
              time_plot_object <- time_plot_object + ggplot2::xlab(input$xlabel)
            }
            
            if (!is.null(input$ylabel)) {
              time_plot_object <- time_plot_object + ggplot2::ylab(input$ylabel)
            }
            
            # Get the actual width and height of the rendered plot
            width_px <- session$clientData$output_plot_width
            height_px <- session$clientData$output_plot_height
            
            ggplot2::ggsave(file,
                            plot = time_plot_object,
                            width = width_px * 5,
                            height = height_px * 5,
                            units = "px"
            )
          }
        )
      }
    }) ##
    ##
    
    # present the options for the time plot
    output$plot_toggles <- renderUI({
      if (!time_plotGenerated()) {
        return(NULL)
      }
      # get the default domain of the time plot
      xmin <- round(min(cc_obj$newdata[cc_obj$time_name]), digits = 5)
      xmax <- round(max(cc_obj$newdata[cc_obj$time_name]), digits = 5)
      # get the default predicted length out (ie, number of datapoints in the fitted model)
      default_pred.length.out <- get_pred_length_out(cc_obj)
      
      # plot options:
      plot_toggles_list <- list(
        checkboxInput("superimpose.data", "Overlay data", FALSE),
        checkboxInput("predict.ribbon", "Show prediction interval", FALSE),
        numericInput("xmin", label = "x-min", value = xmin, width = "250px"),
        numericInput("xmax", label = "x-max", value = xmax, width = "250px"),
        numericInput("prediction_length", label = "Prediction length:", value = default_pred.length.out, width = "250px"),
        textInput("xlabel", "X-axis label:", value = input$time),
        textInput("ylabel", "Y-axis label:", value = input$outcome)
      )
      plot_toggles_list
    })
    
    # if there is a mixed model, allow user to plot each distinct random effect
    output$plot_variables_ranef <- renderUI({
      if (input$add_ranef) {
        checkboxGroupInput("add_ranef_plot", "", "Plot distinct random effects")
      } else {
        return(NULL)
      }
    })
    
    # reactive values that update when the time plot inputs update
    detect_superimpose.data <- reactiveVal(input$superimpose.data)
    detect_predict.ribbon <- reactiveVal(input$predict.ribbon)
    detect_xmin <- reactiveVal(input$xmin)
    detect_xmax <- reactiveVal(input$xmax)
    detect_prediction_length <- reactiveVal(input$prediction_length)
    detect_add_ranef_plot <- reactiveVal(input$add_ranef_plot)
    detect_xlabel <- reactiveVal(input$xlabel)
    detect_ylabel <- reactiveVal(input$ylabel)
    
    
    # polar plot
    # Update plot based on user interaction
    polar_plot_index <- reactiveVal(1)
    polar_plotGenerated <- reactiveVal(FALSE)
    
    # if more than one polar plot is generated, allow user to cycle between them
    output$polar_plot_selector <- renderUI({
      if (!polar_plotGenerated() || input$component_num == 1) {
        return(NULL)
      }
      tagList(
        actionButton("prevButton", "Previous", icon("arrow-left"), style = "display: inline-block; margin-left: 15px;"),
        actionButton("nextButton", "Next", icon("arrow-right"), style = "display: inline-block;")
      )
    })
    
    observeEvent(input$nextButton, {
      polar_plot_index(min(polar_plot_index() + 1, input$component_num))
    })
    
    observeEvent(input$prevButton, {
      polar_plot_index(max(polar_plot_index() - 1, 1))
    })
    
    observe({
      # these correspond to reactive inputs. If any of them change, the plots will
      # be generated again to reflect updated inputs
      polar_plot_index()
      polar_plot_toggle()
      polar_plot_overlay_parameter_info()
      polar_plot_ellipse_opacity()
      polar_plot_clockwise()
      polar_plot_n_breaks()
      polar_plot_grid_angle_segments()
      polar_plot_radial_units()
      polar_plot_start()
      polar_plot_text_size()
      polar_plot_text_opacity()
      
      
      output$polar_plot <- renderPlot({
        # generate the polar plot(s)
        polar_plot_object <- get_polar_plot_inputs(
          cc_obj = cc_obj,
          component_num = input$component_num,
          component_index = polar_plot_index(),
          polar_plot_view_toggle = input$polar_plot_view_toggle,
          overlay_parameter_info = input$overlay_parameter_info,
          ellipse_opacity = input$ellipse_opacity,
          clockwise = input$clockwise,
          ci_level = input$ci_level,
          n_breaks = input$n_breaks,
          grid_angle_segments = input$grid_angle_segments,
          radial_units = input$radial_units,
          start = input$start,
          text_size = input$text_size,
          text_opacity = input$text_opacity
        )
        polar_plotGenerated(TRUE)
        return(polar_plot_object)
      })
    })
    
    # save the polar plot
    output$saveBtn_polar_plot <- renderUI({ ##
      if (!polar_plotGenerated()) {
        return(NULL)
      } else {
        downloadButton("saveBtn_polar_plot", "Save Image")
        downloadHandler(
          filename = function() {
            paste("polar_plot.png")
          },
          content = function(file) {
            plot_obj <- get_polar_plot_inputs(
              cc_obj = cc_obj,
              component_num = input$component_num,
              component_index = polar_plot_index(),
              polar_plot_view_toggle = input$polar_plot_view_toggle,
              overlay_parameter_info = input$overlay_parameter_info,
              ellipse_opacity = input$ellipse_opacity,
              clockwise = input$clockwise,
              ci_level = input$ci_level,
              n_breaks = input$n_breaks,
              grid_angle_segments = input$grid_angle_segments,
              radial_units = input$radial_units,
              start = input$start,
              text_size = input$text_size,
              text_opacity = input$text_opacity
            )
            
            # Get the actual width and height of the rendered plot
            width_px <- session$clientData$output_polar_plot_width
            height_px <- session$clientData$output_polar_plot_height
            
            ggplot2::ggsave(file,
                            plot = plot_obj,
                            width = width_px * 5,
                            height = height_px * 5,
                            units = "px"
            )
          }
        )
      }
    }) ##
    ##
    
    
    
    # present the polar plot options
    output$polar_plot_toggles <- renderUI({
      if (!polar_plotGenerated()) {
        return(NULL)
      }
      
      outputs <- list(
        selectInput(
          "polar_plot_view_toggle", "Select view:",
          c("full", "zoom", "zoom_origin")
        ),
        selectInput("radial_units", "Select the angular units:", c("radians", "degrees", "period")),
        checkboxInput("overlay_parameter_info", "Overlay parameter information", FALSE),
        checkboxInput("clockwise", "Clockwise:", FALSE),
        sliderInput("text_size", "Select the text size:", value = 3, min = 0, max = 20),
        sliderInput("text_opacity", "Select the text opacity:", value = 0.5, min = 0, max = 1),
        sliderInput("ellipse_opacity", "Confidence Ellipse opacity:", value = 0.3, min = 0, max = 1),
        numericInput("n_breaks", "Number of concentric circles:", value = 5, min = 0, max = NA, step = 1),
        numericInput("grid_angle_segments", "Number of angle labels:", value = 8, min = 0, max = NA, step = 1),
        selectInput("start", "Select the location of the starting angle:", c("right", "left", "top", "bottom"))
      )
      outputs
    })
    
    # polar plot reactive inputs. These update if the polar plot inputs update
    polar_plot_toggle <- reactiveVal(input$polar_plot_toggles)
    polar_plot_overlay_parameter_info <- reactiveVal(input$overlay_parameter_info)
    polar_plot_ellipse_opacity <- reactiveVal(input$ellipse_opacity)
    polar_plot_clockwise <- reactiveVal(input$clockwise)
    polar_plot_n_breaks <- reactiveVal(input$n_breaks)
    polar_plot_grid_angle_segments <- reactiveVal(input$grid_angle_segments)
    polar_plot_radial_units <- reactiveVal(input$radial_units)
    polar_plot_start <- reactiveVal(input$start)
    polar_plot_text_size <- reactiveVal(input$text_size)
    polar_plot_text_opacity <- reactiveVal(input$text_opacity)
    
    # store the confidence interval as a reactive value that will be used across the app
    # to prevent errors, there are various safeguards to ensure that there is always
    # a 'valid' confidence interval
    ci_level <- reactiveVal({
      if (is.null(input$ci_level)) {
        ci_level <- 0.95
      } else {
        ci_level <- input$ci_level
        if (is.na(ci_level)) {
          ci_level <- 0.95
        }
        if (ci_level <= 0) {
          ci_level <- 0
        }
        if (ci_level >= 1) {
          ci_level <- 0
        }
      }
      return(ci_level)
    })
  })
  
  # save images
}

# Run the application
shinyApp(ui = ui, server = server)
