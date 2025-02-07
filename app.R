library(shiny)
library(bslib)
library(DT)
library(MonotonicityTest)
library(tidyverse)
library(shinybusy)
library(shinyjs)

ui <- page_sidebar(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .info-btn {
        background-color: #0099E5;
        color: white;
        border: none;
        position: absolute;
        top: -10px;
        right: 0;
      }
      .info-btn:hover {
        background-color: #0077cc;
      }
      .disabled-input {
        opacity: 0.5;  /* Grey out the inputs */
        pointer-events: none;  /* Disable interaction */
      }
    "))
  ),
  title = "Monotonicity Testing Demo",

  sidebar = sidebar(
    div(style = "position: relative;",
        actionButton("info_btn", icon("info-circle"), class = "info-btn"),
        fileInput("file", "Upload CSV",
                  buttonLabel = "Browse...",
                  accept = c(".csv"))
    ),
    div(id = "inputs-container", class="disabled-input",
        selectInput("x_col", "Select X Column", choices = NULL),
        selectInput("y_col", "Select Y Column", choices = NULL),
        sliderInput("m", "Window Size (m):",
                    min = 1, max = 100, value = 25, step = 1),
        sliderInput("bandwidth", "Bandwidth:",
                    min = 0.01, max = 10, value = 1, step = 0.01),
        sliderInput("boot_num", "Number of Bootstraps:",
                    min = 1, max = 100, value = 25, step = 1),
        checkboxInput("negative", "Test for Decreasing Monotonicity", value = FALSE),
        actionButton("run_test", "Run Monotonicity Test", class = "btn-primary")
    )
  ),
  # loading spinner in test window
  # add_busy_spinner(
    # spin = "pixel",
    # position = "top-right",
    # margins = c(140, 70),
    # height = "40px",
    # width = "40px",
    # color = "#086ca4"
  # ),

  add_busy_bar(color = "#086ca4"),

  # left column with data and kernel plot
  layout_columns(
    col_widths = c(6, 6),
    # make div full height
    div(
      style = "height: calc(100vh - 1rem - 50px); display: flex; flex-direction: column;",
      card(
        card_header("Data Preview"),
        style = "flex: 0 0 auto; margin-bottom: 1rem;",
        DTOutput(outputId = "headData"),
        # data will be small
        height = "25%"
      ),
      card(
        style = "flex: 1; display: flex; flex-direction: column; min-height: 0;",
        card_header("Kernel Plot"),
        div(
          style = "flex: 1; min-height: 0;",
          # this should take rest of height
          plotOutput(outputId = "kernel_plot", height = "100%")
        )
      )
    ),
    div(
      # im not sure why this isn't the same height
      style = "height: calc(100vh - 1rem - 75px);",
      card(
        height = "100%",
        style = "display: flex; flex-direction: column;",
        card_header("Monotonicity Test Results"),
        div(
          style = "flex: 1; display: flex; flex-direction: column; padding: 1rem; gap: 1rem;",
          # Results text at top
          div(
            style = "flex: 0 0 auto;",  # Takes only needed space
            textOutput("test_results")
          ),
          # Plot container
          div(
            style = "flex: 1; display: flex; align-items: center;",
            plotOutput("test_plot", width = "100%", height = "500px")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # read data and drop na
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath) %>%
      drop_na() %>%
      select(where(is.numeric)) # select only numeric columns

    if (nrow(df) > 2000) {
      df <- df %>% sample_n(2000)
    }

    req(ncol(df) >= 2)
    return(df)
  })


  observe({
    if (!is.null(data())) {
      shinyjs::removeClass(id="inputs-container", class="disabled-input")  # Enable inputs
    } else {
      shinyjs::addClass("inputs-container", "disabled-input")  # Disable inputs
    }
  })

  # Update choices for X and Y columns when data is uploaded
  observe({
    req(data())
    updateSelectInput(session, "x_col", choices = names(data()))
    updateSelectInput(session, "y_col", choices = names(data()))
  })

  # Update slider for window size (m) when data is uploaded
  observe({
    req(data())
    max_m <- nrow(data())
    updateSliderInput(session, "m", max = max_m, value = floor(max_m * 0.25))
  })

  # Update slider for bandwidth when X column is selected
  observe({
    req(data(), input$x_col)  # Ensure data and x_col are available
    X <- data()[[input$x_col]]
    x_range <- range(X, na.rm = TRUE)
    default <- bw.nrd(X) * (length(X) ^ -0.1)
    updateSliderInput(session, "bandwidth", min = 0.005, max = default * 20, value = default)
  })

  # modal for when info button is pressed
  observeEvent(input$info_btn, {
    showModal(modalDialog(
      title = "Data Cleaning Information",
      HTML("<ul>
             <li>All NA values are automatically removed.</li>
             <li>If your data contains more than 2000 rows, a random 2000 with be selected.</li>
             <li>At least 2 columns are required.</li>
             <li>All non-numeric columns will be dropped.</li>
           </ul>"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  # show head of data nicely
  output$headData <- renderDT({
    req(data())
    datatable(
      head(data(), 5),
      options = list(
        pageLength = 5,
        scrollX = TRUE,
        dom = 't',
        ordering = FALSE,
        scrollY = FALSE
      ),
      class = "cell-border stripe compact",
      rownames = FALSE
    )
  })


  # do the test when button is clicked
  test_results <- eventReactive(input$run_test, {

    req(data(), input$x_col, input$y_col)
    df <- data()
    X <- df[[input$x_col]]
    Y <- df[[input$y_col]]

    result <- monotonicity_test(
      X = X,
      Y = Y,
      bandwidth = input$bandwidth,
      boot_num = input$boot_num,
      m = input$m,
      negative = input$negative
    )
    return(result)
  })

  # show test stats
  output$test_results <- renderText({
    req(test_results())
    result <- test_results()
    paste0(
      "Test Statistic (Tm): ", sprintf("%.4f", result$stat), "\n",
      "P-value: ", sprintf("%.4f", result$p), "\n",
      "Critical Interval: ", paste(sprintf("%d", result$interval), collapse = " ")
    )
  })

  # show test plot
  output$test_plot <- renderPlot({
    req(test_results())
    result <- test_results()
    par(mar = c(4, 4, 2, 1))
    result$plot
  }, height = function() {
    session$clientData$output_kernel_plot_width * 0.7
  })

  # show kernel plot
  output$kernel_plot <- renderPlot({
    req(data(), input$x_col, input$y_col)
    df <- data()
    X <- df[[input$x_col]]
    Y <- df[[input$y_col]]
    par(mar = c(4, 4, 2, 1))
    create_kernel_plot(X, Y, bandwidth = input$bandwidth)
  }, height = function() {
    session$clientData$output_kernel_plot_width * 0.7
  })
}

shinyApp(ui = ui, server = server)
