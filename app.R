library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(data.table)
library(DT)

library(loomtaRt)

source("R/utils.R")

ui <- page_fluid(
  card(
    card_header("Loom Weight and Warp Tension: Controls"),
    min_height = "200px",
    layout_columns(
      column(
        width = 12,
        sliderInput("warpTension", "Warp Tension (g)",
                    min = 1, max = 500,
                    value = 30,
                    width = "100%")
      ),
      column(
        width = 12,
        span("t.b.a.")
      )
    )
  ),
  card(
    card_header("Plot"),
    plotlyOutput("weightByAssessment")
  ),
  card(
    card_header("Data"),
    min_height = "500px",
    DTOutput("histogramTable")
  )
)

server <- function(input, output) {
  raw_data <- reactiveVal(value = read.csv("data/demo_data.csv"))

  plot_data <- reactiveVal()

  observeEvent(input$warpTension, {
    tmp <- raw_data()
    tmp$tplw <- get_threads_per_loom_weight(weight = tmp$weight, tension = input$warpTension)

    tmp$assessment <- assess_threads_per_loom(thread_count = tmp$tplw)
    tmp$assessment <- factor(tmp$assessment,
                             levels = c("impossible", "unlikely", "possible", "optimal"),
                             ordered = TRUE)
    plot_data(tmp)
  })

  output$weightByAssessment <- renderPlotly({
    plot_ly(plot_data(), x = ~weight,
            color = ~assessment,
            customdata = ~assessment,
            type = "histogram",
            xbins = list(size = 10),
            source = "weightHistogram") %>%
      layout(barmode = "stack")

  })

  output$histogramTable <- renderDT({
    click_data <- event_data("plotly_click", source = "weightHistogram")

    if (is.null(click_data)) {
      return(data.frame(Message = "Click a bin"))
    }

    # clicked_x is the bin center, customdata is the assessment
    bin_width <- 10
    plot_data() |>
      filter(
        assessment == click_data$customdata,
        weight >= (click_data$x - bin_width/2),
        weight < (click_data$x + bin_width/2)
      ) |>
      mutate(tplw = round(tplw, digits = 2))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
