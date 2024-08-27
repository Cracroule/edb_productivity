# Load required libraries
library(shiny)
library(ggplot2)

# simple function to make plot fct of i
make_plot <- function(i) {
  ggplot(mtcars, aes(x = wt + i, y = mpg)) +
    geom_point() +
    ggtitle(paste("Plot", i))
}

# option 1: Works!!
l_plots <- lapply(1:8, function(i) {
  ggplot(mtcars, aes(x = wt + i, y = mpg)) + 
    geom_point() +
    ggtitle(paste("Plot", i))
})
names(l_plots) <- 1:8

# option 2: interesting fail!
l_plots <- list()
for (i in 1:8) {
  l_plots[[i]] <- local({
    i <- i
    ggplot(mtcars, aes(x = wt + i, y = mpg)) +
    geom_point() +
    ggtitle(paste("Plot", i))
  })
}
names(l_plots) <- 1:8

# option 3: fail!
l_plots <- list()
for (i in 1:8) {
  l_plots[[i]] <- ggplot(mtcars, aes(x = wt + i, y = mpg)) +
      geom_point() +
      ggtitle(paste("Plot", i))
}
names(l_plots) <- 1:8



# Define the UI
ui <- fluidPage(
  titlePanel("My Shiny App"),
  fluidRow(
    column(12,
           h3("Subtitle: Displaying 8 Plots in a 2x4 Grid")
    )
  ),
  fluidRow(
    # Generate the 2x4 grid of plots
    lapply(names(l_plots), function(i) {
      column(3,
             plotOutput(paste0("plot_", i))
      )
    })
  )
)

# Define the server logic
server <- function(input, output, session) {

    # Create output for each plot
  lapply(names(l_plots), function(i) {
    output[[paste0("plot_", i)]] <- renderPlot({
      l_plots[[i]]
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
