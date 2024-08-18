# Load necessary libraries
library(data.table)
library(shiny)
library(shinydashboard)
library(ggplot2)
options(scipen=999)


source("R/00s_shiny_utils.R")

# load input data
dt <- fread(file.path("data", "extracted_data.csv"))

# split what's considered inputs and outputs
all_inputs <- c("opex", "opex_real", "flow_capital_services", "flow_capital_services_real",
                "annual_charge", "annual_charge_real", "rab_open", "rab_close", "depreciation", "revenue", "capex")
all_outputs <- c("nb_connections", "length_circuit", "length_overhead",
                 "length_underground", "mva_circuit", "mva_overhead", "mva_underground",
                 "transformers", "energy_delivered", "max_demand", "saidi_unplanned_norm")

# aggregate the data to help dynamic filtering
dt_industry <- aggregate_data_by(dt, by="disc_yr") # implies: group edbs
dt_industry_status <- aggregate_data_by(dt, by=c("disc_yr", "status")) # implies: group edb/status
dt_industry[, `:=`(status="All", edb="All")]
dt_industry_status[status == "Exempt", edb:="All - Exempt"]
dt_industry_status[status == "NonExempt", edb:="All - Non-Exempt"]
dt_display <- rbindlist(list(dt, dt_industry, dt_industry_status), use.names = T)


# the below uses many r tricks, but the idea is that for 2 datasets:
# we display some systematic content, i.e.:
# - we create one menu for each dataset
# - we create the below submenus for each dataset
# - we create a plot (some content) for each submenu
# - we make this whole thing work!
cars_fts <- c("mpg", "cyl", "disp", "hp")
arrest_fts <- c("Assault", "Murder", "Rape")


# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "EDB Productivity"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "Introduction", icon=icon("indent")),
      menuItem("Data", tabName= "Data", icon = icon("database"), 
               lapply(c("Inputs", "Outputs"), function(ft) {menuSubItem(ft, tabName = ft)})
      ),
      # do.call(menuItem, 
      #         list(text="Data", tabName = "Data", icon = icon("database"), 
      #              lapply(c("Inputs", "Outputs"), function(ft) {menuSubItem(ft, tabName = ft)}))
      # ),
      # menuItem("USArrests", tabName = "usarrests", icon = icon("flag"),
      #          menuSubItem("Assault", tabName = "Assault"),
      #          menuSubItem("Murder", tabName = "Murder"),
      #          menuSubItem("Rape", tabName = "Rape"))
      do.call(menuItem, 
              list(text="USArrests", tabName = "usarrests", icon = icon("flag"), 
                   lapply(arrest_fts, function(ft) {menuSubItem(ft, tabName = ft)}))
      )
    )
  ),
 
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "Introduction",
              fluidRow(column(width = 9,  h1("Introduction"))),
              # fluidRow(
              #   column(width = 12, renderImage("www/logo.png")) # Adjust height as needed
              # ),
              fluidRow(
                column(12, h4("More details about the project to come soon :D"))
              ),
      ),
      
      tabItem(tabName = "Inputs",
              fluidRow(column(width = 9,  h1("Inputs")),
                       column(width = 3, selectInput("edb_filter_inputs", "EDB filter", sort(unique(dt_display$edb))))
              ),
              fluidRow(
                column(12, h4("Subtitle: More details about inputs here"))
              ),
              generate_matrix_plots_shiny_display(all_inputs, n_columns = 3)
      ),
      
      tabItem(tabName = "Outputs",
              fluidRow(column(width = 9,  h1("Outputs")),
                       column(width = 3, selectInput("edb_filter_outputs", "EDB filter", sort(unique(dt_display$edb))))
              ),
              fluidRow(
                column(12, h4("Subtitle: More details about Outputs here"))
              ),
              generate_matrix_plots_shiny_display(all_outputs, n_columns = 3)
      ),
      
      tabItem(tabName = "Assault", 
              fluidRow(column(width = 12, plotOutput(paste0("plot_", "Assault"))))),
      tabItem(tabName = "Murder", 
              fluidRow(column(width = 12, plotOutput(paste0("plot_", "Murder"))))),
      tabItem(tabName = "Rape", 
              fluidRow(column(width = 12, plotOutput(paste0("plot_", "Rape")))))
    )
  )
)


# Define server logic
server <- function(input, output) {
  
  # # Create a reactive expression for the Inputs filtered plots
  dynamic_plots_inputs <- reactive({
    generate_visualisation_plots(dt_display, display_variables = all_inputs, filters=list(edb=input$edb_filter_inputs))
  })
  
  # # Create a reactive expression for the Outputs filtered plots
  dynamic_plots_outputs <- reactive({
    generate_visualisation_plots(dt_display, display_variables = all_outputs, filters=list(edb=input$edb_filter_outputs))
  })

  # Create output for each plot based on the Inputs reactive expression
  observe({
    l_plots_inputs <- dynamic_plots_inputs()
    lapply(all_inputs, function(nm) { 
      output[[paste0("plot_", nm)]] <- renderPlot({l_plots_inputs[[nm]]})
    })
  })
  
  # Create output for each plot based on the Outputs reactive expression
  observe({
    l_plots_outputs <- dynamic_plots_outputs()
    lapply(all_outputs, function(nm) {
      output[[paste0("plot_", nm)]] <- renderPlot({l_plots_outputs[[nm]]})
    })
  })
  
  # lapply(all_inputs, function(nm) {
  #   output[[paste0("plot_", nm)]] <- renderPlot({
  #     l_plots[[nm]]
  #   })
  # })

  # lapply(all_outputs, function(nm) {
  #   output[[paste0("plot_", nm)]] <- renderPlot({
  #     l_plots[[nm]]
  #   })
  # })

  
  for (ft in arrest_fts) {
    # Use a closure to capture the value of ft for each iteration
    local({
      my_ft <- ft
      p <-ggplot(USArrests, aes_string(x = my_ft)) +
        geom_histogram(fill = "red", color = "black") +
        labs(title = paste0("Distribution of ", my_ft), x = my_ft, y = "Frequency") +
        theme_minimal()
      print(p)
      print(ft)
      output[[paste0("plot_", my_ft)]] <- renderPlot({
        p
      })
    })
  }
}

# Run the application
shinyApp(ui = ui, server = server)
