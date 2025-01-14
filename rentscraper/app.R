#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(forcats)
library(dplyr)
library(reshape2)
library(stringr)
library(ggplot2)
library(data.table)
library(lubridate)
library(zoo)
library(leaflet)
library(rvest)
library(stats)
library(tidygeocoder)
library(tidyverse)
library(GGally)
library(ggmap)
library(plotly)

library(shiny)

# Global functions
rent_scrape <- function(state, postcode, suburb, beds, baths, pages, unit_type) {
  # prepare config for the URL scraping
  # TODO: make this work with carspaces too
  prop_config <- c(beds, baths)
  # Number of pages to search through
  props_to_pull <- pages

  # Create a list of URLs to scrape through
  urls <- paste0(
    'https://www.auhouseprices.com/rent/list/',
    state, "/",
    postcode, "/",
    gsub("\\s+", "+", suburb), "/",
    1:props_to_pull,
    '/?sort=date&type=',
    unit_type,
    '&bmin=',
    prop_config[1],
    '&bmax=',
    prop_config[1]
  )

  # Use lapply() to scrape each URL in urls and combine the results into a single data table
  rent_all <- rbindlist(lapply(urls, function(url) {
    message(paste0('getting ', url))
    Sys.sleep(2)
    temp <- read_html(url)
    address <- temp %>%
      html_nodes('h4') %>%
      html_text() %>%
      .[which(. != ' Search Filter and Sorting ')]
    price_month <- temp %>%
      html_nodes('li') %>%
      html_text() %>%
      str_extract('^Rent.+/week.*\\d{4}$') %>%
      .[which(!is.na(.))]
    config <- temp %>%
      html_nodes('li') %>%
      html_text() %>%
      str_extract(' \\d \\d \\d*[ ]*$') %>%
      .[which(!is.na(.))]

    combined <- data.table(address, price_month, config)
    return(combined)
  }))

  # extract month
  rent_all$month <- str_extract(rent_all$price_month, '[A-Z][a-z]{2} \\d{4}$')
  rent_all$month <- dmy(paste0('01 ', rent_all$month))

  # extract price
  price <- str_extract(rent_all$price_month, '(?<=Rent \\$).*(?=/week)')
  rent_all$price <- as.numeric(ifelse(grepl("^\\d*\\.?\\d+$", price), price, NA))

  # remove any dups
  rent_all <- rent_all[!duplicated(rent_all)]

  # subset to view only those matching property configuration specified above
  pattern <- paste0(prop_config[[1]], '\\s', prop_config[[2]])

  # create our analytical dataset
  ads <- rent_all[grepl(pattern, rent_all$config), ]

  return(ads)
}


# Define UI for application that draws a histogram
# Define the Shiny app UI
ui <- fluidPage(
  titlePanel("Rent Scraper"),
  sidebarLayout(
    sidebarPanel(
      h2("Location"),
      selectInput("state", "State:", choices = c("NSW","VIC","QLD","SA","TAS","WA","ACT")),
      textInput("postcode", "Postcode:", "2088"),
      textInput("suburb", "Suburb:", "Mosman"),
      h2("Unit Details"),
      selectInput("unit_type", "Unit Type:", choices = c("apartment","house")),
      numericInput("beds", "Beds:", 2, min = 1, max = 5),
      numericInput("baths", "Baths:", 1, min = 1, max = 5),
      numericInput("pages", "Pages to Scrape:", 3, min = 1, max = 100),
      h2("Rent Details (price per week)"),
      h4("Current rent is shown on plots in green, proposed rent in red."),
      numericInput("current_rent", "Current Rent", 0, min = 0),
      numericInput("new_rent", "Proposed Rent", 0, min = 0),
      actionButton("scrape", "Scrape Data"),
      downloadButton("download_csv", "Export CSV")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Visualisations",
                 plotlyOutput("boxplots"),
                 plotlyOutput("histogram")
                 ),
        tabPanel("Data", DT::dataTableOutput("results")),
        tabPanel("Map",
                 h4("NOTE: this feature is still under development and will take some time to load."),
                 leafletOutput("map"))
      )
    )
  )
)


# Define server
server <- function(input, output) {
  # Define reactive function to scrape data
  scraped_data <- reactive({
    req(input$scrape)
    isolate(rent_scrape(input$state,
                input$postcode,
                input$suburb,
                input$beds,
                input$baths,
                input$pages,
                input$unit_type))
  })

  # Define output for table
  output$results <- DT::renderDataTable(scraped_data())

                                          # FIXME: Attempt to make property details more clear to users
                                          # TODO: Should probably fix on server side rather than UI
                                          # drop_na() %>%
                                          # separate_wider_delim(config, names = c("bedrooms", "bathrooms", "carspaces"), delim = " ", too_few = "align_start", too_many = "drop") %>%
                                          # # Convert the columns to numeric
                                          # mutate(across(c(bedrooms, bathrooms, carspaces), as.numeric)) %>%
                                          # dplyr::select(address, price_month, month, price, bedrooms, bathrooms, carspaces)

  output$boxplots <- renderPlotly({

    scraped_data() %>%
      drop_na() %>%
      mutate(month = reorder(
        factor(format(month, '%b %Y')), as.numeric(interaction(month(month), year(month)))
      )) %>%
      ggplot(aes(x = month, y = price)) +
      geom_boxplot() +
      geom_jitter(alpha = 0.2) +
      geom_hline(yintercept = input$current_rent, color = "dark green") +
      geom_hline(yintercept = input$new_rent, color = "red") +
      #coord_flip() +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      labs(
        x = 'Month rented',
        y = 'Weekly rent',
        title = paste0('Distribution of weekly rent in ',input$suburb, ", ", input$state),
        subtitle = paste0(input$beds, " bedrooms", input$baths, " bathrooms")
      )
  })

  # Generate the histogram
  output$histogram <- renderPlotly({
    scraped_data() %>%
      drop_na() %>%
      filter(month >= Sys.Date() - months(3)) %>%
      ggplot(aes(x = price)) +
      geom_histogram(binwidth = 50, fill = "lightblue", color = "black") +
      geom_vline(xintercept = input$current_rent, color = "dark green") +
      geom_vline(xintercept = input$new_rent, color = "red") +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      labs(
        x = 'Weekly rent',
        y = 'Count',
        title = 'Distribution of Weekly Rents in the Last 3 Months'
      )
  })

  # Generate the map
  output$map <- renderLeaflet({
    # Create a leaflet map object
    map <- leaflet() %>%
      addTiles()  # Add the default map tiles

    # Geocode addresses and add markers to the map
    data <- scraped_data()
    if (!is.null(data) && nrow(data) > 0) {
      geocoded_data <- tidygeocoder::geocode(data, address = "address", method = "osm")

      # Add markers with rent and configuration information
      for (i in seq_along(geocoded_data$lat)) {
        address <- data$address[i]
        rent <- data$price[i]
        config <- data$config[i]

        map <- map %>%
          addMarkers(
            lat = geocoded_data$lat[i],
            lng = geocoded_data$long[i],
            popup = paste("Address:", address, "<br>",
                          "Rent:", rent, "<br>",
                          "Configuration:", config)
          )
      }
    }

    map  # Return the modified map object
  })
  # Add a reactive value to store the scraped data
  scraped_data_df <- reactiveVal()

  # Update the reactive value when the "Scrape Data" button is clicked
  observeEvent(input$scrape, {
    scraped_data_df(scraped_data())
  })

  # Define the download handler for the CSV file
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("rent_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Save the data to a CSV file
      write.csv(scraped_data_df(), file, row.names = FALSE)
    }
  )

}
# Run the application
shinyApp(ui = ui, server = server)
