library(shiny)
source("setup.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Comparability database"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("indicator",
                        "Select indicator",
                        indicator_selection,
                        selected = "gini"
                        ),
            selectInput("country_list",
                        "Select countries",
                        country_selection,
                        selected = c("COL", "THA", "GHA"),
                        multiple = TRUE,
                        selectize = TRUE),
            sliderInput("year_range",
                        "Select years",
                        min = 1981,
                        max = 2020,
                        value = c(1990, 2020)),
            selectInput("datatype",
                        "Select type of data",
                        datatype_selection,
                        multiple = TRUE,
                        selected = c("Consumption", "Income"),
                        selectize = TRUE),
            selectInput("coverage",
                        "Select type of coverage",
                        coverage_selection,
                        multiple = TRUE,
                        selected = c("National", "Urban", "Rural", "National (Aggregate)"),
                        selectize = TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("line_chart", height = "600px")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$line_chart <- renderPlotly({
        # generate bins based on input$bins from ui.R
        tmp <- df %>%
            filter(countrycode %in% input$country_list,
                   year %in% input$year_range[1]:input$year_range[2],
                   datatype %in% input$datatype,
                   coveragetype %in% input$coverage)
        tmp <- tmp[, c("countryname", "year", "legend_keys", input$indicator)]
        colnames(tmp) <- c("countryname", "year", "legend_keys", "value")



        p <- ggplot(tmp, aes(x = year, y = value, color = countryname)) +
            geom_line(aes(linetype = legend_keys), size = rel(.8)) +
            #scale_y_continuous(limits = c(35, 55), breaks = c(35, 40, 45, 50, 55)) +
            #scale_x_continuous(breaks = c(1990, 2000, 2010, 2020)) +
            geom_point(size = rel(2), aes(shape = countryname)) +
            scale_color_colorblind() +
            scale_linetype_discrete() +
            # guides(colour = FALSE,
            #        shape = FALSE,
            #        linetype = guide_legend(override.aes = list(colour = c("black", "black", "#E69F00", "#56B4E9", "#56B4E9", "#56B4E9")))) +
            labs(
                x = "Year",
                y = input$indicator
            ) +
            theme_clean() +
            theme(
                legend.position = "none",
                legend.title = element_blank(),
                legend.background = element_rect(linetype = c("blank"))
            )

        ggplotly(p, tooltip = c("countryname", "value", "year", "legend_keys"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
