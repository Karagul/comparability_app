library(shiny)
source("setup.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinythemes::shinytheme("flatly"),

    # Application title
    titlePanel("Exploring comparable trends in poverty and inequality indicators"),
    uiOutput("paragraph"),
    
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
                        selectize = TRUE),
            sliderInput("year_range",
                        "Select years",
                        min = 1981,
                        max = 2020,
                        value = c(1990, 2020))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("line_chart", height = "600px"),
           plotlyOutput("legend", height = "300px")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$paragraph <- renderUI({
        tagList(
            p("As countries frequently improve household surveys and measurement methodologies,
      strict comparability of poverty estimates over time is often limited. 
      This app allows you to visualize when those methodological changes took place.",
      br(), "Read more on the comparability metadata database and methodologie in ", 
      url_paper, " and ", url_blog, ". Data sources for this application can be accessed here:
       ", url_povcal, " and ", url_data, ".", br()
            )
        )
    })
    
    # generate bins based on input$bins from ui.R
    out <- reactive({
        out <- df %>%
            filter(countrycode %in% input$country_list,
                   year %in% input$year_range[1]:input$year_range[2],
                   datatype %in% input$datatype,
                   coveragetype %in% input$coverage) %>%
          arrange(legend_keys)
        out <- out[, c("countryname", "year", "legend_keys", "datatype", input$indicator)]
        colnames(out) <- c("countryname", "year", "legend_keys", "datatype", "value")
        
        pad_length <- max(stringr::str_length(out$legend_keys))
        out$legend_keys <- stringr::str_pad(out$legend_keys, width = pad_length, 
                                            side = "right", pad = " ")
        
        return(out)
    })

    output$line_chart <- renderPlotly({
        
        selected_indicator <- names(indicator_selection)[indicator_selection == input$indicator]

        p <- ggplot(out(), aes(x = year, y = value, color = countryname)) +
            geom_line(aes(linetype = interaction(countryname, legend_keys)), size = rel(.8)) +
            geom_point(size = rel(2), aes(shape = datatype,
                                          text = paste("Country:", countryname,
                                                       "<br />Value:", value,
                                                       "<br />Year:", year,
                                                       "<br />Data type:", datatype))) +
            scale_color_colorblind() +
            scale_linetype_discrete() +
            labs(
                #title = paste0("Comparability over time of ", selected_indicator),
                #subtitle = "Each breack in a series represents a methodological break",
                x = "",
                y = selected_indicator
            ) +
            theme_clean() +
            theme(
                legend.position = "none",
                text = element_text(family = "Calibri")
            )

        ggplotly(p, tooltip = "text") %>%
          style(hoveron = "color") %>%
          layout(title = list(text = paste0("Comparability over time of ", 
                                            selected_indicator,
                                            '<br>',
                                            '<sup>',
                                            'Each breack in a series represents a significant methodological change that may affect comparability over time.',
                                            '</sup>'),
                              xanchor = "left",
                              x = 0)
          )
    })
    
    output$legend <- renderPlotly({
        p <- ggplot(out(), aes(x = legend_keys, color = countryname)) +
            geom_linerange(ymin = 0, ymax = 1, size = rel(.8),
                           aes(linetype = interaction(countryname, legend_keys))) +
            geom_point(aes(shape = datatype), y = .5, size = rel(2)) + 
            geom_text(aes(label = legend_keys), y = 1.7, hjust = "left") +
            scale_y_continuous(limits = c(0,5)) +
            scale_color_colorblind() +
            coord_flip() +
          # facet_wrap(~legend_break, nrow = 1, scales = "free") +
            theme(
                legend.position = "none",
                panel.grid = element_blank(),
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                panel.background = element_blank(),
                text = element_text(family = "Calibri")
            )
        
        ggplotly(p, tooltip = NULL)

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
