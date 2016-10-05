library(shiny);

# Initialise
d <- read.csv("summary_of_results_2015_app_round_160322.csv",
              stringsAsFactors = FALSE);

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(

  # Application title
    headerPanel("Visualisation of 2015 NHMRC grant results"),
    
  # Sidebar with a slider input for number of observations
    sidebarPanel(
        
        conditionalPanel(
            condition = "input.conditionedPanels != 'Number of publications'",
            selectInput("grantType",
                    "Select grant type",
                    choices = c("All", sort(unique(d$Grant.Type))),
                    selected = "Project Grants")),
        conditionalPanel(
            condition = "input.conditionedPanels != 'Number of publications' & 
                         input.conditionedPanels != 'Wordcloud of grant descriptions' &
                         input.conditionedPanels != 'Funding money'",
            checkboxInput("asPercentage",
                          "Show values as percentage",
                          value = FALSE)),
        conditionalPanel(
            condition = "input.conditionedPanels != 'Wordcloud of grant descriptions' & 
                         input.conditionedPanels != 'Funding money'",
            numericInput("minFreq",
                     "Minimum frequency cutoff",
                     value = 0)),
        checkboxInput("extendedOptions",
                      "Show additional options",
                      value = FALSE),
        conditionalPanel(
            condition = "input.extendedOptions == 1",
            h4("Plotting options"),
            numericInput("canvasHeight",
                         "Height of drawing canvas [in px]",
                         value = 960),
            numericInput("fontLabel",
                         "Labels",
                         value = 14),
            numericInput("fontAxes",
                         "Axes",
                         value = 14),
            h4("Additional settings"),
            checkboxInput("missingPubsAsZero",
                          "Include missing publications as zeros",
                          value = FALSE)),
        br(),
        div(tags$div(class = "header",
                     checked = NA,
                     tags$p("Visualisation with Shiny. In case of questions contact"),
                     tags$a(href = "mailto:maurits.evers@anu.edu.au", "Maurits Evers")),
            style = "font-size:75%"),
        width = 4
        ),

  # Show a plot of the generated distribution
    mainPanel(
        tabsetPanel(
            tabPanel("Institution",
                     uiOutput("institutionPlotUI")
                     ),
            tabPanel("Gender",
                     uiOutput("genderPlotUI")
                     ),
            tabPanel("Keywords",
                     uiOutput("keywordPlotUI")
                     ),
            tabPanel("Publication journal overview",
                     uiOutput("publicationPlotUI")
                     ),
            tabPanel("Number of publications",
                     uiOutput("publicationsPerGrantPlotUI")
                     ),
            tabPanel("Wordcloud of grant descriptions",
                     uiOutput("wordcloudPlotUI")),
            tabPanel("Funding money",
                     uiOutput("moneyPlotUI")),
            id = "conditionedPanels"
            ),
        width = 8
        )
    ))
