library(shiny)

shinyUI(
    pageWithSidebar(
        # Application title
        headerPanel("BMI Calculator"),
        sidebarPanel(
            helpText("Enter your height and weight in either meters and kg (metric) or feet, inces and lb (english)"),
            radioButtons("units", label = h5("Measure Units"),
                         choices = list("Metric" = 1, "English" = 703),selected = 1),
            conditionalPanel(
                condition = "input.units == 1",
                numericInput('heightM', 'Height (m)', 1.70, min = 1),
                numericInput('weightM', 'Weight (kg)', 60, min = 40)),
            conditionalPanel(
                condition = "input.units == 703",
                numericInput('heightFT', 'Height (ft)', 5, min = 4),
                numericInput('heightIN', 'Height (in)', 7, min = 0),
                numericInput('weightE', 'Weight (lb)', 130, min = 80))
        ),
        mainPanel(
            uiOutput("userDetails"),
            uiOutput("bmiResult"),
            plotOutput('plot1')
        )
    )
)