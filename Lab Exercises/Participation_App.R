library(tidyverse)
library(shiny)
library(bslib)

d_vars = ess |>
  select("vote":"postonl") %>% 
  names()

names(d_vars) <- c("Vote in national elections", 
                   "Contact politicians",
                   "Member of political party",
                   "Wear political symbols",
                   "Demonstrated",
                   "Boycotted",
                   "Signed petition",
                   "Posted political material online"
)

i_vars = ess |>
  select("agecat", "gender", "educat", "unemp":"quin") |>
  names()

names(i_vars) <- c("Age categorised", 
                   "Gender", 
                   "Education", 
                   "Unemployed",
                   "Trust in politics",
                   "Social trust",
                   "Attitudes to immigrants of different ethnicity",
                   "Satisfaction with the economy",
                   "Satisfaction with government",
                   "Left-right ideology scale",
                   "Income quintile")

ui = fluidPage(
  theme = bslib::bs_theme(bootswatch = "superhero"),
  titlePanel("Participation and Democracy"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "country", "Select a country",
        choices = c("Switzerland", "Germany", "France", "UK", 
                    "Netherlands", "Norway", "Poland", "Sweden")
      ),
      selectInput(
        "var", "Select a form of Participation",
        choices = d_vars, selected = "vote"
      ),
      selectInput("ind", "Select an analysis variable",
                  choices = i_vars, selected = "agecat")
    ),
    mainPanel( 
      plotOutput("plot"),
      tableOutput("table")
    )
  ))

# Define a server for the Shiny app
server <- function(input, output) {
  
  data <- reactive({
    ess %>%
      filter(!agecat == "14-17") %>% 
      filter(country == input$country &!is.na(.data[[input$var]]) & !is.na(.data[[input$ind]])) %>%
      group_by(.data[[input$ind]]) %>%
      count(.data[[input$var]]) %>%
      mutate(percent = n/sum(n)*100) %>% 
      filter(!grepl("not", .data[[input$var]]))
  })
  
  
  output$plot <- renderPlot({
    data() %>%
      ggplot(aes_string(input$ind, "percent", fill = input$var))+
      geom_col(position = "dodge")+
      theme_minimal(base_size = 18)+
      labs(x="", y = "%")+
      scale_fill_manual(values = "steelblue")+
      theme(legend.position="none")
  })
  
  output$table <- renderTable({
    data()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
