#### RECIPE MASTER LIST ########
library(tidyverse)
library(shiny)
library(googlesheets4)

# shiny app basics from
# https://stackoverflow.com/questions/73263669/grocery-list-in-shiny

# Pull in a list of recipes and ingredients
# Columns are
# type: dinner, dessert (not used currently)
# meal: name of recipe
# source: cookbook and page
# section: what section of the grocery store (Fiesta Farms) is this ingredient found in?
# ingredient: name of the ingredient
# amount: how much of this ingredient for this recipe?
# unit: what unit of measurement (this is set up to be consistent within an ingredient)
# notes: shopping notes (not used currently)

master_list = read_csv("master_list.csv")
master_list_megan = read_csv("master_list_megan.csv")

# Create a small table of recipe names and sources
recipe_names = master_list %>%
  distinct(meal, source, notes) %>%
  arrange(meal)

ui <- navbarPage(
  "Recipe Manager",

  # Tab 1: Grocery List (existing functionality)
  tabPanel("Grocery List",
    fluidPage(
      # App title
      titlePanel("What are you making this week?"),

      # Sidebar layout
      sidebarLayout(
        # A panel for inputs
        sidebarPanel(
          # Checkbox to include Megan's recipes
          checkboxInput(
            inputId = "include_megan",
            label = "Include Megan's recipes",
            value = FALSE
          ),
          # Use an input type that allows selection of multiple recipes
          selectInput(
            inputId = "masterclass",
            label = "What are my options?",
            choices  = recipe_names$meal,
            multiple = TRUE
          ),
          tags$a(href="https://github.com/meganbontrager/grocery-list", "edit on github")
        ),
        mainPanel(
        tableOutput(outputId = "recipe_list"),
        tableOutput(outputId = "ingredients")
                  )
      )
    )
  ),

  # Tab 2: Recipe Editor (placeholder for now)
  tabPanel("Recipe Editor",
    fluidPage(
      titlePanel("Recipe Editor"),
      mainPanel(
        h3("Recipe Editor"),
        p("This is where the recipe editor will be implemented."),
        p("Features will include:"),
        tags$ul(
          tags$li("Add new recipes"),
          tags$li("Edit existing recipes"),
          tags$li("Manage ingredients")
        )
      )
    )
  )
)



# Define the server logic
server <- function(input, output, session) {

  # Reactive to get the current master list (with or without Megan's recipes)
  current_master_list <- reactive({
    if (input$include_megan) {
      bind_rows(master_list, master_list_megan)
    } else {
      master_list
    }
  })

  # Reactive to get the current recipe names
  current_recipe_names <- reactive({
    current_master_list() %>%
      distinct(meal, source, notes) %>%
      arrange(meal)
  })

  # Update recipe choices when checkbox changes
  observeEvent(input$include_megan, {
    updateSelectInput(session, "masterclass",
                     choices = current_recipe_names()$meal)
  })

  groceries <- eventReactive(input$masterclass, {
    current_master_list() %>%
      filter(meal %in% input$masterclass) %>%
      group_by(section, ingredient, units) %>%
      summarize(total = sum(amount)) %>%
      arrange(section, ingredient) %>%
      ungroup() %>%
      # would be nice to get grocery section in as headings
      select(ingredient, total, units) %>%
      mutate(items = str_c(ingredient, as.character(total), units, sep = " ")) %>%
      select(items)
  })

  recipe_list <- eventReactive(input$masterclass, {
    current_recipe_names() %>%
      filter(meal %in% input$masterclass) %>%
      select(meal, source, notes) %>%
      distinct()
  })

  output$ingredients <- renderTable({
    groceries()
  })

  output$recipe_list <- renderTable({
    recipe_list()
  })

}

shinyApp(ui, server)


