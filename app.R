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

# Load data into reactive values for dynamic updates
master_list_data <- reactiveVal(read_csv("master_list.csv"))
master_list_megan_data <- reactiveVal(read_csv("master_list_megan.csv"))

# Create a small table of recipe names and sources (reactive)
recipe_names <- reactive({
  master_list_data() %>%
    distinct(meal, source, notes) %>%
    arrange(meal)
})

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
            choices  = NULL,  # Will be populated by server
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

  # Tab 2: Recipe Editor
  tabPanel("Recipe Editor",
    fluidPage(
      titlePanel("Recipe Editor"),

      sidebarLayout(
        # Left sidebar for controls
        sidebarPanel(
          width = 3,

          # Mode selector
          h4("Mode"),
          radioButtons(
            inputId = "editor_mode",
            label = "Select mode:",
            choices = list(
              "Add New Recipe" = "add",
              "Edit Existing Recipe" = "edit"
            ),
            selected = "add"
          ),

          # Recipe selector (only visible in edit mode)
          conditionalPanel(
            condition = "input.editor_mode == 'edit'",
            selectInput(
              inputId = "edit_recipe_select",
              label = "Select recipe to edit:",
              choices = c("Select a recipe..." = "")
            )
          ),

          # Action buttons
          br(),
          actionButton(
            inputId = "save_recipe",
            label = "Save Recipe",
            class = "btn-primary"
          ),
          br(), br(),
          actionButton(
            inputId = "clear_form",
            label = "Clear Form",
            class = "btn-secondary"
          )
        ),

        # Right main panel for form
        mainPanel(
          width = 9,

          # Recipe metadata section
          h4("Recipe Information"),
          fluidRow(
            column(6,
              textInput(
                inputId = "recipe_name",
                label = "Recipe Name *",
                placeholder = "e.g., Chocolate Chip Cookies"
              )
            ),
            column(6,
              textInput(
                inputId = "recipe_source",
                label = "Source",
                placeholder = "e.g., Cookbook Name, Page 123"
              )
            )
          ),

          fluidRow(
            column(6,
              selectInput(
                inputId = "recipe_type",
                label = "Type",
                choices = c("dinner", "dessert", "breakfast", "other"),
                selected = "dinner"
              )
            ),
            column(6,
              textInput(
                inputId = "recipe_category",
                label = "Category",
                placeholder = "e.g., pasta, light veg dishes"
              )
            )
          ),

          textAreaInput(
            inputId = "recipe_notes",
            label = "Notes",
            placeholder = "Any special notes or instructions...",
            rows = 2
          ),

          # Ingredients section
          h4("Ingredients"),
          p("Add ingredients for this recipe:"),

          # Dynamic ingredient rows
          uiOutput("ingredient_rows"),

          # Add ingredient button
          actionButton(
            inputId = "add_ingredient",
            label = "Add Another Ingredient",
            class = "btn-success"
          ),

          # Status messages
          br(), br(),
          verbatimTextOutput("editor_status")
        )
      )
    )
  )
)



# Define the server logic
server <- function(input, output, session) {

  # Initialize recipe choices on app start
  observe({
    updateSelectInput(session, "masterclass",
                     choices = recipe_names()$meal)
  })

  # Reactive to get the current master list (with or without Megan's recipes)
  current_master_list <- reactive({
    if (input$include_megan) {
      bind_rows(master_list_data(), master_list_megan_data())
    } else {
      master_list_data()
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

  # Recipe Editor server logic

  # Create reactive values for editor state
  editor_state <- reactiveValues(
    mode = "add",
    selected_recipe = NULL,
    ingredients = list(),
    ingredient_counter = 0
  )

  # Get unique sections, types, and categories from existing data
  all_sections <- reactive({
    bind_rows(master_list_data(), master_list_megan_data()) %>%
      distinct(section) %>%
      filter(!is.na(section)) %>%
      pull(section) %>%
      sort()
  })

  all_types <- reactive({
    bind_rows(master_list_data(), master_list_megan_data()) %>%
      distinct(type) %>%
      filter(!is.na(type)) %>%
      pull(type) %>%
      sort()
  })

  all_categories <- reactive({
    bind_rows(master_list_data(), master_list_megan_data()) %>%
      distinct(category) %>%
      filter(!is.na(category)) %>%
      pull(category) %>%
      sort()
  })

  # Update type dropdown choices
  observe({
    updateSelectInput(session, "recipe_type",
                     choices = all_types())
  })

  # Initialize with one ingredient row
  observe({
    if (length(editor_state$ingredients) == 0) {
      editor_state$ingredient_counter <- 1
      editor_state$ingredients <- list(
        list(
          id = 1,
          section = "produce",
          name = "",
          amount = 1,
          units = ""
        )
      )
    }
  })

  # Handle mode switching - show/hide recipe selector
  observeEvent(input$editor_mode, {
    editor_state$mode <- input$editor_mode

    if (input$editor_mode == "edit") {
      # Populate recipe dropdown with user's recipes (not Megan's)
      user_recipes <- master_list_data() %>%
        distinct(meal) %>%
        arrange(meal) %>%
        pull(meal)

      updateSelectInput(session, "edit_recipe_select",
                       choices = c("Select a recipe..." = "", user_recipes))
    }
  })

  # Add ingredient button
  observeEvent(input$add_ingredient, {
    editor_state$ingredient_counter <- editor_state$ingredient_counter + 1
    new_ingredient <- list(
      id = editor_state$ingredient_counter,
      section = "produce",
      name = "",
      amount = 1,
      units = ""
    )
    editor_state$ingredients <- append(editor_state$ingredients, list(new_ingredient))
  })

  # Remove ingredient button (handled in renderUI)
  observeEvent(input$remove_ingredient, {
    ingredient_id <- as.numeric(input$remove_ingredient)
    editor_state$ingredients <- editor_state$ingredients[!sapply(editor_state$ingredients, function(x) x$id == ingredient_id)]
  })

  # Clear form button
  observeEvent(input$clear_form, {
    # Reset form inputs
    updateTextInput(session, "recipe_name", value = "")
    updateTextInput(session, "recipe_source", value = "")
    updateSelectInput(session, "recipe_type", selected = "dinner")
    updateTextInput(session, "recipe_category", value = "")
    updateTextAreaInput(session, "recipe_notes", value = "")

    # Reset ingredients to single empty row
    editor_state$ingredient_counter <- 1
    editor_state$ingredients <- list(
      list(
        id = 1,
        section = "produce",
        name = "",
        amount = 1,
        units = ""
      )
    )
  })

  # Render dynamic ingredient rows
  output$ingredient_rows <- renderUI({
    if (length(editor_state$ingredients) == 0) return(NULL)

    lapply(editor_state$ingredients, function(ingredient) {
      fluidRow(
        column(3,
          selectInput(
            inputId = paste0("ingredient_section_", ingredient$id),
            label = "Section",
            choices = all_sections(),
            selected = ingredient$section
          )
        ),
        column(3,
          textInput(
            inputId = paste0("ingredient_name_", ingredient$id),
            label = "Ingredient",
            placeholder = "e.g., tomatoes",
            value = ingredient$name
          )
        ),
        column(2,
          numericInput(
            inputId = paste0("ingredient_amount_", ingredient$id),
            label = "Amount",
            value = ingredient$amount,
            min = 0,
            step = 0.1
          )
        ),
        column(2,
          textInput(
            inputId = paste0("ingredient_units_", ingredient$id),
            label = "Units",
            placeholder = "e.g., items, g, ml",
            value = ingredient$units
          )
        ),
        column(2,
          br(),
          if (length(editor_state$ingredients) > 1) {
            actionButton(
              inputId = "remove_ingredient",
              label = "Remove",
              class = "btn-danger btn-sm",
              onclick = paste0("Shiny.setInputValue('remove_ingredient', ", ingredient$id, ", {priority: 'event'})")
            )
          } else {
            div() # Empty div when only one ingredient
          }
        )
      )
    })
  })

  # Update editor status with ingredient count
  output$editor_status <- renderText({
    ingredient_count <- length(editor_state$ingredients)
    paste("Mode:", input$editor_mode,
          if(input$editor_mode == "edit") paste("| Selected:", input$edit_recipe_select) else "",
          "| Ingredients:", ingredient_count)
  })

}

shinyApp(ui, server)


