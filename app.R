#### RECIPE MASTER LIST ########
library(tidyverse)
library(shiny)
library(shinyjs)
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

# Strict CSV validation and fail-fast preload
expected_recipe_columns <- c(
  "type", "category", "meal", "source", "section",
  "ingredient", "amount", "units", "notes"
)

read_csv_with_schema <- function(path) {
  read_csv(
    path,
    show_col_types = FALSE,
    col_types = cols(
      type = col_character(),
      category = col_character(),
      meal = col_character(),
      source = col_character(),
      section = col_character(),
      ingredient = col_character(),
      amount = col_double(),
      units = col_character(),
      notes = col_character()
    )
  )
}

validation_errors <- character(0)

validate_frame <- function(df, file_label) {
  missing_cols <- setdiff(expected_recipe_columns, names(df))
  if (length(missing_cols) > 0) {
    validation_errors <<- c(
      validation_errors,
      paste0(
        file_label,
        ": Missing required columns: ",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  probs <- tryCatch(readr::problems(df), error = function(e) NULL)
  if (!is.null(probs) && nrow(probs) > 0) {
    head_rows <- head(probs, 5)
    msg <- paste(
      apply(head_rows[, c("row", "col", "expected", "actual")], 1, function(r) {
        paste0("row ", r[["row"]], ", col ", r[["col"]], ": expected ", r[["expected"]], ", got ", r[["actual"]])
      }),
      collapse = "; "
    )
    validation_errors <<- c(
      validation_errors,
      paste0(file_label, ": Parse problems detected (showing up to 5): ", msg)
    )
  }
}

load_and_validate_csv <- function(path) {
  df <- tryCatch({
    read_csv_with_schema(path)
  }, error = function(e) {
    validation_errors <<- c(validation_errors, paste0(path, ": ", conditionMessage(e)))
    return(NULL)
  })
  if (!is.null(df)) validate_frame(df, path)
  df
}

load_recipes_from_folder_strict <- function() {
  folder <- "recipes"
  if (!dir.exists(folder)) return(tibble())
  files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) == 0) return(tibble())

  dfs <- map(files, function(f) {
    df <- tryCatch({
      read_csv_with_schema(f)
    }, error = function(e) {
      validation_errors <<- c(validation_errors, paste0(f, ": ", conditionMessage(e)))
      return(NULL)
    })
    if (!is.null(df)) validate_frame(df, f)
    df
  })

  dfs <- dfs[!map_lgl(dfs, is.null)]
  if (length(dfs) == 0) return(tibble())
  bind_rows(dfs)
}

# Preload and validate all CSVs before building reactives
preloaded_master_list <- load_and_validate_csv("master_list.csv")
preloaded_master_list_megan <- load_and_validate_csv("master_list_megan.csv")
preloaded_recipes_folder <- load_recipes_from_folder_strict()

if (length(validation_errors) > 0) {
  stop(
    paste0(
      "CSV validation failed. Please fix the following issues before starting the app:\n\n",
      paste("- ", validation_errors, collapse = "\n")
    )
  )
}

# Load data into reactive values for dynamic updates from validated frames
master_list_data <- reactiveVal(preloaded_master_list)
master_list_megan_data <- reactiveVal(preloaded_master_list_megan)
recipes_folder_data <- reactiveVal(preloaded_recipes_folder)

# Create a small table of recipe names and sources (reactive)
recipe_names <- reactive({
  master_list_data() %>%
    distinct(meal, source, notes) %>%
    arrange(meal)
})

ui <- navbarPage(
  "Recipe Manager",
  useShinyjs(),

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
        h4("Selected Recipes"),
        tableOutput(outputId = "recipe_list"),
        br(),
        div(style = "display: flex; align-items: center; gap: 10px; margin-bottom: 10px;",
          h4("Shopping List", style = "margin: 0;"),
          actionButton(
            inputId = "copy_list",
            label = "📋 Copy List",
            class = "btn btn-sm btn-outline-primary",
            style = "font-size: 14px; padding: 8px 16px; min-height: 44px; touch-action: manipulation;"
          ),
          actionButton(
            inputId = "add_to_grocery_list",
            label = "🛒 Add to Grocery List",
            class = "btn btn-sm btn-outline-success",
            style = "font-size: 14px; padding: 8px 16px; min-height: 44px; touch-action: manipulation;"
          )
        ),
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


          # Ingredients section
          div(id = "ingredients_section",
            h4("Ingredients"),
            p("Add ingredients for this recipe:"),

            # Container for dynamic ingredient rows
            div(id = "ingredient_container")
          ),

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

  # Reactive to get the current master list (with or without Megan's recipes and recipes folder)
  current_master_list <- reactive({
    base_data <- master_list_data()

    # Add Megan's recipes if checkbox is checked
    if (input$include_megan) {
      base_data <- bind_rows(base_data, master_list_megan_data())
    }

    # Always add recipes from folder
    recipes_folder_data <- recipes_folder_data()
    if (nrow(recipes_folder_data) > 0) {
      base_data <- bind_rows(base_data, recipes_folder_data)
    }

    return(base_data)
  })

  # Reactive to get the current recipe names
  current_recipe_names <- reactive({
    current_master_list() %>%
      distinct(meal, source) %>%
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
      group_by(section, ingredient, units, notes) %>%
      summarize(total = sum(amount), .groups = "drop") %>%
      arrange(section, ingredient) %>%
      mutate(
        items = str_c(ingredient, as.character(total), units, sep = " "),
        notes_display = ifelse(
          is.na(notes) | notes == "",
          "",
          notes
        )
      ) %>%
      select(items, notes_display)
  })

  recipe_list <- eventReactive(input$masterclass, {
    current_recipe_names() %>%
      filter(meal %in% input$masterclass) %>%
      select(meal, source) %>%
      distinct()
  })

  output$ingredients <- renderTable({
    groceries()
  })

  output$recipe_list <- renderTable({
    recipe_list()
  })

  # Create formatted grocery list text for copying
  grocery_list_text <- reactive({
    if (nrow(groceries()) == 0) {
      return("No items in shopping list")
    }

    # Format each item as "ingredient + amount + units" with notes if available
    items <- groceries() %>%
      mutate(
        formatted_item = case_when(
          !is.na(notes_display) & notes_display != "" ~ paste0(items, " (", notes_display, ")"),
          TRUE ~ items
        )
      ) %>%
      pull(formatted_item)

    return(paste(items, collapse = "\n"))
  })

  # Handle copy button click
  observeEvent(input$copy_list, {
    # Get the formatted text
    text_to_copy <- grocery_list_text()

    # Create JavaScript to copy to clipboard
    js_code <- paste0("
      navigator.clipboard.writeText(`", text_to_copy, "`).then(function() {
        // Show success message
        var button = document.getElementById('copy_list');
        var originalText = button.innerHTML;
        button.innerHTML = '✅ Copied!';
        button.style.backgroundColor = '#28a745';
        button.style.color = 'white';

        setTimeout(function() {
          button.innerHTML = originalText;
          button.style.backgroundColor = '';
          button.style.color = '';
        }, 2000);
      }).catch(function(err) {
        // Fallback for older browsers
        var textArea = document.createElement('textarea');
        textArea.value = `", text_to_copy, "`;
        document.body.appendChild(textArea);
        textArea.select();
        document.execCommand('copy');
        document.body.removeChild(textArea);

        var button = document.getElementById('copy_list');
        var originalText = button.innerHTML;
        button.innerHTML = '✅ Copied!';
        button.style.backgroundColor = '#28a745';
        button.style.color = 'white';

        setTimeout(function() {
          button.innerHTML = originalText;
          button.style.backgroundColor = '';
          button.style.color = '';
        }, 2000);
      });
    ")

    # Execute the JavaScript
    shinyjs::runjs(js_code)
  })

  # Handle Shortcuts app button click
  observeEvent(input$add_to_grocery_list, {
    # Get the formatted text
    text_to_copy <- grocery_list_text()

    # Create JavaScript to open Shortcuts app with URL-encoded grocery list
    js_code <- paste0("
      // URL encode the grocery list text
      var groceryText = `", text_to_copy, "`;
      var encodedText = encodeURIComponent(groceryText);

      // Create the Shortcuts URL with the encoded text as input
      var shortcutsUrl = 'shortcuts://run-shortcut?name=Bulk%20Add%20Groceries&input=text&text=' + encodedText;

      // Open Shortcuts app with the grocery list as input
      window.location.href = shortcutsUrl;

      // Show success message
      var button = document.getElementById('add_to_grocery_list');
      var originalText = button.innerHTML;
      button.innerHTML = '✅ Opening Shortcuts...';
      button.style.backgroundColor = '#28a745';
      button.style.color = 'white';

      setTimeout(function() {
        button.innerHTML = originalText;
        button.style.backgroundColor = '';
        button.style.color = '';
      }, 3000);
    ")

    # Execute the JavaScript
    shinyjs::runjs(js_code)
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
          units = "",
          notes = ""
        )
      )

      # Insert the first ingredient row
      insertUI(
        selector = "#ingredient_container",
        where = "beforeEnd",
        ui = create_ingredient_row(1)
      )
    }
  })

  # Function to create an ingredient row
  create_ingredient_row <- function(ingredient_id) {
    fluidRow(
      id = paste0("ingredient_row_", ingredient_id),
      column(2,
        selectInput(
          inputId = paste0("ingredient_section_", ingredient_id),
          label = "Section",
          choices = all_sections(),
          selected = "produce"
        )
      ),
      column(2,
        textInput(
          inputId = paste0("ingredient_name_", ingredient_id),
          label = "Ingredient",
          placeholder = "e.g., tomatoes",
          value = ""
        )
      ),
      column(1,
        numericInput(
          inputId = paste0("ingredient_amount_", ingredient_id),
          label = "Amount",
          value = 1,
          min = 0,
          step = 0.1
        )
      ),
      column(1,
        textInput(
          inputId = paste0("ingredient_units_", ingredient_id),
          label = "Units",
          placeholder = "e.g., items, g, ml",
          value = ""
        )
      ),
      column(4,
        textInput(
          inputId = paste0("ingredient_notes_", ingredient_id),
          label = "Notes",
          placeholder = "e.g., substitute heavy coconut milk for evaporated milk",
          value = ""
        )
      ),
      column(1,
        id = paste0("button_container_", ingredient_id),
        br()
      )
    )
  }

  # Handle mode switching - show/hide recipe selector
  observeEvent(input$editor_mode, {
    editor_state$mode <- input$editor_mode

    if (input$editor_mode == "edit") {
      # Populate recipe dropdown with user's recipes (master_list + recipes folder, not Megan's)
      user_recipes <- bind_rows(master_list_data(), recipes_folder_data()) %>%
        distinct(meal) %>%
        arrange(meal) %>%
        pull(meal)

      updateSelectInput(session, "edit_recipe_select",
                       choices = c("Select a recipe..." = "", user_recipes))
    } else {
      # Clear form when switching to add mode
      clear_form()
    }
  })

  # Handle recipe selection for editing
  observeEvent(input$edit_recipe_select, {
    if (!is.null(input$edit_recipe_select) && input$edit_recipe_select != "") {
      load_recipe_for_editing(input$edit_recipe_select)
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
      units = "",
      notes = ""
    )
    editor_state$ingredients <- append(editor_state$ingredients, list(new_ingredient))

    # Insert new ingredient row
    insertUI(
      selector = "#ingredient_container",
      where = "beforeEnd",
      ui = create_ingredient_row(editor_state$ingredient_counter)
    )

    # Update remove buttons for all rows
    update_remove_buttons()
  })

  # Function to update remove buttons
  update_remove_buttons <- function() {
    # Remove ALL possible remove buttons first (IDs 1-100 to be safe)
    for (i in 1:100) {
      removeUI(selector = paste0("#remove_ingredient_", i), immediate = TRUE)
    }

    # Clear all button containers completely
    for (ingredient in editor_state$ingredients) {
      removeUI(selector = paste0("#button_container_", ingredient$id, " > *"), immediate = TRUE)
    }

    # Only add remove buttons if we have more than one ingredient
    if (length(editor_state$ingredients) > 1) {
      # Add a small delay to ensure clearing is complete
      Sys.sleep(0.1)

      for (ingredient in editor_state$ingredients) {
        # Add the remove button
        insertUI(
          selector = paste0("#button_container_", ingredient$id),
          where = "beforeEnd",
          ui = actionButton(
            inputId = paste0("remove_ingredient_", ingredient$id),
            label = "Remove",
            class = "btn-danger btn-sm"
          )
        )
      }
    }
  }

  # Remove ingredient buttons - handle all possible remove buttons
  observe({
    for (ingredient in editor_state$ingredients) {
      local({
        ingredient_id <- ingredient$id
        observeEvent(input[[paste0("remove_ingredient_", ingredient_id)]], {
          # Remove from reactive values
          editor_state$ingredients <- editor_state$ingredients[!sapply(editor_state$ingredients, function(x) x$id == ingredient_id)]

          # Remove from UI
          removeUI(selector = paste0("#ingredient_row_", ingredient_id), immediate = TRUE)

          # Force a delay and then update buttons
          Sys.sleep(0.3)
          update_remove_buttons()
        })
      })
    }
  })

  # Function to load recipe data for editing
  load_recipe_for_editing <- function(recipe_name) {
    # Get all rows for this recipe from both master_list and recipes folder
    recipe_data <- bind_rows(master_list_data(), recipes_folder_data()) %>%
      filter(meal == recipe_name)

    if (nrow(recipe_data) > 0) {
      # Get recipe metadata (from first row)
      first_row <- recipe_data[1, ]

      # Update form fields
      updateTextInput(session, "recipe_name", value = first_row$meal)
      updateTextInput(session, "recipe_source", value = first_row$source)
      updateSelectInput(session, "recipe_type", selected = first_row$type)
      updateTextInput(session, "recipe_category", value = first_row$category)

      # COMPLETELY clear the entire ingredient container and recreate it
      removeUI(selector = "#ingredient_container", immediate = TRUE)

      # Recreate the empty ingredient container
      insertUI(
        selector = "#ingredients_section",
        where = "beforeEnd",
        ui = div(id = "ingredient_container")
      )

      # Also remove any orphaned remove buttons that might exist
      for (i in 1:100) {  # Remove buttons for IDs 1-100 to be safe
        removeUI(selector = paste0("#remove_ingredient_", i), immediate = TRUE)
      }

      # Reset ingredients list completely
      editor_state$ingredients <- list()
      editor_state$ingredient_counter <- 0

      # Create ingredient rows for each ingredient
      for (i in 1:nrow(recipe_data)) {
        ingredient <- recipe_data[i, ]
        editor_state$ingredient_counter <- editor_state$ingredient_counter + 1

        # Add to ingredients list
        editor_state$ingredients <- append(editor_state$ingredients, list(
          list(
            id = editor_state$ingredient_counter,
            section = ingredient$section,
            name = ingredient$ingredient,
            amount = ingredient$amount,
            units = ingredient$units,
            notes = ingredient$notes
          )
        ))

        # Create UI row
        insertUI(
          selector = "#ingredient_container",
          where = "beforeEnd",
          ui = create_ingredient_row_with_values(
            editor_state$ingredient_counter,
            ingredient$section,
            ingredient$ingredient,
            ingredient$amount,
            ingredient$units,
            ingredient$notes
          )
        )
      }

      # Force a small delay to ensure UI updates
      Sys.sleep(0.1)

      # Update remove buttons
      update_remove_buttons()
    }
  }

  # Function to create ingredient row with pre-filled values
  create_ingredient_row_with_values <- function(ingredient_id, section, name, amount, units, notes) {
    fluidRow(
      id = paste0("ingredient_row_", ingredient_id),
      column(2,
        selectInput(
          inputId = paste0("ingredient_section_", ingredient_id),
          label = "Section",
          choices = all_sections(),
          selected = section
        )
      ),
      column(2,
        textInput(
          inputId = paste0("ingredient_name_", ingredient_id),
          label = "Ingredient",
          placeholder = "e.g., tomatoes",
          value = name
        )
      ),
      column(1,
        numericInput(
          inputId = paste0("ingredient_amount_", ingredient_id),
          label = "Amount",
          value = amount,
          min = 0,
          step = 0.1
        )
      ),
      column(1,
        textInput(
          inputId = paste0("ingredient_units_", ingredient_id),
          label = "Units",
          placeholder = "e.g., items, g, ml",
          value = units
        )
      ),
      column(4,
        textInput(
          inputId = paste0("ingredient_notes_", ingredient_id),
          label = "Notes",
          placeholder = "e.g., substitute heavy coconut milk for evaporated milk",
          value = notes
        )
      ),
      column(1,
        id = paste0("button_container_", ingredient_id),
        br()
      )
    )
  }

  # Function to clear form
  clear_form <- function() {
    # Reset form inputs
    updateTextInput(session, "recipe_name", value = "")
    updateTextInput(session, "recipe_source", value = "")
    updateSelectInput(session, "recipe_type", selected = "dinner")
    updateTextInput(session, "recipe_category", value = "")

    # COMPLETELY clear the entire ingredient container and recreate it
    removeUI(selector = "#ingredient_container", immediate = TRUE)

    # Recreate the empty ingredient container
    insertUI(
      selector = "#ingredients_section",
      where = "beforeEnd",
      ui = div(id = "ingredient_container")
    )

    # Also remove any orphaned remove buttons that might exist
    for (i in 1:100) {  # Remove buttons for IDs 1-100 to be safe
      removeUI(selector = paste0("#remove_ingredient_", i), immediate = TRUE)
    }

    # Reset ingredients to single empty row
    editor_state$ingredient_counter <- 1
    editor_state$ingredients <- list(
      list(
        id = 1,
        section = "produce",
        name = "",
        amount = 1,
        units = "",
        notes = ""
      )
    )

    # Insert the first ingredient row
    insertUI(
      selector = "#ingredient_container",
      where = "beforeEnd",
      ui = create_ingredient_row(1)
    )

    # Force a small delay to ensure UI updates
    Sys.sleep(0.1)

    # Update remove buttons after clearing
    update_remove_buttons()
  }

  # Clear form button
  observeEvent(input$clear_form, {
    clear_form()
  })


  # Save recipe functionality
  observeEvent(input$save_recipe, {
    # Validate required fields
    if (is.null(input$recipe_name) || input$recipe_name == "") {
      showNotification("Recipe name is required!", type = "error")
      return()
    }

    # Collect ingredient data directly from inputs
    ingredient_data <- list()

    for (ingredient in editor_state$ingredients) {
      section_val <- input[[paste0("ingredient_section_", ingredient$id)]]
      name_val <- input[[paste0("ingredient_name_", ingredient$id)]]
      amount_val <- input[[paste0("ingredient_amount_", ingredient$id)]]
      units_val <- input[[paste0("ingredient_units_", ingredient$id)]]
      notes_val <- input[[paste0("ingredient_notes_", ingredient$id)]]

      # Skip empty ingredient rows
      if (is.null(name_val) || name_val == "" || is.null(amount_val) || amount_val <= 0) {
        next
      }

      new_ingredient <- list(
        type = input$recipe_type,
        category = ifelse(input$recipe_category == "", NA, input$recipe_category),
        meal = input$recipe_name,
        source = ifelse(input$recipe_source == "", NA, input$recipe_source),
        section = section_val,
        ingredient = name_val,
        amount = amount_val,
        units = ifelse(units_val == "", "items", units_val),
        notes = ifelse(is.null(notes_val) || notes_val == "", NA, notes_val)
      )

      ingredient_data <- append(ingredient_data, list(new_ingredient))
    }

    # Check if we have at least one valid ingredient
    if (length(ingredient_data) == 0) {
      showNotification("At least one ingredient is required!", type = "error")
      return()
    }

    # Convert to data frame
    new_recipe_df <- do.call(rbind, lapply(ingredient_data, function(x) {
      data.frame(
        type = as.character(x$type),
        category = as.character(x$category),
        meal = as.character(x$meal),
        source = as.character(x$source),
        section = as.character(x$section),
        ingredient = as.character(x$ingredient),
        amount = as.numeric(x$amount),
        units = as.character(x$units),
        notes = as.character(x$notes),
        stringsAsFactors = FALSE
      )
    }))

    # Get current data
    current_data <- master_list_data()

    # Handle editing vs adding
    if (editor_state$mode == "edit" && !is.null(input$edit_recipe_select) && input$edit_recipe_select != "") {
      # Editing mode: remove old recipe and add updated one
      original_recipe_name <- input$edit_recipe_select

      # Remove all rows for the original recipe
      updated_data <- current_data %>%
        filter(meal != original_recipe_name)

      # Add the updated recipe
      updated_data <- bind_rows(updated_data, new_recipe_df)

      # Update the recipe selector to show the new name if it changed
      if (input$recipe_name != original_recipe_name) {
        # Update the dropdown choices
        user_recipes <- updated_data %>%
          distinct(meal) %>%
          arrange(meal) %>%
          pull(meal)

        updateSelectInput(session, "edit_recipe_select",
                         choices = c("Select a recipe..." = "", user_recipes),
                         selected = input$recipe_name)
      }

      showNotification(paste("Recipe '", input$recipe_name, "' updated successfully!"), type = "message")
    } else {
      # Adding mode: just add the new recipe
      updated_data <- bind_rows(current_data, new_recipe_df)
      showNotification(paste("Recipe '", input$recipe_name, "' added successfully!"), type = "message")
    }

    # Update reactive data
    master_list_data(updated_data)

    # Save to CSV
    write_csv(updated_data, "master_list.csv")

    # Clear form
    clear_form()
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


