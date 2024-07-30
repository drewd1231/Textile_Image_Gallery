library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(grid)
library(gridExtra)
library(ggtern)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)


source("functions.R")

#Page size global
PAGE_SIZE = 9

#Set working directory to correct folder
#setwd("/Users/drew/Documents/Work/Summer24/textiles_app")

#Read in data
#textiles_data <- read_excel("MaterialDataset01302024.xlsx")
textiles_data <- read_excel("MaterialDataset_07-19-24.xlsx")

#Filter out any textiles without images
textiles_cleaned <- textiles_data %>% 
  filter(image_filename_app != "NA")

#Make list for users searching by textile name
names_cleaned <- textiles_cleaned %>% 
  filter(textile_name != "NA")
names_cleaned <- names_cleaned[order(names_cleaned$textile_name),]

# patterns_cleaned <- textiles_cleaned %>% 
#   filter()

#Code to get list of all colors used
# textile_color <- textiles_data$textile_color_visual %>% 
#   strsplit(", ") %>% 
#    unlist() %>% 
#     unique()

#Hardcoded list of color options
COLOR_LIST <- c("blue", "white", "red", "black", "gold", "natural", "yellow", "green", "purple", "orange", "brown", "pink", "grey") %>% 
  sort()

#Hardcoded list of pattern options
PATTERN_LIST <- c("floral", "geometric", "checkered", "striped", "plain", "figural", "none", "dots", "stars", "foliage") %>% 
  sort()

#Hardcoded list of process options
PROCESS_LIST <- c("painted", "block printed", "resist-dyed", "piece-dyed", "loom-patterned", "printed", "felted", "embroidered", "ikat", "dyed", "damask") %>% 
  sort()

#Hardcoded list of weave options
WEAVE_LIST <- c("satin", "plainweave", "twill", "damask", "velvet", "complex weave", "plainweave with doubled threads", "plainweave, supplemental weft") %>% 
  sort()

#Hardcoded list of fiber options
FIBER_LIST <- c("silk", "cotton", "wool", "linen") %>% 
  sort()

GLOSSARY_TEXTILES_LIST <- c("chintz-kalamkari", "dongris", "gingham", "guinea cloth", "muslin", "nickanees", "perpetuanen", "platillas", "sail cloth", "slaaplakens")

#Define UI
ui <- fluidPage( 
  useShinyjs(),
  
  theme = shinytheme("sandstone"),
  
  titlePanel(title = "Materials Sample Search"), 
  
  
  #Create each input possibility
  sidebarPanel(
    h3("The Material Record"), 
    h5("Search the gallery of textiles using any number of inputs found below. 
       The modifier operator determines if the inputs (excluding the textile name) must all be true or if any one of them can be true."),
    
    #Allows user to select textile name to filter by
    selectInput("textile_name", "Search by textile name", 
                choices = c("All Names", names_cleaned$textile_name), 
                selected = "All Names"), 
    
    #Allows user to filter images by name AND other modifiers or not
    radioGroupButtons("and_or", "Select Modifier Operator",
                 choices = c("AND", "OR"), 
                 selected = "AND", 
                 justified = TRUE),
  
    selectInput("color_choice", "Search by color(s)", 
                choices = COLOR_LIST, 
                multiple = TRUE),
    
    #Allows user to select pattern to filter by
    selectInput("pattern_choice", "Search by pattern", 
                choices = c("All Patterns", PATTERN_LIST)),
    
    #Allows user to select process to filter by
    selectInput("process_choice", "Search by process", 
                choices = c("All Processes", PROCESS_LIST)),
    
    #Allows user to select weave to filter by
    selectInput("weave_choice", "Search by weave", 
                choices = c("All Weaves", WEAVE_LIST)),
    
    #Allows user to select fiber to filter by
    selectInput("fiber_choice", "Search by fiber", 
                choices = c("All Fibers", FIBER_LIST)),
    
    actionButton("comparison_button", "Compare two textiles by material id "),
    
    tags$hr(),
    actionButton("reset_button", "Reset all inputs"), 
    
    
    
  ), 
  
  mainPanel(
    uiOutput("images"), 
    # uiOutput("no_images"),
    actionButton("prev_page", "Previous", 
                 style = "margin-top: 10px; margin-bottom: 5px;"),
    actionButton("next_page", "Next", 
                 style = "margin-top: 10px; margin-bottom: 5px;"), 
    textOutput("page_details") 
    #textOutput("no_results", )
  ), 
  tags$head(
    #Creates HTML layout for how images are displayed (4 columns, gap in between, etc.)
    includeCSS("www/custom_styles.css")
  ),
    #includes javascript file for processing images
  tags$script(src = "custom_script.js")   
)

#Define Server Logic
server <- function(input, output, session) { 
  
  options(shiny.error = function() {
    err <- geterrmessage()
    cat("ERROR: ", err, "\n")
    stop(err)
  })
  
  #initialize reactive values that will be passed from observe blocks to reactive blocks
  rv <- reactiveValues(filter_colors = FALSE, 
                       filtered_color_input = list(), 
                       page_number = 1, 
                       page_size = PAGE_SIZE, 
                       prev_filtered_rows = nrow(textiles_cleaned), 
                       comparison_selection = NULL, 
                       zoomed_dialog_open = FALSE,
                       zoomed_comparison_open = FALSE, 
                       image_index = 1
                       )
  
  #Reactive function we want to call when user selects "AND"
  filtered_and <- reactive({ 
    curr_filtered <- textiles_cleaned
    
    #Filters data based on name
    if (input$textile_name != "All Names") { 
      curr_filtered <- curr_filtered %>% 
        filter(textile_name == input$textile_name)
    }

    #Check if there are colors to be filtered by
    if (rv$filter_colors == TRUE) {
      
      #Loop through each color selected and filter data by each of them
      for (color in rv$filtered_color_input) {
        curr_filtered <- curr_filtered %>%
          filter(grepl(color, textile_color_visual))
      }
    }
    
    #Filters data based on pattern
    if (input$pattern_choice != "All Patterns") { 
      curr_filtered <- curr_filtered %>% 
        filter(grepl(input$pattern_choice, textile_pattern_visual))
    }
    
    #Filters data based on process
    if (input$process_choice != "All Processes") { 
      curr_filtered <- curr_filtered %>% 
        filter(grepl(input$process_choice, textile_process_visual))
    }
    
    #Filters data based on weave
    if (input$weave_choice != "All Weaves") { 
      curr_filtered <- curr_filtered %>% 
        filter(grepl(input$weave_choice, textile_weave_visual))
    }
    
    #Filters data based on fiber
    if (input$fiber_choice != "All Fibers") { 
      curr_filtered <- curr_filtered %>% 
        filter(grepl(input$fiber_choice, textile_fiber_visual))
    }
    
    #Returns filtered data
    curr_filtered
  })
  
  
  #Reactive function we want to call when user selects "OR"
  filtered_or <- reactive ({ 
    name_filtered <- textiles_cleaned
    
    #First filter data based on name
    if (input$textile_name != "All Names") { 
      name_filtered <- name_filtered %>% 
        filter(textile_name == input$textile_name)
    }

    #Initialize empty dataframe to incrementally update with each input filter
    prev_ds <- textiles_cleaned[FALSE,]
    

    #Check if there are colors to be filtered by
    if (rv$filter_colors == TRUE) {
      #Loop through colors in input list and filter the data by each one
      for (color in rv$filtered_color_input) {
        prev_ds <- name_filtered %>%
          filter(grepl(color, textile_color_visual)) %>% 
            rbind(prev_ds) %>% 
              unique()
      }
    }
    
    #Filter data based on name and pattern then combine with previously filtered data
    if (input$pattern_choice != "All Patterns") { 
      #prev_ds <- textiles_cleaned[FALSE,]
      prev_ds <- name_filtered %>% 
        filter(grepl(input$pattern_choice, textile_pattern_visual)) %>% 
          rbind(prev_ds) %>% 
            unique()
    }
    
    #Filter data based on name and process then combine with previously filtered data
    if (input$process_choice != "All Processes") { 
      #prev_ds <- textiles_cleaned[FALSE,]
      prev_ds <- name_filtered %>% 
        filter(grepl(input$process_choice, textile_process_visual)) %>% 
          rbind(prev_ds) %>% 
            unique()
    }
    
    #Filter data based on name and weave then combine with previously filtered data
    if (input$weave_choice != "All Weaves") { 
      #prev_ds <- textiles_cleaned[FALSE,]
      prev_ds <- name_filtered %>% 
        filter(grepl(input$weave_choice, textile_weave_visual)) %>% 
          rbind(prev_ds) %>% 
            unique()
    }
    
    #Filter data based on name and fiber then combine with previously filtered data
    if (input$fiber_choice != "All Fibers") { 
      #prev_ds <- textiles_cleaned[FALSE,]
      prev_ds <- name_filtered %>% 
        filter(grepl(input$fiber_choice, textile_fiber_visual)) %>% 
          rbind(prev_ds) %>% 
            unique()
    }
    
    #Return filtered data
    if (nrow(prev_ds) == 0) { 
      name_filtered  
    }
    else { 
      prev_ds
    }
  })

  
  #Display images initially before any inputs are selected
  session$onFlushed(function() {
    image_urls <- textiles_cleaned$image_filename_app
    display_images(session, image_urls[1:PAGE_SIZE])
  })

  
  #Observe blocks split up to minimize redundant calling of reactive values
  
  #First observe determines if color filter should be added
  observe({ 
    #Convert vector of color inputs to list
    rv$filtered_color_input <- as.list(input$color_choice)
    
    #Check if there are any colors selected
    if (length(rv$filtered_color_input) == 0) { 
      rv$filter_colors = FALSE
    }
    else { 
      rv$filter_colors = TRUE
    }
  })
  
  #Second observe sets the page number back to 1 if image gallery has changed
  observe ({ 
    #If user wants modifiers to be "And-ed" together, call respective reactive function
    if (input$and_or == "AND") { 
      filtered_data <- filtered_and()
    }  
    #If user wants modifiers to be "Or-ed" together, call respective reactive function
    else { 
      filtered_data <- filtered_or()  
    }
    
    #If an input was changed so that a new set of images is being shown, return user to first page
    if (rv$prev_filtered_rows != nrow(filtered_data)) {
      rv$page_number <- 1  
      rv$prev_filtered_rows <- nrow(filtered_data)
    }
  })
    
  #Third observe displays the contents of the current page if inputs are changed or if page number is changed
  observe ({
    #If user wants modifiers to be "And-ed" together, call respective reactive function
    if (input$and_or == "AND") { 
      filtered_data <- filtered_and()
    }  
    #If user wants modifiers to be "Or-ed" together, call respective reactive function
    else { 
      filtered_data <- filtered_or()  
    }
    
    #Display number of images for given page
    start <- ((rv$page_number - 1) * rv$page_size) + 1
    
    #End of page index will either be the position of the last photo that fills 
    #up the page or the last photo of the gallery
    end <- min(rv$page_number * rv$page_size, nrow(filtered_data))
    
    display_images(session, filtered_data$image_filename_app[start:end])
  })
  
  
  #Produce text for page information
  output$page_details <- renderText({ 
    #If user wants modifiers to be "And-ed" together, call respective reactive function
    if (input$and_or == "AND") { 
      filtered_data <- filtered_and()
    }  
    #If user wants modifiers to be "Or-ed" together, call respective reactive function
    else { 
      filtered_data <- filtered_or()  
    }
    
    pages_needed <- ceiling(nrow(filtered_data) / rv$page_size)
    
    #Display what number page user is on out of the total number of pages
    paste("Page ", rv$page_number, " of ", pages_needed)
  })
  
  
  #Check for previous page call and decrement page number if there is a previous page
  observeEvent (input$prev_page, { 
    if (rv$page_number > 1) { 
      rv$page_number <- rv$page_number - 1
    }  
  })
  
  
  #Check for next page call and increment page number if there is a next page
  observeEvent (input$next_page, { 
    if (input$and_or == "AND") { 
      filtered_data <- filtered_and()
    }
    else { 
      filtered_data <- filtered_or()
    }
    
    #Have not reached end of images yet so page can be incremented
    if (rv$page_number * rv$page_size < nrow(filtered_data)) { 
      rv$page_number <- rv$page_number + 1  
    }
  })
  
  observeEvent (input$previous_image, { 
    
    
    
  })
  
  
  #Check for reset button click
  observeEvent (input$reset_button, { 
    updateSelectInput(session, "textile_name", selected = "All Names")
    updateRadioGroupButtons(session, "and_or", selected = "AND")
    updateSelectInput(session, "color_choice", selected = "")
    updateSelectInput(session, "pattern_choice", selected = "All Patterns")
    updateSelectInput(session, "process_choice", selected = "All Processes")
    updateSelectInput(session, "weave_choice", selected = "All Weaves")
    updateSelectInput(session, "fiber_choice", selected = "All Fibers")
    rv$page_number <- 1
  })
  
  
  #Pass output to UI for displaying
  output$images <- renderUI({ 
    tags$div(id = "image_gallery", class = "image-gallery")
  })
  
  
  #Check if user has clicked on an image
  observeEvent(input$selected_image, { 
    
    #Update reactive value of index in df of selected image
    if (input$and_or == "AND") { 
      filtered_data <- filtered_and()
    }
    else { 
      filtered_data <- filtered_or()
    }
    
    selected_info <- textiles_cleaned %>% 
      filter(image_filename_app == input$selected_image)
    
    selected_identifier <- selected_info$textile_identifier
    print(selected_identifier)
    rv$image_index <- which(filtered_data$textile_identifier == selected_identifier)
    
    #Call function to show modal dialog with information and image
    showImageDescription(input$selected_image, textiles_cleaned, GLOSSARY_TEXTILES_LIST, rv, output)
  })
  
  #Reopens first modaldialog when zoomed image is closed
  observeEvent(input$reopen_dialog_button, {
    
    #First check is to see if the modal dialog that has been closed is the zoomed version of the image(s)
    if (rv$zoomed_dialog_open == TRUE) { 
      
      #Check to see if the zoomed modal that was closed was the comparison images or image descriptor zoomed
      if (rv$zoomed_comparison_open == FALSE) { 
        session$sendCustomMessage(type = "update_zoomed_input", "")
        showImageDescription(input$selected_image, textiles_cleaned, GLOSSARY_TEXTILES_LIST, rv, output)
      }
      
      #Modal closed was zoomed comparison tool so retrieve data about textiles and redisplay them
      else { 
        textile_1_url <- "silkstuffs_01"
        if (!is.null(input$textile_id_1)) { 
          textile_1_url <- input$textile_id_1
        }
        textile_2_url <- "noDTTPdata_01"
        if (!is.null(input$textile_id_2)) { 
          textile_2_url <- input$textile_id_2
        }
        session$sendCustomMessage(type = "update_comparison_input", "")
        showComparison(textile_1_url, textile_2_url, textiles_cleaned, GLOSSARY_TEXTILES_LIST, rv)
      }
    }
    
    #Reset reactive values to FALSE since zoomed dialogs are no longer open
    rv$zoomed_dialog_open = FALSE
    rv$zoomed_comparison_open = FALSE
  })
  
  #Check for user comparison between textiles
  observeEvent(input$comparison_button, {   
    showComparison("silkstuffs_01", "noDTTPdata_01", textiles_cleaned, GLOSSARY_TEXTILES_LIST, rv)
  })
  
  #Checks if user clicked on comparison tool link while viewing a textile description
  observeEvent(input$comparison_tool, { 
    #Calls comparison tool function
    showComparison(rv$comparison_selection, "noDTTPdata_01", textiles_cleaned, GLOSSARY_TEXTILES_LIST, rv)
  })
  
  #Checks for new selection of first textile within comparison tool and updates image/details accordingly
  observeEvent(input$textile_id_1,  { 
    textile_2_url <- "noDTTPdata_01"
    if (!is.null(input$textile_id_2)) { 
      textile_2_url <- input$textile_id_2
    }
    removeModal()
    showComparison(input$textile_id_1, textile_2_url, textiles_cleaned, GLOSSARY_TEXTILES_LIST, rv)
  })
  
  #Checks for new selection of second textile within comparison tool and updates image/details accordingly
  observeEvent(input$textile_id_2, { 
    textile_1_url <- "silkstuffs_01"
    if (!is.null(input$textile_id_1)) { 
      textile_1_url <- input$textile_id_1
    }
    removeModal()
    showComparison(textile_1_url, input$textile_id_2, textiles_cleaned, GLOSSARY_TEXTILES_LIST, rv)
  })

  
  #Display zoomed version of image previously clicked
  observeEvent(input$zoomed_image, { 
    selected_url <- input$zoomed_image
    if (!is.na(selected_url)) { 
    
      rv$zoomed_dialog_open = TRUE
      
      #Retrieve information of image clicked on by user
      selected_info <- textiles_cleaned %>% 
        filter(image_filename_app == selected_url)
      
      #Show zoomed image by filling new modalDialog pop-up window
      showModal(modalDialog( 
        class = "zoomed-description",
        tags$img(
          src = selected_url
          #style = "min-height: 400px; width: auto; max-width: 100%; max-height: 800px; margin: auto;", 
        ),
        size = "l", 
        footer = modalButton("Close")
      ))
    }
  })
  
  #User has clicked on one of images in comparison tool
  observeEvent(input$zoomed_comparison, { 
    
    if (!is.na(input$zoomed_comparison)) { 
      #Separate image urls and store them in vector
      images <- input$zoomed_comparison %>% 
        strsplit(" ")
      images <- images[[1]]
      
      image_1 <- images[1]
      image_2 <- images[2]
      
      showZoomedComparison(image_1, image_2, rv)
    }
  })
}




#Run the app
shinyApp(ui = ui, server = server)
