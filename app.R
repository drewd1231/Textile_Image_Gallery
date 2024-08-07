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
library(shinyBS)


source("functions.R")

#Page size global
PAGE_SIZE = 9

#Set working directory to correct folder
#setwd("/Users/drew/Documents/Work/Summer24/textiles_app")

#Read in data
#textiles_data <- read_excel("MaterialDataset01302024.xlsx")
textiles_data <- read_excel("MaterialDataset_08-01-24.xlsx")

#Filter out any textiles without images
textiles_cleaned <- textiles_data %>% 
  filter(image_filename_app != "NA")
textiles_cleaned <- textiles_cleaned[order(textiles_cleaned$mat_no),]

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

NAME_LIST <- names_cleaned$textile_name

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
  
  titlePanel(title = ""), 
  
  
  #Create each input possibility
  sidebarPanel(
    
    #Allows user to filter images by name AND other modifiers or not
    radioGroupButtons("and_or", "Select Modifier Operator",
                      choices = c("AND", "OR"), 
                      selected = "AND", 
                      justified = TRUE),
    
    #Allows user to select textile name to filter by
    tags$div(class = "backspace_name",
      selectInput("textile_name", "Search by textile name", 
                  choices = c("All Names", NAME_LIST), 
                  selected = "All Names")
    ), 
  
    selectInput("color_choice", "Search by color(s)", 
                choices = COLOR_LIST, 
                multiple = TRUE),
    
    tags$div(class = "backspace_pattern",
      #Allows user to select pattern to filter by
      selectInput("pattern_choice", "Search by pattern(s)", 
                  choices = PATTERN_LIST, 
                  multiple = TRUE)
    ),
    
    tags$div(class = "backspace_process",
      #Allows user to select process to filter by
      selectInput("process_choice", "Search by process(s)", 
                  choices = PROCESS_LIST, 
                  multiple = TRUE)
    ),
    
    tags$div(class = "backspace_weave",
      #Allows user to select weave to filter by
      selectInput("weave_choice", "Search by weave structure(s)", 
                  choices = WEAVE_LIST, 
                  multiple = TRUE)
    ),
    
    tags$div(class = "backspace_fiber",
      #Allows user to select fiber to filter by
      selectInput("fiber_choice", "Search by fiber(s)", 
                  choices = FIBER_LIST, 
                  multiple = TRUE)
    ),
    
    #Activates comparison tool
    actionButton("comparison_button", "Compare two textiles by material id "),
    
    tags$hr(),
    actionButton("reset_button", "Reset all inputs"), 
    
    
    
  ), 
  
  mainPanel(
    #Creates ui shell for image gallery
    uiOutput("images"), 
    #tags$div(id = "images"),
    
    #Creates buttons for users to change pages
    actionButton("prev_page", "Previous", 
                 style = "margin-top: 10px; margin-bottom: 5px;"),
    actionButton("next_page", "Next", 
                 style = "margin-top: 10px; margin-bottom: 5px;"), 
    
    #Displays what page user is on and how many pages there are total for input
    textOutput("page_details") 
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
  
  #initialize reactive values that will be passed between observe/reactive blocks
  rv <- reactiveValues(filter_colors = FALSE, 
                       filtered_color_input = list(), 
                       filtered_data = textiles_cleaned, 
                       filtered_pattern_input = list(), 
                       filter_patterns = FALSE, 
                       filtered_process_input = list(), 
                       filter_processes = FALSE, 
                       filter_weave_input = list(), 
                       filter_weaves = FALSE, 
                       filtered_fiber_input = list(), 
                       filter_fibers = FALSE,
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
    if (rv$filter_patterns == TRUE) { 
        for (pattern in rv$filtered_pattern_input) { 
          curr_filtered <- curr_filtered %>% 
            filter(grepl(pattern, textile_pattern_visual))
        }
    }
    
    #Filters data based on process
    if (rv$filter_processes == TRUE) { 
      for (process in rv$filtered_process_input) {
        curr_filtered <- curr_filtered %>% 
          filter(grepl(process, textile_process_visual))
      }
    }
    
    #Filters data based on weave
    if (rv$filter_weaves == TRUE) { 
      for (weave in rv$filtered_weave_input) {
        curr_filtered <- curr_filtered %>% 
          filter(grepl(weave, textile_weave_visual))
      }
    }
    
    #Filters data based on fiber
    if (rv$filter_fibers == TRUE) { 
      for (fiber in rv$filtered_fiber_input) {
        curr_filtered <- curr_filtered %>% 
          filter(grepl(fiber, textile_fiber_visual))
      }
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
    if (rv$filter_patterns == TRUE) { 
      #prev_ds <- textiles_cleaned[FALSE,]
      for (pattern in rv$filtered_pattern_input) { 
        prev_ds <- name_filtered %>% 
          filter(grepl(pattern, textile_pattern_visual)) %>% 
            rbind(prev_ds) %>% 
              unique()
      }
    }
    
    #Filter data based on name and process then combine with previously filtered data
    if (rv$filter_processes == TRUE) { 
      for (process in rv$filtered_process_input) { 
        prev_ds <- name_filtered %>% 
          filter(grepl(process, textile_process_visual)) %>% 
            rbind(prev_ds) %>% 
              unique()
      }
    }
    
    #Filter data based on name and weave then combine with previously filtered data
    if (rv$filter_weaves == TRUE) { 
      for (weave in rv$filtered_weave_input) {
        prev_ds <- name_filtered %>% 
          filter(grepl(weave, textile_weave_visual)) %>% 
            rbind(prev_ds) %>% 
              unique()
      }
    }
    
    #Filter data based on name and fiber then combine with previously filtered data
    if (rv$filter_fibers == TRUE) { 
      for (fiber in rv$filtered_fiber_input) {
        prev_ds <- name_filtered %>% 
          filter(grepl(fiber, textile_fiber_visual)) %>% 
            rbind(prev_ds) %>% 
              unique()
      }
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
  
  #First observe determines if filters should be added for each modifier (if there are inputs)
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
    
    
    #Repeat process for other possible inputs
    rv$filtered_pattern_input <- as.list(input$pattern_choice)
    if (length(rv$filtered_pattern_input) == 0) { 
      rv$filter_patterns = FALSE
    }
    else { 
      rv$filter_patterns = TRUE  
    }
    
    
    rv$filtered_process_input <- as.list(input$process_choice)
    if (length(rv$filtered_process_input) == 0) { 
      rv$filter_processes = FALSE
    }
    else { 
      rv$filter_processes = TRUE
    }
    
    
    rv$filtered_weave_input <- as.list(input$weave_choice)
    if (length(rv$filtered_weave_input) == 0) { 
      rv$filter_weaves = FALSE  
    }
    else { 
      rv$filter_weaves = TRUE  
    }
    
    
    rv$filtered_fiber_input <- as.list(input$fiber_choice)
    if (length(rv$filtered_fiber_input) == 0) { 
      rv$filter_fibers = FALSE  
    }
    else { 
      rv$filter_fibers = TRUE  
    }
  })
  
  #Second observe sets the page number back to 1 if image gallery has changed
  #Also updates filtered_data reactive value if any changes are made to inputs
  observe ({ 
    #If user wants modifiers to be "And-ed" together, call respective reactive function
    if (input$and_or == "AND") { 
      rv$filtered_data <- filtered_and()
    }  
    #If user wants modifiers to be "Or-ed" together, call respective reactive function
    else { 
      rv$filtered_data <- filtered_or()  
    }
    
    #If an input was changed so that a new set of images is being shown, return user to first page
    if (rv$prev_filtered_rows != nrow(rv$filtered_data)) {
      rv$page_number <- 1  
      rv$prev_filtered_rows <- nrow(rv$filtered_data)
    }
  })
    
  #Third observe displays the contents of the current page if inputs are changed or if page number is changed
  observe ({
    
    #Display number of images for given page
    start <- ((rv$page_number - 1) * rv$page_size) + 1
    
    #End of page index will either be the position of the last photo that fills 
    #up the page or the last photo of the gallery
    end <- min(rv$page_number * rv$page_size, nrow(rv$filtered_data))
    
    display_images(session, rv$filtered_data$image_filename_app[start:end])
  })
  
  
  #Produce text for page information
  output$page_details <- renderText({ 
    
    #Calculate how many pages are needed to show number of images for given inputs
    pages_needed <- ceiling(nrow(rv$filtered_data) / rv$page_size)
    
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
    #Have not reached end of images yet so page can be incremented
    if (rv$page_number * rv$page_size < nrow(rv$filtered_data)) { 
      rv$page_number <- rv$page_number + 1  
    }
  })
  
  #User has clicked on previous button within modal dialog
  observeEvent (input$previous_image, { 
    if (rv$image_index > 1) { 
      rv$image_index <- rv$image_index - 1
    }

    #Check if current selected textile is at beginning of page, then page needs to be changed
    if ((rv$image_index != 1) & (((rv$image_index) %% 9) == 0)) { 
      rv$page_number <- rv$page_number - 1
    }
    
    #Get new selection using decremented index and call function to display description
    new_selection <- rv$filtered_data[rv$image_index,]
    showImageDescription(new_selection$image_filename_app, textiles_cleaned, GLOSSARY_TEXTILES_LIST, rv, output)
  })
  
  #User has clicked on next button within modal dialog
  observeEvent (input$next_image, { 
    n <- nrow(rv$filtered_data)
    
    #Check that image selected is not the last of the textiles being shown
    if (rv$image_index < n) { 
      rv$image_index <- rv$image_index + 1
    }

    #Check if current selected textile is at beginning of page, then page needs to be changed
    if ((rv$image_index != n) & (((rv$image_index-1) %% 9) == 0)) { 
        rv$page_number <- rv$page_number + 1
    }
    
    #Get new selection using incremented index and call function to display description
    new_selection <- rv$filtered_data[rv$image_index,]
    showImageDescription(new_selection$image_filename_app, textiles_cleaned, GLOSSARY_TEXTILES_LIST, rv, output)
  })
  
  observeEvent (input$name_backspace, { 
    updateSelectInput(session, "textile_name", choices = c("All Names", NAME_LIST), selected = "All Names")  
  })
  
  
  #Observe block to change possible user selections when textile name is chosen
  observeEvent(input$textile_name, { 
    
    #Get all rows of data with matching textile name
    curr_data <- textiles_cleaned %>% 
      filter(textile_name == input$textile_name)
    
    if (input$textile_name == "All Names") { 
      #Set all select inputs to regular options
      updateSelectInput(session, "color_choice", choices = COLOR_LIST, selected = "")
      updateSelectInput(session, "pattern_choice", choices = PATTERN_LIST, selected = "")
      updateSelectInput(session, "process_choice", choices = PROCESS_LIST, selected = "")
      updateSelectInput(session, "weave_choice", choices = WEAVE_LIST, selected = "")
      updateSelectInput(session, "fiber_choice", choices = FIBER_LIST, selected = "")
    }
    else { 
      #User has selected specific textile
      color_lst <- list()
      #Loop through color list and check if each is present in current set of data
      for (color in COLOR_LIST) {
        if(length(grep(color, curr_data$textile_color_visual)) > 0) { 
          #If color is present, append to color list
          color_lst <- c(color_lst, list(color))
        }
      }
      #Update select input box options with colors seen in this data set
      updateSelectInput(session, "color_choice", choices = color_lst, selected = "")

      #Repeat process for patterns
      pattern_lst <- list()
      for (pattern in PATTERN_LIST) {
        if(length(grep(pattern, curr_data$textile_pattern_visual)) > 0) {
          pattern_lst <- c(pattern_lst, list(pattern))
        }
      }
      updateSelectInput(session, "pattern_choice", choices = pattern_lst, selected = "")
      
      #Repeat process for processes
      process_lst <- list()
      for (process in PROCESS_LIST) { 
        if (length(grep(process, curr_data$textile_process_visual)) > 0) { 
          process_lst <- c(process_lst, list(process))  
        }  
      }
      updateSelectInput(session, "process_choice", choices = process_lst, selected = "")
      
      #Repeat process for weaves
      weave_lst <- list()
      for (weave in WEAVE_LIST) { 
        if (length(grep(weave, curr_data$textile_weave_visual)) > 0) { 
          weave_lst <- c(weave_lst, list(weave))  
        }  
      }
      updateSelectInput(session, "weave_choice", choices = weave_lst, selected = "")
      
      #Repeat process for fibers
      fiber_lst <- list()
      for (fiber in FIBER_LIST) { 
        if (length(grep(fiber, curr_data$textile_fiber_visual)) > 0) { 
          fiber_lst <- c(fiber_lst, list(fiber))  
        }  
      }
      updateSelectInput(session, "fiber_choice", choices = fiber_lst, selected = "")
    }
    
  })
  
  
  #Check for reset button click - reset all inputs if so
  observeEvent (input$reset_button, { 
    updateSelectInput(session, "textile_name", choices = c("All Names", NAME_LIST), selected = "All Names")
    updateRadioGroupButtons(session, "and_or", selected = "AND")
    updateSelectInput(session, "color_choice", choices = COLOR_LIST, selected = "")
    updateSelectInput(session, "pattern_choice", choices = PATTERN_LIST, selected = "")
    updateSelectInput(session, "process_choice", choices = PROCESS_LIST, selected = "")
    updateSelectInput(session, "weave_choice", choices = WEAVE_LIST, selected = "")
    updateSelectInput(session, "fiber_choice", choices = FIBER_LIST, selected = "")
    rv$page_number <- 1
  })
  
  
  #Pass output to UI for displaying
  output$images <- renderUI({ 
    tags$div(id = "image_gallery", class = "image-gallery") 
  })
  
  #Check if user has clicked on an image
  observeEvent(input$selected_image, { 
    
    selected_info <- textiles_cleaned %>% 
      filter(image_filename_app == input$selected_image)
    
    #Update index of image we have selected
    selected_identifier <- selected_info$textile_identifier
    rv$image_index <- which(rv$filtered_data$textile_identifier == selected_identifier)
    
    #Call function to show modal dialog with information and image
    showImageDescription(input$selected_image, textiles_cleaned, GLOSSARY_TEXTILES_LIST, rv, output)
  })
  
  #Reopens first modaldialog when zoomed image is closed
  observeEvent(input$reopen_dialog_button, {
    
    #First check is to see if the modal dialog that has been closed is the zoomed version of the image(s)
    if (rv$zoomed_dialog_open == TRUE) { 
      
      #Check to see if the zoomed modal that was closed was the comparison images or image descriptor zoomed
      if (rv$zoomed_comparison_open == FALSE) { 
        #Let js know that zoomed window has been closed
        session$sendCustomMessage(type = "update_zoomed_input", "")
        
        #Show image description modal dialog
        new_selection <- rv$filtered_data[rv$image_index,]
        showImageDescription(new_selection$image_filename_app, textiles_cleaned, GLOSSARY_TEXTILES_LIST, rv, output)
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
        
        #Lets js know that the zoomed comparison of the images is open
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
    
    #Checks if selection has already been made for textile two
    if (!is.null(input$textile_id_2)) { 
      textile_2_url <- input$textile_id_2
    }
    removeModal()
    showComparison(input$textile_id_1, textile_2_url, textiles_cleaned, GLOSSARY_TEXTILES_LIST, rv)
  })
  
  #Checks for new selection of second textile within comparison tool and updates image/details accordingly
  observeEvent(input$textile_id_2, { 
    textile_1_url <- "silkstuffs_01"
    
    #Checks if selection has already been made for textile one
    if (!is.null(input$textile_id_1)) { 
      textile_1_url <- input$textile_id_1
    }
    removeModal()
    showComparison(textile_1_url, input$textile_id_2, textiles_cleaned, GLOSSARY_TEXTILES_LIST, rv)
  })

  
  #Display zoomed version of image previously clicked
  observeEvent(input$zoomed_image, { 
    selected_url <- input$zoomed_image
    #Make sure input exists
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
      
      #Call zoomed comparison function with retrieved images
      showZoomedComparison(image_1, image_2, rv)
    }
  })
}




#Run the app
shinyApp(ui = ui, server = server)
