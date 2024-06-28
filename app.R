library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(jpeg)
library(grid)
library(gridExtra)
library(ggtern)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(shinyPagerUI)

#Set working directory to correct folder
setwd("/Users/drew/Documents/Work/Summer24/textiles_app")

#Read in data
textiles_data <- read_excel("MaterialDataset01302024.xlsx")

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
color_list <- c("blue", "white", "red", "black", "gold", "natural", "yellow", "green", "purple", "orange", "brown", "pink", "grey") %>% 
  sort()

#Hardcoded list of pattern options
pattern_list <- c("floral", "geometric", "checkered", "striped", "plain", "figural", "none", "dots", "stars", "foliage") %>% 
  sort()

#Hardcoded list of process options
process_list <- c("painted", "block printed", "resist-dyed", "piece-dyed", "loom-patterned", "printed", "felted", "embroidered", "ikat", "dyed", "damask") %>% 
  sort()

#Hardcoded list of weave options
weave_list <- c("satin", "plainweave", "twill", "damask", "velvet", "complex weave", "plainweave with doubled threads", "plainweave, supplemental weft") %>% 
  sort()

#Hardcoded list of fiber options
fiber_list <- c("silk", "cotton", "wool", "linen") %>% 
  sort()

glossary_textiles_list <- c("chintz-kalamkari", "dongris", "gingham", "guinea cloth", "muslin", "nickanees", "perpetuanen", "platillas", "sail cloth", "slaaplakens")

#Define UI
ui <- fluidPage( 
  useShinyjs(),
  
  theme = shinytheme("sandstone"),
  
  titlePanel(title = "Materials Sample Search"), 
  
  #Create each input possibility
  sidebarPanel(
    h3("The Material Record"), 
    
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
                choices = color_list, 
                multiple = TRUE),
    
    #Allows user to select pattern to filter by
    selectInput("pattern_choice", "Search by pattern", 
                choices = c("All Patterns", pattern_list)),
    
    #Allows user to select process to filter by
    selectInput("process_choice", "Search by process", 
                choices = c("All Processes", process_list)),
    
    #Allows user to select weave to filter by
    selectInput("weave_choice", "Search by weave", 
                choices = c("All Weaves", weave_list)),
    
    #Allows user to select fiber to filter by
    selectInput("fiber_choice", "Search by fiber", 
                choices = c("All Fibers", fiber_list)),
    
    textOutput("name_out")
  ), 
  
  mainPanel(
    uiOutput("images"), 
    actionButton("prev_page", "Previous"),
    actionButton("next_page", "Next"), 
    textOutput("page_details")
  ), 
  tags$head(
    #Creates HTML layout for how images are displayed (4 columns, gap in between, etc.)
    tags$style(HTML("
      .image-gallery { 
        display: grid;
        grid-template-columns: repeat(3, 1fr);
        gap: 10px;
      }
      .gallery-image { 
        width: 100%;
        height: 250px;
        object-fit: cover; 
        //transition: transform 0.2sec;
      }
      .rotated { 
        transform: rotate(90deg);
      }
      
      //#custom-modal .modal-dialog { 
      //height: 1000px !important;
      //max-width: none !important;
      //}
      
      //#custom-modal .modal-content { 
      //height: 80vh;
      //}
      
      .modal-body { 
        display: flex;
        justify-content: space-between;
      }
      
      .content, .image-container { 
        flex: 1;
        padding: 10px;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      
      .image-container { 
        overflow: hidden;
      }
      
      .image-container img { 
        max-width: 100%;
        max-height: 100%;
        width: 300px;
        height: auto;
        margin-left: 10px;
        margin-right: 10px;
        margin-top: 20px;
        margin-bottom: 20px;
        transition: transform 0.5s ease;
      }
      
      .image-container img:hover { 
        transform: scale(1.5);
      }
      
      .modal-dialog { 
        max-width: 100%;
        height: 80%;
      }
      
    "))
  ),
    
    tags$script(HTML('
    
      Shiny.addCustomMessageHandler("update_images", function(message) { 
        update_images(message.image_urls);  
      });
    
       function update_images(image_urls) { 
        
         //Error Checks that image paths passed are in an array
         if (!Array.isArray(image_urls)) { 
          console.error("image_urls is not an array");
          return;
         }
        
        //Clears the gallery before we look for images we want
         $("#image_gallery").empty();
         
         //Loops through all urls passed by reactive R input
         image_urls.forEach(function(url) { 
           
           //Creates image object for each url and checks for click event
           var img = $("<img>").attr("src", url).addClass("gallery-image").click(function() { 
              Shiny.setInputValue("selected_image", url, {priority: "event"});
           });
           
           img.on("load", function() { 
            if (this.naturalHeight > this.naturalWidth) { 
              $(this).css("transform", "rotated");
            }
           });
           
           //Appends each image object to the "image_gallery"
           $("#image_gallery").append(img);
        });
       }
       
      //Check for clicks on image when first modal dialog pop-up
      $(document).on("click", ".zoomed_image", function() { 
        var path_name = $(this).data("path");
        //Tell shiny that image has been clicked on and we should now zoom in
        Shiny.setInputValue("zoomed_image", path_name)
      })
    ')
  ) 
)

#Define Server Logic
server <- function(input, output, session) { 
  
  #Function that sends message to js to update images
  display_images <- function(url_list) { 
    image_urls <- as.list(url_list)
    
    #Sends message to js to call update_images function
    session$sendCustomMessage(type = "update_images", 
                              message = list(image_urls = image_urls))
  }
  
  #initialize reactive values that will be passed from observe block to reactive block
  rv <- reactiveValues(filter_colors = FALSE, 
                       filtered_color_input = list(), 
                       page_number = 1, 
                       page_size = 9)
  
  #Reactive function we want to call when user selects "AND"
  filtered_and <- reactive({ 
    #print(filter_colors)
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

    curr_filtered <- name_filtered
    
    #Check if there are colors to be filtered by
    if (rv$filter_colors == TRUE) {
      
      #Loop through colors in input list and filter the data by each one
      for (color in rv$filtered_color_input) {
        curr_filtered <- curr_filtered %>%
          filter(grepl(color, textile_color_visual)) %>% 
            rbind(curr_filtered) %>% 
              unique()
      }
    }
    
    #Filter data based on name and pattern then combine with previously filtered data
    if (input$pattern_choice != "All Patterns") { 
      curr_filtered <- name_filtered %>% 
        filter(grepl(input$pattern_choice, textile_pattern_visual)) %>% 
          rbind(curr_filtered) %>% 
            unique()
    }
    
    #Filter data based on name and process then combine with previously filtered data
    if (input$process_choice != "All Processes") { 
      curr_filtered <- name_filtered %>% 
        filter(grepl(input$process_choice, textile_process_visual)) %>% 
          rbind(curr_filtered) %>% 
            unique()
    }
    
    #Filter data based on name and weave then combine with previously filtered data
    if (input$weave_choice != "All Weaves") { 
      curr_filtered <- name_filtered %>% 
        filter(grepl(input$weave_choice, textile_weave_visual)) %>% 
          rbind(curr_filtered) %>% 
            unique()
    }
    
    #Filter data based on name and fiber then combine with previously filtered data
    if (input$fiber_choice != "All Fibers") { 
      curr_filtered <- name_filtered %>% 
        filter(grepl(input$fiber_choice, textile_fiber_visual)) %>% 
          rbind(curr_filtered) %>% 
            unique()
    }
    
    #Return filtered data
    curr_filtered
  })

  
  #Display images initially before any inputs are selected
  session$onFlushed(function() {
    image_urls <- textiles_cleaned$image_filename_app
    #HARDCODED (sketchy??)
    display_images(image_urls[1:9])
  })

  #Wait for any inputs to occur  
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
    
    #Call function to display images to users screen
    display_images(filtered_data$image_filename_app[start:end])
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
    
    if (rv$page_number * rv$page_size < nrow(filtered_data)) { 
      rv$page_number <- rv$page_number + 1  
    }
  })
  
  
  
  #Pass output to UI for displaying
  output$images <- renderUI({ 
    #Provides R with the HTML output produced by the js
    tags$div(id = "image_gallery", class = "image-gallery")
  })
  
  #Check if user has clicked on an image
  observeEvent(input$selected_image, { 
    selected_url <- input$selected_image
    
    #Retrieve information of image clicked on by user
    selected_info <- textiles_cleaned %>% 
      filter(image_filename_app == selected_url)
    
    #Various information about source of the textile in one string
    collection_image_info <- paste(selected_info$collection, selected_info$id_no, selected_info$image_filename_orig, sep = ", ")
    
    #Split the textiles name up into vector if it has multiple names within it
    find_name <- strsplit(selected_info$textile_name, split = ", ") %>% 
      unlist() %>% 
        strsplit(split = "/") %>% 
          unlist()
    
    #Get first name listed if there are multiple names
    find_name <- find_name[1]
    
    #Check if the name found is within the names in the glossary
    glossary_page <- grep(find_name[1], glossary_textiles_list)
    
    #If name is in glossary, record how it is labeled on the website
    in_glossary <- glossary_textiles_list[glossary_page[1]]
    
    #Put together link if glossary page for textile exists
    if (!is.na(in_glossary)) { 
      glossary_url <- paste("https://dutchtextiletrade.org/textiles/", in_glossary, "/", sep = "")
    }
    else{ 
      glossary_url <- "https://dutchtextiletrade.org/textiles/"
    }
      
    #Print details of image clicked on
    output$textile_details <- renderUI({ 
      tagList( 
        tags$p(strong("Textile Name: "), selected_info$textile_name), 
        tags$p(strong("Text from source: "), selected_info$text_source),
        tags$p(strong("Color: "), selected_info$textile_color_visual), 
        tags$p(strong("Fiber: "), selected_info$textile_fiber_visual), 
        tags$p(strong("Pattern: "), selected_info$textile_pattern_visual),
        tags$p(strong("Process: "), selected_info$textile_process_visual), 
        tags$p(strong("Weave: "), selected_info$textile_weave_visual), 
        tags$p(strong("Date: "), selected_info$orig_date), 
        tags$p(strong("Additional Info: "), selected_info$addtl_info), 
        tags$p(strong("Collection/image file: "), collection_image_info), 
        tags$p(strong("Search for "), strong(selected_info$textile_name), strong("in: ")),
        tags$a(href = "https://dutchtextiletrade.org/projects/textile-geographies/", strong("Map App")),
        tags$p(),
        tags$a(href = "https://dutchtextiletrade.org/projects/textiles-modifiers-and-values/", strong("Values App")),
        tags$p(),
        tags$a(href = glossary_url, strong("Glossary"))
      )
    })
    
    showModal(modalDialog( 
      
      title = "Image Information",
      
      #NEW CHANGES:::
      div(
        class = "modal-body", 
        div(
          class = "content", 
          uiOutput("textile_details")
        ), 
        div(
          class = "image-container", 
          img(
            src = selected_url, 
            #height = "300px", 
            # width = "80%", 
            class = "zoomed_image",
            'data-path' = selected_url
            #max-height = "300px"
          )
        )
      ),
      
      
      
      size = "l", 
      easyClose = TRUE
    ))
  })
  
  #Display zoomed version of image previously clicked
  observeEvent(input$zoomed_image, { 

    selected_url <- input$zoomed_image
    
    #Retrieve information of image clicked on by user
    selected_info <- textiles_cleaned %>% 
      filter(image_filename_app == selected_url)
    
    #Show zoomed image by filling new modalDialog pop-up window
    showModal(modalDialog( 
      tags$img(
        src = selected_url, 
        style = "width: auto; max-width: 100%; max-height: 800px; margin: auto;", 
      ),
      size = "l"
    ))
  })
}


#Run the app
shinyApp(ui = ui, server = server)



#Creates custom layout of modaldialog
# tags$div(
#   #Aligns details and image around center, puts gap in between 
#   style = "display: flex; align-items: center; gap: 10px",
#   
#   #Displays the details of each textile clicked on
#   tags$div(
#     uiOutput("textile_details")
#   ),
#   
#   #Display image within modal of fixed width (auto height to retain aspect-ratio)
#   tags$img(
#     src = selected_url, 
#     style = "height: 300px; width: auto; max-width: 70%", #margin-right: 10px; 
#     class = "zoomed_image", 
#     'data-path' = selected_url
#   )
# ), 