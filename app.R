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
fiber_list <- c("silk", "cotton", "wool", "linen")

#Define UI
ui <- fluidPage( 
  useShinyjs(),
  
  theme = shinytheme("sandstone"),
  
  titlePanel(title = "Materials Sample Search"), 
  
  #Create each input possibility
  sidebarPanel(
    h3("The Material Record"), 
    
    selectInput("textile_name", "Search by textile name", 
                choices = c("All Names", names_cleaned$textile_name), 
                selected = "All Names"), 
    
    #Allows user to filter images by name AND other modifiers or not
    radioGroupButtons("and_or", "Select Modifiers",
                 choices = c("AND", "OR"), 
                 selected = "AND", 
                 justified = TRUE),
    
    #Allows user to select color to filter by
    selectInput("color_choice", "Search by color(s)", 
                choices = c("All Colors", color_list)),
    
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
    uiOutput("images")
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
           //Appends each image object to the "image_gallery"
           $("#image_gallery").append(img);
        });
       }
       
      $(document).on("click", ".zoomed_image", function() { 
        var path_name = $(this).data("path");
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
  
  #Reactive function we want to call when user selects "AND"
  filtered_and <- reactive({ 
    curr_filtered <- textiles_cleaned
    
    #Filters data based on name
    if (input$textile_name != "All Names") { 
      curr_filtered <- curr_filtered %>% 
        filter(textile_name == input$textile_name)
    }
    
    #Filters data based on color
    if (input$color_choice != "All Colors") { 
      curr_filtered <- curr_filtered %>% 
        filter(grepl(input$color_choice, textile_color_visual))
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
    
    #Filter data based on name and color then combine with previously filtered data
    if (input$color_choice != "All Colors") { 
      curr_filtered <- name_filtered %>% 
        filter(grepl(input$color_choice, textile_color_visual)) %>% 
          rbind(curr_filtered) %>% 
            unique()
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
    session$sendCustomMessage(type = "update_images", 
                              message = list(image_urls = image_urls))
  })

  #Wait for any inputs to occur  
  observe({ 
    #If user wants modifiers to be "And-ed" together, call respective reactive function
    if (input$and_or == "AND") { 
      filtered_data <- filtered_and()
    }  
    #If user wants modifiers to be "Or-ed" together, call respective reactive function
    else { 
      filtered_data <- filtered_or()  
    }
    
    #Call function to display images to users screen
    display_images(filtered_data$image_filename_app)
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
    
    collection_image_info <- paste(selected_info$collection, selected_info$id_no, selected_info$image_filename_orig, sep = ", ")
    
    #Print details of image clicked on
    output$textile_details <- renderUI({ 
      tagList( 
        tags$p(strong("Textile Name: "), selected_info$textile_name), 
        tags$p(strong("Color: "), selected_info$textile_color_visual), 
        tags$p(strong("Fiber: "), selected_info$textile_fiber_visual), 
        tags$p(strong("Pattern: "), selected_info$textile_pattern_visual),
        tags$p(strong("Process: "), selected_info$textile_process_visual), 
        tags$p(strong("Weave: "), selected_info$textile_weave_visual), 
        tags$p(strong("Date: "), selected_info$orig_date), 
        tags$p(strong("Additional Info: "), selected_info$addtl_info), 
        tags$p(strong("Collection/image file: "), collection_image_info)
      )
    })
    
    showModal(modalDialog( 
      title = "Textile Description", 
      
      #Creates custom layout of modaldialog
      tags$div(
        #Aligns details and image around center, puts gap in between 
        style = "display: flex; align-items: center; gap: 10px",
        
        #Displays the details of each textile clicked on
        tags$div(
          uiOutput("textile_details")
        ),
        
        #Display image within modal of fixed width (auto height to retain aspect-ratio)
        tags$img(
          src = selected_url, 
          style = "margin-right: 10px; height: 300px; width: auto;", 
          class = "zoomed_image", 
          'data-path' = selected_url
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
    
    showModal(modalDialog( 
      tags$img(
        src = selected_url, 
        style = "max-width: 100%; height: auto;"
      ),
      size = "l"
    ))
  })
}


#Run the app
shinyApp(ui = ui, server = server)

