#CONTAINS ALL HELPER FUNCTIONS USED IN APP.R FILE



#Function that sends message to js to update images
display_images <- function(session, url_list) { 
  image_urls <- as.list(url_list)
  
  #Sends message to js to call update_images function
  session$sendCustomMessage(type = "update_images", 
                            message = list(image_urls = image_urls))
}


#Function used to retrieve details of textile clicked on by user
get_image_details <- function(image_selection, GLOSSARY_TEXTILES_LIST, rv) { 
  
  rv$comparison_selection <- image_selection$textile_identifier
  #Various information about source of the textile in one string
  collection_image_info <- paste(image_selection$collection, image_selection$id_no, image_selection$image_filename_orig, sep = ", ")
  
  #Split the textiles name up into vector if it has multiple names within it
  find_name <- strsplit(image_selection$textile_name, split = ", ") %>% 
    unlist() %>% 
    strsplit(split = "/") %>% 
    unlist()
  
  #Get first name listed if there are multiple names
  find_name <- find_name[1]
  
  #Check if the name found is within the names in the glossary
  glossary_page <- grep(find_name[1], GLOSSARY_TEXTILES_LIST)
  
  #If name is in glossary, record how it is labeled on the website
  in_glossary <- GLOSSARY_TEXTILES_LIST[glossary_page[1]]
  
  #Put together link if glossary page for textile exists
  if (!is.na(in_glossary)) { 
    glossary_url <- paste("https://dutchtextiletrade.org/textiles/", in_glossary, "/", sep = "")
  }
  else{ 
    glossary_url <- "https://dutchtextiletrade.org/textiles/"
  }
  
  #Get URL of collection or image (depending on which is given)
  url_substring_exists <- grep("https", image_selection$catalogue_url)
  
  if (length(url_substring_exists > 0)) { 
    catalogue_image_url <- image_selection$catalogue_url
  }
  else { 
    catalogue_image_url <- image_selection$catalogue_url_image  
  }
  
  #Set project query string equal to textile name (ONLY WORKS IN SOME CASES, NAMES IN VALUES APP ARENT THE SAME?)
  values_link <- paste("https://dutchtextiletradeapps.shinyapps.io/values/?name=", image_selection$textile_name, sep ="")
  
  #Organizes textile information into taglist for side of image description
  tagList( 
    tags$p(strong("Textile Name: "), image_selection$textile_name), 
    tags$p(strong("Text from source: "), image_selection$text_source),
    tags$p(strong("Color: "), image_selection$textile_color_visual), 
    tags$p(strong("Fiber: "), image_selection$textile_fiber_visual), 
    tags$p(strong("Pattern: "), image_selection$textile_pattern_visual),
    tags$p(strong("Process: "), image_selection$textile_process_visual), 
    tags$p(strong("Weave: "), image_selection$textile_weave_visual), 
    tags$p(strong("Date: "), image_selection$orig_date), 
    tags$p(strong("Additional Info: "), image_selection$addtl_info), 
    tags$p(strong("Collection/image file: "), collection_image_info), 
    tags$p(strong("Collection/image URL: "), catalogue_image_url),
    tags$p(strong("Textile ID: "), image_selection$textile_identifier),
    tags$p(strong("Search for "), strong(image_selection$textile_name), strong("in: ")),
    tags$p(actionLink("comparison_tool", strong("Comparison Tool"))),
    tags$a(href = "https://dutchtextiletrade.org/projects/textile-geographies/", strong("Map App"), target = "_blank"),
    tags$p(),
    tags$a(href = values_link, strong("Values App"), target = "_blank"),
    tags$p(),
    tags$a(href = glossary_url, strong("Glossary"), target = "_blank")
  )  
}


#Displays pop-up containing both textile information as well as the image of the textile
showImageDescription <- function(selected_image, textiles_cleaned, GLOSSARY_TEXTILES_LIST, rv, output) { 
  selected_url <- selected_image
  
  #Retrieve information of image clicked on by user
  selected_info <- textiles_cleaned %>% 
    filter(image_filename_app == selected_url)
  
  #Store details of image clicked on in taglist
  output$textile_details <- renderUI({ 
    get_image_details(selected_info, GLOSSARY_TEXTILES_LIST, rv)
  })
  
  showModal(modalDialog( 
    title = "Image Information",
    div(
      class = "modal-body", 
      div(
        #Displays details about textile on left side of modal dialog
        class = "content", 
        uiOutput("textile_details")
      ), 
      div(
        #Creates space for image and caption below image
        class = "image-container", 
        img(
          src = selected_url, 
          class = "zoomed_image",
          #Provides javascript with the path to the image in case user wants zoomed version of the image
          'data-path' = selected_url
        ),
        div(class = "caption", strong("Click on image for full size"))
      ), 
    ),
    
    size = "l", 
    easyClose = TRUE
  ))
}


#Function to display modalDialog comparison tool with two textiles 
showComparison <- function(selected_textile_1, selected_textile_2, textiles_cleaned, GLOSSARY_TEXTILES_LIST, rv) {
  
  #Retrieve correct rows for first and second textile selections
  selected_info_1 <- textiles_cleaned %>% 
    filter(textile_identifier == selected_textile_1)
  
  selected_info_2 <- textiles_cleaned %>% 
    filter(textile_identifier == selected_textile_2)
  
  identifier_sorted <- textiles_cleaned$textile_identifier %>% 
    sort()
  
  #Create modalDialog
  showModal(modalDialog(
    title = "Textile Comparison", 
    div(
      #Creates div for outer container that will contain two containers within (one for each textile)
      class = "comparison-body", 
      #Creates div within outer container that will hold input selection, textile imag, and details below
      div(class = "content",
          selectInput("textile_id_1", "Select First Textile's ID:", choices = identifier_sorted, selected = selected_textile_1),
          
          div(class = "comparison-container", 
              #Displays image
              img(src = selected_info_1$image_filename_app, 
                  class = "zoomed_comparison", 
                  'data-path' = c(selected_info_1$image_filename_app, selected_info_2$image_filename_app)),
              
              #Retrieves details of textile to be displayed below image
              div(class = "caption", 
                  get_image_details(selected_info_1, GLOSSARY_TEXTILES_LIST, rv)
              )
              
          )
          
      ),
      div(class = "content", 
          selectInput("textile_id_2", "Select Second Textile's ID:", choices = identifier_sorted, selected = selected_textile_2),
          
          div(class = "comparison-container", 
              img(src = selected_info_2$image_filename_app, 
                  class = "zoomed_comparison", 
                  'data-path' = c(selected_info_1$image_filename_app, selected_info_2$image_filename_app)),
              
              div(class = "caption", 
                  get_image_details(selected_info_2, GLOSSARY_TEXTILES_LIST, rv)
              )
          )
      )
    ), 
    size = 'l'
  ))
}


#Function to show pop-up of images selected in comparison dialog
showZoomedComparison <- function(selected_image_1, selected_image_2, rv) { 
  #Update reactive vals about dialog accordingly
  rv$zoomed_dialog_open = TRUE
  rv$zoomed_comparison_open = TRUE
  
  #Display modal dialog with zoomed images side by side
  showModal(modalDialog(
    title = "Image Comparison", 
    div(
      class = "modal-body", 
      div(
        class = "zoomed-container", 
        img(src = selected_image_1)
      ), 
      div(
        class = "zoomed-container", 
        img(src = selected_image_2)
      )
    ), 
    size = "l"
  ))
}
