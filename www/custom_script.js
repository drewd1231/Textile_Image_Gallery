//Checks for shiny code looking to update image gallery
Shiny.addCustomMessageHandler("update_images", function(message) { 
  update_images(message.image_urls, message.textile_names);  
});


//Function that updates image gallery
function update_images(image_urls, textile_names) { 

  //Error Checks that image paths passed are in an array
  if (!Array.isArray(image_urls)) { 
    console.error("image_urls is not an array");
    return;
  }
  
  if (!Array.isArray(textile_names)) { 
    console.error("textile_names is not an array");
    return;
  }
  
  //Clears the gallery before we look for images we want
  $("#image_gallery").empty();
  
  //Check if there are no images matching input
  if (image_urls[0] === null) {
    var noImagesMessage = $("<h3>").text("No images match your criteria");
  
    //Add text to image_gallery object to tell user that search is invalid
    $("#image_gallery").append(noImagesMessage);
    return;
  }

  var count = 0;
  
  //Loops through all urls passed by reactive R input
  image_urls.forEach(function(url) { 
  
    //Creates image object for each url and checks for click event
    var img = $("<img>").attr("src", url).addClass("gallery-image").click(function() { 
      Shiny.setInputValue("selected_image", url, {priority: "event"});
    });
    
    //Changes users cursor when hovering above an image
    img.css("cursor", "pointer");
    
    
    var tt_container = $("<div>").addClass("tooltip-container");
    
    //Set pop-up text box to name of textile
    if (textile_names[count] === null) { 
      var tt_text = $("<span>").addClass("tooltip-text").text("No Name");
    }
    else { 
      var tt_text = $("<span>").addClass("tooltip-text").text(textile_names[count]);
    }
    
    
    
    tt_container.append(img).append(tt_text);
    //Appends each image object in tooltip container to the "image_gallery"
    $("#image_gallery").append(tt_container);
    
    count = count+1;
  });
}


//Series of checks for user deleting inputs
$(document).on("keydown", ".backspace_name", function(e) { 
  if (e.key === "Backspace") { 
    var select_input = $(this);
    
    if (select_input.val() === "") { 
      var rand_int = Math.random();
      Shiny.setInputValue("name_backspace", rand_int);
    }
  }
})

//Check for clicks on image when first modal dialog pop-up
$(document).on("click", ".zoomed_image", function() { 
  var path_name = $(this).data("path");
  //Tell shiny that image has been clicked on and we should now zoom in
  Shiny.setInputValue("zoomed_image", path_name)
})

//Checks for when modal dialogs are closed
$(document).on("hidden.bs.modal", ".modal", function(e) { 
  //Sets variable to random int so that shiny sees new input
  var rand_int = Math.random();
  
  //Passes value to shiny resetting input
  Shiny.setInputValue("reopen_dialog_button", rand_int);
});

//Checks for user click on one of images shown in comparison tool
$(document).on("click", ".zoomed_comparison", function() { 
  //Stores textile info with data path
  var path_name = $(this).data("path");
  //Tell shiny that image has been clicked on and we should now zoom in
  Shiny.setInputValue("zoomed_comparison", path_name)
})


//Sets shiny value to null when zoomed dialog is closed
Shiny.addCustomMessageHandler("update_zoomed_input", function(message) { 
  Shiny.setInputValue("zoomed_image", null);
});

//Sets shiny value to null when comparison zoomed dialog is closed
Shiny.addCustomMessageHandler("update_comparison_input", function(message) { 
  Shiny.setInputValue("zoomed_comparison", null);
});
