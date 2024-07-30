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
  
  //Check if there are no images matching input
  if (image_urls[0] === null) {
    console.log("here");
    var noImagesMessage = $("<h3>").text("No images match your criteria");
  
    //Add text to image_gallery object to tell user that search is invalid
    $("#image_gallery").append(noImagesMessage);
    return;
  }


  //Loops through all urls passed by reactive R input
  image_urls.forEach(function(url) { 
  
  var temp_image = new Image();
  temp_image.src = url;
  
  //temp_image.onload = function() { 
  
  
  //Creates image object for each url and checks for click event
  var img = $("<img>").attr("src", url).addClass("gallery-image").click(function() { 
    Shiny.setInputValue("selected_image", url, {priority: "event"});
  });
  
  //Changes users cursor when hovering above an image
  img.css("cursor", "pointer");
  
  //if (this.naturalHeight > this.naturalWidth) { 
  // img.css("transform", "rotate(90deg)");
  //$(this).addClass("rotated");
  //}
  
  //Appends each image object to the "image_gallery"
  $("#image_gallery").append(img);
  //};
  });
}

//Check for clicks on image when first modal dialog pop-up
$(document).on("click", ".zoomed_image", function() { 
  console.log("here");
  var path_name = $(this).data("path");
  //Tell shiny that image has been clicked on and we should now zoom in
  Shiny.setInputValue("zoomed_image", path_name)
})

$(document).on("hidden.bs.modal", ".modal", function(e) { 
  //console.log("here");
  //var modal_id = $(e.target).attr('id');
  //if (modal_id === )
  var rand_int = Math.random();
  
  Shiny.setInputValue("reopen_dialog_button", rand_int);
});
  //})
//})

$(document).on("click", ".zoomed_comparison", function() { 
  //console.log("here");
  var path_name = $(this).data("path");
  //Tell shiny that image has been clicked on and we should now zoom in
  Shiny.setInputValue("zoomed_comparison", path_name)
})

Shiny.addCustomMessageHandler("update_zoomed_input", function(message) { 
  Shiny.setInputValue("zoomed_image", null);
});

Shiny.addCustomMessageHandler("update_comparison_input", function(message) { 
  Shiny.setInputValue("zoomed_comparison", null);
});


//TESTING
/*
document.addEventListener("DOMContentLoaded", function() {
    console.log("one");
    $(document).on("mouseover", ".zoomed-description", function(e) { 
      const zoomedImage = document.querySelector('.zoomed-description');
      //const img = zoomedImage.querySelector('img');
      
      const img = zoomedImage.querySelector('img');
      const magnifier = document.createElement('div');
      magnifier.classList.add('magnifier');
      const magnifier_img = document.createElement('img');
      magnifier_img.src = img.src;
      magnifier.appendChild(magnifier_img);
      zoomedImage.appendChild(magnifier);
  
      zoomedImage.addEventListener('mousemove', function(e) {
        magnifier.style.display = 'block';
        const rect = zoomedImage.getBoundingClientRect();
        const x = e.clientX - rect.left;
        const y = e.clientY - rect.top;

        const magnifierX = x - magnifier.offsetWidth / 2;
        const magnifierY = y - magnifier.offsetHeight / 2;

        magnifier.style.left = `${magnifierX}px`;
        magnifier.style.top = `${magnifierY}px`;

        const imgX = -((x / rect.width) * img.width - magnifier.offsetWidth / 2);
        const imgY = -((y / rect.height) * img.height - magnifier.offsetHeight / 2);

        magnifier_img.style.left = `${imgX}px`;
        magnifier_img.style.top = `${imgY}px`;
      });
  
      zoomedImage.addEventListener('mouseleave', function() {
          magnifier.style.display = "none";
    });
  });
});





*/