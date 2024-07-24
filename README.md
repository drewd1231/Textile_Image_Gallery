Interactive Textile Image Gallery

PROJECT DESCRIPTION
This project allows users to search through over 300 images of globally-sourced textiles documented on Dutch ships during the seventeenth and eighteenth centuries. Users can select a number of inputs to further refine their search. Additionally, when an image of interest is clicked on, information regarding the selected textile is displayed to provide the user with a deeper understanding of the history of the object itself. Two textiles can also be compared side by side through the use of the comparison tool embedded within the app. 

This app was built primarily using R and the Shiny open source R library, as well as javascript and CSS. R Shiny made it possible to design the framework of the ui and server that make the app both interactive and simple to use. The CSS included in this project was vital to all of the images and pop-ups displayed, as it provides both a neat and readable layout that is clear to any using the app. Lastly, javascript was used to retrieve images stored locally and display them within the shiny app at a faster rate. Originally using R Shiny's built in functions to display images, this app operated at an extremely slow speed. Javascript enables the user to enjoy a faster and more reliable experience with the Interactive Textile Image Gallery. 

It is a goal of the Dutch Textile Trade Project's team to incorporate more textiles and images in the future and widen the scope of the project. The project can then hopefully reach more users and be of more benefit to researchers, professors, students, or any individuals who choose to use it. 

HOW TO SET-UP AND RUN PROJECT LOCALLY
In order to recreate this project and work with it on your own device, please do the following: 
1. Create local directory to store all files in
2. Download and save all files included in this repository to your local directory
3. Install the latest version of R (available on any browser)
4. Set your working directory to the local directory you stored all necessary files in with the command 'setwd(YOUR_DIRECTORY)' in your R console
5. Run the line 'runApp()' in your console and the Shiny App should open!

HOW TO USE THE APPLICATION
