Interactive Textile Image Gallery

PROJECT DESCRIPTION:

This project allows users to search through over 300 images of globally-sourced textiles documented on Dutch ships during the seventeenth and eighteenth centuries. Users can select a number of inputs to further refine their search. Additionally, when an image of interest is clicked on, information regarding the selected textile is displayed to provide the user with a deeper understanding of the history of the object itself. Two textiles can also be compared side by side through the use of the comparison tool embedded within the app. 

This app was built primarily using R and the Shiny open source R library, as well as javascript and CSS. R Shiny made it possible to design the framework of the ui and server that make the app both interactive and simple to use. The CSS included in this project was vital to all of the images and pop-ups displayed, as it provides both a neat and readable layout that is clear to any using the app. Lastly, javascript was used to retrieve images stored locally and display them within the shiny app at a faster rate. Originally using R Shiny's built in functions to display images, this app operated at an extremely slow speed. Javascript enables the user to enjoy a faster and more reliable experience with the Interactive Textile Image Gallery. 

It is a goal of the Dutch Textile Trade Project's team to incorporate more textiles and images in the future and widen the scope of the project. The project can then hopefully reach more users and be of more benefit to researchers, professors, students, or any individuals who choose to use it. 

HOW TO SET-UP AND RUN PROJECT LOCALLY:

In order to recreate this project and work with it on your own device, please do the following: 
1. Create local directory to store all files in
2. Download and save all files included in this repository to your local directory
3. Install the latest version of R (available on any browser)
4. Set your working directory to the local directory you stored all necessary files in with the command 'setwd(YOUR_DIRECTORY)' in your R console
5. Run the line 'runApp()' in your console and the Shiny App should open!

HOW TO USE THE APPLICATION:

This Shiny App allows users to describe the textile(s) they are interested in looking for in a number of ways: textile name, color (user can choose any number of colors), pattern, process, weave, and fiber. Users can choose any option from each drop-down menu or begin to type the option they want then select any one that matches. 

The modifier operator (options being AND/OR) is an important distinguisher for users to decide what their search will look like. If the user selects the 'AND' operator, only images whose attributes match every one of the selections will be shown. On the other hand, when the 'OR' operator is selected, each image shown matches at least one of the inputs selected by the user. The exception to this rule is if the user has selected a textile name; in this case, the textiles shown will always match the textile name provided. 

CREDITS

Thank you to Carrie Anderson and Marsely Kehoe for their contributions during this process!
