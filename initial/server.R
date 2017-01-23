#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

options(shiny.maxRequestSize=30*1024^2)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  #boolean logic to control first time execution of certain code
  controlVar <- reactiveValues(fileUploaded = FALSE, outputTable = TRUE)
  #global table creation
  dat <- omitAllCodesFix <- NULL
  #useful for when we are checking what has been omitted from our table
  rowCount <- currentOmissionCount <- 0
  
  #whenever a new file has been uploaded
  observeEvent(input$upload_file, {
    controlVar$fileUploaded <- FALSE
    if (is.null(input$upload_file))
      return()
    #we grab file metadata
    inFile <- input$upload_file
    #read in file data using fread to a data.frame, so we can use in program
    dat <<- fread(inFile$datapath)
    #a small fix for those cells of breeder_cross_code with nothing contained, let's use group_code instead!
    for(i in 1:length(dat$breeder_cross_code)){
      if(dat$breeder_cross_code[i] == ""){
        dat$breeder_cross_code[i] <<- gsub("\\.", "_", dat$group_code[i])
      }
    }
    #we will interchange inbetween these two, always good to keep a clean backup of the table once we begin messing with the other
    omitAllCodesFix <<- dat
    #set our choices of omissions to the unique breeder_cross_codes
    currentOmission <<- updateSelectizeInput(session, 'tableFilter', choices = dat$breeder_cross_code)
    if(!is.data.frame(dat))
      return(dat)
    controlVar$fileUploaded <- TRUE
  })
  
  observeEvent(input$clearOmission, {
    #reset the table back to pre omission state
    currentOmission <<- updateSelectizeInput(session, 'tableFilter', choices = omitAllCodesFix$breeder_cross_code)
    dat <<- omitAllCodesFix
    #now can run the main program
    # meatAndBeans(dat)
  })
  
  observeEvent(input$tableFilter, {
    #at any point where your total number of omissions don't match the count of rows in the table you use
    if(length(input$tableFilter) != rowCount){
      #keep omitting rows
      dat <<- subset(dat, !(breeder_cross_code %in% input$tableFilter))
    }
    else{
      #reset the table back to pre omission state
      dat <<- omitAllCodesFix
      currentOmission <<- updateSelectizeInput(session, 'tableFilter', choices = dat$breeder_cross_code)
    }
    #run the main program again
    # meatAndBeans(dat)
    return(input$tableFilter)
  })
  
  #whenever the output changes, we run the function to change the output
  output$contents = renderTable({
    # if (controlVar$fileUploaded || controlVar$outputTable){
    #   print(dat)
    # }
  })
  
})
