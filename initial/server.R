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
    if(dat$breeder_cross_code == ""){
      dat$breeder_cross_code <<- gsub("\\.", "_", dat$group_code)
    }
    #we will interchange inbetween these two, always good to keep a clean backup of the table once we begin messing with the other
    omitAllCodesFix <<- dat
    #set our choices of omissions to the unique breeder_cross_codes
    currentOmission <<- updateSelectizeInput(session, 'tableFilter', choices = dat$breeder_cross_code)
    if(!is.data.frame(dat))
      return(dat)
    controlVar$fileUploaded <- TRUE
    start.time <- Sys.time()
    meatAndBeans(dat)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)
  })
  
  observeEvent(input$clearOmission, {
    #reset the table back to pre omission state
    currentOmission <<- updateSelectizeInput(session, 'tableFilter', choices = omitAllCodesFix$breeder_cross_code)
    dat <- omitAllCodesFix
    #now can run the main program
    meatAndBeans(dat)
  })
  
  observeEvent(input$tableFilter, {
    #at any point where your total number of omissions don't match the count of rows in the table you use
    if(length(input$tableFilter) != rowCount){
      #keep omitting rows
      dat <- subset(dat, !(breeder_cross_code %in% input$tableFilter))
    }
    else{
      #reset the table back to pre omission state
      dat <<- omitAllCodesFix
      currentOmission <<- updateSelectizeInput(session, 'tableFilter', choices = dat$breeder_cross_code)
    }
    #run the main program again
    meatAndBeans(dat)
    return(input$tableFilter)
  })
  
  cleanFile <- function(dat){
    #grab the columns that matter for our analysis
    testParents <- mutate(select(dat, breeder_cross_code, Concept, data_type, harvest_dm), MotherCode = '', FatherCode = '')
    testParents <- testParents[grep("Green", testParents$Concept), ]
    testParents <- testParents[grep("Destructive", testParents$data_type), ]
    testParents <- testParents[!(is.na(testParents$breeder_cross_code) | testParents$breeder_cross_code==""), ]
    testParents <- aggregate(harvest_dm ~ breeder_cross_code, data = testParents, FUN = "mean")
    testParents$harvest_dm <- round(testParents$harvest_dm, digits = 2)
    #testParents <- cbind(harvest_dm, ~ breeder_cross_code+Concept+data_type, data = dat, FUN = sum)
    #logging the special features of all the various formats of brcrcodes, needed for formatting them in the correct way for output
    slashCount <- str_count(testParents$breeder_cross_code, '/')
    underscoreCount <- str_count(testParents$breeder_cross_code, '_')
    spaceCheck <- str_count(testParents$breeder_cross_code, ' ')
    hyphenCheck <- str_count(testParents$breeder_cross_code, '-')
    testParents <- cleanbreeder_cross_codeInFile(testParents, slashCount, underscoreCount, spaceCheck, hyphenCheck)
    #we must make these columns contents as numeric, otherwise we cannot perform math on them
    testParents$harvest_dm <- as.numeric(testParents$harvest_dm)
    return(testParents)
  }
  
  cleanbreeder_cross_codeInFile <- function(x, slashCount, underscoreCount, spaceCheck, hyphenCheck){
    #for all the rows in our table
    for(i in 1:(dim(x)[1])){
      if(hyphenCheck[i] == 1){
        print(x$breeder_cross_code[i])
        x$breeder_cross_code[i] <- gsub("-", "_", x$breeder_cross_code[i])
        x$MotherCode[i] <- as.character(str_extract(x$breeder_cross_code[i], "[aA-zZ]+"))
        x$FatherCode[i] <- as.character(str_extract(x$breeder_cross_code[i], "[0-9]+"))
        underscoreCount[i] <- 1
        print(x$breeder_cross_code[i])
      }
      #if the breeder_cross_code is formatted like this (eg. ZI 343)
      if(spaceCheck[i]==1){
        #we drop the space, will be picked up by underscorecount later on
        x$breeder_cross_code[i] <- gsub(" ", "", x$breeder_cross_code[i])
      }
      if(underscoreCount[i] == 1){
        x$MotherCode[i] <- unlist(strsplit(x$breeder_cross_code[i], split = '_', fixed=TRUE))[1]
        x$FatherCode[i] <- unlist(strsplit(x$breeder_cross_code[i], split = '_', fixed=TRUE))[2]
      }
      #if the breeder_cross_code is formatted like this (eg. cyb_476/1)
      if(slashCount[i]==1) {
        if(underscoreCount[i]==1){
          #if the number of characters after the forward slash is three (eg. rjr_441/434)
          if(nchar(unlist(strsplit(x$breeder_cross_code[i], split = '/', fixed=TRUE))[2]) == 3){
            #ignore that type of code, it's valid in this instance
            next 
          }
          else{
            #strip anything after the forward slash away
            x$MotherCode[i] <- unlist(strsplit(x$breeder_cross_code[i], split = '/', fixed=TRUE))[1]
            #set to the breeder_cross_code, it's formatted now
            x$breeder_cross_code[i] <- x$MotherCode[i]
          }
        }
        #mother code is before the underscore, father code is after the underscore
        x$MotherCode[i] <- unlist(strsplit(x$breeder_cross_code[i], split = '_', fixed=TRUE))[1]
        x$FatherCode[i] <- unlist(strsplit(x$breeder_cross_code[i], split = '_', fixed=TRUE))[2]
      }
      #if the breeder_cross_code is formatted like this (eg. T01/i/don't/know)
      if(slashCount[i]==3) {
        temp <- x$breeder_cross_code[i]
        #codes like those are control types, we'll tag them as control, with the full breeder_cross_code
        x$MotherCode[i] <- "control_"
        x$FatherCode[i] <- temp
        x$breeder_cross_code[i] <- paste(x$MotherCode[i], x$FatherCode[i], sep = "")
        #not flipping this variable would trigger the next if statement, formatting this code differently, we don't want that
        underscoreCount[i] <- 1
      }
      #if the breeder_cross_code is formatted like this (eg. HO515)
      if(underscoreCount[i] == 0) {
        #strip the code apart by character type, mother code uses letters, father code uses numbers, by convention, there may be some outliers though
        x$MotherCode[i] <- as.character(str_extract(x$breeder_cross_code[i], "[aA-zZ]+"))
        x$FatherCode[i] <- as.character(str_extract(x$breeder_cross_code[i], "[0-9]+"))
        x$breeder_cross_code[i] <- paste(x$MotherCode[i], "_", x$FatherCode[i], sep = "")
      }
    }
    return(x)
  }
  
  meatAndBeans <- function(dat){
    #drops unneeded columns and masks brcrcodes with counts for certain special characters, to clean the codes into a single way of input (XX_123) in the best we can
    testParentsVerbaitum <- testParents <- cleanFile(dat)
    drops <- c("MotherCode", "FatherCode")
    testParentsVerbaitum <- testParentsVerbaitum[ , !(names(testParentsVerbaitum) %in% drops)]
    output$x1 = DT::renderDataTable(testParentsVerbaitum, server = FALSE, filter = "bottom")
    #output and download link for combination data table
    output$x5 = downloadHandler('parents-drymatter.csv', content = function(file) {
      s = input$x1_rows_all
      write.csv(testParents[s, , drop = FALSE], file)
    })
    testMothers <- select(testParents, MotherCode, harvest_dm)
    testMothers <- aggregate(harvest_dm ~ MotherCode, data = testMothers, FUN = "mean")
    testMothers$harvest_dm <- round(testMothers$harvest_dm, digits = 2)
    testFathers <- select(testParents, FatherCode, harvest_dm)
    testFathers <- aggregate(harvest_dm ~ FatherCode, data = testFathers, FUN = "mean")
    testFathers$harvest_dm <- round(testFathers$harvest_dm, digits = 2)
    
    #why cannot print out anything from dat?!
    #output$overallStats <- renderText(dat)
    # #let's chuck the same table into all the other output, we'll format them differently in the output
    # testMothers <- testFathers <- testParentsVerbaitum <- testParents <- aggregate(cbind(Planted, nonPsa, PsaDeaths) ~ BrCrCode+MotherCode+FatherCode+survivalrate, data = testParents, FUN = sum)
    # #the fix for things like (rOJ _M101) space and underscore in the same code
    # testParents <- doubleUnderscoreCheck(testParents, testMothers, testFathers, underscoreCount)
    # #combination table calculation for survival rate
    # testParentsVerbaitum <- verbatimSanity(testParentsVerbaitum)
    # #we use the global rowCount variable to check for when we omit everything
    # rowCount <<- length(testParentsVerbaitum$BrCrCode)
    # #output and download link for mothers data table
    # testMothers <- creationTable(testMothers, testParents$MotherCode)
    output$x2 = DT::renderDataTable(testMothers, server = FALSE, filter = "bottom")
    output$x6 = downloadHandler('mothers-filtered.csv', content = function(file) {
      s = input$x2_rows_all
      write.csv(testMothers[s, , drop = FALSE], file)
    })
    # #output and download link for fathers data table
    # testFathers <- creationTable(testFathers, testParents$FatherCode)
    output$x3 = DT::renderDataTable(testFathers, server = FALSE, filter = "bottom")
    output$x7 = downloadHandler('fathers-filtered.csv', content = function(file) {
      s = input$x3_rows_all
      write.csv(testFathers[s, , drop = FALSE], file)
    })
    # #output and download link for 2D data table
    # twoDtableDF <- twoDTableCreation(testMothers, testFathers, testParents, testParentsVerbaitum)
    # output$x4 = DT::renderDataTable(twoDtableDF, server = FALSE)
    # output$x8 = downloadHandler('overall2dmatrix.csv', content = function(file) {
    #   s = input$x4_rows_all
    #   write.csv(twoDtableDF[s, , drop = FALSE], file)
    # })
    
  }
  
  #whenever the output changes, we run the function to change the output
  output$contents = renderTable({
    # if (controlVar$fileUploaded || controlVar$outputTable){
    #   print(dat)
    # }
    meatAndBeans(dat)
  })
  
})
