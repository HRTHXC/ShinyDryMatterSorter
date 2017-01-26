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
  testParentsVerbaitum <- NULL
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
      dat <- subset(omitAllCodesFix, !(breeder_cross_code %in% input$tableFilter))
    }
    else{
      #reset the table back to pre omission state
      dat <<- omitAllCodesFix
      currentOmission <<- updateSelectizeInput(session, 'tableFilter', choices = dat$breeder_cross_code)
    }
    meatAndBeans(dat)
    return(input$tableFilter)
  })
  
  cleanFile <- function(dat){
    #grab the columns that matter for our analysis
    testParents <- mutate(select(dat, breeder_cross_code, Concept, data_type, harvest_dm), MotherCode = '', FatherCode = '')
    testParents <- testParents[grep("Green", testParents$Concept), ]
    testParents <- testParents[grep("Destructive", testParents$data_type), ]
    testParents <- testParents[!(is.na(testParents$breeder_cross_code) | testParents$breeder_cross_code==""), ]
    testParents <- aggregate(harvest_dm ~ breeder_cross_code+Concept+data_type, data = testParents, FUN = "mean")
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
    if(controlVar$fileUploaded == FALSE){
      omitAllCodesFix <<- testParents
      rowCount <<- length(testParents$breeder_cross_code)
      currentOmission <<- updateSelectizeInput(session, 'tableFilter', choices = omitAllCodesFix$breeder_cross_code)
      controlVar$fileUploaded == TRUE
    }
    return(testParents)
  }
  
  cleanbreeder_cross_codeInFile <- function(x, slashCount, underscoreCount, spaceCheck, hyphenCheck){
    #for all the rows in our table
    for(i in 1:(dim(x)[1])){
      if(hyphenCheck[i] == 1){
        x$breeder_cross_code[i] <- gsub("-", "_", x$breeder_cross_code[i])
        x$MotherCode[i] <- as.character(str_extract(x$breeder_cross_code[i], "[aA-zZ]+"))
        x$FatherCode[i] <- as.character(str_extract(x$breeder_cross_code[i], "[0-9]+"))
        underscoreCount[i] <- 1
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
  
  twoDTableCreation <- function(testMothers, testFathers, testParents){
    #the 2D table is created in here, special techniques must be used, a vector and logging where the survival rate should be indexed
    #make a blank matrix the size of mothercode x fathercode counts
    twoDtable <- matrix(c(""), nrow=length(testMothers$MotherCode), ncol=length(testFathers$FatherCode))
    #we set the names of the rows and columns to the mother and father codes
    rownames(twoDtable) <- testMothers$MotherCode
    colnames(twoDtable) <- testFathers$FatherCode
    #counter variables help with check all combinations of codes to find matches that related to the 2D table
    l <- 1
    m <- 0
    #for the amount of combination codes that we have
    for(i in 1:length(testParents$breeder_cross_code)){
      #mothercodefound refers to the y position of the cell in the 2d table
      motherCodeFound <- fatherCodeFound <- 0
      #find the code that matches Lth mother code
      for(j in 1:length(testMothers$MotherCode)){
        #if those two codes match
        if(grepl(paste("^", testMothers$MotherCode[j], "$", sep=""), testParents$MotherCode[l])){
          #the number we receive in the end is y position in the 2d table for the survival rate
          motherCodeFound <- j
          break
        }
      }
      #find the code that matches Kth mother code
      for(k in 1:length(testFathers$FatherCode)){
        #if those two codes match
        if(grepl(paste("^", testFathers$FatherCode[k], "$", sep=""), testParents$FatherCode[l])){
          #the number we receive in the end is x position in the 2d table for the survival rate
          fatherCodeFound <- k
          m <- m + 1
          break
        }
      }
      #find the code that matches Nth mother code
      for(n in 1:length(testParents$breeder_cross_code)){
        #if those two codes match
        if(grepl(paste("^", testParentsVerbaitum$breeder_cross_code[n], "$", sep=""), testParents$breeder_cross_code[i])){
          twoDtable[motherCodeFound, fatherCodeFound] = paste(testParents$harvest_dm[n])
          break
        }
      }
      print(paste("[", motherCodeFound, ",", fatherCodeFound, "]", sep=""))
      l <- l + 1
    }
    #our table has now be created, we can use it in our output
    twoDtableDF <- data.frame(twoDtable)
    return(twoDtableDF)
  }
  
  meatAndBeans <- function(dat){
    #drops unneeded columns and masks brcrcodes with counts for certain special characters, to clean the codes into a single way of input (XX_123) in the best we can
    testParents <- cleanFile(dat)
    if(controlVar$fileUploaded == FALSE){
      testParentsVerbaitum <<- testParents
      controlVar$fileUploaded <- TRUE
    }
    output$x1 = DT::renderDataTable(testParents, server = FALSE, filter = "bottom")
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
    #output and download link for mothers data table
    output$x2 = DT::renderDataTable(testMothers, server = FALSE, filter = "bottom")
    output$x6 = downloadHandler('mothers-drymatter.csv', content = function(file) {
      s = input$x2_rows_all
      write.csv(testMothers[s, , drop = FALSE], file)
    })
    # #output and download link for fathers data table
    output$x3 = DT::renderDataTable(testFathers, server = FALSE, filter = "bottom")
    output$x7 = downloadHandler('fathers-drymatter.csv', content = function(file) {
      s = input$x3_rows_all
      write.csv(testFathers[s, , drop = FALSE], file)
    })
    #output and download link for 2D data table
    twoDtableDF <- twoDTableCreation(testMothers, testFathers, testParents)
    output$x4 = DT::renderDataTable(twoDtableDF, server = FALSE)
    output$x8 = downloadHandler('overall2dmatrixdrymatter.csv', content = function(file) {
      s = input$x4_rows_all
      write.csv(twoDtableDF[s, , drop = FALSE], file)
    })
  }
  
  #whenever the output changes, we run the function to change the output
  output$contents = renderTable({
    if (controlVar$fileUploaded){
      meatAndBeans(dat)
    }
  })
  
})
