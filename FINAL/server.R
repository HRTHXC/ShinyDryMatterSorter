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
    #a small fix for those cells of brcrcode with nothing contained, let's use crossname instead!
    for(i in 1:length(dat$BrCrCode)){
      if(dat$BrCrCode[i] == ""){
        dat$BrCrCode[i] <<- gsub("\\.", "_", dat$CrossName[i])
      }
    }
    #we will interchange inbetween these two, always good to keep a clean backup of the table once we begin messing with the other
    omitAllCodesFix <<- dat
    #set our choices of omissions to the unique brcrcodes
    currentOmission <<- updateSelectizeInput(session, 'tableFilter', choices = dat$BrCrCode)
    if(!is.data.frame(dat))
      return(dat)
    controlVar$fileUploaded <- TRUE
  })
  
  observeEvent(input$clearOmission, {
    #reset the table back to pre omission state
    currentOmission <<- updateSelectizeInput(session, 'tableFilter', choices = omitAllCodesFix$BrCrCode)
    dat <- omitAllCodesFix
    #now can run the main program
    meatAndBeans(dat)
  })
  
  observeEvent(input$tableFilter, {
    #at any point where your total number of omissions don't match the count of rows in the table you use
    if(length(input$tableFilter) != rowCount){
      #keep omitting rows
      dat <- subset(dat, !(BrCrCode %in% input$tableFilter))
    }
    else{
      #reset the table back to pre omission state
      dat <<- omitAllCodesFix
      currentOmission <<- updateSelectizeInput(session, 'tableFilter', choices = dat$BrCrCode)
    }
    #run the main program again
    meatAndBeans(dat)
    return(input$tableFilter)
  })
  
  cleanFile <- function(dat){
    #grab the columns that matter for our analysis
    testParents <- mutate(select(dat, BrCrCode, Planted, nonPsa, PsaDeaths), MotherCode = '', FatherCode = '')
    #logging the special features of all the various formats of brcrcodes, needed for formatting them in the correct way for output
    slashCount <- str_count(testParents$BrCrCode, '/')
    underscoreCount <- str_count(testParents$BrCrCode, '_')
    spaceCheck <- str_count(testParents$BrCrCode, ' ')
    testParents <- cleanBrCrCodeInFile(testParents, slashCount, underscoreCount, spaceCheck)
    #we must make these columns contents as numeric, otherwise we cannot perform math on them
    testParents$Planted <- as.numeric(testParents$Planted)
    testParents$PsaDeaths <- as.numeric(testParents$PsaDeaths)
    testParents$nonPsa <- as.numeric(testParents$nonPsa)
    #calculate the rate of survival from PSA for each unique row in the table 
    testParents$survivalrate <- format(round((value = 100 - ((testParents$PsaDeaths / (testParents$Planted - testParents$nonPsa) ) * 100)), 2), nsmall = 2)
    return(testParents)
  }
  
  overallSurvival <- function(testParents){
    #rounding the survival rate to 2dp for readability purposes
    overallSurvival <- format(round( 100 - ((sum(as.numeric(testParents$PsaDeaths)) / sum(as.numeric(testParents$Planted - testParents$nonPsa))) * 100), 2), nsmall = 2)
    return(overallSurvival)
  }
  
  cleanBrCrCodeInFile <- function(x, slashCount, underscoreCount, spaceCheck){
    #for all the rows in our table
    for(i in 1:(dim(x)[1])){
      #if the brcrcode is formatted like this (eg. ZI 343)
      if(spaceCheck[i]==1){
        #we drop the space, will be picked up by underscorecount later on
        x$BrCrCode[i] <- gsub(" ", "", x$BrCrCode[i])
      }
      #if the brcrcode is formatted like this (eg. cyb_476/1)
      if(slashCount[i]==1) {
        if(underscoreCount[i]==1){
          #if the number of characters after the forward slash is three (eg. rjr_441/434)
          if(nchar(unlist(strsplit(x$BrCrCode[i], split = '/', fixed=TRUE))[2]) == 3){
            #ignore that type of code, it's valid in this instance
            next 
          }
          else{
            #strip anything after the forward slash away
            x$MotherCode[i] <- unlist(strsplit(x$BrCrCode[i], split = '/', fixed=TRUE))[1]
            #set to the brcrcode, it's formatted now
            x$BrCrCode[i] <- x$MotherCode[i]
          }
        }
        #mother code is before the underscore, father code is after the underscore
        x$MotherCode[i] <- unlist(strsplit(x$BrCrCode[i], split = '_', fixed=TRUE))[1]
        x$FatherCode[i] <- unlist(strsplit(x$BrCrCode[i], split = '_', fixed=TRUE))[2]
      }
      #if the brcrcode is formatted like this (eg. T01/i/don't/know)
      if(slashCount[i]==3) {
        temp <- x$BrCrCode[i]
        #codes like those are control types, we'll tag them as control, with the full brcrcode
        x$MotherCode[i] <- "control_"
        x$FatherCode[i] <- temp
        x$BrCrCode[i] <- paste(x$MotherCode[i], x$FatherCode[i], sep = "")
        #not flipping this variable would trigger the next if statement, formatting this code differently, we don't want that
        underscoreCount[i] <- 1
      }
      #if the brcrcode is formatted like this (eg. HO515)
      if(underscoreCount[i] == 0) {
        #strip the code apart by character type, mother code uses letters, father code uses numbers, by convention, there may be some outliers though
        x$MotherCode[i] <- as.character(str_extract(x$BrCrCode[i], "[aA-zZ]+"))
        x$FatherCode[i] <- as.character(str_extract(x$BrCrCode[i], "[0-9]+"))
        x$BrCrCode[i] <- paste(x$MotherCode[i], "_", x$FatherCode[i], sep = "")
      }
    }
    return(x)
  }
  
  verbatimSanity <- function(y){
    #we format the hell of the table in order to display in the combination table output
    y$MotherCode <- y$FatherCode <- y$survivalrate <- NULL
    y <- mutate(aggregate(cbind(Planted, nonPsa, PsaDeaths) ~ BrCrCode, data = y, FUN = sum), survivalrate = round((100 - (PsaDeaths / (Planted - nonPsa)) * 100), 2))
    return(y)
  }
  
  doubleUnderscoreCheck <- function(z, testMothers, testFathers, underscoreCount){
    #for codes like rOJ _M101, where an space and underscore is present, resulting code is rOJ_M101
    thirdIndexDetection <- which(sapply(strsplit(paste(z$BrCrCode, 1:length(z$BrCrCode), sep="_"), "_"), function(x)x[[2]]) == "")
    z$MotherCode <- testMothers$BrCrCode <- sapply(strsplit(z$BrCrCode, split = '_', fixed=TRUE), function(x)x[[1]])
    z$FatherCode <- testFathers$BrCrCode <- sapply(strsplit(paste(z$BrCrCode, 1:length(z$BrCrCode), sep="_"), "_"), function(x)x[[2]])
    return(z)
  }
  
  creationTable <- function(parentTable, parentCode){
    #we make the table for either mother or father table output
    parentTable$BrCrCode <- parentCode
    parentTable <- aggregate(cbind(Planted, nonPsa, PsaDeaths) ~ BrCrCode, data = parentTable, FUN = sum)
    parentTable <- mutate(parentTable, survivalrate = round((100 - (PsaDeaths / (Planted - nonPsa)) * 100), 2))
    return(parentTable)
  }
  
  twoDTableCreation <- function(testMothers, testFathers, testParents, testParentsVerbaitum){
    #the 2D table is created in here, special techniques must be used, a vector and logging where the survival rate should be indexed
    #make a blank matrix the size of mothercode x fathercode counts
    twoDtable <- matrix(c(""), nrow=length(testMothers$BrCrCode), ncol=length(testFathers$BrCrCode))
    #we set the names of the rows and columns to the mother and father codes
    rownames(twoDtable) <- testMothers$BrCrCode
    colnames(twoDtable) <- testFathers$BrCrCode
    #counter variables help with check all combinations of codes to find matches that related to the 2D table
    l <- 1
    m <- 0
    #for the amount of combination codes that we have
    for(i in 1:length(testParents$BrCrCode)){
      #mothercodefound refers to the y position of the cell in the 2d table
      motherCodeFound <- fatherCodeFound <- 0
      #find the code that matches Lth mother code
      for(j in 1:length(testMothers$BrCrCode)){
        #if those two codes match
        if(grepl(testMothers$BrCrCode[j], testParents$MotherCode[l])){
          #the number we receive in the end is y position in the 2d table for the survival rate
          motherCodeFound <- j
          break
        }
      }
      #find the code that matches Kth mother code
      for(k in 1:length(testFathers$BrCrCode)){
        #if those two codes match
        if(grepl(testFathers$BrCrCode[k], testParents$FatherCode[l])){
          #the number we receive in the end is x position in the 2d table for the survival rate
          fatherCodeFound <- k
          m <- m + 1
          break
        }
      }
      #find the code that matches Nth mother code
      for(n in 1:length(testParentsVerbaitum$BrCrCode)){
        #if those two codes match
        if(grepl(testParentsVerbaitum$BrCrCode[n], testParents$BrCrCode[i])){
          #if the planted total for a plant (excluding those dying of nonPsa causes) is less than 40, we must tag it to show it's less than the statistical signifcant baseline when we print it out to the table (survival rate), if not, just print it out to the table (survival rate) 
          ifelse((testParentsVerbaitum$Planted[n] - testParentsVerbaitum$nonPsa[n]) < 40, (twoDtable[motherCodeFound, fatherCodeFound] = paste(testParentsVerbaitum$survivalrate[n], "STS", sep = " ")), (twoDtable[motherCodeFound, fatherCodeFound] = testParentsVerbaitum$survivalrate[n]))
          break;
        }
      }
      l <- l + 1
    }
    #our table has now be created, we can use it in our output
    twoDtableDF <- data.frame(twoDtable)
    return(twoDtableDF)
  }
  
  meatAndBeans <- function(dat){
    #drops unneeded columns and masks brcrcodes with counts for certain special characters, to clean the codes into a single way of input (XX_123) in the best we can
    testParents <- cleanFile(dat)
    #each rows survival rate is calculated
    overallSurvival <- overallSurvival(testParents)
    #let's chuck the same table into all the other output, we'll format them differently in the output
    testMothers <- testFathers <- testParentsVerbaitum <- testParents <- aggregate(cbind(Planted, nonPsa, PsaDeaths) ~ BrCrCode+MotherCode+FatherCode+survivalrate, data = testParents, FUN = sum)
    #the fix for things like (rOJ _M101) space and underscore in the same code
    testParents <- doubleUnderscoreCheck(testParents, testMothers, testFathers, underscoreCount)
    #combination table calculation for survival rate
    testParentsVerbaitum <- verbatimSanity(testParentsVerbaitum)
    #we use the global rowCount variable to check for when we omit everything
    rowCount <<- length(testParentsVerbaitum$BrCrCode)
    #synopsis of the data inputted
    output$overallStats <- renderText(paste("In this block, ", sum(testParents$Planted), " plants were planted, and ", sum(testParents$PsaDeaths), " plants surcumb to PSA disease. ", sum(testParents$nonPsa), " individual plants died of non PSA related issues. This is a survival rate of ~", overallSurvival, "%", sep = ""))
    #output and download link for combination data table
    output$x1 = DT::renderDataTable(testParentsVerbaitum, server = FALSE, filter = "bottom")
    output$x5 = downloadHandler('parents-filtered.csv', content = function(file) {
      s = input$x1_rows_all
      write.csv(testParentsVerbaitum[s, , drop = FALSE], file)
    })
    #output and download link for mothers data table
    testMothers <- creationTable(testMothers, testParents$MotherCode)
    output$x2 = DT::renderDataTable(testMothers, server = FALSE, filter = "bottom")
    output$x6 = downloadHandler('mothers-filtered.csv', content = function(file) {
      s = input$x2_rows_all
      write.csv(testMothers[s, , drop = FALSE], file)
    })
    #output and download link for fathers data table
    testFathers <- creationTable(testFathers, testParents$FatherCode)
    output$x3 = DT::renderDataTable(testFathers, server = FALSE, filter = "bottom")
    output$x7 = downloadHandler('fathers-filtered.csv', content = function(file) {
      s = input$x3_rows_all
      write.csv(testFathers[s, , drop = FALSE], file)
    })
    #output and download link for 2D data table
    twoDtableDF <- twoDTableCreation(testMothers, testFathers, testParents, testParentsVerbaitum)
    output$x4 = DT::renderDataTable(twoDtableDF, server = FALSE)
    output$x8 = downloadHandler('overall2dmatrix.csv', content = function(file) {
      s = input$x4_rows_all
      write.csv(twoDtableDF[s, , drop = FALSE], file)
    })
  }
  
  #whenever the output changes, we run the function to change the output
  output$contents = renderTable({
    if (controlVar$fileUploaded || controlVar$outputTable){
      meatAndBeans(dat)
    }
  })
})