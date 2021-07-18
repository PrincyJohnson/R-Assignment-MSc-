shinyServer(function(input, output, session) {
  
  data <- reactive({
    inFile <- input$file1 
    if (is.null(inFile)){return(NULL)} 
    read.csv(inFile$datapath, header=input$header, sep=input$sep)
  })
  
  # user selects variable 1
  observe({
    updateSelectInput(
      session,
      "var1",
      choices=names(data()))
    
  })
  # user selects variable 2
  observe({
    updateSelectInput(
      session,
      "var2",
      choices=names(data()))
    
  })
  
  
  # Output a data table for the upload tab page
  output$contents <- renderTable({
    inFile <- input$file1 
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header=input$header, sep=input$sep)
    
  })
  
     # Create a two sample t-test reactive function
  ttestout <- reactive({
    var1 <- data()[,input$var1]
    conf <- input$conf
    var2 <- data()[,input$var2]
    if (is.null(var2)){return(NULL)}
    t2 <- t.test(var1, var2, alternative = input$tail, var.equal = T, conf.level = 0.95)
    if(input$sample == "twoSamp") {return(t2)}
    
  })
  
  # Output of one sample t value of t-test
  output$tvalue <- renderPrint({
    vals <- ttestout()
    if (is.null(vals)){return(NULL)}
    vals$statistic
  })
  
  # Output of p value
  output$pvalue <- renderPrint({
    vals <- ttestout()
    if (is.null(vals)){return(NULL)}
    vals$p.value 
    })
  
  
  # Output of key statistical parametric
  output$parametric <- renderTable({
    var1 <- data()[,input$var1]
    if (is.null(var)){return(NULL)}
    var2 <- data()[,input$var2]
    if (is.null(var)){return(NULL)}
    mean1 <- mean(var1)
    mean2 <- mean(var2)
    standard_deviation1 <- sd(var1)
    standard_deviation2 <- sd(var2)
    standard_error1 <- sd(var1)/sqrt(length(var1))
    standard_error2 <- sd(var2)/sqrt(length(var2))
    parametric1 <- data.frame(mean = mean1, 
                              standard_deviation=standard_deviation1, 
                              standard_error=standard_error1)
    rownames(parametric1) <- input$var1
    parametric2 <- data.frame(mean = mean2, 
                              standard_deviation=standard_deviation2, 
                              standard_error=standard_error2)
    rownames(parametric2) <- input$var2
    if(input$sample == "twoSamp") {return(rbind(parametric1,parametric2))}
  })
  
   
    #creating heatmap from the user input 
  plotdata <- eventReactive(input$getHmap, { 
    data <- as.matrix(data()[-1])
    row.names(data) <- data()$X
    data[is.na(data)] <- 0
    data
    })

  #output for heatmap   
  output$Heatplot = renderPlot({ 
    pheatmap(plotdata())
     
    })
  
  ## Diagnostic test  
  
  ##table 1: Controls 
  
  output$outputld2 <- renderTable({
    
    CtrlGrp <- subset(data()[2:11])
    ctrl <- as.matrix(CtrlGrp)
    
  }) 
  
  ## table 2: Samples  
  
  output$outputld3 <- renderTable({
    
    SampleGrp <- subset(data()[12:21])
    sample <- as.matrix(SampleGrp)
    
  })
  
  ## t test for all 10 samples-controls and samples  
  
  ## t-tests for all genes 
  output$pvalue2 <- renderTable({
    sample1 <- as.matrix(data()[12:21])
    colnames(sample1) <- NULL
    ctrl1 <- as.matrix(data()[2:11])
    colnames(ctrl1) <- NULL 
    t_test<- col_t_paired(sample1,ctrl1, alternative = "two.sided", mu=0)
    
  })
  ##diagnostic answer
  matrixpvalue <- reactive ({
    samples <- as.matrix(data()[12:21])
    colnames(samples) <- NULL
    controls <- as.matrix(data()[2:11])
    colnames(controls) <- NULL
    matrixttest <- col_t_paired(samples,controls, alternative = "two.sided", mu=0)
  })
  
  
  output$diagnosis <- renderPrint({
    vals <- matrixpvalue()
    for(i in 1:vals$pvalue){
      if ( vals$pvalue[i] < 0.05 ){ 
        print ("Disease Positive")
        break
      }
      print("Disease Negative")
    }
  })
  
})