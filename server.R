#Two file apps: server.R

if (!require("shiny"))
  install.packages('shiny')
if (!require("DT"))
  install.packages('DT')
if (!require("httr"))
  install.packages('httr')
if (!require("XML"))
  install.packages('XML')
if (!require("Quandl"))
  install.packages('Quandl')

#Immediately enter the browser when an error occurs
#options(error = browser)

API <- "hDUA4xzpnCvZwFygscUf"
Quandl.auth(API)

params <- list(
  query = "*",
  source_code = "FSE",
  per_page = 300,
  page = 1,
  auth_token = API
)
page <- GET("https://www.quandl.com/",
            path = "api/v2/datasets.xml",
            query = params)

#XML type data
doc <- xmlTreeParse(page)
r <- xmlRoot(doc)
nodes <- r[["docs"]] #FSE had 160 indices on 06/08/2015 (in Quandl)
id <-
  as.integer(sapply(getNodeSet(nodes,"//id[@type='integer']"),xmlValue))
code <- sapply(getNodeSet(nodes,"//code"),xmlValue)
name <- sapply(getNodeSet(nodes,"//name"),xmlValue)
freq <- unlist(sapply(getNodeSet(nodes,"//frequency"),xmlValue))
from <- unlist(sapply(getNodeSet(nodes,"//from-date"),xmlValue))
to <- unlist(sapply(getNodeSet(nodes,"//to-date"),xmlValue))
#sx <- sapply(getNodeSet(r[["sources"]],"//name"),xmlValue) #NAME OF THE STOCK EXCHANGE
sorted <- order(code)
no <- length(code)
METADATA <- data.frame(integer(no),
                       numeric(no),
                       code[sorted],
                       name[sorted],
                       freq[sorted],
                       from[sorted],
                       to[sorted],
                       id[sorted])
names(METADATA) <-
  c("Units","Price","Code","Name","Frequency","From","To","ID")
rm(page,doc,r,nodes)#,id,code,name,freq,from,to,sx,sorted)
#print(ls())


shinyServer(function(input, output) {
  output$downloadMetaData <- downloadHandler(
    filename = function() {
      paste("METADATA-",Sys.Date(), '.csv', sep = '')
    },
    content = function(file) {
      write.csv(METADATA, file)
    }
  )
  
  output$downloadRawData <- downloadHandler(
    filename = function() {
      paste("FSE-",Sys.Date(), '.csv', sep = '')
    },
    content = function(file) {
      write.csv(FSE, file)
    }
  )
  
  output$downloadLogReturn <- downloadHandler(
    filename = function() {
      paste("FSE.ret-",Sys.Date(), '.csv', sep = '')
    },
    content = function(file) {
      write.csv(FSE.ret, file)
    }
  )
  
  output$fse <- renderText({
    "Frankfurt Stock Exchange"
  })
  
  output$table <-
    DT::renderDataTable(METADATA,server = T, rownames = FALSE)
  
  output$portfolio <- DT::renderDataTable({
    inFile <- input$pffile
    if (is.null(inFile))
      return(NULL)
    dataset <-
      read.csv(
        inFile$datapath, header = input$header, sep = input$sep,
        quote = input$quote, stringsAsFactors = FALSE
      )
    data <- dataset[dataset$"Units" > 0,c("Units","Price","Code")]
    xdata <<- data
    data
  }, rownames = FALSE)
  
  observeEvent(input$getData,{
    fsecol <- "Close"

    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Downloading past stock prices of", value = 0)
    loop <- length(xdata$"Code")
    
    # Number of times we'll go through the loop
    
    for (c in xdata$"Code") {
      code <- paste("FSE",c,sep = "/")
      if (!exists("FSE")) {
        FSE = Quandl(code,type = "zoo")[,fsecol,drop = F]
        attrname <- c
      } else {
        X = Quandl(code,type = "zoo")[,fsecol]
        FSE <- merge(FSE,X)
        attrname <- c(attrname, c)
      }
      # Increment the progress bar, and update the detail text.
      progress$inc(1/loop, detail = c)
    }
    colnames(FSE) <- attrname
    
    #Select range of dates of interest (merge to keep class property)
    FSE <-
      merge(window(FSE,start = input$dates[1],end = input$dates[2]),drop = F)
    colnames(FSE) <- attrname
    
    #Compute returns
    FSE.ret <- na.omit(diff(log(FSE)))[drop = F]
    
    #Set stock prices and number of units
    alpha <-
      as.integer(xdata$"Units")   #sample current portfolio composition
    Sprice <- as.numeric(xdata$"Price")  #on 2015-07-31
    rm(xdata,pos = 1)
    #print(ls())
    
    #Implement variance-covariance analysis
    #(Dynamic of VaR and ES)
    weights <- alpha * Sprice
    muhat <- apply(FSE.ret,2,mean)
    Sigmahat <- var(FSE.ret)
    meanloss <- -sum(weights * muhat)
    varloss <- t(weights) %*% Sigmahat %*% weights
    
    output$headtail <- renderPrint({
      print(head(FSE.ret))
      for (i in 1:3)
        cat("\t\t.\n")
      tail(FSE.ret)
    })
    
    output$summary <- renderPrint({
      summary(FSE.ret)
    })
    
    output$pfstat <- renderPrint({
      print(muhat)
      print(Sigmahat)
      cat("\nMean Loss = ",meanloss,"\n")
      cat("Var Loss  = ",varloss,"\n")
    })
    
    q <- c(.7, .8, .9, .95, .99)
    #eps <- .01 #to compute quantiles
    #q <- seq(from = 0.5,to = 1 - eps,by = eps)
    #rm(alpha,Sprice,weights,muhat,Sigmahat,eps)
    
    #Compute VaR and ES with implicit loops
    no <- length(q)
    RES <-
      data.frame(q = numeric(no),VaR = numeric(no),ES = numeric(no))
    RES$q <- q
    RES$VaR <- meanloss + sqrt(varloss) * qnorm(q)
    RES$ES <- meanloss + sqrt(varloss) * dnorm(qnorm(q)) / (1 - q)
    rm(no,q,FSE,weights,output,alpha,attrname,c,code,fsecol,X)

    output$pfPlot <- renderPlot({
      plot(RES$q,RES$VaR,type="b",col="green",
           xlab="Quantile",
           ylab="VaR(green) vs. ES(red)",
           ylim=range(RES$VaR,RES$ES))
      lines(RES$q,RES$ES,col = "red",type = "b")
      title("Dynamic of VaR vs ES")
    })
    output$pfVaRES <- renderTable({
      RES
    })
  })
  
  output$headtail <- renderPrint({
    print(head(FSE.ret))
    for (i in 1:3)
      cat("\t\t.\n")
    tail(FSE.ret)
  })
  
  output$summary <- renderPrint({
    summary(FSE.ret)
  })
  
})