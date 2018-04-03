# git push -u origin master
server <- function(input, output, session){
  session$onSessionEnded(stopApp)
  
  ### Close App with a button ##################################################
  observeEvent(input$close, {
    js$closeWindow()
    stopApp()
  }) # ends Session if Window is closed
  
  ### Functions ################################################################
  
  trades <- eventReactive(input$is_file,{
    trades <- read_excel(file$datapath, sheet = "List of Trades", skip = 1)
    trades$Time <- strftime(trades$Time, format = "%H:%M")
    trades$Datetime <- strptime(paste(trades$Date, trades$Time), "%Y-%m-%d %H:%M")
    return(trades)
  })
  
  is_trades <- reactive({
    if (is.null(input$is_file)) {
      return(NULL)
    }
    file <- input$is_file
    trades <- read_excel(file$datapath, 
                         sheet = "List of Trades", skip = 1, 
                         col_types = c(rep("numeric", 2), rep("text",2), 
                                       rep("guess",2), rep("numeric",10)))
    trades <- format_file(trades)
    return(trades)
  })
  
  oos_trades <- reactive({
    if (is.null(input$oos_file)) {
      return(NULL)
    }
    file <- input$oos_file
    trades <- read_excel(file$datapath, 
                         sheet = "List of Trades", skip = 1, 
                         col_types = c(rep("numeric", 2), rep("text",2), 
                                       rep("guess",2), rep("numeric",10)))
    trades <- format_file(trades)
    return(trades)
  })
  
  format_file <- function(file){
    trades <- file
    trades$Time <- strftime(trades$Time, format = "%H:%M")
    trades$Date <- format(as.Date(trades$Date, '%Y-%m-%d'), "%m/%d/%Y")
    trades$Datetime <- strptime(paste(trades$Date, trades$Time), "%m/%d/%Y %H:%M", tz = "GMT")
    trades <- as.xts(x = trades[,-c(4:6,17)], order.by = trades$Datetime)
    trades <- xts(coredata(trades), as.POSIXct(index(trades)))
    trades$Type[trades$Type == "EntryLong"] <- 1
    trades$Type[trades$Type == "ExitLong"] <- -1
    colnames(trades) <- c("Trade", "Order","Type","Price","Contracts","Profit",
                          "Profit_pct","Cum_Profit","Cum_Profit_pct","Runup",
                          "Runup_pct","Drawdown","Drawdown_pct")
    storage.mode(trades) <- "numeric"
    trades$Trade <- na.locf(as.numeric(trades$Trade))
    trades[is.na(trades)] <- 0
    return(trades)
  }
  
  entries <- function(data){
    entries <- data[seq(1,nrow(data),2),]
    return(entries)
  }
  
  exits <- function(data){
    exits <- data[seq(2,nrow(data),2),]
    return(exits)
  }
  
  ### Output ###################################################################
  
  # oputput$is_table <- renderDataTable({
  #   req(input$is_file)
  #   table(summary(trades(is_file)))
  # })
  
  output$is_table <- renderDataTable({
    req(input$is_file)
    is_trades <- is_trades()
    table.Stats(is_trades$Profit_pct)
  })
  
  output$is_monthly <- renderDataTable({
    req(input$is_file)
    is_entries <- entries(is_trades())
    t(table.CalendarReturns(is_entries$Profit_pct))
  })
  
  output$is_ratios <- renderText({
    req(input$is_file)
    entries <- entries(is_trades())
    
    print()
  })
  
  output$is_perf_summary <- renderPlot({
    req(input$is_file)
    is_entries <- entries(is_trades())
    charts.PerformanceSummary(R = is_entries$Profit_pct, Rf = 0, main = "In Sample")
  })
  
  output$is_hist <- renderPlot({
    req(input$is_file)
    hist(entries(is_trades()$Profit), breaks = 20, main = "Histogram of Returns",
         xlab = "Profit")
  })
  
  output$is_cum_profit <- renderPlot({
    req(input$is_file)
    is_trades <- is_trades()
    plot(cumsum(is_trades$Profit), main = "Cumulative Returns")
  })
  
  output$is_cum_profit_pct <- renderPlot({
    req(input$is_file)
    is_trades <- is_trades()
    plot(cumsum(is_trades$Profit_pct), main = "Cumulative Returns")
  })
  
  output$is_drawdown <- renderPlot({
    req(input$is_file)
    is_trades <- is_trades()
    plot(is_trades$Drawdown, main = "Drawdown")
  })
  
  output$is_drawdown_pct <- renderPlot({
    req(input$is_file)
    is_trades <- is_trades()
    plot(is_trades$Drawdown_pct, main = "Drawdown")
  })
  
  ### OOS ######################################################################
  output$oos_table <- renderDataTable({
    req(input$oos_file)
    oos_trades <- oos_trades()
    table.Stats(oos_trades$Profit_pct)
  })
  
  output$oos_monthly <- renderDataTable({
    req(input$oos_file)
    oos_entries <- entries(oos_trades())
    t(table.CalendarReturns(oos_entries$Profit_pct))
  })
  
  output$oos_ratios <- renderText({
    req(input$oos_file)
    entries <- entries(oos_trades())
    
    print()
  })
  
  output$oos_perf_summary <- renderPlot({
    req(input$oos_file)
    oos_entries <- entries(oos_trades())
    charts.PerformanceSummary(R = oos_entries$Profit_pct, Rf = 0, main = "In Sample")
  })
  
  output$oos_hist <- renderPlot({
    req(input$oos_file)
    hist(entries(oos_trades()$Profit), breaks = 20, main = "Hoostogram of Returns",
         xlab = "Profit")
  })
  
  output$oos_cum_profit <- renderPlot({
    req(input$oos_file)
    oos_trades <- oos_trades()
    plot(cumsum(oos_trades$Profit), main = "Cumulative Returns")
  })
  
  output$oos_cum_profit_pct <- renderPlot({
    req(input$oos_file)
    oos_trades <- oos_trades()
    plot(cumsum(oos_trades$Profit_pct), main = "Cumulative Returns")
  })
  
  output$oos_drawdown <- renderPlot({
    req(input$oos_file)
    oos_trades <- oos_trades()
    plot(oos_trades$Drawdown, main = "Drawdown")
  })
  
  output$oos_drawdown_pct <- renderPlot({
    req(input$oos_file)
    oos_trades <- oos_trades()
    plot(oos_trades$Drawdown_pct, main = "Drawdown")
  })
}