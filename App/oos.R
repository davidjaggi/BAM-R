oos <- tabItem(tabName = "oos",
              fluidPage(
                fluidRow(
                  box(width = 12,
                      fileInput(inputId = "oos_file", label = "Input Out-of-Sample file",
                                accept = c(".xlsx")
                      )),
                  box(width = 12,
                      dataTableOutput("oos_table")),
                  box(width = 12,
                      dataTableOutput("oos_monthly")),
                  box(width = 12,
                      plotOutput("oos_perf_summary")),
                  box(width = 12,
                      plotOutput("oos_hist")),
                  box(width = 12,
                      plotOutput("oos_cum_profit")),
                  box(width = 12,
                      plotOutput("oos_cum_profit_pct")),
                  box(width = 12,
                      plotOutput("oos_drawdown")),
                  box(width = 12,
                      plotOutput("oos_drawdown_pct"))
                )
              ))