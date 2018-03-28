is <- tabItem(tabName = "is",
              fluidPage(
                fluidRow(
                  box(width = 12,
                      fileInput(inputId = "is_file", label = "Input In-Sample file",
                                accept = c(".xlsx")
                      )),
                  box(width = 12,
                      dataTableOutput("is_table")),
                  box(width = 12,
                      dataTableOutput("is_monthly")),
                  box(width = 12,
                      plotOutput("is_perf_summary")),
                  box(width = 12,
                      plotOutput("is_hist")),
                  box(width = 12,
                      plotOutput("is_cum_profit")),
                  box(width = 12,
                      plotOutput("is_cum_profit_pct")),
                  box(width = 12,
                      plotOutput("is_drawdown")),
                  box(width = 12,
                      plotOutput("is_drawdown_pct"))
                )
              ))