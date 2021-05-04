library(shiny)
library(ggplot2)
library(readr)
library(dplyr)

options(scipen = 999)
theme_set(theme_minimal())

BTC = read_csv("https://api.blockchair.com/bitcoin/blocks?a=date,sum(transaction_count)&export=csv")
ETH = read_csv("https://api.blockchair.com/ethereum/blocks?a=date,sum(transaction_count)&export=csv")
DOGE = read_csv("https://api.blockchair.com/dogecoin/blocks?a=date,sum(transaction_count)&export=csv")
LTC = read_csv("https://api.blockchair.com/litecoin/blocks?a=date,sum(transaction_count)&export=csv")
DASH = read_csv("https://api.blockchair.com/dash/blocks?a=date,sum(transaction_count)&export=csv")
ZEC = read_csv("https://api.blockchair.com/zcash/blocks?a=date,sum(transaction_count)&export=csv")

colnames(BTC)[2] = "BTC"
colnames(ETH)[2] = "ETH"
colnames(DOGE)[2] = "DOGE"
colnames(LTC)[2] = "LTC"
colnames(DASH)[2] = "DASH"
colnames(ZEC)[2] = "ZEC"

df = left_join(BTC, ETH, by = "date")
df = left_join(df, DOGE, by = "date")
df = left_join(df, LTC, by = "date")
df = left_join(df, DASH, by = "date")
df = left_join(df, ZEC, by = "date")

df[is.na(df)] = 0


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Daily Transactions of Other Cryptocurrencies"),

    sidebarLayout(
        sidebarPanel(
            selectInput("coin",
                        "Select crypto to compare with Bitcoin:",
                        choices = colnames(df)[-1],
                        selected = "BTC",
                        multiple = F)
        ),
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot(
        ggplot(df, aes(date))+
            geom_line(aes(y = BTC), color = "grey20", alpha = 0.9)+
            geom_line(aes_string(y = input$coin), color = "orangered2", alpha = 0.9)+
            ggtitle("Historical Daily Transactions")+
            xlab("Date")+
            ylab("Transactions Per Day")
        
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
