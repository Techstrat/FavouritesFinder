#---Shiny app to find special and current prices for products across retailers------
library(shiny)
library(trundler)
library(tibble)
library(dplyr)
library(DT)

set_api_key("#############")
all_retailers <- retailer()     #trundler function    
retailList <- all_retailers %>% 
    filter(currency=="ZAR")  %>% 
    arrange(retailer)  %>% 
    select(retailer)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Check prices and specials for favourites"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            tags$style(type="text/css", "
           #loadmessage {
             position: fixed;
             top: 0px;
             left: 0px;
             width: 100%;
             padding: 5px 0px 5px 0px;
             text-align: center;
             font-weight: bold;
             font-size: 100%;
             color: #ffffff;
             background-color: #4b4ed1;
             z-index: 105;
           }
        "),           
            h3("Select retailers"),
            selectizeInput(
                "retailer_list","Retailer List",choices = retailList,
                selected = c("Cybercellar","Getwine"), 
                multiple = TRUE, options = list(maxItems = 5)
            ),
            em("Select at least 1 and up to 5 retailers."),  

            h3("Product keys"),
              textInput("string1","Key 1",value = ""),
              textInput("string2","Key 2",value = ""),
              textInput("string3","Key 3",value = ""),
            em("Give at least 1 and up to 3 key text values that should be in the name of your favourites."),  
            em("The more specific the keys, the faster you'll get results"),# - results per retailer will be limited to 5."),  
            
            submitButton("Find"),
            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                             tags$div("Searching for and loading products with their prices...Depending on how specific your search is, this might take a few minutes.",id="loadmessage"))
            ),

        # Show a plot of the generated distribution
        mainPanel(
        h3("Latest prices in ZAR"),
        dataTableOutput("priceTable"),
        textOutput("Feedback")
        )
    )
)

#----------------------------------------------------
# Define server logic 
server <- function(input, output) {
   #-----------Define functions-----------
    allProducts <- function(retailer_list,regExpression) {
        #-----------Load products with product keys in the name for retailers on the list------------
        #Assumption: SKU is unique
        all_products <- tibble(product_id=numeric(),product=character(),sku=character(),
                               retailerID=numeric(),retailerName=character())
        all_retailers <- retailer()     #trundler function    
        
        for (retailerName in retailer_list) {
            retailerID <- all_retailers[all_retailers$retailer == retailerName,]$retailer_id
            Sys.sleep(2)  #delay to avoid getting blocked
            products <- retailer_products(retailerID, product = regExpression,limit=1)  #trundler function
            
            if (nrow(products)==0) { #no products retrieved - add blank row
                products <- tibble(product_id=numeric(),product=character(),sku=character())
                products <- products %>% add_row(product = "No products",sku = "Empty",product_id = 0)
            }
            else {
                products <- distinct(products, sku, .keep_all = TRUE) #remove duplicate product listing
                drops <- c("model","brand")
                products <- products[,!(names(products) %in% drops)]
                products <- products[complete.cases(products), ]
     #           products <- head(products,5)  #limit to 5 products per retailer
            }
            products$retailerID = retailerID
            products$retailerName = retailerName  
            all_products <- rbind(all_products,products)
        }
        return(all_products)
    }
    
    latestPrices <- function(retailer_list,regExpression) {
        #-----------Load prices for the selected products-----------------------
        latest_prices <- tibble(Retailer=character(),Product=character(),
                                Date=character(),Price=numeric(),Discounted=numeric())
        products <- allProducts(retailer_list,regExpression)
        products <- arrange(products,product)   
        
        for (i in 1:nrow(products)) {
            if (products[i,]$sku == "Empty") {
                price_row <- tibble_row(Retailer = products[i,]$retailerName,
                                        Product = products[i,]$product, Date = "", Price = 0,
                                        Discounted = 0)
                
            }
            else {
                Sys.sleep(2) #delay to avoid getting blocked
                price_page <- product_prices(products[i,]$product_id,limit=1)  #trundler function
                last_price <- price_page %>%  #do not assume latest price is first on the list, sort
                    arrange(desc(time)) %>%
                    head(1)
                date <- format(last_price$time,format='%Y-%m-%d')
                promotion <- last_price$price - last_price$price_effective
    
                price_row <- tibble_row(Retailer = products[i,]$retailerName,
                                        Product = products[i,]$product, Date = date, 
                                        Price = last_price$price_effective,
                                        Discounted = promotion)
            }
            latest_prices <- rbind(latest_prices,price_row)
        }
        return(latest_prices)
    }
    #-----------Capture input variables-----------
    checkKeys <- reactive({         #check if there are at least one product key
        !(nchar(paste(input$string1,input$string2,input$string3,sep=""))==0) & !(is.null(retailer_list()))

    })
    
    retailer_list <- reactive({     #get list of retailers
        input$retailer_list
    })
    
    regExpression <- reactive({     #build regular expression from product keys
        paste("(?=.*?(",input$string1,"))(?=.*?(",input$string2,"))(?=.*?(",input$string3,"))",sep="")
    })
    
    feedback <- reactive({
        ifelse(checkKeys(),"","Select at least one retailer and enter at least one key value before selecting Find")
    })
    
    prices <- reactive({
        if (checkKeys()) {
            latestPrices(retailer_list(),regExpression())
        }
        else {
            tibble(Retailer=character(),Product=character(),
                   Date=character(),Price=numeric(),Discounted=numeric())
        }
    })
    
#Output
    output$priceTable <- DT::renderDataTable({
        DT::datatable(prices(),rownames=FALSE, 
                      extensions = c('Buttons'),
                      options = list(order = list(3, 'asc'),
                                     dom = 'Bfrtip',
                                     buttons = c('pageLength','copy'),
                                     pageLength = 25,
                                     lengthChange = TRUE,
                                rowCallback = JS(
            "function(row, data) {",
            "var num = 'R'+data[3].toFixed(2).replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');",
            "$('td:eq(3)', row).html(num);",
            "var num = 'R'+data[4].toFixed(2).replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');",
            "$('td:eq(4)', row).html(num);",
            "}"
                                    )
            ))
    })

    output$Feedback <- renderText({
        feedback()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
