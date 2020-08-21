# Favourites Finder

The Favourites Finder is an r shiny app that allows the user to select up to 5 retailers, give up to 3 key words the product has in common across the retailers and click find.  Within minutes the Favourites Finder lists the Retailer, Product and up to date price,highlighting any specials with the discount.

## Link
https://techstrat.shinyapps.io/SpecialsAlert/

## Make-up

The Shiny app uses the trundler api. It calls three functions from the trundler package:
* retailer() – returns the complete list of retailers
* retailer_products() – returns a list of products containing the key phrases per retailer
* product_prices() – returns recent prices for a specific product

Other package used are
* shiny
* tibble
* dplyr
