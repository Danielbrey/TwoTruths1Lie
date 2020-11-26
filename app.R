library(tidyverse)
library(rvest)
library(shiny)


ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            textInput(inputId = "celebrity.name",
                      label = "Celebrity Name"),
            radioButtons(inputId = "celebrity.guess",
                         label = "Choose the wrong statement!",
                         choices = c("Age", "Birthplace", "Zodiac Sign")),
            actionButton(inputId = "button",
                         label = "Guess!")
        ),
        mainPanel(
            htmlOutput(outputId = "celebrity.details"),
            htmlOutput(outputId = "celebrity.answer")
        )
    )
)


server <- function(input, output) {

    wrong <- reactive({input$celebrity.name
        sample(1:3, 1)
    })
    
    output$celebrity.details <- renderUI({
        tryCatch({
            
            #Zodiac
            url1 <- paste0("https://www.famousbirthdays.com/people/", str_replace_all(input$celebrity.name, " ", "-"), ".html")
            zodiac <- url1 %>% 
                read_html() %>% 
                html_nodes(xpath = '/html/body/div[1]/div[1]/div[1]/div/div[3]/div/div/div[2]/div/div[1]/div/div[4]/div/a') %>% 
                html_text()
            
            #Age
            age <- url1 %>% 
                read_html() %>% 
                html_nodes(xpath = '/html/body/div[1]/div[1]/div[1]/div/div[3]/div/div/div[2]/div/div[1]/div/div[3]/div/a') %>% 
                html_text()
            
            age.mod <- tail(age, 1)
            age.mod2<- str_extract(age.mod, "[:digit:]+")
            
            #Birthplace
            birthplace <- url1 %>% 
                read_html() %>% 
                html_nodes(xpath = '/html/body/div[1]/div[1]/div[1]/div/div[3]/div/div/div[2]/div/div[1]/div/div[2]/div/a[1]') %>% 
                html_text()
            
            
            if(wrong() == 2){
                birthplace <- "Redding"
            }
            if(wrong() == 3){
                zodiac <- "Unicorn"
            }
            if(wrong() == 1){
                age.mod2 <- "7"
            }
            str1<-  paste0("Age: ", age.mod2, " years old")
            str2<- paste0("Birthplace: ", birthplace[[1]])
            str3<- paste0("Zodiac: ",zodiac)
            HTML(paste(str1, str2, str3, sep = "<br/>"))
            
        },
        warning=function(w) { 
            print("Warning")
        },
        error=function(e) {
            HTML("Please give a valid celebrity's name")
        })
    })
    
    
    observeEvent(input$button,
                 {
                     if((wrong() == 1 && input$celebrity.guess == "Age") || (wrong() == 2 && input$celebrity.guess == "Birthplace") || (wrong() == 3 && input$celebrity.guess == "Zodiac Sign")){
                         output$celebrity.answer <- renderUI({
                             HTML("Guess is correct!")
                         })
                     } else{
                         output$celebrity.answer <- renderUI({
                             HTML("Guess is incorrect :(")
                         })
                     }
                 })
}


shinyApp(ui = ui, server = server)
