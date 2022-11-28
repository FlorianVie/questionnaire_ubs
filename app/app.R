library(shiny)

ui <- fluidPage(
  
  titlePanel("Analyse Factorielle Exploratoire"),
  
  sidebarLayout(
    
    sidebarPanel(
      checkboxGroupInput("itemSelect", "Items sélectionnés :",
                         c("LDS2" = "LDS2", "LDS3" = "LDS3", "LDS4" = "LDS4", "LDS5" = "LDS5", "LDS6" = "LDS6", "NSUB1" = "NSUB1", "NSUB2" = "NSUB2", "NSUB3" = "NSUB3", "NSUB4" = "NSUB4", "NSUB5" = "NSUB5", "CF1" = "CF1", "CF2" = "CF2", "CF3" = "CF4", "CF5" = "CF5", "CF6" = "CF6", "CF7" = "CF7", "CF8" = "CF8", "AFE1" = "AFE1", "AFE2" = "AFE2", "AFE4" = "AFE4", "AFE5" = "AFE5", "APF1" = "APF1", "APF2" = "APF2", "APF3" = "APF3", "APF4" = "APF4", "APF5" = "APF5", "APF6" = "APF6", "IEIP1" = "IEIP1", "IEIP2" = "IEIP2", "IEIP3" = "IEIP3", "IEIP4" = "IEIP4", "MOT1" = "MOT1", "MOT2" = "MOT2", "MOT3" = "MOT3", "MOT4" = "MOT4", "MOT5" = "MOT5", "MOT6" = "MOT6", "MOT7" = "MOT7", "MOT8" = "MOT8", "MOT9" = "MOT9", "INTU1" = "INTU1"),
                         selected = c("LDS2" = "LDS2", "LDS3" = "LDS3", "LDS4" = "LDS4", "LDS5" = "LDS5", "LDS6" = "LDS6", "NSUB1" = "NSUB1", "NSUB2" = "NSUB2", "NSUB3" = "NSUB3", "NSUB4" = "NSUB4", "NSUB5" = "NSUB5", "CF1" = "CF1", "CF2" = "CF2", "CF3" = "CF4", "CF5" = "CF5", "CF6" = "CF6", "CF7" = "CF7", "CF8" = "CF8", "AFE1" = "AFE1", "AFE2" = "AFE2", "AFE4" = "AFE4", "AFE5" = "AFE5", "APF1" = "APF1", "APF2" = "APF2", "APF3" = "APF3", "APF4" = "APF4", "APF5" = "APF5", "APF6" = "APF6", "IEIP1" = "IEIP1", "IEIP2" = "IEIP2", "IEIP3" = "IEIP3", "IEIP4" = "IEIP4", "MOT1" = "MOT1", "MOT2" = "MOT2", "MOT3" = "MOT3", "MOT4" = "MOT4", "MOT5" = "MOT5", "MOT6" = "MOT6", "MOT7" = "MOT7", "MOT8" = "MOT8", "MOT9" = "MOT9", "INTU1" = "INTU1"),
                         inline = T),
      numericInput("nbFacteurs", "Nombre de facteurs :", 1, min = 1),
      h3("Scree Plot"),
      plotOutput("screePlot"),
      h3("Normalité multivariée"),
      tableOutput("normTable"),
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Description", tableOutput("descTable")),
                  
                  tabPanel("Densité", plotOutput("densPlot")),
                  
                  tabPanel("Corrélations", tabsetPanel(type = "pills",
                                                       tabPanel("Corrélogramme", plotOutput("corrPlot")),
                                                       tabPanel("Matrice", tableOutput("corrMat")),
                                                       tabPanel("Table", tableOutput("corrTable"))
                  )),
                  
                  tabPanel("Alpha", verbatimTextOutput("fideliteText")),
                  
                  tabPanel("AFE", verbatimTextOutput("facText")),
                  
                  tabPanel("Loadings", verbatimTextOutput("loadingsText")),
                  
                  tabPanel("Diagramme", plotOutput("diagPlot")),
                  
      )
    ),
  )
  
  
  
)

# Define server logic required to draw a histogram

library(tidyverse)
library(ggpubr)
library(rstatix)
library(jsonlite)
library(corrplot)
library(GPArotation)
library(psych)
library(MVN)

data_raw <- read.csv("https://raw.githubusercontent.com/FlorianVie/questionnaire_ubs/main/data/reponses.csv")


imotris <- data_raw 

items <- imotris %>%
  select(-Rep)

imotris_long <- imotris %>%
  pivot_longer(c('LDS2','LDS3','LDS4','LDS5','LDS6','NSUB1','NSUB2','NSUB3','NSUB4','NSUB5','CF1','CF2','CF3','CF4','CF5','CF6','CF7','CF8','AFE1','AFE2','AFE4','AFE5','APF1','APF2','APF3','APF4','APF5','APF6','IEIP1','IEIP2','IEIP3','IEIP4','MOT1','MOT2','MOT3','MOT4','MOT5','MOT6','MOT7','MOT8','MOT9','INTU1'), names_to = "item")





# --------------------------

server <- function(input, output) {
  
  output$densPlot <- renderPlot({
    ggdensity(imotris_long, x = "value", facet.by = "item", add = "mean")
  })
  
  output$corrPlot <- renderPlot({
    items <- items %>%
      select(input$itemSelect)
    
    items %>%
      cor_mat() %>%
      cor_plot(method="color", type="lower")
  })
  
  output$corrTable <- renderTable({
    items <- items %>%
      select(input$itemSelect)
    
    items %>%
      cor_test()
  }, hover = T)
  
  output$corrMat <- renderTable({
    items <- items %>%
      select(input$itemSelect)
    
    items %>%
      cor_mat()
  }, hover = T)
  
  output$screePlot <- renderPlot({
    items <- items %>%
      select(input$itemSelect)
    
    fa.parallel(items, fa="fa", fm="pa")
  })
  
  output$facText <- renderPrint({
    items <- items %>%
      select(input$itemSelect)
    
    print(fa(items, nfactors = input$nbFacteurs, rotate = "oblimin", fm = "pa", use = "pairwise"))
  })
  
  output$loadingsText <- renderPrint({
    items <- items %>%
      select(input$itemSelect)
    
    fit <- fa(items, nfactors = input$nbFacteurs, rotate = "oblimin", fm = "pa", use = "pairwise")
    print(fit$loadings, sort = T, cutoff = 0.3)
  })
  
  output$diagPlot <- renderPlot({
    items <- items %>%
      select(input$itemSelect)
    
    fa.diagram(fa(items, nfactors = input$nbFacteurs), digits = 2)
  })
  
  output$fideliteText <- renderPrint({
    items <- items %>%
      select(input$itemSelect)
    
    alpha(items)
  })
  
  output$normTable <- renderTable({
    items <- items %>%
      select(input$itemSelect)
    
    mvn(items)$multivariateNormality
  }, hover = T)
  
  output$descTable <- renderTable({
    items <- items %>%
      select(input$itemSelect)
    
    describe(items)
  }, rownames = T, hover = T)
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
