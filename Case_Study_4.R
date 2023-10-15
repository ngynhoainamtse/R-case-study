library(shiny)
library(ggplot2)

processus <- function(n, cas, taille_popu = 3 * n, p=0.5) {
  # Initialize pivot variables
  pivot_M1 <- 0
  pivot_M2 <- 0
  
  # Simulation of the choice of voters
  vec_etat <- switch(cas, IC = rbinom(3, n, p), IAC_star = rbinom(3, n, runif(3)))
  
  # Vainqueur de l'élection M1
  vote_popu <- sum(vec_etat)
  val_seuil_pop <- (taille_popu + 1) / 2
  winn_D_M1 <- (vote_popu >= val_seuil_pop)
  
  # Vainqueur de l'élection M2
  nb_etat_gagnant <- sum(vec_etat >= (n + 1) / 2)
  winn_D_M2 <- (nb_etat_gagnant > 1)
  
  # Probability of being a pivot in the case of M1
  if ((winn_D_M1 & (vote_popu == val_seuil_pop)) | (!winn_D_M1 & (vote_popu == (taille_popu - 1) / 2))) {
    pivot_M1 <- val_seuil_pop
  }
  
  # Probability of being a pivot in the case of M2
  if (winn_D_M2 & nb_etat_gagnant == 2) {
    pivot_M2 <- sum((n + 1) / 2 * (vec_etat == (n + 1) / 2))
  } else if (!winn_D_M2 & nb_etat_gagnant == 1) {
    pivot_M2 <- sum((n + 1) / 2 * (vec_etat == (n - 1) / 2))
  }
  
  return(c(pivot_M1 = pivot_M1, pivot_M2 = pivot_M2))
}

simul_elec <- function(n, cas, B = 1000, p=0.5) {
  
  # vérification
  stopifnot(n %% 2 == 1, cas %in% c("IC", "IAC_star"), p >=0, p <=1)
  
  # initialisation: nombre total d'électeurs
  taille_popu <- 3 * n
  
  # réplication of the function processus()
  res_simul <- replicate(B, processus(n = n, cas = cas, taille_popu = taille_popu, p=p))
  
  # on retourne les résultats
  return(rowMeans(res_simul) / taille_popu)
}

ui <- fluidPage(
  titlePanel("Case Study"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("Size", "Size of population:", value =5),
      selectInput("Option", "Probabilistic Model:", choices = c("IC", "IAC_star")),
      conditionalPanel(
        condition = "input.Option == 'IC'",
        numericInput("Prob", "Probability:", value =0.5)
      ),
      numericInput("Rep", "Number of replications:", value =100000),
      selectInput("PlotType", "Type of Output:", choices = c("Histogram", "Pie Chart", "Table"))
    ),
    
    mainPanel(
      uiOutput("dynamic_output")
    )
  )
)

server <- function(input, output) {
  
  output$dynamic_output <- renderUI({
    switch(input$PlotType,
           "Histogram" = plotOutput("histogram", width = "500px", height = "300px"),
           "Pie Chart" = plotOutput("piechart", width = "500px", height = "300px"),
           "Table" = tableOutput("table")
    )
  })
  
  output$histogram <- renderPlot({
    res <- simul_elec(n = input$Size, cas = input$Option, B = input$Rep, p=input$Prob)
    
    data <- data.frame(Model=c("M1","M2"), Probability=res)
    p <- ggplot(data,aes(x=Model,y=Probability)) +
      geom_histogram(stat="identity",position="dodge")+
      xlab("Mechanism")+
      ylab("Probability")+
      labs(fill="Mechanism")+
      theme_minimal()+
      theme(axis.text.x=element_text(angle=0,vjust=0.2))+
      geom_text(aes(label=round(Probability, 2)), vjust=-0.3, color="black")
    
    p+ggtitle("Mechanism comparison")
  })
  
  output$piechart <- renderPlot({
    res <- simul_elec(n = input$Size, cas = input$Option, B = input$Rep, p=input$Prob)
    
    data <- data.frame(Model=c("M1","M2"), Probability=res)
    p <- ggplot(data,aes(x="",y=Probability,fill=Model))+
      geom_bar(width=1,stat="identity")+
      coord_polar(theta="y")+
      xlab("")+
      ylab("")+
      labs(fill="Mechanism")+
      theme_void()
    
    p+ggtitle("Mechanism comparison")
  })
  
  output$table <- renderTable({
    res <- simul_elec(n = input$Size, cas = input$Option, B = input$Rep, p=input$Prob)
    
    data.frame(Model=c("M1","M2"), Probability=res)
  })
}

#HOLA
shinyApp(ui = ui, server = server) # I add a comment here