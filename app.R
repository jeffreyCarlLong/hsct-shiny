#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# 
# The Recipient Donor Mapping App
#
# get shiny, DBI, dplyr and dbplyr from CRAN
# install.packages("shiny")
# install.packages("DBI")
# install.packages("dplyr")
# install.packages("dbplyr")

# get pool from GitHub, since it's not yet on CRAN
# devtools::install_github("rstudio/pool")

# install.packages("KMsurv")
library(KMsurv)
library(survival)
library(shiny)
library(DBI)
library(dplyr)
library(dbplyr)
library(devtools)
library(pool)
# install.packages("RSQLite")
library(RSQLite)

library(ggplot2)

# con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")

# remotes::install_github("higgi13425/medicaldata")
library(medicaldata)

# data(package = "medicaldata")

# names(cytomegalovirus)
# str(cytomegalovirus)

# copy_to(con, medicaldata::cytomegalovirus, "cytomegalovirus",
#         temporary = FALSE, 
#         indexes = list(
#           "ID", 
#           "diagnosis", 
#           "diagnosis.type",
#           "time.to.transplant",
#           "recipient.cmv",
#           "donor.cmv",
#           "TBI.dose"
#         )
# )

# install.packages("plotly")
library(plotly)


# Data Type Management
cytomegalovirus$ID <- as.factor(cytomegalovirus$ID)
cytomegalovirus$sex <- as.factor(cytomegalovirus$sex)
cytomegalovirus$race <- as.factor(cytomegalovirus$race)
cytomegalovirus$diagnosis <- as.factor(cytomegalovirus$diagnosis)
cytomegalovirus$diagnosis.type <- as.factor(cytomegalovirus$diagnosis.type)
cytomegalovirus$prior.radiation <- as.factor(cytomegalovirus$prior.radiation)
cytomegalovirus$prior.chemo <- as.factor(cytomegalovirus$prior.chemo)
cytomegalovirus$prior.transplant <- as.factor(cytomegalovirus$prior.transplant)
cytomegalovirus$recipient.cmv <- as.factor(cytomegalovirus$recipient.cmv)
cytomegalovirus$donor.sex <- as.factor(cytomegalovirus$donor.sex)
cytomegalovirus$TBI.dose <- as.factor(cytomegalovirus$TBI.dose)
colnames(cytomegalovirus)[names(cytomegalovirus) == "C1/C2"] <- "C1_C2"
cytomegalovirus$`C1_C2` <- as.factor(cytomegalovirus$`C1_C2`)
cytomegalovirus$aKIRs <- as.factor(cytomegalovirus$aKIRs)
cytomegalovirus$cmv <- as.factor(cytomegalovirus$cmv)
cytomegalovirus$agvhd <- as.factor(cytomegalovirus$agvhd)
cytomegalovirus$cgvhd <- as.factor(cytomegalovirus$cgvhd)



dict <- read.delim("~/r/hsct-shiny/hsct_data_dictionary.tsv", sep = "\t")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
  # Retrospective Cohort Study of the Effects of Donor KIR genotype 
  # on the Reactivation of cytomegalovirus (CMV) after myeloablative 
  # allogeneic hematopoietic stem cell transplant.
    titlePanel("Retrospective Cohort Study of Myeloablative 
               Allogeneic Hematopoietic Stem Cell Transplant."),

    tabsetPanel(
      tabPanel("Study Details", fluid = TRUE,
               mainPanel(
                 h3("Study Abstract"),
                 
                 h4("This data set contains 64 consecutive patients who underwent 
                 T-cell replete, matched sibling donor reduced-intensity 
                 conditioning allogeneic hematopoietic stem cell transplant. 
                 A number of demographic, baseline and transplant 
                 characteristics were also collected. The 
                 dataset is cleaned and relatively complete. There are no 
                 outliers or data problems. "),
                 
                 h3("Indications: "),
                 
                 verbatimTextOutput("indications"),
                 
                 h3("Primary Risk Factor: "),
                 
                 h4("The number of activating killer immunoglobulin-like 
                 receptors (aKIRs: 1-4 vs. 5-6)"),
                 
                 h3("Primary Outcome: "),
                 
                 h4("The primary outcome is presence of and time to 
                 cytomegalovirus reactivation. "),
                 
                 h3("Background: "),
                 
                 h4("Hematopoietic stem cell transplantation (HSCT) is the transplantation of multipotent hematopoietic stem cells, from bone marrow, peripheral blood, or umbilical cord blood. It is a medical procedure most often performed for patients with certain cancers of the blood or bone marrow, such as multiple myeloma or leukemia. Allogeneic HSCT involves two people: the (healthy) donor and the (patient) recipient. Allogeneic HSC donors must have a tissue (HLA) type that matches the recipient. In myeloablative allogeneic HSCT, chemotherapy or irradiation is given immediately prior to a transplant (the conditioning regimen) with the purpose of eradicating the patient's disease prior to the infusion of HSC and to suppress immune reactions. The bone marrow can be ablated (destroyed) with dose- levels that cause minimal injury to other tissues. For many patients who are at high risk for transplant-related mortality with myeloablative allogeneic HSCT, reduced- intensity conditioning allogeneic hematopoietic stem cell transplant has proven effective. Although the reduced-intensity conditioning allogeneic HSCT may avoid many of the organ toxicities associated with myeloablative conditioning, the risk for developing graft-versus-host disease and infection including cytomegalovirus remains significant."),
                 
                 h4("Cytomegalovirus (CMV) is a common virus that can infect almost anyone. Once infected, your body retains the virus for life. Most people don't know they have CMV because it rarely causes problems in healthy people. But if pregnant or having a weakened immune system, CMV is cause for concern. For people with compromised immunity, such as after allogeneic HSCT, CMV infection can be fatal. Natural killer (NK) and T cells provide protection against CMV reactivation. The reactivity of NK cells and some T-cell subsets are regulated by the interaction of killer immunoglobulin-like receptors (KIRs) with target cell HLA class 1 molecules. The donor activating KIR genotype has been implicated as a contributing factor for CMV reactivation after myeloablative allogeneic HSCT."),
                 
                 h3("References: "),
                 
                 h4("These are data from a study by 
                 Sobecks et al. “Cytomegalovirus Reactivation After Matched 
                 Sibling Donor Reduced-Intensity Conditioning Allogeneic 
                 Hematopoietic Stem Cell Transplant Correlates With Donor 
                 Killer Immunoglobulin-like Receptor Genotype”. Exp Clin 
                 Transplant 2011; 1: 7-13.")
                 )
              ),
      tabPanel("Data Dictionary", fluid = TRUE,
               mainPanel(
                 tableOutput("dictionary")
                        )
               ),
      tabPanel("Kaplan Meier Plot", fluid = TRUE,
               sidebarPanel(
               h3("Survivial Graph"),
               # SelectInput gives you the option to choose the variable you want to observe in a dropdown list
               selectInput('sur_var', 'Factor of Survival', names(cytomegalovirus)[names(cytomegalovirus) != "time.to.transplant" & 
                                                                                     names(cytomegalovirus) != "ID" & 
                                                                                     names(cytomegalovirus) != "age" &       # derive age groups
                                                                                     names(cytomegalovirus) != "TNC.dose" & 
                                                                                     names(cytomegalovirus) != "CD34.dose" & 
                                                                                     names(cytomegalovirus) != "CD3.dose" & 
                                                                                     names(cytomegalovirus) != "CD8.dose" & 
                                                                                     names(cytomegalovirus) != "donor.cmv" & # NAs issue?
                                                                                     names(cytomegalovirus) != "time.to.cgvhd" & 
                                                                                     names(cytomegalovirus) != "time.to.agvhd" & 
                                                                                     names(cytomegalovirus) != "time.to.cmv"]),
               # SliderInput, in this case, let you select the time point you want to observe
               # sliderInput('xvalue', 'Survival Months = ',value=5, min=1, max=max(cytomegalovirus$time.to.transplant))
               ),
               mainPanel(
                 h3(textOutput("caption")),
                 plotOutput("kmplot"), 
                 tableOutput("center")
                        )
               ),
      tabPanel("Boxplot", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(
                   selectInput("boxplot_x","Select boxplot variable for x-axis",choices = names(cytomegalovirus), selected = "C1_C2"),
                   selectInput("boxplot_y","Select boxplot variable for y-axis",choices = names(cytomegalovirus), selected = "time.to.transplant")
                   ),
                 mainPanel(
                   plotOutput("box")
                   )
                 )
               ),
      tabPanel("Sactterplot", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(
                   selectInput("scatterplot_x", "Select variable for scatterplot x-axis", choices = names(cytomegalovirus), selected = "diagnosis"),
                   selectInput("scatterplot_y", "Select variable for scatterplot y-axis", choices = names(cytomegalovirus), selected = "time.to.transplant")
                   ),
                 mainPanel(
                   plotOutput(outputId = "scatter")
                   )
                 )
               ),
      tabPanel("Data_Table", fluid = TRUE,
               mainPanel(
                 dataTableOutput("table")
                 )
               ),
      tabPanel("Statistical Summary", fluid = TRUE,
               mainPanel(
                 verbatimTextOutput("summary")
                   )
               )
      
    
#     sidebarLayout(
#         sidebarPanel(
#           selectInput("boxplot_x","Select boxplot variable for x-axis",choices = names(cytomegalovirus)),
#           selectInput("boxplot_y","Select boxplot variable for y-axis",choices = names(cytomegalovirus)),
#           selectInput("scatterplot_x", "Select variable for scatterplot x-axis", choices = names(cytomegalovirus)),
#           selectInput("scatterplot_y", "Select variable for scatterplot y-axis", choices = names(cytomegalovirus))
#         ),
# 
#     mainPanel(
#       plotOutput(outputId = "scatterplot"),
#       plotOutput("boxplot"),
#       dataTableOutput("table"),
#       verbatimTextOutput("summary")
# 
#     )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    cytomegalovirus[, c(input$sur_var)]
  })
  
  # This is a caption that will show on top of the graph; the name will change based on which variable you choose
  output$caption <- renderText({
    paste("Survival Graph of", input$sur_var, sep="\n")
  })
  
  # Running the survival function
  runSur <- reactive({
    #survfit(as.formula(paste("Surv(time.to.transplant,delta) ~ ",paste(input$sur_var))),data=cytomegalovirus)
    survfit(as.formula(paste("Surv(time.to.transplant) ~ ",paste(input$sur_var))),data=cytomegalovirus)
  })
  
  # Plot the survival graph
  output$kmplot <- renderPlot({
    
    plot(runSur(), 
         col=c("red","sky blue","green","purple","orange","yellow"), xlab="Days", ylab="S(t)")
    legend("bottomleft",cex=0.9,levels(selectedData()),fill= c("red","sky blue","green","purple","orange","yellow"))
    abline(v=input$xvalue,col=1,lty=2)
  })
  
  # This table will give you the probability of survival for each class at a given time
  output$center <- renderTable({
    as.data.frame(summary(runSur(), times=cytomegalovirus$time.to.transplant )[c("surv", "time", "strata")])
  })
  
  output$indications <- renderPrint({
    unique(cytomegalovirus$diagnosis)
  })
  
  output$dictionary <- renderTable({
    dict
  })  
  
  output$scatter <- renderPlot({
    p = ggplot(data = cytomegalovirus) +
      aes_string(x = input$scatterplot_x, y = input$scatterplot_y) +
      geom_point()
    plot(p)
    observeEvent(input$update, print(as.numeric(input$update)))
  })
  
  output$box <- renderPlot({
    boxplot(get(input$boxplot_y) ~ get(input$boxplot_x) , data=cytomegalovirus)
  })
  
  output$table <- renderDataTable({
    cytomegalovirus
  })
  
  output$summary <- renderPrint({
    summary(cytomegalovirus)
  })
  
  }

# Run the application 
shinyApp(ui = ui, server = server)
