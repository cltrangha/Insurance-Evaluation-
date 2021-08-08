library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(formattable)

header <- dashboardHeader(title = "Benefit Valuation Tool")

sidebar <- dashboardSidebar(
  
  selectInput(inputId = "type_of_contr",
              label = "1. Select Type of Contract",
              choices = c("Single"  = "single_contr",
                          "Joint" = "joint_contr")),
  conditionalPanel(condition = "input.type_of_contr == 'single_contr'",
                   selectInput(inputId = "benefit_single",
                               label = "2. Choose a contract",
                               choices = c("Whole life insurance" = "wl_assurance",
                                           "Whole life annuity"= "wl_annuity",
                                           "Term insurance" = "term_assurance",
                                           "Term annuity" = "term_annuity",
                                           "Guaranteed Whole life annuity" = "gr_wl_annuity",
                                           "Pure endowment" = "pure_endowment",
                                           "Endowment" = "endowment"))),
  
  conditionalPanel(condition = "input.type_of_contr == 'joint_contr'",
                   selectInput(inputId = "benefit_joint",
                               label = "2. Choose a contract",
                               choices = c("Joint Whole life assurance" = "joint_wl_assurance",
                                           "Joint Term assurance" = "joint_term_assurance",
                                           "Joint Whole life annuity" = "joint_annuity",
                                           "Joint Term annuity" = "joint_term_annuity"))),
  numericInput(inputId = "int",
               label = "3. Interest Rate (In %)",
               value = "4",
               min = 0,
               max = 50),
  
  
  numericInput(inputId = "in_exp",
               label = "4. Initial expenses (% of gross premium)",
               value = "0",
               min = 0),
  
  conditionalPanel(condition = "input.premium_payment == 'level'",
                               numericInput(inputId = "pr_exp",
                                            label = "4.1. Premium expenses (% of gross premium)",
                                            value = "0",
                                            min = 0)),
  numericInput(inputId = "cl_exp",
               label = "5. Claim expenses (% of benefit amount)",
               value = "0",
               min = 0)
)


body <- dashboardBody(
  fluidRow(
    box(
      title = "Policyholder",
      solidHeader = T,
      width = 4,
      collapsible = T,
      collapsed = TRUE,
      conditionalPanel(condition = "input.type_of_contr == 'single_contr'",
                       
                       sliderInput(inputId = "age",
                                   label = "Age of policyholder",
                                   value = 1, min = 20, max = 100),
                       
                       selectInput("group", 
                                   label = "Choose a mortality group",
                                   choices = c("Group X" = "group_x",
                                               "Group Y" = "group_y"))
                       
      ),
      conditionalPanel(condition = "input.type_of_contr == 'joint_contr'",
                       sliderInput(inputId = "age1",
                                   label = "Age of the first policyholder",
                                   value = 1, min = 20, max = 100),
                       selectInput(inputId = "group1",
                                   label = "Mortality group for 1st policyholder",
                                   choices = c("Group X" = "group_x",
                                               "Group Y" = "group_y")),
                       sliderInput(inputId = "age2",
                                   label = "Age of the second policyholder",
                                   value = 1, min = 20, max = 100),
                       selectInput(inputId = "group2",
                                   label = "Mortality group for 2nd policyholder",
                                   choices = c("Group X" = "group_x",
                                               "Group Y" = "group_y"),
                                   selected = "group_y"))
      
    ),
    box(title = "Benefit Settings",
        solidHeader = T,
        width = 4,
        collapsible = T,
        collapsed = TRUE,
        numericInput(inputId = "assured_sum",
                     label = "Benefit Amount (a lump sum or per period)",
                     value = "1"),
        
        conditionalPanel(condition = "input.type_of_contr == 'single_contr'",               
                         conditionalPanel(condition = "input.benefit_single == 'pure_endowment'|
                                       input.benefit_single == 'term_assurance'|
                                       input.benefit_single == 'endowment'|
                                       input.benefit_single == 'term_annuity'|
                                       input.benefit_single == 'gr_wl_annuity'",
                                          numericInput(inputId = "term_single", 
                                                       label = "Benefit Term (in years)",
                                                       value = "10")),
                         
                         conditionalPanel(condition = "input.benefit_single == 'wl_assurance'|
                                       input.benefit_single == 'endowment'|
                                       input.benefit_single == 'term_assurance'",
                                          selectInput(inputId = "benefit_payment_single",
                                                      label = "Time of payment",
                                                      choices = c("End of year of death" = "arrears",
                                                                  "Immediately on death" = "immediate"))),
                         
                         conditionalPanel(condition= "input.benefit_single == 'wl_annuity'|
                                       input.benefit_single == 'term_annuity'|
                                       input.benefit_single == 'gr_wl_annuity'",
                                          selectInput(inputId = "benefit_frequency_single",
                                                      label="Frequency of payments",
                                                      choices = c("Monthly"="monthly",
                                                                  "Yearly"="yearly"),
                                                      selected = "yearly"))),
        conditionalPanel(condition = "input.type_of_contr == 'joint_contr'",
                         
                         conditionalPanel(condition =  "input.benefit_joint == 'joint_term_annuity'|
                                       input.benefit_joint == 'joint_term_assurance'",
                                          numericInput(inputId = "term_joint", 
                                                       label = "Benefit Term (in years)",
                                                       value = "10")),
                         
                         conditionalPanel(condition = "input.benefit_joint == 'joint_wl_assurance'|
                                       input.benefit_joint == 'joint_term_assurance'",
                                          selectInput(inputId = "benefit_payment_joint",
                                                      label = "Time of payment",
                                                      choices = c("End of year of death" = "arrears",
                                                                  "Immediately on death" = "immediate"))),
                         
                         conditionalPanel(condition= "input.benefit_joint == 'joint_annuity'|
                                       input.benefit_joint == 'joint_term_annuity'",
                                          selectInput(inputId = "benefit_frequency_joint",
                                                      label = "Frequency of payments",
                                                      choices = c("Monthly" = "monthly",
                                                                  "Yearly" ="yearly"),
                                                      selected = "yearly")))
    ),
    box(title = "Premium Settings",
        solidHeader = T,
        width = 4,
        collapsible = T,
        collapsed = TRUE,
        selectInput(inputId = "premium_payment",
                    label = "Premium Payment",
                    choices = c("Single" = "single",
                                "Level" = "level")),
        
        conditionalPanel(condition = "input.premium_payment == 'level'",
                         selectInput(inputId = "frequency_premium",
                                     label = "Frequency of payment",
                                     choices = c("Monthly"="monthly",
                                                 "Yearly"="yearly"),
                                     selected = "yearly")))
    
  ),
  fluidRow(
    column(width = 4,
           valueBoxOutput("premium", width = NULL),
           conditionalPanel(condition = "input.premium_payment =='level'& input.frequency_premium == 'monthly'",
                            valueBoxOutput("premium_pa", width = NULL)
    )),
    column(width = 8,
           box(width = NULL,title = "Evolution of Reserves",
               solidHeader = T,
               collapsible = F,
               plotlyOutput("plot"))
    
  ))
  
)

ui <- dashboardPage(header, sidebar, body, skin = "purple")


