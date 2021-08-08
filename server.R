library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(formattable)

lx <- c(10000.0000,9994.0000,9988.0636,9982.2006,9976.3909,9970.6346,9964.9313,9959.2613,9953.6144,9947.9807,9942.3402,9936.6730,9930.9694,9925.2094,9919.3535,9913.3821,9907.2655,9900.9645,9894.4299,9887.6126,9880.4540,9872.8954,9864.8688,9856.2863,9847.0510,9837.0661,9826.2060,9814.3359,9801.3123,9786.9534,
        9771.0789,9753.4714,9733.8865,9712.0728,9687.7149,9660.5021,9630.0522,9595.9715,9557.8179,9515.1040,9467.2906,9413.8004,9354.0040,9287.2164,9212.7143,9129.7170,9037.3973,8934.8771,8821.2612,8695.6199,8557.0118,8404.4916,8237.1329,8054.0544,7854.4508,7637.6208,7403.0084,7150.2401,6879.1673,6589.9258,
        6282.9803,5959.1680,5619.7577,5266.4604,4901.4789,4527.4960,4147.6708,3765.5998,3385.2479,3010.8395,2646.7416,2297.2976,1966.6499,1658.5545,1376.1906,1121.9889,897.5025,703.3242,539.0643,403.4023,294.2061,208.7060,143.7120,95.8476,61.7733,38.3796,22.9284,13.1359,7.1968,3.7596,1.8669,0.8784,0.3903,0.1632,0.0640,0.0234,
        0.0080,0.0025,0.0007,0.0002,0.0000,0.0000,0.0000000000,0.0000)

Age = seq(17,120)
mort_table <- data.frame(Age,lx)
mort_table_y <- data.frame(Age = Age[4:104],lx=lx[1:101])

Assurance <- function(table, age,ii){
  V = 1/(1+ii)
  Index = which(table$Age==age)
  N = length(table$Age[Index:length(table$Age)])
  DiscountingPart = (V^(1:(N-1)))
  MortalityPart   = ( table$lx[(Index):(length(table$lx)-1)] - table$lx[(Index+1):(length(table$lx))] ) / table$lx[Index]
  PresentValue = sum( DiscountingPart * MortalityPart )
  return(PresentValue)}

Immediate_Assurance <- function(table, age, ii){
  Immediate_Assurnace = Assurance(table,age, ii)*(1+ii)^0.5
  return(Immediate_Assurnace)}

Term_Assurance <- function(table, age, ii, n){
  V = 1/(1+ii)
  Assurance_x <- Assurance(table, age, ii)
  Assurance_x_n <- Assurance(table, (age+n) ,ii)
  Index = which(table$Age==age)
  N = length(table$Age[Index:length(table$Age)])
  npx = table$lx[(Index+n)] / table$lx[Index]
  Term_Assurance = Assurance_x - V^n*npx*Assurance_x_n
  return(Term_Assurance)}

Immediate_Term_Assurance <- function(table, age, ii,n){
  Immediate_Term_Assurance = Term_Assurance(table, age, ii, n)*(1+ii)^0.5
  return(Immediate_Term_Assurance)}

Pure_Endowment <- function(table, age, ii, n){
  V = 1/(1+ii)
  Index = which(table$Age==age)
  N = length(table$Age[Index:length(table$Age)])
  npx = table$lx[(Index+n)] / table$lx[Index]
  Pure_Endowment = npx*V^n
  return(Pure_Endowment)}

Endowment <- function(table, age, ii, n){
  Survival_benefit = Pure_Endowment(table, age, ii, n)
  Death_benefit = Term_Assurance(table, age, ii, n)
  Endowment = Survival_benefit+Death_benefit
  return(Endowment)}

Immediate_Endowment <- function(table, age, ii, n){
  Immediate_Endowment = Immediate_Term_Assurance(table, age, ii, n)+Pure_Endowment(table, age, ii, n)
  return(Immediate_Endowment)}

Annuity <- function(table,age,ii){
  V = 1/(1+ii)
  Index = which(table$Age==age)
  N = length(table$Age[Index:length(table$Age)])
  DiscountingPart = (V^(1:(N-1)))
  SurvivalPart   = table$lx[(Index+1):(length(table$lx))] / table$lx[Index]
  PresentValue = sum( DiscountingPart * SurvivalPart )
  return(PresentValue)}

Monthly_Annuity <- function(table,age,ii){
  Annuity <- Annuity(table, age,ii)
  Monthly_Annuity <-  Annuity+11/24
  return(Monthly_Annuity)}

Annuity_Due <- function(table, age, ii){
  Annuity_Due = Annuity(table, age, ii) + 1
  return(Annuity_Due)}

Monthly_Annuity_Due <- function(table, age, ii){
  Annuity <- Annuity(table, age,ii)
  Monthly_Annuity_Due = 1/12+ Annuity
  return(Monthly_Annuity_Due)}

Term_Annuity <- function(table, age, ii, n){ 
  V = 1/(1+ii)
  Index = which(table$Age==age)
  N = length(table$Age[Index:length(table$Age)])
  npx = table$lx[(Index+n)] / table$lx[Index]
  annuity_x <- Annuity(table, age, ii)
  annuity_x_n <- Annuity(table, age+n, ii)
  Term_Annuity = annuity_x - V^n*npx*annuity_x_n
  return(Term_Annuity)}

Monthly_Term_Annuity <- function(table, age, ii, n){
  V = 1/(1+ii)
  Index = which(table$Age == age)
  npx = table$lx[(Index+n)] / table$lx[Index]
  term_annuity <- Term_Annuity(table, age, ii, n)
  m_term_annuity = term_annuity+((11/24)*(1-V^n*npx))
  return(m_term_annuity)}

Term_Annuity_Due <- function(table, age, ii, n){
  V = 1/(1+ii)
  Index = which(table$Age==age)
  N = length(table$Age[Index:length(table$Age)])
  npx = table$lx[(Index+n)] / table$lx[Index]
  Term_Annuity_Due = Term_Annuity(table, age, ii, n) + 1 - (V^n)*npx
  return(Term_Annuity_Due)}

Monthly_Term_Annuity_Due <- function(table,age,ii, n){
  V = 1/(1+ii)
  Index = which(table$Age==age)
  N = length(table$Age[Index:length(table$Age)])
  npx = table$lx[(Index+n)] / table$lx[Index]
  Monthly_Term_Annuity_Due = Term_Annuity_Due(table,age,ii,n) - 11/24*(1-((V^n)*npx))
  return(Monthly_Term_Annuity_Due)}

Guaranteed_Annuity <- function(table, age, ii, n){
  V =1/(1+ii)
  Index = which(table$Age==age)
  N = length(table$Age[Index:length(table$Age)])
  npx = table$lx[(Index+n)] / table$lx[Index]
  annuity_x_n <- Annuity(table, age+n, ii)
  Guaranteed_Annuity = (1-V^n)/ii + V^n*(npx)*annuity_x_n
  return(Guaranteed_Annuity)}

Monthly_Guaranteed_Annuity <- function(table,age,ii, n){
  V = 1/(1+ii)
  Index = which(table$Age==age)
  N = length(table$Age[Index:length(table$Age)])
  npx = table$lx[(Index+n)] / table$lx[Index]
  annuity_x_n <- Annuity(table, age+n, ii)
  Monthly_Guaranteed_Annuity = (1-V^n)/(12*((1+ii)^(1/12)-1))+V^n*(npx)*(annuity_x_n+((12-1)/24))
  return(Monthly_Guaranteed_Annuity)}

Guaranteed_Annuity_Due <- function(table, age, ii, n){
  V =1/(1+ii)
  Index = which(table$Age==age)
  N = length(table$Age[Index:length(table$Age)])
  npx = table$lx[(Index+n)] / table$lx[Index]
  annuity_x_n <- Annuity(table, age+n, ii)
  Guaranteed_Annuity_Due = (1+ii)*(1-V^n)/ii + V^n*(npx)*(annuity_x_n+1)
  return(Guaranteed_Annuity_Due)}

Monthly_Guaranteed_Annuity_Due <- function(table,age,ii, n){
  V = 1/(1+ii)
  Index = which(table$Age==age)
  N = length(table$Age[Index:length(table$Age)])
  npx = table$lx[(Index+n)] / table$lx[Index]
  annuity_x_n <- Annuity(table, age+n, ii)
  Monthly_Guaranteed_Annuity_Due = (1+ii)^(1/12)*((1-V^n)/(12*((1+ii)^(1/12)-1)))+V^n*(npx)*(annuity_x_n+((13)/24))
  return(Monthly_Guaranteed_Annuity_Due)}

joint_wl_assurance <- function(table1, table2, age1, age2, ii){
  d = ii/(1+ii)
  joint_annuity_due <- joint_annuity_due(table1, table2, age1, age2, ii)
  joint_wl_assurance = 1- d* joint_annuity_due
  return(joint_wl_assurance)}

imm_joint_wl_assurance <- function(table1, table2, age1, age2, ii){
  joint_wl_assurance_immediately = (1+ii)^0.5*joint_wl_assurance(table1, table2, age1, age2, ii)
  return(joint_wl_assurance_immediately)}

joint_term_assurance <- function(table1,table2, age1, age2, ii, n){
  d= ii/(1+ii)
  V = 1/(1+ii)
  Index1 = which(table1$Age == age1)
  Index2 = which(table2$Age == age2)
  npx = table1$lx[(Index1+n)] / table1$lx[Index1]
  npy = table2$lx[(Index2+n)] / table2$lx[Index2]
  joint_term_annuity_due <- joint_term_annuity_due(table1, table2, age1, age2, ii, n)
  joint_end_assurance <-  1-d*joint_term_annuity_due
  joint_term_assurance= joint_end_assurance - V^n*npx*npy
  return(joint_term_assurance)}

imm_joint_term_assurance <- function(table1,table2, age1, age2, ii, n){
  Index1 = which(table1$Age == age1)
  Index2 = which(table2$Age == age2)
  joint_term_assurance <- joint_term_assurance(table1, table2, age1, age2, ii, n)
  imm_joint_term_assurance = ((1+ii)^0.5)*joint_term_assurance
  return(imm_joint_term_assurance)}

joint_annuity <- function(table1, table2, age1, age2, ii){
  V = 1/(1+ii)
  Index1 = which(table1$Age == age1)
  Index2 = which(table2$Age == age2)
  if (age1 >= age2){
    N = length(table1$Age[Index1:length(table1$Age)])}
  else {N = length(table2$Age[Index2:length(table2$Age)])}
  DiscountingPart = (V^(1:(N-1)))
  SurvivalPart1 = table1$lx[(Index1+1):(length(table1$lx))] / table1$lx[Index1]
  SurvivalPart2 = table2$lx[(Index2+1):(length(table2$lx))] / table2$lx[Index2]
  PresentValue = sum( DiscountingPart*SurvivalPart1*SurvivalPart2)
  return(PresentValue)}

m_joint_annuity <- function(table1, table2, age1, age2, ii){
  PresentValue <- joint_annuity(table1, table2, age1, age2, ii) + 11/24
  return(PresentValue)}

m_joint_annuity_due <- function(table1, table2, age1, age2, ii){
  PV = m_joint_annuity(table1, table2, age1, age2, ii)+1
  return(PV)}

joint_annuity_due <- function(table1, table2, age1, age2, ii){
  PresentValue = joint_annuity(table1, table2, age1, age2, ii) + 1
  return(PresentValue)}

joint_term_annuity <- function(table1, table2, age1, age2, ii, n){
  V = 1/(1+ii)
  Index1 = which(table1$Age == age1)
  Index2 = which(table2$Age == age2)
  npx = table1$lx[(Index1+n)] / table1$lx[Index1]
  npy = table2$lx[(Index2+n)] / table2$lx[Index2]
  joint_annuity <- joint_annuity(table1, table2, age1, age2, ii)
  joint_annuity_n <- joint_annuity(table1, table2, age1+n, age2+n, ii)
  PV = joint_annuity - V^n*npx*npy*joint_annuity_n
  return(PV)}

m_joint_term_annuity <- function(table1, table2, age1, age2, ii, n){
  V = 1/(1+ii)
  Index1 = which(table1$Age == age1)
  Index2 = which(table2$Age == age2)
  npx = table1$lx[(Index1+n)] / table1$lx[Index1]
  npy = table2$lx[(Index2+n)] / table2$lx[Index2]
  joint_term_annuity <- joint_term_annuity(table1, table2, age1, age2, ii,n)
  m_joint_term_annuity = joint_term_annuity+((11/24)*(1-V^n*npx*npy))
  return(m_joint_term_annuity)}

joint_term_annuity_due <- function(table1, table2, age1, age2, ii, n){
  V = 1/(1+ii)
  Index1 = which(table1$Age == age1)
  Index2 = which(table2$Age == age2)
  npx = table1$lx[(Index1+n)] / table1$lx[Index1]
  npy = table2$lx[(Index2+n)] / table2$lx[Index2]
  joint_term_annuity <- joint_term_annuity(table1, table2, age1, age2, ii, n)
  joint_term_annuity_due = joint_term_annuity + 1 - (V^n)*npx*npy
  return(joint_term_annuity_due)}

m_joint_term_annuity_due <- function(table1, table2, age1, age2, ii, n){
  Index1 = which(table1$Age == age1)
  Index2 = which(table2$Age == age2)
  V = 1/(1+ii)
  npx = table1$lx[(Index1+n)] / table1$lx[Index1]
  npy = table2$lx[(Index2+n)] / table2$lx[Index2]
  m_joint_term_annuity <- m_joint_term_annuity(table1, table2, age1, age2, ii, n)
  m_joint_term_annuity_due = m_joint_term_annuity+1- (V^n)*npx*npy
  return(m_joint_term_annuity_due)}

runningEPV <- function(mort_table, type1, type2, type3, age, int, term){
  running_EPV <- 0

  if (nrow(mort_table) > 102){
    lim <- 116
  }else{
    lim <- 119
  }
  
  if (type1 == "term_assurance"){
    
    if (type2 == "arrears"){
      
      for (t in seq(0,term)){
        running_EPV[t+1] <- Term_Assurance(mort_table, age+t, int, term - t)}}
    
    else if(type2=="immediate"){
      
      for (t in seq(0,term)){
        running_EPV[t+1] <- Immediate_Term_Assurance(mort_table, age+t, int, term - t)}}
    
    else {return(NULL)}}
  
  else if (type1 == "pure_endowment"){
    
    if (type2 == "arrears"){
      
      for (t in seq(0,term)){
        running_EPV[t+1] <- Pure_Endowment(mort_table, age+t, int, term - t)}}
    
    else if (type2 =="immediate"){
      
      for (t in seq(0,term)){
        running_EPV[t+1] <- Pure_Endowment(mort_table, age+t, int, term - t)}}
    
    else {return(NULL)}}
  
  else if (type1 =="endowment"){
    
    if (type2 =="arrears"){
      
      for (t in seq(0,term)){ 
        running_EPV[t+1] <- Endowment(mort_table, age+t, int, term - t)}}
    
    else if(type2 =="immediate"){
      
      for (t in seq(0,term)){
        running_EPV[t+1] <- Immediate_Endowment(mort_table, age+t, int, term - t)}}
  }
  
  else if(type1 == "wl_assurance"){
    
    if (type2 == "arrears"){
      for (t in seq(0, lim-age)) {
        running_EPV[t+1] <- Assurance(mort_table, age+t, int)
      }
    }
    
    else if(type2 == "immediate"){
      for (t in seq(0, lim - age)) {
        running_EPV[t+1] <- Immediate_Assurance(mort_table, age+t, int)
      }
    }
  }
  else if(type1 == "wl_annuity"){
    
    if(type3 == "yearly"){
      for(t in seq(0, lim-age ) ){
        running_EPV[t+1] <- Annuity(mort_table, age+t, int)}}
    
    else if(type3 == "monthly"){
      for(t in seq(0, lim-age)){
        running_EPV[t+1] <- Monthly_Annuity(mort_table, age+t, int)}
    }
  }
  else if (type1 == "term_annuity"){
    
    if (type3 == "yearly"){
      for (t in seq(0,term)){
        running_EPV[t+1] <- Term_Annuity(mort_table, age+t, int, term-t)
      }
    }else{
      for (t in seq(0, term)){
        running_EPV[t+1] <- Monthly_Term_Annuity(mort_table, age+t, int, term - t)
      }
    }
  }
  return(running_EPV)  
}

runningEPV_premium_wl <- function(mort_table, type1, type2, age, int, term){
  runningEPV_premium <- 0
  if (type1 == "single"){
    runningEPV_premium <- c(1,rep(0, term))
  }
  else if(type1=="level"){
    
    if(type2=="yearly"){
      for (t in seq(0,term)){
        runningEPV_premium[t+1] <- Annuity_Due(mort_table, age+t, int)
      }
    }
    else if(type2=="monthly"){
      for(t in seq(0, term)){
        runningEPV_premium[t+1]<- Monthly_Annuity_Due(mort_table, age+t, int)
      }
    }
  }
  return(runningEPV_premium)
}

runningEPV_premium <- function(mort_table, type1,type2, age, int, term){
  runningEPV_premium <- 0
  if (type1 == "single"){
    runningEPV_premium <- c(1,rep(0, term))
  }else if(type1=="level"){
    if(type2=="yearly"){
      for (t in seq(0,term)){
        runningEPV_premium[t+1] <- Term_Annuity_Due(mort_table,age+t, int, term - t)}}
    else if(type2=="monthly"){
      for(t in seq(0, term)){
        runningEPV_premium[t+1]<- Monthly_Term_Annuity_Due(mort_table,age+t, int, term - t)}}}
  return(runningEPV_premium)}  

runningEPV_joint <- function(table1, table2, type1, type2, type3, age1, age2, int, term){
  runningEPV <- 0 
  
  if (age1 >= age2){
    n <- age1
    if (nrow(table1)>102){
      lim <- 116
    }else
      lim <- 119
  }else{
    n <- age2
    if (nrow(table2)>102){
      lim <- 116
    }else{
      lim <- 119
    }
  }
  
  if (type1 == "joint_term_annuity"){
    
    if(type3== "yearly"){
      for (t in seq(0,term)){
        runningEPV[t+1] <- joint_term_annuity(table1, table2, age1+t, age2+t, int, term-t)}
    }
    else if(type3== "monthly"){
      for(t in seq(0,term)){
        runningEPV[t+1] <- m_joint_term_annuity(table1, table2, age1+t, age2+t, int, term-t)
      }
    }
  }
  else if(type1 == "joint_term_assurance"){
    
    if(type2 == "arrears"){
      for (t in seq(0,term)){
        runningEPV[t+1] <- joint_term_assurance(table1, table2, age1+t, age2+t, int, term-t)
      }
    }
    else if(type2 == "immediate"){
      for (t in seq(0, term)){
        runningEPV[t+1] <- imm_joint_term_assurance(table1, table2, age1+t, age2+t, int, term-t)
      }
    }
  }
  else if(type1 == "joint_wl_assurance"){
    
    if(type2 == "arrears"){
      for (t in (seq(0, lim-n))){
        runningEPV[t+1] <- joint_wl_assurance(table1, table2, age1+t, age2+t, int)
      }
    }else if(type2 == "immediate"){
      for (t in (seq(0, lim-n))){
        runningEPV[t+1] <- imm_joint_wl_assurance(table1, table2, age1+t, age2+t, int)
      }
    }
  }
  else if (type1 == "joint_annuity"){
    
    if(type3 == "yearly"){
      for (t in (seq(0, lim - n))){
        runningEPV[t+1] <- joint_annuity(table1, table2, age1 + t, age2 + t, int)
      }
    }else if (type3 == "monthly"){
      for (t in (seq(0, lim - n))){
        runningEPV[t+1] <- m_joint_annuity(table1, table2, age1 + t, age2 + t, int)
      }
    }
  }
  else {return(NULL)}
  return(runningEPV)
}

runningEPV_premium_joint <- function(table1, table2,type1,type2, age1, age2, int, term){
  runningEPV_premium <- 0
  
  if (type1 == "single"){
    runningEPV_premium <- c(1,rep(0, term))
  }
  else if(type1=="level"){
    
    if(type2=="yearly"){
      for (t in seq(0,term)){
        runningEPV_premium[t+1] <- joint_term_annuity_due(table1, table2,age1+t, age2+t, int, term-t )
      }
    }
    else if(type2=="monthly"){
      for (t in seq(0,term)){
        runningEPV_premium[t+1] <- m_joint_term_annuity_due(table1, table2,age1+t, age2+t, int, term-t )
      }
    }
  }
  return(runningEPV_premium)
}

runningEPV_premium_joint_wl <- function(table1, table2,type1, type2, age1, age2, int, term){
  runningEPV_premium <- 0
  
  if (type1 == "single"){
    runningEPV_premium <- c(1,rep(0, term))
  }
  else if(type1=="level"){
    
    if(type2=="yearly"){
      for (t in seq(0,term)){
        runningEPV_premium[t+1] <- joint_annuity_due(table1, table2,age1+t, age2+t, int)
      }
    }
    else if(type2=="monthly"){
      for (t in seq(0,term)){
        runningEPV_premium[t+1] <- m_joint_annuity_due(table1, table2,age1+t, age2+t, int)
      }
    }
  }
  return(runningEPV_premium)
}

running_EPV_gr <- function(table,type, age, int, term){
  if (nrow(table) > 102){
    lim <- 116
  }else{
    lim <- 119
  }
  
  runningEPV <- 0
  if (type == "yearly"){
    for (t in seq(0,term-1)){
      runningEPV[t+1] <- Guaranteed_Annuity(table, age + t, int, term - t)
    }
    for (t in seq(term, lim - age)){
      runningEPV[t+1] <- Annuity(table, age + t, int)}
  }else{
    for (t in seq(0, term-1)){
      runningEPV[t+1] <- Monthly_Guaranteed_Annuity(table, age+t, int, term-t)
    }
    for (t in seq(term, lim-age)){
      runningEPV[t+1] <- Monthly_Annuity(table, age+t, int)
    }
  }
  
  return(runningEPV)
}


server <- function(input, output){
  
  rv <- reactiveValues() 
  
  observeEvent(input$type_of_contr, { rv$type_of_contr = input$type_of_contr })
  
  observeEvent(input$age, { rv$age = input$age })
  observeEvent(input$age1 , { rv$age1 = input$age1 })
  observeEvent(input$age2, { rv$age2 = input$age2 })
  
  observeEvent(input$int, { rv$int = input$int })
  observeEvent(input$in_exp, { rv$in_exp = input$in_exp })
  observeEvent(input$pr_exp, { rv$pr_exp_1 = input$pr_exp })
  observeEvent(input$cl_exp, { rv$cl_exp = input$cl_exp })
  observeEvent(input$assured_sum, { rv$assured_sum_period = input$assured_sum })
  
  observeEvent(input$term_single, { rv$term_single = input$term_single })
  observeEvent(input$term_joint, { rv$term_joint = input$term_joint })
  
  observeEvent(input$benefit_single, { rv$benefit_single = input$benefit_single })
  observeEvent(input$benefit_joint, { rv$benefit_joint = input$benefit_joint })
  
  observeEvent(input$premium_payment, { rv$premium_payment = input$premium_payment })
  observeEvent(input$frequency_premium, { rv$frequency_premium=input$frequency_premium })
  
  observeEvent(input$benefit_payment_single, { rv$benefit_payment_single = input$benefit_payment_single })
  observeEvent(input$benefit_payment_joint, { rv$benefit_payment_joint = input$benefit_payment_joint })
  
  observeEvent(input$benefit_frequency_joint,{ rv$benefit_frequency_joint = input$benefit_frequency_joint })
  observeEvent(input$benefit_frequency_single,{ rv$benefit_frequency_single = input$benefit_frequency_single })  
  
  output$plot <- renderPlotly({
    
    rv$benefit_payment <- switch(input$type_of_contr,
                                 single_contr = rv$benefit_payment_single,
                                 joint_contr = rv$benefit_payment_joint)
  
    rv$benefit_frequency <- switch(input$type_of_contr,
                                   single_contr = rv$benefit_frequency_single,
                                   joint_contr = rv$benefit_frequency_joint)
    
    
    rv$benefit <- switch(input$type_of_contr, 
                         single_contr = rv$benefit_single,
                         joint_contr = rv$benefit_joint)
    
    rv$term <- switch(input$type_of_contr,
                      single_contr = rv$term_single,
                      joint_contr = rv$term_joint)
    
    rv$mort_table_1 <- switch(input$group1, 
                              group_x = mort_table,
                              group_y = mort_table_y)
    
    rv$mort_table_2 <- switch(input$group2,
                              group_x = mort_table,
                              group_y = mort_table_y)
    
    rv$mort_table <- switch(input$group,
                            group_x = mort_table,
                            group_y = mort_table_y)
    
    if (nrow(rv$mort_table) > 102){
      rv$lim_single <- 116
    }else{
      rv$lim_single <- 119
    }
    
    if(rv$age1 >= rv$age2){
      rv$n <- rv$age1
      if (nrow(rv$mort_table_1) > 102){
        rv$lim_joint <- 116
      }else
        rv$lim_joint <- 119
    }else {
      rv$n <- rv$age2
      if (nrow(rv$mort_table_2)>102){
        rv$lim_joint <- 116
      }else
        rv$lim_joint <- 119
    }
    
    rv$lim <- switch(rv$type_of_contr,
                     single_contr = rv$lim_single,
                     joint_contr = rv$lim_joint)
    
    rv$term1 <- switch(rv$benefit,
                       wl_assurance = rv$lim - rv$age,
                       wl_annuity = rv$lim - rv$age,
                       pure_endowment = rv$term,
                       term_assurance = rv$term,
                       term_annuity = rv$term,
                       endowment = rv$term,
                       joint_term_annuity = rv$term,
                       joint_term_assurance = rv$term,
                       joint_wl_assurance = rv$lim - rv$n,
                       joint_annuity = rv$lim - rv$n,
                       gr_wl_annuity = rv$lim - rv$age)
    
    if(rv$benefit == "endowment"){
      if(rv$benefit_payment == "arrears"){
        rv$EPV_benefit <- Endowment(rv$mort_table, rv$age, rv$int/100, rv$term1)
        rv$running_EPV <- runningEPV(rv$mort_table,"endowment", "arrears","yearly", rv$age, rv$int/100, rv$term1)}
      else{
        rv$EPV_benefit <- Immediate_Endowment(rv$mort_table, rv$age, rv$int/100, rv$term1)
        rv$running_EPV <- runningEPV( rv$mort_table,"endowment", "immediate","yearly", rv$age, rv$int/100, rv$term1)}
    }
    
    else if(rv$benefit == "wl_annuity"){
      if(rv$benefit_frequency=="yearly"){
        rv$EPV_benefit <- Annuity(rv$mort_table,rv$age, rv$int/100)
        rv$running_EPV <- runningEPV(rv$mort_table, "wl_annuity", "arrears","yearly", rv$age, rv$int/100, rv$term1)
      }else if(rv$benefit_frequency=="monthly"){
        rv$EPV_benefit <- Monthly_Annuity(rv$mort_table,rv$age, rv$int/100)
        rv$running_EPV <- runningEPV(rv$mort_table, "wl_annuity", "arrears","monthly", rv$age, rv$int/100, rv$term1)}
      else{return(NULL)}
    }
    
    else if (rv$benefit =="term_assurance"){
      if(rv$benefit_payment == "arrears"){
        rv$EPV_benefit <- Term_Assurance(rv$mort_table, rv$age, rv$int/100, rv$term1)
        rv$running_EPV <- runningEPV(rv$mort_table,"term_assurance", "arrears","yearly", rv$age, rv$int/100,rv$term1)
      }else{
        rv$EPV_benefit <- Immediate_Term_Assurance(rv$mort_table, rv$age, rv$int/100, rv$term1)
        rv$running_EPV <- runningEPV(rv$mort_table,"term_assurance", "immediate","yearly", rv$age, rv$int/100,rv$term1)
      }
    }
    else if(rv$benefit == "term_annuity"){
      if (rv$benefit_frequency == "yearly"){
        rv$EPV_benefit <- Term_Annuity(rv$mort_table, rv$age, rv$int/100, rv$term1)
        rv$running_EPV <- runningEPV(rv$mort_table, "term_annuity", "arrears", "yearly",rv$age, rv$int/100, rv$term1)
      }else if(rv$benefit_frequency == "monthly"){
        rv$EPV_benefit <- Monthly_Term_Annuity(rv$mort_table, rv$age, rv$int/100, rv$term1)
        rv$running_EPV <- runningEPV(rv$mort_table, "term_annuity", "arrears", "monthly",rv$age, rv$int/100, rv$term1)
      }}
    
    else if(rv$benefit == "gr_wl_annuity"){
      if (rv$benefit_frequency == "yearly"){
        rv$EPV_benefit <- Guaranteed_Annuity(rv$mort_table, rv$age, rv$int/100, rv$term)
        rv$running_EPV <- running_EPV_gr(rv$mort_table, "yearly", rv$age, rv$int/100, rv$term)
      }else if(rv$benefit_frequency == "monthly"){
        rv$EPV_benefit <- Monthly_Guaranteed_Annuity(rv$mort_table, rv$age, rv$int/100, rv$term)
        rv$running_EPV <- running_EPV_gr(rv$mort_table, "monthly", rv$age, rv$int/100, rv$term)
      }
    }
    
    else if(rv$benefit == "pure_endowment"){
      rv$EPV_benefit <- Pure_Endowment(rv$mort_table, rv$age, rv$int/100, rv$term1)
      rv$running_EPV <- runningEPV(rv$mort_table,"pure_endowment", "arrears", "yearly",rv$age, rv$int/100, rv$term1)
      
    }
    else if(rv$benefit == "wl_assurance"){
      if (rv$benefit_payment == "arrears"){
        rv$EPV_benefit <- Assurance(rv$mort_table, rv$age, rv$int/100)
        rv$running_EPV <- runningEPV(rv$mort_table,"wl_assurance", "arrears","yearly",rv$age, rv$int/100,0)
      }
      else {
        rv$EPV_benefit <- Immediate_Assurance(rv$mort_table, rv$age, rv$int/100)
        rv$running_EPV <- runningEPV(rv$mort_table,"wl_assurance", "immediate","yearly", rv$age, rv$int/100,0)
      }
    }
    
    else if(rv$benefit == "joint_term_annuity"){
      if (rv$benefit_frequency == "yearly"){
        rv$EPV_benefit <- joint_term_annuity(rv$mort_table_1, rv$mort_table_2, rv$age1, rv$age2, rv$int/100, rv$term1)
        rv$running_EPV <- runningEPV_joint(rv$mort_table_1, rv$mort_table_2,"joint_term_annuity", "arrears", "yearly", rv$age1, rv$age2, rv$int/100, rv$term1)
      }
      else if(rv$benefit_frequency == "monthly"){
        rv$EPV_benefit <- m_joint_term_annuity(rv$mort_table_1, rv$mort_table_2, rv$age1, rv$age2, rv$int/100, rv$term1)
        rv$running_EPV <- runningEPV_joint(rv$mort_table_1, rv$mort_table_2,"joint_term_annuity", "arrears", "monthly", rv$age1, rv$age2, rv$int/100, rv$term1)}
    }
    
    else if (rv$benefit == "joint_annuity"){
      if (rv$benefit_frequency == "yearly"){
        rv$EPV_benefit <- joint_annuity(rv$mort_table_1, rv$mort_table_2, rv$age1, rv$age2, rv$int/100)
        rv$running_EPV <- runningEPV_joint(rv$mort_table_1, rv$mort_table_2, "joint_annuity", "arrears", "yearly", rv$age1, rv$age2, rv$int/100, rv$term1)
      } 
      else if(rv$benefit_frequency == "monthly"){
        rv$EPV_benefit <- m_joint_annuity(rv$mort_table_1, rv$mort_table_2, rv$age1, rv$age2, rv$int/100)
        rv$running_EPV <- runningEPV_joint(rv$mort_table_1, rv$mort_table_2, "joint_annuity", "arrears", "monthly", rv$age1, rv$age2, rv$int/100, rv$term1)
      }
    }
    
    else if(rv$benefit =="joint_term_assurance"){
      if(rv$benefit_payment == "arrears"){ 
        rv$EPV_benefit <- joint_term_assurance(rv$mort_table_1, rv$mort_table_2, rv$age1, rv$age2, rv$int/100, rv$term1)
        rv$running_EPV <- runningEPV_joint(rv$mort_table_1, rv$mort_table_2,"joint_term_assurance","arrears","yearly", rv$age1, rv$age2, rv$int/100, rv$term1)
      }
      else if(rv$benefit_payment == "immediate"){
        rv$EPV_benefit <- imm_joint_term_assurance(rv$mort_table_1, rv$mort_table_2, rv$age1, rv$age2, rv$int/100, rv$term1)
        rv$running_EPV <- runningEPV_joint(rv$mort_table_1, rv$mort_table_2,"joint_term_assurance","immediate","yearly",rv$age1, rv$age2, rv$int/100, rv$term1) 
      }
    }
    
    else if(rv$benefit == "joint_wl_assurance"){
      if(rv$benefit_payment == "arrears"){
        rv$EPV_benefit <- joint_wl_assurance(rv$mort_table_1, rv$mort_table_2, rv$age1, rv$age2, rv$int/100)
        rv$running_EPV <- runningEPV_joint(rv$mort_table_1, rv$mort_table_2, "joint_wl_assurance", "arrears","yearly", rv$age1, rv$age2, rv$int/100, rv$term1)
      }
      else if(rv$benefit_payment == "immediate"){
        rv$EPV_benefit <- imm_joint_wl_assurance(rv$mort_table_1, rv$mort_table_2, rv$age1, rv$age2, rv$int/100)
        rv$running_EPV <- runningEPV_joint(rv$mort_table_1, rv$mort_table_2, "joint_wl_assurance", "immediate","yearly", rv$age1, rv$age2, rv$int/100, rv$term1)
      }
    }
    else{return(NULL)}
    
    if((rv$benefit == "wl_annuity"|rv$benefit == "term_annuity"|rv$benefit == "gr_wl_annuity"|rv$benefit == "joint_term_annuity"|rv$benefit == "joint_annuity")&rv$benefit_frequency == "monthly"){
      rv$assured_sum <- rv$assured_sum_period*12
    }else
      rv$assured_sum <- rv$assured_sum_period
    
    #Calculating running EPV of premiums and determining value for premium expense (0 if single premium)
    
    if(rv$premium_payment == "single"){
      rv$pr_exp <- 0
      rv$EPV_premium <- 1
      
      if (rv$type_of_contr == "single_contr"){
        rv$running_EPV_premium <- runningEPV_premium(rv$mort_table,"single","yearly", rv$age, rv$int/100,rv$term1)}
      else if(rv$type_of_contr == "joint_contr"){
        rv$running_EPV_premium <- runningEPV_premium_joint(rv$mort_table_1, rv$mort_table_2,"single","yearly", rv$age1,rv$age2, rv$int/100,rv$term1)}
      }
    
    else if(rv$premium_payment == "level"){
      rv$pr_exp <- rv$pr_exp_1
      
      if(rv$frequency_premium == "yearly"){
        
        if (rv$type_of_contr == "single_contr"){
          if (rv$benefit == "wl_annuity"|rv$benefit == "gr_wl_annuity"|rv$benefit == "wl_assurance"){
            rv$EPV_premium <- Annuity_Due(rv$mort_table, rv$age, rv$int/100)
            rv$running_EPV_premium <- runningEPV_premium_wl(rv$mort_table,"level", "yearly", rv$age, rv$int/100, rv$term1)
          }else{
            rv$EPV_premium <- Term_Annuity_Due(rv$mort_table,rv$age,rv$int/100,rv$term1)
            rv$running_EPV_premium <- runningEPV_premium(rv$mort_table,"level","yearly",rv$age, rv$int/100,rv$term1)
          }}
        
        else if(rv$type_of_contr == "joint_contr"){
          if (rv$benefit == "joint_wl_assurance"| rv$benefit == "joint_annuity"){
            rv$EPV_premium <- joint_annuity_due(rv$mort_table_1, rv$mort_table_2, rv$age1, rv$age2, rv$int/100)
            rv$running_EPV_premium <- runningEPV_premium_joint_wl(rv$mort_table_1, rv$mort_table_2, "level","yearly", rv$age1, rv$age2, rv$int/100,rv$term1)
          }else{
            rv$EPV_premium <- joint_term_annuity_due(rv$mort_table_1, rv$mort_table_2, rv$age1, rv$age2, rv$int/100, rv$term1)
            rv$running_EPV_premium <- runningEPV_premium_joint(rv$mort_table_1, rv$mort_table_2, "level","yearly", rv$age1, rv$age2, rv$int/100,rv$term1)
          }
        }
      }
      else if(rv$frequency_premium == "monthly"){
        
        if (rv$type_of_contr == "single_contr"){
          if (rv$benefit == "wl_annuity"|rv$benefit == "gr_wl_annuity"|rv$benefit == "wl_assurance"){
            rv$EPV_premium <- Monthly_Annuity_Due(rv$mort_table, rv$age, rv$int/100)
            rv$running_EPV_premium <- runningEPV_premium_wl(rv$mort_table,"level", "monthly", rv$age, rv$int/100, rv$term1)
          }else{
            rv$EPV_premium <- Monthly_Term_Annuity_Due(rv$mort_table,rv$age,rv$int/100,rv$term1)
            rv$running_EPV_premium <- runningEPV_premium(rv$mort_table,"level","monthly",rv$age, rv$int/100,rv$term1)
          }
        }
        else if(rv$type_of_contr == "joint_contr"){
          if (rv$benefit == "joint_wl_assurance"| rv$benefit == "joint_annuity"){
            rv$EPV_premium <- m_joint_annuity_due(rv$mort_table_1, rv$mort_table_2, rv$age1, rv$age2, rv$int/100)
            rv$running_EPV_premium <- runningEPV_premium_joint_wl(rv$mort_table_1, rv$mort_table_2, "level","monthly", rv$age1, rv$age2, rv$int/100,rv$term1)
          }else{
            rv$EPV_premium <- m_joint_term_annuity_due(rv$mort_table_1, rv$mort_table_2, rv$age1, rv$age2, rv$int/100, rv$term1)
            rv$running_EPV_premium <- runningEPV_premium_joint(rv$mort_table_1, rv$mort_table_2, "level","monthly", rv$age1, rv$age2, rv$int/100,rv$term1)
          }
        }
      }
    }
    
    #Calculating running initial expense (i.e. setting all values after year 1 to 0)
    rv$running_in_exp <- c(rv$in_exp, rep(0,rv$term1))
    
    #Calculating gross premium G
    rv$premium <- 
      ((1+rv$cl_exp/100)*rv$EPV_benefit*rv$assured_sum)/
      (rv$EPV_premium-(rv$in_exp/100)-(rv$pr_exp/100)*(rv$EPV_premium-1))
    
    
    rv$df <- data.frame(Time = seq(0,rv$term1),
                        Reserve = (1+rv$cl_exp/100)*rv$running_EPV*rv$assured_sum +
                          rv$premium*(rv$running_in_exp/100) +              
                          rv$premium*rv$running_EPV_premium*(rv$pr_exp/100 - 1) -
                          rv$pr_exp/100*rv$premium)
    
    plot <- ggplot(rv$df,aes(x = Time, y = Reserve)) + 
      labs(x = "Policy Senority", y = "")  + 
      geom_line() +
      theme_gray() + 
      #ylim(0, NA) +
      theme(panel.background = element_rect(fill = "aliceblue"))
    
    
    return(ggplotly(plot))
  })
  
  output$premium <- renderValueBox({
    if(rv$premium_payment=="single"){
      valueBox(currency(rv$premium, digits = 4),
               "a lump sum at the outset", icon = icon("coins"))
    }else{
      if (rv$frequency_premium == "yearly"){
        valueBox(currency(rv$premium, digits = 4),
                 "per annum", icon = icon("coins"))
      }else{
      valueBox(currency(rv$premium/12, digits = 4),
               "per month", icon = icon("coins"))}
    }
  })
  
  output$premium_pa <- renderValueBox({
    valueBox(currency(rv$premium, digits = 4),
             "per annum equivalent", icon = icon("balance-scale"), color = "olive")
  })
  
}

