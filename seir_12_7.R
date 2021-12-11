library(shiny)
library(tidyverse)

# this plot has one plotly graph

##########################################################################
# school numbers 
# using fall 2020 student-to-faculty ratios
# all data from Common Data Sets (CDS)
# full-time equivalent students
# full-time instructional faculty (no TAs)

# brown: #https://oir.brown.edu/sites/g/files/dprerj381/files/2020-04/CDS_2020_2021_Final2_0.pdf
brownstudents= 6610 

brownfaculty= 1056 

brown <- brownstudents + brownfaculty

# harvard: https://oir.harvard.edu/files/huoir/files/harvard_cds_2020-2021.pdf
harvardstudents= 5186

harvardfaculty= 973 

harvard <- harvardstudents + harvardfaculty  

# yale: https://oir.yale.edu/sites/default/files/cds_2020-2021_yale_vf_030521.pdf
yalestudents= 4698

yalefaculty= 1102

yale <- yalestudents + yalefaculty

#upenn: https://ira.upenn.edu/sites/default/files/UPenn-Common-Data-Set-2020-21.pdf
upennstudents= 9752

upennfaculty= 1686

upenn <- upennstudents + upennfaculty

#cornell: http://irp.dpb.cornell.edu/wp-content/uploads/2021/06/CDS_2020-2021_FINAL.pdf
corstudents= 14708
  
corfaculty= 1688
  
cornell <- corstudents + corfaculty

# ------ Simulation function --------
simfunc <- function(A = 1786, tao_arrival = 14, tao_1=6, tao_2=10,mu=0.66, y=1, C_max=13, i_N=0.037,
                    E0=10,I0=1,R0=0, len=200) {
  alpha=0.2*y
  r_recover=1/tao_2
  S0 = A*tao_arrival
  N = A*tao_arrival
  record = data.frame(matrix(0,nrow=len, ncol=4))
  colnames(record) <- c('susceptible', 'exposed', 'infected','recovered')
  
  for (i in 1:len){
    S = S0
    E = E0
    I = I0
    R = R0
    # Transmission dynamics:
    # Code below are based on equation 3,4,6,8 from the article (simplified)
    S0 = max(0, S - i_N*C_max*S/N * (mu*E + alpha*I))  # equation 3
    E0 = max(0, E + i_N*C_max*S/N * (mu*E + alpha*I) - E/tao_1) # equation 4
    I0 = max(0, I + E/tao_1 - I0/tao_2) # equation 6
    R0 = max(0, R + I0/tao_2) # equation 8
    
    record[i,1] = S0
    record[i,2] = E0
    record[i,3] = I0
    record[i,4] = R0
  }
  record$days = c(1:len)
  
  return(record)
}

# Get daily record: you should use simfunc() as data when calling this function:
func_getdaily <- function(data){
  daily <- data %>% select(c(1:5)) %>% 
    mutate(lags=lag(susceptible), lage=lag(exposed),lagi=lag(infected),lagr=lag(recovered),
           d_sus = pmax(0,lags-susceptible, na.rm=TRUE),
           d_exp = pmax(0,exposed-lage, na.rm=TRUE), 
           d_inf = pmax(0,infected-lagi, na.rm=TRUE), 
           d_rec = pmax(0,recovered-lagr, na.rm=TRUE)) %>%
    select(days, d_sus, d_exp,d_inf,d_rec)
  return(daily)
}

# ourrecord <- simfunc()
# daily <- func_getdaily(ourrecord)

# -------- Shiny App ---------
library(plotly)

ui <- fluidPage(
  
  # Application title
  titlePanel("Simulation-Based What-if Analysis of COVID-19 Spread in Universities"),
  h4("Kara Rofe, Talia Seshaiah, Yifan Zhao"),
  p("insert paragraph here"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      uiOutput("seir"),
      sliderInput("cmax", "Constant contact rate:",
                  min=0,
                  max=40,
                  value=13), 
      sliderInput("i_n", "Infection probability:", 
                  min=0, 
                  max=1, 
                  value=0.037
      ), 
      # select ivy school
      selectInput("ivy", "Choose a school:",
                  choices= c("Brown University",
                             "Harvard University",
                             "Yale University",
                             "University of Pennsylvania",
                             "Cornell University", 
                             "None of the above"),
                  selected = "None of the above"
                  )
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("distPlot"),
      plotlyOutput("distPlotdaily")
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  record_reac <- reactive({ 
    simfunc(A = input$students,C_max = input$cmax, i_N =input$i_n)
  })
  daily_reac <- reactive({ 
    # record <- simfunc(A = input$students,C_max = input$cmax, i_N =input$i_n)
    func_getdaily(record_reac())
  })
  output$seir <- renderUI({
    if (input$ivy=="None of the above"){
      sliderInput("students",
                         "Arrival rate of students:",
                         min = 10,
                         max = 10000,
                         value = 100)
    }
    else if (input$ivy=="Brown University"){
      sliderInput("students",
                  "Arrival rate of students:",
                  min = 10,
                  max = 10000,
                  value = round(brown/14))
    }
    else if (input$ivy=="Harvard University"){
      sliderInput("students",
                  "Arrival rate of students:",
                  min = 10,
                  max = 10000,
                  value = round(harvard/14))
    }
    else if (input$ivy=="Yale University"){
      sliderInput("students",
                  "Arrival rate of students:",
                  min = 10,
                  max = 10000,
                  value = round(yale/14))
    }
    else if (input$ivy=="University of Pennsylvania"){
      sliderInput("students",
                  "Arrival rate of students:",
                  min = 10,
                  max = 10000,
                  value = round(upenn/14))
    }
    else if (input$ivy=="Cornell University"){
      sliderInput("students",
                  "Arrival rate of students:",
                  min = 10,
                  max = 10000,
                  value = round(cornell/14))
    }
  })
  # plot
  
  colors <- c("Susceptible" = "black",
              "Exposed" = "yellow", 
              "Infected" = "red", 
              "Recovered" = "green")
  
  output$distPlot <- renderPlotly({
    ggplot(data=record_reac()) +
      geom_line(aes(x=days, y=susceptible, color="Susceptible")) +
      geom_line(aes(x=days, y=exposed, color="Exposed"))+
      geom_line(aes(x=days, y=infected, color="Infected"))+
      geom_line(aes(x=days, y=recovered, color="Recovered"))+
      labs(y="Cumulative cases", x= "Days")+
      scale_color_manual(values = colors)+
      ggtitle("Cumulative cases of COVID-19")
  })
  
  # New: plot that shows daily cases
  output$distPlotdaily <- renderPlotly({
    ggplot(data=daily_reac()) +
      geom_line(aes(x=days, y=d_sus, color="Susceptible"))+
      geom_line(aes(x=days, y=d_exp, color="Exposed"))+
      geom_line(aes(x=days, y=d_inf, color="Infected"))+
      geom_line(aes(x=days, y=d_rec, color="Recovered")) +
      labs(y="Daily cases", x= "Days")+
      scale_color_manual(values = colors)+
      ggtitle("Daily cases of COVID-19")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)