
tao_1 = 6 # day; average time to move from early to late stage of the disease
tao_2 = 10 # day; infection post-exposure period
mu = 0.66 # relative infectivity of exposed to undocumented cases
y=1 # scaler
alpha = 0.2 * y # relative infectivity of documented to undocumented cases
# T = 14
C_max = 13 # constant contact rate; person/day
i_N = 0.037 # infection probablity absense of mask; person/day

# Test related - not used yet
T_sp = 0.998
T_sn = 0.8
T_c = 500 # test/day
k_s = 0.01
k_E = 0.6

# Population
A = 400 # person/day
tao_arrival = 14 # arrival duration
a_s = 0.97 
A_E = 0.003
a_IU = 0.0015
B_3060 = 2500
B_gt60 = 500

# Policy - not used yet
M = 0
theta = 1
h = 100
beta_3060 = 1
beta_gt60 = 1

# Fatality - not used yet
f_lt30 = 0.00004
f_3060 = 0.0005
f_gt60 = 0.03

# ---- initialization -----
S0_inc = A*tao_arrival
N = A*tao_arrival # initial population
E0 = 10 # initial exposure
I0 = 1 # initial infection
R0 = 0 # initial recovered
len = 200 # days of observation
record = data.frame(matrix(0,nrow=len, ncol=4))

# rates:
alpha = 0.0002 * y # relative infectivity of documented to undocumented cases
mu = 0.66 # relative infectivity of exposed to undocumented cases
r_recover = 1/tao_2



ui <- fluidPage(
  
  # Application title
  titlePanel("Simulation-Based What-if Analysis of COVID-19 Spread in Universities"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("students",
                  "Arrival rate of students:",
                  min = 10,
                  max = 10000,
                  value = 100)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  S0 <- reactive({
    input$students * tao_arrival
  })
  
  N <- reactive({
    input$students * tao_arrival
  })
  
  E0 <- reactive({10})
  
  I0 <- reactive({1})
  
  R0 <- reactive({0})
  
  record_reac <- eventReactive(input$students, {
    record = data.frame(matrix(0,nrow=len, ncol=4))
    
    for (i in 1:len){
      
      if(i==1){
        S = S0()
        E = E0()
        I = I0()
        R = R0()
      }
      else{
        S = S0_1
        E = E0_1
        I = I0_1
        R = R0_1    
      }
      
      # Transmission dynamics:
      # Code below are based on equation 3,4,6,8 from the article (simplified)
      S0_1 = max(0, S - i_N*C_max*S/N() * (mu*E + alpha*I))  # equation 3
      E0_1 = max(0, E + i_N*C_max*S/N() * (mu*E + alpha*I) - E/tao_1) # equation 4
      I0_1 = max(0, I + E/tao_1 - I/tao_2) # equation 6
      R0_1 = max(0, R + I/tao_2) # equation 8
      
      record[i,1] = S0_1
      record[i,2] = E0_1
      record[i,3] = I0_1
      record[i,4] = R0_1
    }
    record
  })
  
  output$distPlot <- renderPlot({
    ggplot(data=record_reac()) +
      geom_line(aes(x=1:len, y=X1)) +
      geom_line(aes(x=1:len, y=X2), color='yellow')+
      geom_line(aes(x=1:len, y=X3), color='red')+
      geom_line(aes(x=1:len, y=X4), color='green')
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)