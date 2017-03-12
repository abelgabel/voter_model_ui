# #####################################################
#
#
#
# #####################################################
library(shiny)
library(ggplot2)
library(gridExtra)
library(grid)
# #####################################################
# Load functions
# #####################################################
 
folder_dir<-getwd()
source('/Users/cuatrooctavos/Desktop/abelgabel_github/voter_model/two_dimensional_sim.R')
source('/Users/cuatrooctavos/Desktop/abelgabel_github/voter_model/two_dimensional_sim.R')  
 
 

server <- function(input, output, session) {


 observeEvent(input$simulation, {
 periods_c<-input$t_periods
size_c<-  input$size
periods_c<-as.numeric(periods_c)
size_c<-as.numeric(size_c)
    simulated_model<-mayority_model_dim2(periods_c,size_c,' ')
#simulated_model$stoptime

 observe({                    
time<-input$mydata
	 # Plot
	 print(time)
	 output$plotopinion<-renderPlot({
 	if(is.null(time))
    {a<-"Do nothing"
    	}
 	else{

	 # Plot: reaching absorbing state
	if(!is.na(simulated_model$stoptime[1])){
	if(time<=simulated_model$stoptime[2]){
		 period1<-1+10*(time-1)
 plot_opinion(simulated_model$opinion[time,], period1)		
	 		}
	 else {
	 	time_l<-simulated_model$stoptime
	 plot_opinion(simulated_model$opinion[time_l[2],], time_l[1])	 
	 }
	 }
	 else
	 {
	 	 periods_last<-round( periods_c/10)
	 	if(time<= periods_last){
		 period1<-1+10*(time-1)
 plot_opinion(simulated_model$opinion[time,], period1)		
	 		}
	 else {
	 plot_opinion(simulated_model$opinion[ periods_last,], periods_c)	
	 }
	 }
	 }
	 })		
	 	 
	}) 	
	 }) 			
		
}



ui <- fluidPage(htmlTemplate( "simulations.html",
theme = "css/mystyle.css",



#startsimulationss=actionButton(inputId ='simulations','Simulation', style="color:white;
#	background-color:#F19F4D;
#	border-radius: 8px;
#	height:28px;"),
#size_chosen=tags$input(id='size',type="number", value=10, min=5, max=50),
#num_periods=tags$input(id='t_periods',type="number", value=1000, min=5, max=10000),
time_chosen=tags$input(id='time',type="number", value=10, min=5, max=500),

plot_opinion=plotOutput('plotopinion'),
numbers_id= dataTableOutput('table')

)
  )
  
  
  shinyApp(ui=ui,server=server)