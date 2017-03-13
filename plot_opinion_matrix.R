

plot_opinion<-function(opinions, time){
N<-sqrt(length(opinions))
	colmat<-matrix(opinions, nrow=N ,ncol=N)
  	print(levelplot(colmat,col.regions=c('#F9CF00','#4484CE'),at=c(-1,0,1),ylab='',colorkey=F ,main=list(paste0('Period: ',time),side=1,line=0.5),xlab=''))
}