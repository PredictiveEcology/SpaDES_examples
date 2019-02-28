library(fmsb)

# Create data: note in High school for several students
#set.seed(99)
#a <- lapply(1:3, function(x) sample(0:100, 5))

data=as.data.frame(matrix( sample( 30:80 , 15 , replace=F) , ncol=5))
colnames(data)=c("Carbon sequestration" , "Old growth retention" , 
                 "MPB population declining" , 
                 "Fire proofing within 100km communities" , 
                 "Caribou recovery (lambda > 1.02)" )
rownames(data)=paste("Management scenario" , letters[1:3] , sep=" ")

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data=rbind(rep(100,5) , rep(0,5) , data)





#==================
# Plot 1: Default radar chart proposed by the library:
radarchart(data)


#==================
# Plot 2: Same plot with custom features
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
radarchart( data  , axistype=2,  
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)
legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , 
       col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)

