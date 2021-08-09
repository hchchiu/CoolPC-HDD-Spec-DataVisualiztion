library(shiny)
library(datasets)
library(plotly)
library(leaflet)
source("global.r")

shinyServer(function(input, output) {

    datasetInput <- reactive({

   })
   output$brandui<-renderUI({
      list(selectInput("brand","Choose Brand:",choices = unique(allstorage$Brand), selected= unique(allstorage$Brand),multiple=T))
   })
  
   ######select size#######
   output$sizeui<-renderUI({
    optsize=c("Please select Brand!")

    choosesize=list()
    if(length(input$brand)!=0){
      for(i in 1:length(input$brand)){
         pos=which(allstorage$Brand==input$brand[i] & allstorage$Class == input$class)#find pos
         choosesize=rbind(choosesize,allstorage[pos,])
      }

      choosesize=unique(choosesize[,4])#mod
      choosesize=sort(choosesize)
      optsize=list()

      for(i in 1:length(choosesize)){
        if(choosesize[i]>12){
          optsize=c(optsize,paste(toString(choosesize[i]),"GB"))
        }
        else
          optsize=c(optsize,paste(toString(choosesize[i]),"TB"))
      }
    }

    selectInput("size","Choose Size:",choices = optsize)
   })
   
   ######select price range#######
   output$priceui<-renderUI({
     chooseprice=0
     maxprice=0
     minprice=0
     size=0
     warr=as.numeric(input$warranty)
     if(length(input$brand)!=0){
        ###NA modify ###
        if(length(nchar(input$size))!=0 ){
           if(nchar(input$size)<20){
              size=as.numeric(substr(input$size,1,(nchar(input$size)-2)))
           }
        }
        ################
       for(i in 1:length(input$brand)){
         pos=which(allstorage$Brand==input$brand[i] & allstorage$Class==input$class 
                   & allstorage$Size== size & allstorage$Warranty>=warr)#mod
         chooseprice=allstorage[pos,]$Price.x
         if(length(pos)>0){
           if(max(chooseprice) > maxprice){
              maxprice=max(chooseprice)
           }
  
           if((min(chooseprice) < minprice) | (minprice == 0)){
              minprice=min(chooseprice)
           }
         }
       }
       
     }
     
     sliderInput("price", "Select Price Range:", min = minprice, max = maxprice, value = c(minprice,maxprice))
   })
   
   #######draw bar plot#######
   output$plot<-renderPlotly({
     #transfer 500GB -> 500
     size=0
     if(length(nchar(input$size))!=0 ){
        if(nchar(input$size)<20){
            size=as.numeric(substr(input$size,1,(nchar(input$size)-2)))
        }
     }
     #get warranty
     warr=as.numeric(input$warranty)
     
     data=list()
     for(i in 1:length(input$brand)){
         data=rbind(data,allstorage[allstorage$Brand==input$brand[i] & allstorage$Class==input$class 
                                    & allstorage$Size ==size & allstorage$Price.x <= input$price[2]
                                    & allstorage$Price.x >= input$price[1] & allstorage$Warranty>=warr,])#mod
     }
     p = plot_ly(x = paste(data[,3],data[,1]), y = data[,7], type = "bar",name="原價屋",marker = list(color = 'rgb(158,202,225)',
                           line = list(color = 'rgb(8,48,107)', width = 1.5)))
     
     p = p %>% add_trace(y = data[,9],name="PChome",marker = list(color = 'rgb(58,200,225)',
                         line = list(color = 'rgb(8,48,107)', width = 1.5)))
     
     p <- p %>% layout(title = "",
                           xaxis = list(title = "型號"),
                           yaxis = list(title = "價格"))
     p
   })
   
   #######draw pie chart#######
   output$pie<-renderPlotly({
      #transfer 500GB -> 500
      size=0
      if(length(nchar(input$size))!=0 ){
         if(nchar(input$size)<20){
            size=as.numeric(substr(input$size,1,(nchar(input$size)-2)))
         }
      }
      #get warranty
      warr=as.numeric(input$warranty)
      
      data=list()
      count=list()
      j=1
      for(i in 1:length(input$brand)){
         data=rbind(data,allstorage[allstorage$Brand==input$brand[i] & allstorage$Class==input$class,])
      }
      freq=table(data$Brand)

      p <- plot_ly(data, labels = sort(input$brand), values = freq, type = 'pie')
      p <- p %>% layout(title = '',
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
     
   })
   #####draw line chart#######
   output$line<-renderPlotly({
      size=as.numeric(substr(input$size,1,(nchar(input$size)-2)))
      
      #get warranty
      warr=as.numeric(input$warranty)
      
      data=list()
      for(i in 1:length(input$brand)){
         data=rbind(data,allstorage[allstorage$Brand==input$brand[i] & allstorage$Class==input$class 
                                    & allstorage$Size ==size & allstorage$Warranty>=warr ,])#mod
      }
      month=c('2020/11','2020/12','2021/1','2021/2','2021/3','2021/4')

      data=data[(!is.na(data$Price11) & !is.na(data$Price12) & !is.na(data$Price1) & !is.na(data$Price2) & !is.na(data$Price3)),]

      p=0
      if(nrow(data)>0){
         for(i in 1:nrow(data)){
            col=data[i,c(10:14,7)]
            col=c(col[1,1],col[1,2],col[1,3],col[1,4],col[1,5],col[1,6])
            if(i == 1){
               p <- plot_ly(data, x = month, y = col[1:6], name = paste(data[i,3],data[i,1]), type = 'scatter', mode = 'markers +  lines')
            }
            else{
               p <-p %>% add_trace(y = col[1:6] , name = paste(data[i,3],data[i,1]),type = 'scatter',  mode = 'markers + lines')
            }
         }
         p <- p %>% layout(xaxis = list(title = "月份"),
                           yaxis = list (title = "價格"))
         p
      }
      
      
   })
   
   
   #####draw coolpc location in Taiwan#######
   output$mpgPlot <- renderLeaflet({
      data=coolpclocation
      m = leaflet(data =  data) %>% setView(lng = 121.2680507 , lat = 24.972073, zoom = 8) %>% addTiles() %>% addMarkers(~lng, ~lat,popup=paste(data$name, "<br>","電話:",data$formatted_phone_number)  )
      m
      
   })
})
