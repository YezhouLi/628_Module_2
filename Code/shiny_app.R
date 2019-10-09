library(shiny)
library(plotrix)
require(shinythemes)

dt.fat = read.csv("BodyFat.csv")
body.fat = subset(dt.fat, select = c(-IDNO, -DENSITY))
body.fat.clean = body.fat[-c(182,216,163,221),]
data = body.fat.clean
data.invwei <- data.frame(BODYFAT = data$BODYFAT,HEIGHT = data$HEIGHT/data$WEIGHT,
                          ADIPOSITY = data$ADIPOSITY/data$WEIGHT,NECK = data$NECK/data$WEIGHT,
                          CHEST = data$CHEST/data$WEIGHT,ABDOMEN = data$ABDOMEN/data$WEIGHT,
                          HIP = data$HIP/data$WEIGHT,THIGH = data$THIGH/data$WEIGHT,
                          KNEE = data$KNEE/data$WEIGHT,ANKLE = data$ANKLE/data$WEIGHT,
                          BICEPS = data$BICEPS/data$WEIGHT,FOREARM = data$FOREARM/data$WEIGHT,
                          WRIST = data$WRIST/data$WEIGHT)
data.invwei$inv_wei <- 1/data$WEIGHT
model = lm(BODYFAT ~ ABDOMEN + inv_wei, data = data.invwei)

ui<-fluidPage(
  theme = shinytheme("superhero"),
  titlePanel(h1("Predict your bodyfat")),
  
  sidebarLayout(position="left",
                sidebarPanel(
                  helpText(h3("Using your weight and Abdomen 2 circumference to predict your bodyfat.")),
                  numericInput("weight", 
                               label = h2("Please input your weight"), 
                               value = 150),
                  radioButtons("weight_unit", label = h5("weight unit"),
                               choices = list("lbs" = 1, "kg" = 2),selected = 1),
                  numericInput("abdomen", 
                               label = h2("Please input your Abdomen 2 circumference"), 
                               value = 80),
                  radioButtons("Abdomen_unit", label = h5("Abdomen unit"),
                               choices = list("cm" = 1, "inch" = 2),selected = 1),
                  submitButton("Submit")
                ),
                
                mainPanel(
                  h2(textOutput("text1")),
                  plotOutput("distPlot2"),
                  h3(textOutput("text2")),
                  plotOutput("distPlot"),
                  h5("if you have any question, feel free to contact us: chao.chang@wisc.edu")
                )
  )
  
  
)

server<-function(input, output) {
  output$text1 <- renderText({ 
    if(input$weight_unit==1&input$Abdomen_unit==1){
    if(input$abdomen>=70&input$abdomen<=160&input$weight>=100&input$weight<=400&round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen/input$weight),interval = "prediction",level = 0.95)[1],digits = 2)<40&round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen/input$weight),interval = "prediction",level = 0.95)[1],digits = 2)>0){
    paste("Your bodyfat prediction is", pred<-round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen/input$weight),interval = "prediction",level = 0.95)[1],digits = 2),"%")
    }else{
        "Your input number is wrong"
    }
    }else if(input$weight_unit==1&input$Abdomen_unit==2){
      if(input$abdomen>=70/2.54&input$abdomen<=160/2.54&input$weight>=100&input$weight<=400&round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen*2.54/input$weight),interval = "prediction",level = 0.95)[1],digits = 2)<40&round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen*2.54/input$weight),interval = "prediction",level = 0.95)[1],digits = 2)>0){
        paste("Your bodyfat prediction is", pred<-round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen*2.54/input$weight),interval = "prediction",level = 0.95)[1],digits = 2),"%")
      }else{
        "Your input number is wrong"
      }      
    }else if(input$weight_unit==2&input$Abdomen_unit==1){
      if(input$abdomen>=70&input$abdomen<=160&input$weight>=100/2.2046&input$weight<=400/2.2046&round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen/(input$weight*2.2046)),interval = "prediction",level = 0.95)[1],digits = 2)<40&round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen/(input$weight*2.2046)),interval = "prediction",level = 0.95)[1],digits = 2)>0){
        paste("Your bodyfat prediction is", pred<-round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen/(input$weight*2.2046)),interval = "prediction",level = 0.95)[1],digits = 2),"%")
      }else{
        "Your input number is wrong"
      }
    }else{
      if(input$abdomen>=70/2.54&input$abdomen<=160/2.54&input$weight>=100/2.2046&input$weight<=400/2.2046&round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen*2.54/(input$weight*2.2046)),interval = "prediction",level = 0.95)[1],digits = 2)<40&round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen*2.54/(input$weight*2.2046)),interval = "prediction",level = 0.95)[1],digits = 2)>0){
        paste("Your bodyfat prediction is", pred<-round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen*2.54/(input$weight*2.2046)),interval = "prediction",level = 0.95)[1],digits = 2),"%")
      }else{
        "Your input number is wrong"
      }
    }
  })
  
  output$distPlot2 <- renderPlot({
    if(input$weight_unit==1&input$Abdomen_unit==1){
      if(input$abdomen>=70&input$abdomen<=160&input$weight>=100&input$weight<=400&round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen/input$weight),interval = "prediction",level = 0.95)[1],digits = 2)<40&round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen/input$weight),interval = "prediction",level = 0.95)[1],digits = 2)>0){
        p<-100-c(round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen/input$weight),interval = "prediction",level = 0.95)[1],digits = 2),100-round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen/input$weight),interval = "prediction",level = 0.95)[1],digits = 2))
        lab<-paste(c("other","fat"),p,"%")
        pie3D(p,explode = 0.5,labels = lab,height = 0.1,radius = 1)      
        }else{
      }
    }else if(input$weight_unit==1&input$Abdomen_unit==2){
      if(input$abdomen>=70/2.54&input$abdomen<=160/2.54&input$weight>=100&input$weight<=400&round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen*2.54/input$weight),interval = "prediction",level = 0.95)[1],digits = 2)<40&round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen*2.54/input$weight),interval = "prediction",level = 0.95)[1],digits = 2)>0){
        p<-100-c(round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen*2.54/input$weight),interval = "prediction",level = 0.95)[1],digits = 2),100-round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen*2.54/input$weight),interval = "prediction",level = 0.95)[1],digits = 2))
        lab<-paste(c("other","fat"),p,"%")
        pie3D(p,explode = 0.5,labels = lab,height = 0.1,radius = 1)
        }else{
      }      
    }else if(input$weight_unit==2&input$Abdomen_unit==1){
      if(input$abdomen>=70&input$abdomen<=160&input$weight>=100/2.2046&input$weight<=400/2.2046&round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen/(input$weight*2.2046)),interval = "prediction",level = 0.95)[1],digits = 2)<40&round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen/(input$weight*2.2046)),interval = "prediction",level = 0.95)[1],digits = 2)>0){
        p<-100-c(round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen/(input$weight*2.2046)),interval = "prediction",level = 0.95)[1],digits = 2),100-round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen/(input$weight*2.2046)),interval = "prediction",level = 0.95)[1],digits = 2))
        lab<-paste(c("other","fat"),p,"%")
        pie3D(p,explode = 0.5,labels = lab,height = 0.1,radius = 1)
        }else{
      }
    }else{
      if(input$abdomen>=70/2.54&input$abdomen<=160/2.54&input$weight>=100/2.2046&input$weight<=400/2.2046&round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen*2.54/(input$weight*2.2046)),interval = "prediction",level = 0.95)[1],digits = 2)<40&round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen*2.54/(input$weight*2.2046)),interval = "prediction",level = 0.95)[1],digits = 2)>0){
        p<-100-c(round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen*2.54/(input$weight*2.2046)),interval = "prediction",level = 0.95)[1],digits = 2),100-round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen*2.54/(input$weight*2.2046)),interval = "prediction",level = 0.95)[1],digits = 2))
        lab<-paste(c("other","fat"),p,"%")
        pie3D(p,explode = 0.5,labels = lab,height = 0.1,radius = 1)
        }else{
      }
    }    
  })
  
  output$text2 <- renderText({ 
    if(input$weight_unit==1&input$Abdomen_unit==1){
      if(input$abdomen>=70&input$abdomen<=160&input$weight>=100&input$weight<=400&round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen/input$weight),interval = "prediction",level = 0.95)[1],digits = 2)<40&round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen/input$weight),interval = "prediction",level = 0.95)[1],digits = 2)>0){
        paste("Your bodyfat has 95% probability between", pred<-round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen/input$weight),interval = "prediction",level = 0.95)[2],digits = 2),"%","and",pred<-round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen/input$weight),interval = "prediction",level = 0.95)[3],digits = 2),"%")
      }else{
      }
    }else if(input$weight_unit==1&input$Abdomen_unit==2){
      if(input$abdomen>=70/2.54&input$abdomen<=160/2.54&input$weight>=100&input$weight<=400&round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen*2.54/input$weight),interval = "prediction",level = 0.95)[1],digits = 2)<40&round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen*2.54/input$weight),interval = "prediction",level = 0.95)[1],digits = 2)>0){
        paste("Your bodyfat has 95% probability between", pred<-round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen*2.54/input$weight),interval = "prediction",level = 0.95)[2],digits = 2),"%","and",pred<-round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen*2.54/input$weight),interval = "prediction",level = 0.95)[3],digits = 2),"%")
      }else{
      }      
    }else if(input$weight_unit==2&input$Abdomen_unit==1){
      if(input$abdomen>=70&input$abdomen<=160&input$weight>=100/2.2046&input$weight<=400/2.2046&round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen/(input$weight*2.2046)),interval = "prediction",level = 0.95)[1],digits = 2)<40&round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen/(input$weight*2.2046)),interval = "prediction",level = 0.95)[1],digits = 2)>0){
        paste("Your bodyfat has 95% probability between", pred<-round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen/(input$weight*2.2046)),interval = "prediction",level = 0.95)[2],digits = 2),"%","and",pred<-round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen/(input$weight*2.2046)),interval = "prediction",level = 0.95)[3],digits = 2),"%")
      }else{
      }
    }else{
      if(input$abdomen>=70/2.54&input$abdomen<=160/2.54&input$weight>=100/2.2046&input$weight<=400/2.2046&round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen*2.54/(input$weight*2.2046)),interval = "prediction",level = 0.95)[1],digits = 2)<40&round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen*2.54/(input$weight*2.2046)),interval = "prediction",level = 0.95)[1],digits = 2)>0){
        paste("Your bodyfat has 95% probability between", pred<-round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen*2.54/(input$weight*2.2046)),interval = "prediction",level = 0.95)[2],digits = 2),"%","and",pred<-round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen*2.54/(input$weight*2.2046)),interval = "prediction",level = 0.95)[3],digits = 2),"%")
      }else{
      }
    }
  })

  output$distPlot <- renderPlot({
    if(input$weight_unit==1&input$Abdomen_unit==1){
      if(input$abdomen>=70&input$abdomen<=160&input$weight>=100&input$weight<=400&round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen/input$weight),interval = "prediction",level = 0.95)[1],digits = 2)<40&round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen/input$weight),interval = "prediction",level = 0.95)[1],digits = 2)>0){
        hist(body.fat.clean$BODYFAT, breaks = 10, xlab="bodyfat",main="Histogram of bodyfat in population",col = 'darkgray', border = 'white')
        abline(v=pred<-round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen/input$weight),interval = "prediction",level = 0.95)[1],digits = 2),lwd=4,col="red")
        legend("topright",title = "Your bobyfat position among people","You",lty=1,col="red")
      }else{
      }
    }else if(input$weight_unit==1&input$Abdomen_unit==2){
      if(input$abdomen>=70/2.54&input$abdomen<=160/2.54&input$weight>=100&input$weight<=400&round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen*2.54/input$weight),interval = "prediction",level = 0.95)[1],digits = 2)<40&round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen*2.54/input$weight),interval = "prediction",level = 0.95)[1],digits = 2)>0){
        hist(body.fat.clean$BODYFAT, breaks = 10, xlab="bodyfat",main="Histogram of bodyfat in population",col = 'darkgray', border = 'white')
        abline(v=pred<-round(predict(model,new<-data.frame(inv_wei=1/input$weight,ABDOMEN=input$abdomen*2.54/input$weight),interval = "prediction",level = 0.95)[1],digits = 2),lwd=4,col="red")
        legend("topright",title = "Your bobyfat position among people","You",lty=1,col="red")
      }else{
      }      
    }else if(input$weight_unit==2&input$Abdomen_unit==1){
      if(input$abdomen>=70&input$abdomen<=160&input$weight>=100/2.2046&input$weight<=400/2.2046&round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen/(input$weight*2.2046)),interval = "prediction",level = 0.95)[1],digits = 2)<40&round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen/(input$weight*2.2046)),interval = "prediction",level = 0.95)[1],digits = 2)>0){
        hist(body.fat.clean$BODYFAT, breaks = 10, xlab="bodyfat",main="Histogram of bodyfat in population",col = 'darkgray', border = 'white')
        abline(v=pred<-round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen/(input$weight*2.2046)),interval = "prediction",level = 0.95)[1],digits = 2),lwd=4,col="red")
        legend("topright",title = "Your bobyfat position among people","You",lty=1,col="red")
      }else{
      }
    }else{
      if(input$abdomen>=70/2.54&input$abdomen<=160/2.54&input$weight>=100/2.2046&input$weight<=400/2.2046&round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen*2.54/(input$weight*2.2046)),interval = "prediction",level = 0.95)[1],digits = 2)<40&round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen*2.54/(input$weight*2.2046)),interval = "prediction",level = 0.95)[1],digits = 2)>0){
        hist(body.fat.clean$BODYFAT, breaks = 10, xlab="bodyfat",main="Histogram of bodyfat in population",col = 'darkgray', border = 'white')
        abline(v=pred<-round(predict(model,new<-data.frame(inv_wei=1/(input$weight*2.2046),ABDOMEN=input$abdomen*2.54/(input$weight*2.2046)),interval = "prediction",level = 0.95)[1],digits = 2),lwd=4,col="red")
        legend("topright",title = "Your bobyfat position among people","You",lty=1,col="red")
      }else{
      }
    }
  })

}



shinyApp(ui=ui,server=server)