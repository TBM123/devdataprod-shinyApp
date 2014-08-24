library(shiny)

calcBMI <- function(height, weight, heightFT=0, units=1) {
    (weight / ((height + heightFT*12)^2))*units
}

convertToEnglish <- function(height, weight, heightFT=0, units=1) {
    if (units == "1") {
        h <- round(height*39.3701, 2)
        w <- round(weight*2.20462, 2)
    } else {
        h <- height + heightFT*12
        w <- weight
    }
    c(h,w)
}

userInput <- function(height, weight, heightFT=0, units=1){
    if (units == "1") {
        w <- paste(weight, "kg")
        h <- paste(height, "m")
    } else {
        w <- paste(weight, "lb")
        h <- paste0(heightFT, '"', height, "'")
    }
    
    paste("You weigh", w, "at", h)
}

userResult <- function(bmi){
    
    if (bmi <= 18.5) info <- "<span style='color: red; font-weight:bold;'>Underweight</span>"
    else if (bmi <= 25.0) info <- "<span style='color: green; font-weight:bold;'>Normal (healthy weight)</span>"
    else if (bmi <= 30.0) info <- "<span style='color: orange; font-weight:bold;'>Overweight</span>"
    else info <- "<span style='color: red; font-weight:bold;'>Obese</span>"
    
    paste("Your BMI is", round(bmi, 2), info)
}

shinyServer(
    function(input, output) {
        output$inputValueH <- renderPrint({input$heightM})
        output$inputValueW <- renderPrint({input$weightM})
        output$unitsVal <- renderPrint({input$units})
        output$bmi <- renderPrint({
            switch(input$units,
                   "1" = calcBMI(input$heightM, input$weightM),
                   "703" = calcBMI(input$heightIN, input$weightE, input$heightFT, 703))
            })
        output$userDetails <- renderText({
            switch(input$units,
                   "1" = userInput(input$heightM, input$weightM),
                   "703" = userInput(input$heightIN, input$weightE, input$heightFT, 703))
        })
        output$bmiResult <- renderText({
            bmi = switch(input$units,
                         "1" = calcBMI(input$heightM, input$weightM),
                         "703" = calcBMI(input$heightIN, input$weightE, input$heightFT, 703))
            userResult(bmi)
            
        })
        output$plot1 <- renderPlot({
            ht <- seq(48,84, length.out=1000)
            wt <- seq(90,300, length.out=1000)
            wtht <- expand.grid(x=ht, y=wt)
            bmi <- function(h,w) {(w * 703)/(h*h)}
            bmiwtht <- matrix(bmi(wtht$x,wtht$y),length(ht),length(wt))
            
            contour(ht,wt,bmiwtht,levels = c(18.5,25,30), drawlabels=FALSE,
                    xlab="Height (inches)",ylab="Weight (lbs)",
                    main="BMI categories by height and weight", col=c("Green","Yellow","Red"))
            
            text(55,200,"Obese",cex=2,srt=45)
            text(65,165,"Overweight",cex=2,srt=40)
            text(70,150,"Normal",cex=2,srt=35)
            text(75,120,"Underweight",cex=2,srt=18)
            userPoint <- switch(input$units,
                                "1" = convertToEnglish(input$heightM, input$weightM),
                                "703" = convertToEnglish(input$heightIN, input$weightE, input$heightFT, 703))
            points(userPoint[1], userPoint[2], pch = 4, cex = 3, lwd = 3, col="Blue")
        }, height=600)
    }
)