# data setup
clouds = read.csv("data/clouds.csv")
fit = lm(rainfall ~ seeding + cloudcover + prewetness + echomotion + sne, data=clouds)

newX = data.frame(sne = seq(1, 5, by=.05))

seeding = clouds[clouds$seeding == 'yes',]
fitSeeding = lm(rainfall ~ sne, data=seeding)
predictSeeding = predict(fitSeeding, newSne, interval = "prediction")

noSeeding = clouds[clouds$seeding == 'no',]
fitNoSeeding = lm(rainfall ~ sne, data=noSeeding)
predictNoSeeding = predict(fitNoSeeding, newSne, interval = "prediction")

shinyServer(
  function(input, output) {
    
    output$print1 = renderPrint({
      iVar = switch(input$iVar,
                    "Seeding" = clouds$seeding,
                    "Time" = clouds$time,
                    "SNE" = clouds$sne,
                    "Cloud Cover" = clouds$cloudcover,
                    "Prewetness" = clouds$prewetness,
                    "Echo Motion" = clouds$echomotion)
      if (length(levels(iVar)) == 2) {
        test = t.test(clouds[iVar == levels(iVar)[1],]$rainfall, 
                      clouds[iVar == levels(iVar)[2],]$rainfall, 
                      alternative = "two.sided")
        print(test)
      } else {
        summary(iVar)
      }
    })
    
    output$plot1 = renderPlot({
      iVar = switch(input$iVar,
                "Seeding" = clouds$seeding,
                "Time" = clouds$time,
                "SNE" = clouds$sne,
                "Cloud Cover" = clouds$cloudcover,
                "Prewetness" = clouds$prewetness,
                "Echo Motion" = clouds$echomotion)

      plot(iVar, clouds$rainfall, xlab=input$iVar, ylab="Rainfall")
   })
   
   output$plotLM = renderPlot({
     plotType = switch(input$plotType,
                       "Residuals vs Fitted" = 1,
                       "Normal Q-Q" = 2,
                       "Scale-Location" = 3, 
                       "Cook's distance" = 4,
                       "Residuals vs Leverage" = 5,
                       "Cook's dist vs Leverage" = 6
     )
     plot(fit, plotType)
   })
   
   output$summaryLM = renderPrint({
     summary(fit)
   })
   
   output$anovaLM = renderPrint({
     anova(fit)
   })
   
   output$plotS = renderPlot({
     xName = switch(input$sVar,
                   "SNE" = "sne",
                   "Cloud Cover" = "cloudcover",
                   "Prewetness" = "prewetness"
     )
     xIndex = switch(input$sVar,
                    "SNE" = 4,
                    "Cloud Cover" = 5,
                    "Prewetness" = 6
     )
     newMin = min(clouds[,xIndex])
     newMax = max(clouds[,xIndex])
     newStep = (newMax - newMin) / 200
     newX = structure(data.frame(seq(newMin, newMax, by=newStep)), names=xName)
     
     fitSeeding = lm(as.formula(paste("rainfall ~ ", xName)), data=seeding)
     predictSeeding = predict(fitSeeding, newX, interval = "prediction")
     
     fitNoSeeding = lm(as.formula(paste("rainfall ~ ", xName)), data=noSeeding)
     predictNoSeeding = predict(fitNoSeeding, newX, interval = "prediction")
     
      plot(newX[,1], predictSeeding[,1], type="l", lwd=2, ylim=c(0,25), col="red", xlab=input$sVar, ylab="Rainfall")
      lines(newX[,1], predictSeeding[,2], lty=2, col="red")
      lines(newX[,1], predictSeeding[,3], lty=2, col="red")
      
      lines(newX[,1], predictNoSeeding[,1], lwd=2, col="blue")
      lines(newX[,1], predictNoSeeding[,2], lty=2, col="blue")
      lines(newX[,1], predictNoSeeding[,3], lty=2, col="blue")
     
      legend("topleft", legend = c("Seeding", "No Seeding"),
            col = c("red", "blue"), lwd=c(2,2))
     
      #mtext("Some more text", side=1, outer=T)
     
     output$summarySeeding = renderPrint({
       summary(fitSeeding)
     })
     
     output$summaryNoSeeding = renderPrint({
       summary(fitNoSeeding)
     })

   })
   
  }
)