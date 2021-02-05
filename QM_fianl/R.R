library(shiny)

shinyui<-fluidPage(
  #theme = shinytheme('superhero'),
  titlePanel("管制圖上下界限快速計算"),
  #headerPanel("管制圖上下界限快速計算"),
  sidebarLayout(
    sidebarPanel(
      helpText("您可在底下輸入您所要的μ、n、R、σ"),
      radioButtons("select",label = "選擇管制圖",choices = list("x_bar管制圖"="x_bar","R管制圖"="R","S管制圖"="S"),selected ="x_bar"),
      #selectInput("select","選擇管制圖",c(x_bar管制圖="x_bar",R管制圖="R",S管制圖="S")),
      conditionalPanel(
        condition="input.select=='x_bar'",
        numericInput("μ_value", "請選擇您所要的μ (可為-100,000~10,000,000)", 0, min = -100000, max = 10000000),
        numericInput("n_value", "請選擇您所要的n (可為2~25)", 2, min = 2, max = 25),
        numericInput("R_value", "請選擇您所要的R (可為0~10,000,000)", 0, min = 0, max = 10000000),
      ),
      conditionalPanel(
        condition="input.select=='R'",
        numericInput("n1_value", "請選擇您所要的n (可為2~25)", 2, min = 2, max = 25),
        numericInput("σ1_value", "請選擇您所要的σ (可為0~10,000,000)", 0, min = 0, max = 10000000),
      ),
      conditionalPanel(
        condition="input.select=='S'",
        numericInput("n2_value", "請選擇您所要的n (可為2~25)", 2, min = 2, max = 25),
        numericInput("σ2_value", "請選擇您所要的σ (可為0~10,000,000)", 0, min = 0, max = 10000000),
      ),
    ),
    mainPanel(
      h5(textOutput("UCL")),
      textOutput("UCL_output"),
      h5(textOutput("CL")),
      textOutput("CL_output"),
      h5(textOutput("LCL")),
      textOutput("LCL_output"),
    )
  )
)
shinyserver<-function(input,output){
  A2<- c( 1.880,1.023,0.729,0.577,0.483,0.419,0.373,0.337,0.308,
          0.285,0.266,0.249,0.235,0.223,0.212,0.203,0.194,0.187,
          0.180,0.173,0.167,0.162,0.157,0.153)
  D2<- c( 3.686,4.358,4.698,4.918,5.078,5.203,5.307,5.394,5.469,
          5.534,5.592,5.646,5.693,5.737,5.779,5.817,5.854,5.888,
          5.922,5.950,5.979,6.006,6.031,6.058)
  D1<- c( 0,0,0,0,0,0.205,0.387,0.546,0.687,
          0.812,0.924,1.026,1.121,1.207,1.285,1.359,1.426,1.490,
          1.548,1.606,1.659,1.710,1.759,1.804)
  d_2<-c( 1.128,1.693,2.059,1.326,2.534,2.704,2.847,2.970,3.078,
          3.173,3.258,3.336,3.407,3.472,3.532,3.588,3.640,3.689,
          3.735,3.778,3.819,3.885,3.895,3.931)
  D4<- c( 3.267,2.575,2.282,2.115,2.004,1.924,1.864,1.816,1.777,
          1.744,1.716,1.692,1.671,1.652,1.636,1.621,1.606,1.596,
          1.586,1.575,1.586,1.557,1.548,1.541)
  D3<- c( 0,0,0,0,0,0.076,0.136,0.184,0.233,
          0.256,0.284,0.308,0.329,0.348,0.364,0.379,0.392,0.404,
          0.414,0.425,0.434,0.443,0.452,0.459)
  C4<- c( 0.7979,0.8862,0.9213,0.9400,0.9515,0.9594,0.9650,0.9693,
          0.9727,0.9754,0.9776,0.9794,0.9810,0.9823,0.9835,0.9845,
          0.9854,0.9862,0.9869,0.9876,0.9882,0.9887,0.9892,0.9896)
  B6<- c( 2.606,2.276,2.088,1.964,1.874,1.806,1.751,1.707,1.669,
          1.637,1.610,1.585,1.563,1.544,1.526,1.511,1.496,1.483,
          1.470,1.459,1.448,1.438,1.429,1.420)
  B5<- c( 0,0,0,0,0.029,0.113,0.179,0.232,0.276,0.313,0.346,0.374,
          0.399,0.421,0.440,0.458,0.475,0.490,0.504,0.516,0.528,
          0.539,0.549,0.559)
  
  output$μ_value_output <- renderText({ input$μ_value })
  output$n_value_output <- renderText({ input$n_value })
  output$R_value_output <- renderText({ input$R_value })
  output$σ_value_output <- renderText({ input$σ_value })

  output$UCL<-renderText(
    if(input$select=='x_bar'){
      "x_bar管制圖的上界限："
    }
    else if(input$select=='R'){
      "R_bar管制圖的上界限："
    }
    else if(input$select=='S'){
      "S管制圖的上界限："
    }
  )
  output$CL<-renderText(
    if(input$select=='x_bar'){
      "x_bar管制圖的中心線："
    }
    else if(input$select=='R'){
      "R_bar管制圖的中心線："
    }
    else if(input$select=='S'){
      "S管制圖的中心線："
    }
  )
  output$LCL<-renderText(
    if(input$select=='x_bar'){
      "x_bar管制圖的下界限："
    }
    else if(input$select=='R'){
      "R_bar管制圖的下界限："
    }
    else if(input$select=='S'){
      "S管制圖的下界限："
    }
  )
  output$UCL_output<-renderText(
    if(input$select=='x_bar'){
      input$μ_value + input$R_value*A2[input$n_value-1]
    }
    else if(input$select=='R'){
      input$σ1_value*D2[input$n1_value-1]
    }
    else if(input$select=='S'){
      input$σ2_value*B6[input$n2_value-1]
    }
  )
  output$CL_output<-renderText(
    if(input$select=='x_bar'){
      input$μ_value
    }
    else if(input$select=='R'){
      input$σ1_value*d_2[input$n1_value-1]
    }
    else if(input$select=='S'){
      input$σ2_value*C4[input$n2_value-1]
    }
  )
  output$LCL_output<-renderText(
    if(input$select=='x_bar'){
      input$μ_value - input$R_value*A2[input$n_value-1]
    }
    else if(input$select=='R'){
      input$σ1_value*D1[input$n1_value-1]
    }
    else if(input$select=='S'){
      input$σ2_value*B5[input$n2_value-1]
    }
  )


}
shinyApp(ui=shinyui,server=shinyserver) 