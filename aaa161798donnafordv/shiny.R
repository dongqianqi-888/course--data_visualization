library(shiny)

ui<-fluidPage(
  titlePanel("STQD6124"),
  sidebarLayout(
    sidebarPanel(
      sidebarPanel(
        h1(strong("UKM"),em("All the Best"),align="center"),
        ### strong加粗，em斜体，align居中
        p("Dew drops glisten on green grass"),
        ### 普通文本
        p(code("Birds sing notes that sweetly pass")),
        ### 代码状文本
        div("I walk light where flowers bloom",style="color:red"),
        ### 带颜色的方块文本，这里是红色
        br(),
        ### 换行
        img(src="image.JPG", width="200px"),
        ### 图片
        tags$audio(src="media.mp3",type="audio/mp3",autoplay=F,controls=T),
        ### 音频
        actionButton("button","button 1"),
        ### 一个按钮，可触发事件
        submitButton("Submit")
        ### 提交按钮
        ),
      mainPanel(
        fluidRow(
          column(3,h3("Checkbox"),#这一列占这行3/12的宽度，可叠加
                 checkboxInput("checkbox","Chocolate",value = T), 
                 ### 单个复选框(可叠加),选是否喜欢chocolate
                 checkboxGroupInput("checkboxGroup",h3("Another Checkbox"),
                                    choices = list("KFC"=1,"McD"=2,"Texas Chicken"=3),
                                    selected=1), 
                 ### 一组复选框,123我全都要
                 dateInput("date",h3("Insert a Date"),
                           value="2026-5-12"),
                 ### 选一个具体日期
                 dateRangeInput("dateRange",h3("Insert a Duration")),
                 ### 选起始时间和结束时间
                 fileInput("file",h3("Upload File")),
                 ### 上传一个数据文件
                 numericInput("numeric",h3("Insert a number:"),
                              value=3),
                 ### 数字输入框
                 textInput("text",h3("Insert Full Name:"),
                           value="My name is..."),
                 ### 文字输入框
                 radioButtons("radio",h3("Choose"),
                              choices = list("Horror","Action","Comedy"),
                              selected = "Comedy"),
                 ### 多个里面选一个
                 selectInput("select",h3("Select"),
                             choices = list("R"=1,"Python"=2,"Java"=3),
                             selected = 3),
                 ### 下拉选择框
                 sliderInput("slider",h3("Choose a Range"),
                             min = 0,
                             max = 50,
                             value = c(3,30))
                 ### 滑块选择器
                 )),
        fluidRow(textOutput("text"),
                 plotOutput("plot"))))
    ))

server<-function(input,output){
  output$text<-renderText({
    paste("You have selected a ", input$radio,"as your choice of movie genre.")
  })
  
  output$plot<-renderPlot({
    ndata<-switch (input$select,
                   "Durability" = marvel$Durability,
                   "Strength"=marvel$Strength,
                   "Fighting"=marvel$Fighting,
                   "Speed"=marvel$Speed,
                   "Energy"=marvel$Energy,
                   "Intelligence"=marvel$Intelligence)
    
    new<-data.frame(marvel$Character,ndata)
    names(new)<-c("Character","Criteria")
    
    mypal<-colorRampPalette(brewer.pal(9,"Set1"))(23)
    ggplot(new,aes(x=Character,y=Criteria))+
      geom_bar(stat = "identity",aes(fill=Character))+
      theme(axis.text.x = element_text(angle = 90,vjust=0.5))+
      scale_fill_manual(values = mypal)
  })
}

shinyApp(ui=ui,server=server)