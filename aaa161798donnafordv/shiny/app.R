library(shiny)
# ui: a user interface object- controls the layout and appearance of your app
## the text in shiny
ui<-fluidPage(
  titlePanel("Donna's shiny app."),
  sidebarLayout(position='right',
                sidebarPanel('This is a side bar.'),
                mainPanel(
                          #the bigger number means the smaller size
                          h6('Data visualization',align='center'),
                          h5('Year:5/12/2026',align='center'),
                          h4('Semester 2',align='center'),
                          h3('Location: bate lab',align='center'),
                          h2('Department: FST',align='center'),
                          h1(strong('UKM'),em('All the best'),align='center'),
                          
                          #p() means text, br() means change line, strong() means tight line, em() means 
                          p('Let us get down to business, to default the Huns'),
                          p('Did they send me a daughters, when I asked for sons?'),
                          p('You are the saddest bunch i have ever met',style='font-family:"arial"'),
                          p(code('But you can bet before we are through')),
                          div('Mister, I will make a man out of you.',style='color:red'),
                          br(),
                          p('Tranquil as a forest but on fire within.
                            Once you find your center, you are sure to win.'),
                          p('You are a',span(' spineless, pale, pathetic lot',style='color:red'),
                            'and you have not got the clue'),
                          p(strong('Somehow I will make a man out of you'),style='color:red'),
                          img(src='mulan.jpg'),
                          tags$audio(src='mulan.mp3',type='audio/mp3',autoplay=NA,controls=TRUE)
                  
  )))


## the widgets we could interact
### button could be click
ui<-fluidPage(
  titlePanel("widgets"),
  sidebarLayout(
    sidebarPanel(
      h3('buttons'),
      br(),
      br(),
      actionButton('button','button 1'),
      br(),
      br(),
      submitButton('submit')),
    mainPanel()))

### select the choice
ui<-fluidPage(
  titlePanel("widgets"),
  # 1st row
  fluidRow(
    column(3,h3('checkboxInput'),
          checkboxInput('check 1','Chocolate',value=T),
          checkboxInput('check 2','Strawberry'),
          checkboxInput('check 3','Vanilla')),
    column(3,checkboxGroupInput('checks',h3('checkboxGroupInput'),
                              choices = list('kfc'=1,'mcd'=2),selected=1))),
  # 2nd row
  fluidRow(
    column(5,dateInput('date',h3('dateInput'),value='12-05-2026')),
    column(5,dateRangeInput('dates',h3('dateRangeInput')))),
  # 3rd row
  fluidRow(
    column(4,fileInput('file',h3('fileInput'))),
    column(4,numericInput('number',h3('numericInput'),value=1)),
    column(4,textInput('text',h3('textInput'),value='my name is ...'))),
  # 4th row
  fluidRow(
    column(4,radioButtons('radio',h3('radioButtons'),
                          choices = list('horror','action','comedy'),selected = 'comedy')),
    column(4,selectInput('select',h3('selectInput'),
                          choices = list('r'=1,'python'=2,'java'=3),selected = 3)),
    column(4,sliderInput('slider',h3('sliderInput'),min=0,max=50,value=c(2,20))))
)

ui<-fluidPage(
  titlePanel('example'),
  sidebarLayout(
    fluidRow(
      column(7,radioButtons('radio',h3('choose'),choices=list('horror','action','comedy'))),
      column(7,selectInput('select',h3('select'),choices=list('R','Python','Java'),selected = 'Java')),
      column(7,sliderInput('slider',h3('choose a range'),min=0,max=50,value=c(3,30)))
    ),
    mainPanel(
      textOutput('myradio'),
      textOutput('myselect'),
      textOutput('myslider')
    )
  )
)


# server: function contain the instruction that your computer needs to build your app
# textOutput(),plotOutput(),imageOutput(),plotlyOutput(),uiOutput(),dataTableOutput()
# renderText(),renderPlot(),renderImage(),renderPlotly(),renderUI(),renderDataTable()
server<-function(input,output){
  output$myradio<-renderText({
    paste('you prefer to continue with',input$radio,'as your choice of movie genre.')
  })
  output$myselect<-renderText({
    paste('you have selected a ',input$select,'programming.')
  })
  output$myslider<-renderText({
    paste('you prefered range is between',input$slider[1],'and',input$slider[2])
  })
}
  
  
# shinyApp: a function that creates the shiny app- connect ui and server
shinyApp(ui=ui,server=server)

# if app in working directory
runApp('shiny')
# if app not in working directory
#runApp('full path')


######################################## exercise ##############################################
ui<-fluidPage(
  titlePanel('exercise'),
  fluidRow(
    column(3,dateInput('date',h3("Insert today's date:"),value='2026-5-14')),
    column(3,radioButtons('gender',h3("Gender:"),choices=list('Male','Female'),selected = 'Male')),
    column(3,textInput('name',h3("Full name:"),value='Donna')),
    column(3,numericInput('age',h3("Age:"),value=20)),
    textOutput('newdate'),
    textOutput('newmember')
  )
)
server<-function(input,output){
  output$newdate<-renderText({paste('A new membership is register on',input$date,'.')})
  
  output$newmember<-renderText({paste(input$gender,'named',input$name,'of age',input$age,'is recorded.')})
}
shinyApp(ui=ui,server=server)

library(ggplot2)
library(RColorBrewer)
marvel<-read.csv('Marvel.csv',header = T)
ui<-fluidPage(
  titlePanel('Marvel character'),
  fluidRow(
    column(3,selectInput('select',h3("Select Criteria:"),
                         choices=list('Durability','Strength','Fighting','Speed','Energy','Intelligence'),
                         selected ='Intelligence')),
    textOutput('text'),
    plotOutput('plot')
  )
)


server<-function(input,output){
  a<-reactive({switch(input$select,
                      'Durability'=marvel$Durability,
                      'Strength'=marvel$Strength,
                      'Fighting'=marvel$Fighting,
                      'Speed'=marvel$Speed,
                      'Energy'=marvel$Energy,
                      'Intelligence'=marvel$Intelligence)})
  
  output$text<-renderText({paste('Bar chart for',input$select,'of Marvel Characters.')})
  
  output$plot<-renderPlot({
    ndata<-a()
    newdata<-data.frame(marvel$Character,ndata)
    names(newdata)<-c('character','criteria')
    mypal<-colorRampPalette(brewer.pal(9,'Set1'))(23)
    ggplot(newdata,aes(x=character,y=criteria))+
      geom_bar(stat='identity',aes(fill=character))+
      theme(axis.text.x = element_text(angle = 90,vjust=0.5))+
      scale_fill_manual(values = mypal)
  })
}


shinyApp(ui=ui,server=server)

