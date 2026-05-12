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


# server: function contain the instruction that your computer needs to build your app
server<-function(input,output){
  
}
  
  
# shinyApp: a function that creates the shiny app- connect ui and server
shinyApp(ui=ui,server=server)

# if app in working directory
runApp('shiny')
# if app not in working directory
#runApp('full path')