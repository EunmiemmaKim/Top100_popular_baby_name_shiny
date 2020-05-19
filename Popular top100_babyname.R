#import requried library
install.packages("DT")
library(tidyverse)
library(shiny)
library(readxl)
library(DT)


#import dataset(specific area)
gname<- read_excel("Top100_Popular_Baby_Names.xlsx", sheet="Girls' Names", range ="C7:GN107")
bname<- read_excel("Top100_Popular_Baby_Names.xlsx", sheet="Boys' Names", range = "C7:GN107")


# Remove null  & NA values
gname2 <- gname[,!sapply(gname,function(x)any(is.na(x)))]
bname2 <- bname[,!sapply(bname,function(x)any(is.na(x)))]

#Assign column name

colnames(gname2)[1:130]<- rep(c("Name","No"),2)
colnames(bname2)[1:130]<- rep(c("Name","No"),2)


# Girl's name pre-processing 
glist <- list()

for (i in 1:65){
  glist[[i]]<-gname2[,c(2*i-1,2*i)]}

df<-data.frame()

for (i in 1:65){
  df <- rbind(df,glist[[i]])
}


Rank<-rep(1:100,times=65)
Year<- data.frame(rep(c(1954:2018), each = 100))
gname3<-cbind(Rank,df)
gname3<-cbind(Year,gname3)


colnames(gname3)[1]<-'Year'
colnames(gname3)[2]<-'Rank'

# Finding a duplicated value 
gname3[duplicated(gname3[c("Year", "Name")]),]

# Boy's name pre-processing 
blist <- list()

for (i in 1:65){
  blist[[i]]<-bname2[,c(2*i-1,2*i)]}

df_1<-data.frame()

for (i in 1:65){
  df_1 <- rbind(df_1,blist[[i]])
}


Rank<-rep(1:100,times=65)
Year<- data.frame(rep(c(1954:2018), each = 100))
bname3<-cbind(Rank,df_1)
bname3<-cbind(Year,bname3)
View(bname3)

colnames(bname3)[1]<-'Year'
colnames(bname3)[2]<-'Rank'

# Finding a duplicate 
bname3[duplicated(bname3[c("Year", "Name")]),]

# Assign the duplicate with a new value 
which((bname3$"Year"=="1988")&(bname3$"Name"=="Michael"),arr.ind=TRUE)

bname3[3402,]['Name']='Michael1'

# Shiny

library(shiny)
library(tidyverse)


# UI

ui <- fluidPage(
  titlePanel("Top100_Popular_Baby_Names"),
  sidebarLayout(
    
    # Input(s)
    sidebarPanel(
      
      conditionalPanel(
        'input.dataset === "Girl name top 10"',
        helpText("Observe top 10 names in a given year"),
        selectInput(inputId = "Year",
                    label = "Select a year: ",
                    choices = unique(gname3$Year),
                    selected ="1945")
      ),
      
      conditionalPanel(
        'input.dataset == "Boy name top 10"',
        helpText("Observe top 10 names in a given year"),
        selectInput(inputId = "Year2",
                    label = "Select a year: ",
                    choices = unique(bname3$Year),
                    selected ="1945")
      ),
      conditionalPanel(
        'input.dataset == "The popularity of a girls name"',
        helpText("Observe the popularity of a girl's name"),
        selectInput(inputId = "Name",
                    label = "Select a name: ",
                    choices = unique(gname3$Name),
                    selected ="Christine")
      ),
      conditionalPanel(
        'input.dataset == "The popularity of a boys name"',
        helpText("Observe the popularity of a boy's name"),
        selectInput(inputId = "Name2",
                    label = "Select a name: ",
                    choices = unique(bname3$Name),
                    selected ="Michael"))
    ),
    
    
    # output 
    
    mainPanel(
      tabsetPanel( 
        id = 'dataset',
        tabPanel("Girl name top 10",DT::dataTableOutput("girlname")),
        tabPanel("Boy name top 10", DT::dataTableOutput("boyname")),
        tabPanel("The popularity of a girls name", plotOutput(outputId ="plot1")),
        tabPanel("The popularity of a boys name", plotOutput(outputId ="plot2"))
        
      )
    )
  )
)

View(gname3)

# Server


server <- function(input, output)({
  
  output$girlname<- DT::renderDataTable(
    DT::datatable(data = head(gname3 %>% filter(gname3$Year == input$Year),10), 
                  options = list(paging=FALSE),
                  rownames = FALSE))
  
  output$boyname<- DT::renderDataTable(
    
    DT::datatable(data = head(bname3 %>% filter(bname3$Year == input$Year2),10), 
                  options = list(paging=FALSE),
                  rownames = FALSE))
  
  
  output$plot1 <- renderPlot({
    gname4 = gname3[(gname3$Name==input$Name),]
    return(
      ggplot(data =gname4 , aes(x = gname4$Year, y = gname4$No, colour = No )) +
        labs(x="Year", y="No") +
        geom_line() + geom_point(color='black') + 
        xlim(1954, 2018))})
  
  
  output$plot2 <- renderPlot({
    bname4 = bname3[(bname3$Name==input$Name2),]
    return(
      ggplot(data =bname4 , aes(x = bname4$Year, y = bname4$No, colour = No )) +
        labs(x="Year", y="No") +
        geom_line() + geom_point(color='black')) + 
      xlim(1954, 2018)})
  
})


# Create a Shiny app object
shinyApp(ui = ui, server = server)

