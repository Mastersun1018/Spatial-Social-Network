library(shiny)
library(leaflet)
library(shinydashboard)
library(igraph)
library(ggplot2)
library(reshape2)
library(sp)
library(mapview)
library(twitteR)
library(tweet2r)
library(ROAuth)
library(rtweet)
Sys.setlocale(category = "LC_ALL",locale = "chinese (simplified)")
my_key<-""
my_secret<-""
my_access_token<-""
my_access_secret<-""
setup_twitter_oauth(my_key,my_secret,my_access_token,my_access_secret)
setwd("d:/")
files<-dir('d:/mexico')
lines<-c()
setwd('d:/mexico')
Sys.setlocale("LC_TIME", "C")
#read data from files
# for(i in 1:length(files)){
#   
#   lines<-c(lines,readLines(files[i]))
# }
# 
# data<-lapply(lines[which(lines!="")],rjson::fromJSON)



#####################new version 
#time fragment
#begin<-as.numeric(unclass(as.POSIXct("2017-11-06"))+10*3600)
#extract useful data from data
leng<-length(data)
df0<-c()
#dftime<<-c()
h1<-c()
h2<-c()





# Definition of Sidebar elements
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Collect Data", tabName = "data", icon = icon("globe")),
    
    menuItem("Geotweets", tabName = "twitter", icon = icon("globe")),
    #sidebarMenu(
    menuItem("Geolocate user", tabName = "user", icon = icon("globe")),
    # sidebarMenu(
    menuItem("Sampling user", tabName = "samplinguser", icon = icon("globe")),
    menuItem("Analyse", tabName = "analyse", icon = icon("globe"),
             menuSubItem("Social Measure" ,tabName = "socialmeasure", icon = icon("map-o")), 
             menuSubItem("Heat Map", tabName = "heatmap", icon = icon("map-o"))
             
             
             
             # menuSubItem(
             #   HTML(paste("Diffuse kilder NH", tags$sub("3"), sep = "")),
             #   tabName = "map_dif_nh3", icon = icon("map-o"), selected = TRUE
             # )
    ),
    menuItem("User Track",tabName = "user track",icon = icon("globe"),
             menuSubItem("Movement And Compare" ,tabName = "movementandcompare", icon = icon("map-o")), 
             menuSubItem("User Timeline", tabName = "usertimeline", icon = icon("map-o")),
             menuSubItem("Area", tabName = "area", icon = icon("map-o"))
             
             
             ),
    
    menuItem("Notation", tabName = "notation", icon = icon("globe"))
    
    # menuItem("Maps2", tabName = "maps1", icon = icon("globe"),
    #          menuSubItem(
    #            HTML(paste("Diffuse kilder NH", tags$sub("3"), sep = "")),
    #            tabName = "map_dif_nh3", icon = icon("map-o"), selected = TRUE
    #          )
    #)
  )
  #)
  #)
  #uiOutput("out1")
)
# Definition of body elements
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "data", 
            leafletOutput("dataplot", width = "100%", height = 600),
            absolutePanel(id = "controlu1",fixed = T,
                          draggable = TRUE, top = 80, left = 300, right = "auto", bottom = "auto",
                          width = 200, height = 50,cursor = c("auto","move", "default", "inherit"),
                          style = "opacity: 0.65; z-index: 1000;",
                          
                          textInput("bbbox", label = "Set Rectangle Area", value = "Please enter two pairs of coordinates"),
                          
                          actionButton("plotarea", "Plot a Rectangle"),
                          numericInput("numdata","Set Time out(second)",value = 1),
                          actionButton("pulldata", "Pull DATA")
                          
                          
                      
                          
                          
                          
            )
            
  ),
    
    tabItem(tabName = "twitter",
            leafletOutput("geotwitter", width = "100%", height = 600),
            absolutePanel(id = "controlu2", fixed = TRUE,
                          draggable = TRUE, top = 80, left = 300, right = "auto", bottom = "auto",
                          width = 200, height = 50,
                          style = "opacity: 0.65; z-index: 1000;",
                          actionButton("do", "Choose data set"),
                          selectInput("dataset","Data Accuracy",c("None"="none","All"="all","LON,LAT Coordinate"="coordinate","Bounding_Box"="boundingbox")),
                          
                          textOutput("tweetsnumber"),
                          selectInput("linerelationship","Relationship",c("Mention"="mention","Reply"="reply"))
                         
            
            
            
            )),
    
    tabItem(tabName = "user",
            leafletOutput("geouser", width = "100%", height = 600),
            absolutePanel(id = "controlu3", fixed = TRUE,
                          draggable = TRUE, top = 80, left = 300, right = "auto", bottom = "auto",
                          width = 200, height = 50,
                          style = "opacity: 0.65; z-index: 1000;",
                          helpText("Please Coose a Location Assessment Techniques"),
                          selectInput("chooseuser","Location Assessment Techniques",c(        "None"="none",
                                                                                  "Nday"="nday",
                                                                                  "Plurality"="plurality",
                                                                                  "Geometrical center"="geometricalcenter",
                                                                                  "GemetricMedian"="gmedian")),
                          uiOutput("out2"),
                          textOutput("locationnumber")
                          #selectInput("relationship","Relationship",c("Mention"="mention","Reply"="reply")),
                          #selectInput("centrality","Centrality",c("Spatial Degree in"="degreein","Spatial Degree out"="degreeout","Spatial Betweenness"="betweenness"))
            )        
            ),
    
    tabItem(tabName="samplinguser",
            leafletOutput("sampling", width = "100%", height = 600),
            absolutePanel(id = "controls4", fixed = TRUE,
                          draggable = TRUE, top = 80, left = 300, right = "auto", bottom = "auto",
                          width = 200, height = 50,
                          style = "opacity: 0.65; z-index: 1000;",
                          selectInput("samplingstrategy","Sampling Strategy",c("Random Sampling"="sampling","Grid Sampling"="grid")),
                          textOutput("nums"),
                         numericInput("nums","Sampling Quantity",value = 1)
            )
    ),
    
    
    tabItem(tabName="heatmap",
            plotOutput("pheatmap",width = 700,height=800)
            
            
    ),
    tabItem(tabName="socialmeasure",
            leafletOutput("psocialmeasure",width = "100%",height=600),
            absolutePanel(id = "scontrol5", fixed = TRUE,
                          draggable = TRUE, top = 80, left = 300, right = "auto", bottom = "auto",
                          width = 200, height = 50,
                          style = "opacity: 0.65; z-index: 1000;",
                          
    #selectInput("relationship","Relationship",c("Mention"="mention","Reply"="reply")),
    textOutput("analysenumber"),
    selectInput("centrality","Centrality",c("Spatial Degree Centrality"="degree","Spatial Closeness Centrality "="physicaldistance")),
    selectInput("relation","Network Relationship",c("Mention"="mention","Reply"="reply"))
    
    
        
    )
    ),
    tabItem(tabName = "movementandcompare",
            absolutePanel(id = "scontrolu6", fixed = FALSE,
                          draggable = FALSE, top = "auto", left = 10, right = "auto", bottom = "auto",
                          width = 450, height = 650,
                          #style = "opacity: 0.65; z-index: 1000;",
                          leafletOutput("usertrack1",width = 460,height="100%"),
                          
                          
                          absolutePanel(id = "scontrolu61", fixed = FALSE,
                                        draggable = FALSE, top = "auto", left = 10, right = "auto", bottom = "160",
                                        width = 200, height = 50,
                                        style = "opacity: 0.65; z-index: 1000;",
                          
                          sliderInput("slider1", "Time Range Hour", min = 0,
                                      max = 294, value = c(0, 0)),
                          
                          
                          textOutput("comparenumber1")
                          
                          )
                          ),
      absolutePanel(id = "scontrolu7", fixed = FALSE,
                          draggable = FALSE, top = "auto", left = "auto", right = 10, bottom = "auto",
                          width = 450, height = 650,
                    #style = "opacity: 0.65; z-index: 1000;",
                          leafletOutput("usertrack2",width = 460,height="100%"),
                          absolutePanel(id = "scontrolu71", fixed = FALSE,
                          draggable = FALSE, top = "auto", left = "auto", right = 235, bottom = "160",
                          width = 200, height = 50,
                          style = "opacity: 0.65; z-index: 1000;",
                          sliderInput("slider2", "Time Range", min = 0,
                          max = 294, value = c(0, 0)),
                        
                          textOutput("comparenumber2")
                          
                          )
       )
          
            
     ),
    
    tabItem(tabName="usertimeline",
            leafletOutput("pusertimeline",width = "100%",height=600),
            absolutePanel(id = "controlu8", fixed = TRUE,
                          draggable = TRUE, top = 80, left = 300, right = "auto", bottom = "auto",
                          width = 200, height = 50,
                          style = "opacity: 0.65; z-index: 1000;",
                         textOutput("usertimenumber")                                     
                         
            )      
            
            
    ),
    tabItem(tabName="area",
            leafletOutput("areaselect",width = "100%",height=600),
            absolutePanel(id = "controlu9", fixed = TRUE,
                          draggable = TRUE, top = 60, left = 300, right = "auto", bottom = "auto",
                          width = 200, height = 50,
                          style = "opacity: 0.65; z-index: 1000;",
                          selectInput("areatype","Area type",c("Circle"="circle","Bounding_box"="boundingbox","Hexagon"="hexagon","Rectangle"="rectangle")),
                          #textOutput("inputnote"),
                          # if(input$areatype=="circle")
                          # {
                          uiOutput("out1"),
                          #textInput("textinput", label = "Text Input", value = "Enter text..."),
                         # },
                        
                          #selectInput("list","LIST",c("Mexico"="mexico","China"="china")),
                          # },
                          actionButton("confirm", "Plot Area"),
                         
                          sliderInput("slider3", "Time Range(Hour)", min = 0,
                                      max = 294, value = c(0, 0)),
                         sliderInput("slider33", "Left(Minute)", min = 0,
                                     max = 60, value = 0),
                         sliderInput("slider333", "Right(Minute)", min = 0,
                                     max = 60, value = 0),
                         
                         textOutput("areatweetsnumber"),
                         actionButton("confirm2", "Export Data")
                          
                                                        
                          
            )      
            
            
    ),
  tabItem(tabName="notation",
          mainPanel(
            textOutput("notationtext"),
            tableOutput("notationtable")
            
          )
  
  )
          
    
  
  
))

ui <- dashboardPage(
  dashboardHeader(title = "Navigation"),
  sidebar,
  body
)


server <- function(input, output,session) {

  observeEvent(input$dataset,  {
   print("action")
    updateSliderInput(session,"slider1",min=0,max =(tweetsset$end-tweetsset$begin)/3600,value = c(0,0))
    updateSliderInput(session,"slider2",min=0,max =(tweetsset$end-tweetsset$begin)/3600,value = c(0,0))
    updateSliderInput(session,"slider3",min=0,max =(tweetsset$end-tweetsset$begin)/3600,value = c(0,0))
    
  })

  
  output$dataplot<- renderLeaflet({
  mapdata <-leaflet() %>%
    # Base groups
    addTiles(group = "OSM (default)") %>%
    addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
    addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
    addProviderTiles(providers$Esri.WorldImagery , group = "Esri.WorldImagery") %>%
    addProviderTiles(providers$Hydda , group = "Hydda") %>%
    # Overlay groups
    #addCircles(~df0[,2],~df0[,3], group = "Tweets",color ="red",radius=30,label=~df0[,1]) %>%
    # addCircles(~lon,lat, group = "Outline")%>%
    # Layers control
    #addRectangles(
    #   lng1=~df0[,5], lat1=~df0[,6],
    #   lng2=~df0[,7], lat2=~df0[,8],
    #   fillColor = "transparent",group = "bounding_box"
    # )%>%
    addLayersControl(
      baseGroups = c( "Toner","OpenStreetMap","Esri.WorldImagery","Hydda"),
      options = layersControlOptions(collapsed =TRUE)
    )%>%
    addLegend("bottomright", 
              colors =c("red"),
              labels= c("Tweets"),
              title= "Geolocated tweets",
              opacity = 10)
  
  

  
  observeEvent(input$plotarea,{
    
    str<-strsplit(input$bbbox,",")
    tweetsset$data<<-str[[1]]
    
    lon1<-as.numeric(str[[1]][1])
    lat1<-as.numeric(str[[1]][2])
    lon2<-as.numeric(str[[1]][3])
    lat2<-as.numeric(str[[1]][4])
    
    
    umap4<-leafletProxy("dataplot")%>%
      clearGroup("datagroup")%>%
      addRectangles(
        lng1=lon1, lat1=lat1,
        lng2=lon2, lat2=lat2,
        fillColor = "transparent"
        ,label="rectangle",group = "datagroup")
    
    
    
    
    
  })
  
  
  
  observeEvent(input$pulldata,{
    x1<-as.numeric(tweetsset$data[1])
    y1<-as.numeric(tweetsset$data[2])
    x2<-as.numeric(tweetsset$data[3])
    y2<-as.numeric(tweetsset$data[4])
      datafile<- stream_tweets(
      c(x1,y1, x2, y2),
      timeout = (as.numeric(input$numdata)),
      parse = FALSE,
      file_name = "tweet.json"
    )
    
    
    
    
  })
  
  
  
  
  
  
  mapdata
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$out1 <- renderUI({
    if(input$areatype=="circle")
    {
      dyn_ui<- textInput("allinput", label = "Geographic Coordinate Radius(m)", value = "eg:lon,lat,number(meter)")
    }
    if(input$areatype=="boundingbox")
    {
      dyn_ui<- textInput("allinput", label = "Place Name", value = "eg:Iztapalapa")
    }
    if(input$areatype=="hexagon")
    {
      dyn_ui<- textInput("allinput", label = "Six pairs of Geographic Coordinate", value = "eg:lon1,lat1,lon2,lat2,lon3,lat3,lon4,lat4,lon5,lat5,lon6,lat6")
    }
    if(input$areatype=="rectangle")
    {
      dyn_ui<- textInput("allinput", label = "Two pairs of Geographic Coordinate ", value = "eg:lon1,lat1,lon2,lat2")
    }
    dyn_ui
  })
  output$out2 <- renderUI({
    if(input$chooseuser=="nday")
    {
      dyn_uin<- numericInput("numn","Set parameter N",value = 1)
      dyn_uin
  
    }

    

  })
  
  dft<-c()
  rdf<-c()
  reply<-c()
  mention<-c()
  # observeEvent(input$relationship,  {
  #   updateSelectInput(session,"relationship",choices = "none")
  # })
  # 
  
  dataset<<-reactiveValues()
  tweetsset<<-reactiveValues()

  ############################3
  #geotwitter
  output$geotwitter <- renderLeaflet({
    # print(input$circleinput)
    observeEvent(input$do, {
      file<- file.choose(new = TRUE)
      line<-readLines(file)
      dataset$file<<-lapply(line[which(line!="")],rjson::fromJSON)
    
    })
  
   

    
    
    

    
    
    mapt <- leaflet("geotwitter") %>%
      # Base groups
      #addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery , group = "Esri.WorldImagery") %>%
      addProviderTiles(providers$Hydda , group = "Hydda") %>%
      # Overlay groups
      #addCircles(~df0[,2],~df0[,3], group = "Tweets",color ="red",radius=30,label=~df0[,1]) %>%
      # addCircles(~lon,lat, group = "Outline")%>%
      # Layers control
      #addRectangles(
      #   lng1=~df0[,5], lat1=~df0[,6],
      #   lng2=~df0[,7], lat2=~df0[,8],
      #   fillColor = "transparent",group = "bounding_box"
      # )%>%
      addLayersControl(
        baseGroups = c( "Toner","OpenStreetMap","Esri.WorldImagery","Hydda"),
        overlayGroups = c("Tweets","bounding_box"),
        options = layersControlOptions(collapsed =TRUE)
      )%>%
      addLegend("bottomright", 
                colors =c("red"),
                labels= c("Tweets"),
                title= "Geolocated tweets",
                opacity = 10)
    
    
    
    
    
    
    
    
    
    observe({
      
      
      
      
      
      output$tweetsnumber<-renderText(
        {
          n<-paste("There are",nrow(tweetsset$number),"tweets")
          n
        }
      )
      if(length(dataset$file)>0){
 
      for(jj in 1:length(dataset$file)){

          a<-dataset$file[[jj]]$user$screen_name
          if(length(dataset$file[[jj]][["in_reply_to_screen_name"]])==1)
          {

            b<-dataset$file[[jj]][["in_reply_to_screen_name"]]
            c<-c()
            c<-cbind(a,b)
            reply<-rbind(c,reply)
            dfreply<<-as.data.frame(reply,stringsAsFactors=FALSE)
          }
          if (length(dataset$file[[jj]]$entities$user_mentions) > 0) {

            b <- sapply(dataset$file[[jj]]$entities$user_mentions, function(mention) {

              mention$screen_name
            })

            c<-c()
            c<-cbind(a,b)
            mention<-rbind(c,mention)
            dfmention<<-as.data.frame(mention,stringsAsFactors=FALSE)

          }

        if(jj==1)
        {
          t1<-unclass(as.POSIXct(as.Date(paste(strsplit(dataset$file[[jj]][["created_at"]]," ")[[1]][3],strsplit( dataset$file[[jj]][["created_at"]]," ")[[1]][2],strsplit( dataset$file[[jj]][["created_at"]]," ")[[1]][6]),format='%d %b %Y')))+as.numeric(strsplit(strsplit(dataset$file[[jj]][["created_at"]]," ")[[1]][4],":")[[1]][1])*3600
          tweetsset$begin<<-t1-(t1 %% 86400)

        }
        if(jj==length(dataset$file))
        {
          t2<-unclass(as.POSIXct(as.Date(paste(strsplit(dataset$file[[jj]][["created_at"]]," ")[[1]][3],strsplit( dataset$file[[jj]][["created_at"]]," ")[[1]][2],strsplit( dataset$file[[jj]][["created_at"]]," ")[[1]][6]),format='%d %b %Y')))+as.numeric(strsplit(strsplit(dataset$file[[jj]][["created_at"]]," ")[[1]][4],":")[[1]][1])*3600
          tweetsset$end<<-t2-(t2 %% 86400)+86400

        }

        if(length(dataset$file[[jj]]$place)!=0)
        {
        
        if(length(dataset$file[[jj]]$coordinates)!=0)
        {
          lon<-dataset$file[[jj]]$coordinates[[2]][1]
          lat<-dataset$file[[jj]]$coordinates[[2]][2]
          lon<-format(round(lon, 4), nsmall = 4)
          lat<-format(round(lat, 4), nsmall = 4)
          flag<-"coordinate"
        }
        if(length(dataset$file[[jj]]$coordinates)==0)
        {
          lon<-mean(c(dataset$file[[jj]]$place$bounding_box$coordinates[[1]][[1]][1],dataset$file[[jj]]$place$bounding_box$coordinates[[1]][[2]][1],dataset$file[[jj]]$place$bounding_box$coordinates[[1]][[3]][1],dataset$file[[jj]]$place$bounding_box$coordinates[[1]][[4]][1]))
          lat<-mean(c(dataset$file[[jj]]$place$bounding_box$coordinates[[1]][[2]][2],dataset$file[[jj]]$place$bounding_box$coordinates[[1]][[3]][2],dataset$file[[jj]]$place$bounding_box$coordinates[[1]][[3]][2],dataset$file[[jj]]$place$bounding_box$coordinates[[1]][[4]][2]))
          
         
          lon<-format(round(lon, 4), nsmall = 4)
          lat<-format(round(lat, 4), nsmall = 4)
          flag<-"boundingbox"
        }
        basetime<-unclass(as.POSIXct(as.Date(paste(strsplit(dataset$file[[jj]][["created_at"]]," ")[[1]][3],strsplit( dataset$file[[jj]][["created_at"]]," ")[[1]][2],strsplit( dataset$file[[jj]][["created_at"]]," ")[[1]][6]),format='%d %b %Y')))+as.numeric(strsplit(strsplit(dataset$file[[jj]][["created_at"]]," ")[[1]][4],":")[[1]][1])*3600+as.numeric(strsplit(strsplit(dataset$file[[jj]][["created_at"]]," ")[[1]][4],":")[[1]][2])*60
        dff<-cbind(dataset$file[[jj]]$user$screen_name,lon,lat,dataset$file[[jj]]$place$name,dataset$file[[jj]]$place$bounding_box$coordinates[[1]][[1]][1],dataset$file[[jj]]$place$bounding_box$coordinates[[1]][[1]][2],dataset$file[[jj]]$place$bounding_box$coordinates[[1]][[3]][1],dataset$file[[jj]]$place$bounding_box$coordinates[[1]][[3]][2],basetime,flag)
        
        info<-cbind(dataset$file[[jj]]$user$screen_name,dataset$file[[jj]][["id_str"]],dataset$file[[jj]][["text"]],dataset$file[[jj]][["user"]][["created_at"]],dataset$file[[jj]][["user"]][["followers_count"]],dataset$file[[jj]][["user"]][["friends_count"]],dataset$file[[jj]][["user"]][["favourites_count"]],dataset$file[[jj]][["user"]][["lang"]])

        rdf<-rbind(info,rdf)
        
        
  
        dft<-rbind(dff,dft)
        }
      }
        

      
      dft<-as.data.frame(dft,stringsAsFactors=FALSE)
      dft[,2]<-as.numeric(as.character(dft[,2]))
      dft[,3]<-as.numeric(as.character(dft[,3]))
      dft[,5]<-as.numeric(as.character(dft[,5]))
      dft[,6]<-as.numeric(as.character(dft[,6]))
      dft[,7]<-as.numeric(as.character(dft[,7]))
      dft[,8]<-as.numeric(as.character(dft[,8]))
 kkk<<-dft
 dfrdf<-as.data.frame(rdf,stringsAsFactors=FALSE)
 colnames(dfrdf)<-c("screen_name","id_str","text","created_at","followers_count","friends_count","favourites_count","lang")
 information<<-dfrdf
 
 
 
 
 #build a network 
 #reply four kind of degree
 gii1 <<- graph_from_data_frame(dfreply)
 reply_label1<<-V(gii1)$name
 matrix_degree_in1<<-degree(gii1,mode = "in")
 matrix_degree_out1<<-degree(gii1,mode = "out")
 #matrix_cloeness1<<-closeness(gii1)+1
 matrix_betweeness1<<-betweenness(gii1)
 replytable<<-cbind(reply_label1,matrix_degree_in1,matrix_degree_out1,matrix_betweeness1)
 #mention four kind of degree
 gii2 <<- graph_from_data_frame(dfmention)
 mention_label2<<-V(gii2)$name
 matrix_degree_in2<<-degree(gii2,mode = "in")
 matrix_degree_out2<<-degree(gii2,mode = "out")
 #matrix_cloeness2<<-closeness(gii2)+1
 matrix_betweeness2<<-betweenness(gii2)
 mentiontable<<-cbind(mention_label2,matrix_degree_in2,matrix_degree_out2,matrix_betweeness2)
 colnames(replytable)[1]<<-"V1"
 colnames(mentiontable)[1]<<-"V1"
 replytable<<-as.data.frame(replytable,stringsAsFactors=FALSE)
 mentiontable<<-as.data.frame(mentiontable,stringsAsFactors=FALSE)
 
      }
     
     
      
      
      
      
      
      if(input$dataset=="none"){
        tweetsset$number<<-c()
        mapt<-leafletProxy("geotwitter") %>%
          clearGroup("alltweets")
          #addCircles(selectuser()[,2],selectuser()[,3],radius = 50, color = lengendcolor(),label=selectuser()[,1],group = "selectgroup")
      }
      if(input$dataset=="all"){
       tweetsset$number<<-dft
        mapt<-leafletProxy("geotwitter")%>%
          clearGroup("alltweets")%>%
          addCircles(data=tweetsset$number,tweetsset$number[,2],tweetsset$number[,3],radius = 50, color = "red",label=tweetsset$number[,1],group = "alltweets")

      }
      if(input$dataset=="coordinate"){
        tweetsset$number<<-subset(dft,dft[,10]=="coordinate")
        mapt<-leafletProxy("geotwitter")%>%
          clearGroup("alltweets")%>%
          addCircles(data=tweetsset$number,tweetsset$number[,2],tweetsset$number[,3],radius = 50, color = "red",label=tweetsset$number[,1],group = "alltweets")
        
      }
      if(input$dataset=="boundingbox"){
        tweetsset$number<<-subset(dft,dft[,10]=="boundingbox")
        mapt<-leafletProxy("geotwitter")%>%
          clearGroup("alltweets")%>%
          addCircles(data=tweetsset$number,tweetsset$number[,2],tweetsset$number[,3],radius = 50, color = "red",label=tweetsset$number[,1],group = "alltweets")
        
      }
      
      if(length(tweetsset$number)>0)
      {
      r<-c()
      dfuni<-unique(tweetsset$number[,c(1,2,3)])
      for (jj in 1:nrow(dfuni)) {
        lon<-dfuni[jj,2]
        lat<-dfuni[jj,3]
        
        s1<-subset(lineselectrelation(),lineselectrelation()[,1]==dfuni[jj,1])
            
        if(nrow(s1)>0)
        {
        s2<-s1[,2]
        dfs2<-as.data.frame(s2)
        colnames(dfs2)<-c("V1")
  
        
       s3<-merge(dfs2,tweetsset$number,by="V1",all=FALSE)
     
        

        if(nrow(s3)>0)
        {
            print(s3)
          print("jinlaile")
          for (ii in 1:nrow(s3)) {
            c<-c()
            c<-cbind(lon,lat,s3[ii,2],s3[ii,3])
            r<-rbind(c,r)
          }

        
       }
      }
        

      }
      if(length(r)>0)
      {
 
      dfr<-as.data.frame(r)
     
     dfr[,1]<-as.numeric(as.character(dfr[,1]))
     dfr[,2]<-as.numeric(as.character(dfr[,2]))
     dfr[,3]<-as.numeric(as.character(dfr[,3]))
     dfr[,4]<-as.numeric(as.character(dfr[,4]))
     colnames(dfr)<-c("lon1","lat1","lon2","lat2")
     dfrr<<-dfr
     print("dfr")
     print(dfr)
      

        mapt<-leafletProxy("geotwitter")%>%
        clearGroup("relation")
        for (i in 1:nrow(dfr))
          mapt<-mapt%>%addPolylines(data=dfr,lat=c(dfr[,2],dfr[,4]),lng=c(dfr[,1],dfr[,3]),group ="relation")


      }
      
      

      }
      

      
      
    })
    
    mapt
    
    
  })
  lineselectrelation<<- reactive({
    switch(input$linerelationship,
           "mention"= dfmention,
           "reply" = dfreply
           )
  })
  
  selectrelation<- reactive({
    switch(input$relation,
           "mention"= dfmention,
           "reply" = dfreply)
           
    
  })
  


  
  ###############################
  #geouser
  output$geouser <- renderLeaflet({
    mapu <- leaflet("geouser") %>%
      #addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery , group = "Esri.WorldImagery") %>%
      addProviderTiles(providers$Hydda , group = "Hydda") %>%
      #  addCircles(data = wndfdf,~wndfdf[,2],~wndfdf[,3],radius=50,label=~wndfdf[,1],group = "n-day",color = "yellow")%>%
      # addCircles(data = wpdfdf,~wpdfdf[,2],~wpdfdf[,3],radius=50,label=~wpdfdf[,1],group = "plurality",color = "blue")%>%
      #  addCircles(data = wgdfdf,~wgdfdf[,2],~wgdfdf[,3],radius=50,label=~wgdfdf[,1],group = "geometrical center",color="green")%>%
      #  addCircles(data = wmdfdf,~wmdfdf[,2],~wmdfdf[,3],radius=50,label=~wmdfdf[,1],group = "Spatial median",color="purple")%>%
      addLayersControl(
        baseGroups = c("Toner", "OpenStreetMap", "Esri.WorldImagery","Hydda"),
       # overlayGroups = c("n-day", "plurality","geometrical center","Spatial median"),
        options = layersControlOptions(collapsed =TRUE)
      )%>%
     addLegend("bottomright", 
                              colors =c("yellow", "blue", "green", "purple"),
                              labels= c("L-N","L-P","L-GC","L-GM"),
                              title= "LAT",
                              opacity = 10)
    
    
    lengendcolor<- reactive({
      switch(input$chooseuser,
             "nday"= "yellow",
             "plurality" = "blue",
             "geometricalcenter" = "green",
             "gmedian"= "purple" )
      
    })
    
    observe({
  
      
   
      if(input$chooseuser=="none"){
        
        mapu<-leafletProxy("geouser")%>%
          clearGroup("selectgroup")
        
      }
      if(input$chooseuser=="nday"){
        n<-input$numn
        df0<-tweetsset$number
         #n-day
         df<-c()
         c<-c()
         tab<-table(df0$V1,df0$V4)
         dftab<-as.data.frame(tab,stringsAsFactors=FALSE)
         dfdftab<<-dftab
         if(length(input$numn)>0)
         {
       
         ndftab<-dftab[dftab[,3]>=input$numn,]
         if(nrow(ndftab)>0)
         {
         #print(ndftab)
         
         for (ii in 1:nrow(ndftab)) {
           #print(nrow(ndftab))
           lon<-median(df0[df0[,1]==ndftab[ii,1] & df0[,4]==ndftab[ii,2] ,][,2])
           lat<-median(df0[df0[,1]==ndftab[ii,1] & df0[,4]==ndftab[ii,2] ,][,3])
           c<-cbind(dftab[ii,1],lon,lat,dftab[ii,2])

           df<-rbind(c,df)
         }
         ndfdf<-as.data.frame(df,stringsAsFactors=FALSE)

         ndfdf[,2]<-as.numeric(ndfdf[,2])
         ndfdf[,3]<-as.numeric(ndfdf[,3])
         wndfdf<<-ndfdf

         mapu<-leafletProxy("geouser",data=wndfdf) %>%
           # hideGroup(c("n-day","plurality","geometrical center","Gmedian"))%>%
           clearGroup("selectgroup")%>%
           addCircles(wndfdf[,2],wndfdf[,3],radius = 50, color = lengendcolor(),label=wndfdf[,1],group = "selectgroup")
         output$locationnumber<-renderText(
           {
             n<-paste(length(unique(ndftab[,1])),"Users have",nrow(ndftab),"Locations")
             n
           }
         )

        
        
         }
         if(nrow(ndftab)<=0)
         {
           
           mapu<-leafletProxy("geouser",data=wndfdf) %>%
             # hideGroup(c("n-day","plurality","geometrical center","Gmedian"))%>%
             clearGroup("selectgroup")
           output$locationnumber<-renderText(
             {
               n<-paste("0","Users have","0","Locations")
               n
             }
           )
         }
         
         
         
         }
      }
      
      if(input$chooseuser=="plurality"){
        print("ok")
        df0<-tweetsset$number
        tab<-table(df0$V1,df0$V4)
        df<-c()
        c<-c()
        for (jj in 1:nrow(tab)) {
          k<-which.max(tab[jj,])
          lon=lon<-mean(df0[df0[,1]==rownames(tab)[jj] & df0[,4]==colnames(tab)[k],][,2])
          lat=lat<-mean(df0[df0[,1]==rownames(tab)[jj] & df0[,4]==colnames(tab)[k],][,3])
          c<-cbind(rownames(tab)[jj],lon,lat,colnames(tab)[k])
          df<-rbind(c,df)
          
        }
        
        pdfdf<-as.data.frame(df,stringsAsFactors=FALSE)
        pdfdf[,2]<-as.numeric(pdfdf[,2])
        pdfdf[,3]<-as.numeric(pdfdf[,3])
        wpdfdf<<-pdfdf
    
          mapu<-leafletProxy("geouser",data=wpdfdf) %>%
          clearGroup("selectgroup")%>%
          addCircles(wpdfdf[,2],wpdfdf[,3],radius = 50, color = lengendcolor(),label=wpdfdf[,1],group = "selectgroup")
        output$locationnumber<-renderText(
          {
            n<-paste(length(unique(wpdfdf[,1])),"Users have",nrow(wpdfdf),"Locations")
            n
          }
        )
        
      }
      if(input$chooseuser=="geometricalcenter"){
        df0<-tweetsset$number
        tab<-table(df0$V1,df0$V4)
        c<-c()
        df<-c()
        for (kk in 1:nrow(tab)) {
          k<-which.max(tab[kk,])
          lon=mean(df0[df0[,1]==rownames(tab)[kk],][,2])
          lat=mean(df0[df0[,1]==rownames(tab)[kk],][,3])
          c<-cbind(rownames(tab)[kk],lon,lat,colnames(tab)[k])
          df<-rbind(c,df)
          
        }
        gdfdf<-as.data.frame(df,stringsAsFactors=FALSE)
        gdfdf[,2]<-as.numeric(gdfdf[,2])
        gdfdf[,3]<-as.numeric(gdfdf[,3])
        wgdfdf<<-gdfdf
        
        mapu<-leafletProxy("geouser",data=wgdfdf) %>%
          # hideGroup(c("n-day","plurality","geometrical center","Gmedian"))%>%
          clearGroup("selectgroup")%>%
          addCircles(wgdfdf[,2],wgdfdf[,3],radius = 50, color = lengendcolor(),label=wgdfdf[,1],group = "selectgroup")
        output$locationnumber<-renderText(
          {
            n<-paste(length(unique(wgdfdf[,1])),"Users have",nrow(wgdfdf),"Locations")
            n
          }
        )
      }
      if(input$chooseuser=="gmedian"){
        df0<-tweetsset$number
        tab<-table(df0$V1,df0$V4)
        c<-c()
        df<-c()
        for (ll in 1:nrow(tab)) {
          k<-which.max(tab[ll,])
          lon=median(df0[df0[,1]==rownames(tab)[ll],][,2])
          lat=median(df0[df0[,1]==rownames(tab)[ll],][,3])
          c<-cbind(rownames(tab)[ll],lon,lat,colnames(tab)[k])
          df<-rbind(c,df)
          
        }
        mdfdf<-as.data.frame(df,stringsAsFactors=FALSE)
        mdfdf[,2]<-as.numeric(mdfdf[,2])
        mdfdf[,3]<-as.numeric(mdfdf[,3])
        wmdfdf<<-mdfdf
        mapu<-leafletProxy("geouser",data=wmdfdf) %>%
          # hideGroup(c("n-day","plurality","geometrical center","Gmedian"))%>%
          clearGroup("selectgroup")%>%
          addCircles(wmdfdf[,2],wmdfdf[,3],radius = 50, color = lengendcolor(),label=wmdfdf[,1],group = "selectgroup")
        output$locationnumber<-renderText(
          {
            n<-paste(length(unique(wmdfdf[,1])),"Users have",nrow(wmdfdf),"Locations")
            n
          }
        )
        
        
      }
      
      
      
      
      
      
    })
    mapu
  })
  
  selectuser<<- reactive({
    switch(input$chooseuser,
           "nday"= wndfdf,
           "plurality" = wpdfdf,
           "geometricalcenter" = wgdfdf,
           "gmedian"= wmdfdf )
 
    
  })
 
  ###############3
  #reactive lengend color
  lengendcolor<- reactive({
    switch(input$chooseuser,
           "nday"= "yellow",
           "plurality" = "blue",
           "geometricalcenter" = "green",
           "gmedian"= "purple" )
    
  })

  


  #####################
  #sampling
  output$sampling <- renderLeaflet({
 
    output$nums<-renderText(
      {
      
        numm<-paste("please enter a number less than",nrow(selectuser()))
        numm
      
      }
    )
    
    
    
    
    maps<-leaflet("samplinig")%>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery , group = "Esri.WorldImagery") %>%
      addProviderTiles(providers$Hydda , group = "Hydda") %>%
      addLayersControl(
        baseGroups = c("Toner", "OpenStreetMap", "Esri.WorldImagery","Hydda"),
        options = layersControlOptions(collapsed =TRUE)
      )%>%
      addLegend("bottomright", 
                colors =c("yellow", "blue", "green", "purple"),
                labels= c("L-N","L-P","L-GC","L-GM"),
                title= "LAT",
                opacity = 10)
    observe({
      #qianti tiao jian 
      if(input$chooseuser!="none")
      {
        if(input$nums<nrow(selectuser()))
        {
          
          if(input$samplingstrategy=="sampling")
          {
            ssdfdf<<-selectuser()[sample(nrow(selectuser()),input$nums),]
     
            maps<-leafletProxy("sampling") %>%
              clearGroup("samplinggroup")%>%
              addCircles(ssdfdf[,2],ssdfdf[,3],radius = 50, color = lengendcolor(),label=ssdfdf[,1],group = "samplinggroup")
          }
          if(input$samplingstrategy=="grid")
          {
            #auto build a grid according input$nums
            for (i in ceiling(sqrt(input$nums)):1) {
              #browser()
            
              if(input$nums %% i==0 ){
                x<-i
                y<-input$nums/i
                break
              }
             
            }
           
            gridd<-expand.grid(x= seq(as.numeric(tweetsset$data[1]),as.numeric(tweetsset$data[3]),length.out = x), y = seq(as.numeric(tweetsset$data[2]), as.numeric(tweetsset$data[4]), length.out = y))
       
            dfgrid<-head(gridd,input$nums)
           
            #compute min distance
            r<-c()
            for(ii in 1:nrow(dfgrid))
            {
              c<-c()
              for (jj in 1:nrow(selectuser())) {
                dis<-(dfgrid[ii,1]-selectuser()[jj,2])^2+(dfgrid[ii,2]-selectuser()[jj,3])^2
                c<-c(c,dis)
              }
              r<-rbind(r, selectuser()[which.min(c),])
            }
            ssdfdf<<-as.data.frame(r,stringsAsFactors=FALSE)
            
            maps<-leafletProxy("sampling") %>%
              clearGroup("samplinggroup")%>%
              addCircles(ssdfdf[,2],ssdfdf[,3],radius = 50, color = lengendcolor(),label=ssdfdf[,1],group = "samplinggroup")
            
          }
        }
      }
      
    })
    
    maps
  })
  
  output$pheatmap<-renderPlot(
    {
      
      
      if(input$nums>0){
        a<-table(tweetsset$number$V1,tweetsset$number$V4)
        b<-melt(a)
        colnames(b)[1]<-"V1"
        c<-merge(b,ssdfdf,by="V1",all.y=T)
        d<-ggplot(c, aes(x =Var2, y =V1 ,fill=value)) +xlab('Place')+ylab("Name")+geom_tile()+theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                                                                                                           hjust = 1))
        d
        
      }
      
      
      
      
      
      
      
    }
  )
  
  
 
  output$psocialmeasure<-renderLeaflet(
    {
      observeEvent(input$nums,{
      c<-c()
      neighbor1<-c()
      neighbor2<-c()
      neinumber<-0
      spatialcentrality<<-c()
      for (ii in 1:nrow(ssdfdf)) {
        #if mention  or reply
        neighbor1<-subset(tweetsset$number,tweetsset$number[,4]==ssdfdf[ii,4])
        neighbor2<-subset(neighbor1,neighbor1[,1]!=ssdfdf[ii,1])
        neighbor3<-subset(selectrelation(),selectrelation()==ssdfdf[ii,1])
        
        set1<-neighbor2[,1]
  
        set2<-neighbor3[,2]
  
        jiao<-intersect(set1,set2)
   
        neinumber<-(length(jiao)+1)*30
        c<-c(c,neinumber)
        
      }
     
      spatialcentrality<<-as.data.frame(c)
      })
  
  
 
      mmap<-leaflet("psocialmeasure")%>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
        addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
        addProviderTiles(providers$Esri.WorldImagery , group = "Esri.WorldImagery") %>%
        addProviderTiles(providers$Hydda , group = "Hydda") %>%
        addLayersControl(
          baseGroups = c("Toner", "OpenStreetMap", "Esri.WorldImagery","Hydda"),
          options = layersControlOptions(collapsed =TRUE)
        )%>%
        addLegend("bottomright", 
                  colors =c("yellow", "blue", "green", "purple"),
                  labels= c("L-N","L-P","L-GC","L-GM"),
                  title= "LAT",
                  opacity = 10)
   
      observe({
  
        
        output$analysenumber<-renderText(
          {
            
            numm<-paste("You have chosen",input$nums,"locations as sample")
            numm
            
          }
        )
        
        
        
        
        
          if(input$centrality=="degree"){
            mmap<-leafletProxy("psocialmeasure")%>%
           clearGroup("socialgroup")%>%
           addCircles(data=ssdfdf,ssdfdf[,2],ssdfdf[,3],radius=as.numeric(as.character(spatialcentrality[,1])),label=ssdfdf[,1],group = "socialgroup",color=lengendcolor())
           lastcent<-as.numeric(as.character(spatialcentrality[,1]))/30
            tweetsset$centrality<<-cbind(ssdfdf[,1],ssdfdf[,2],ssdfdf[,3],lastcent)
          }
        
        
        
        
        
       
        
        if(input$centrality=="physicaldistance"){
          sumdistance<-0
          distanceset<-c()
          
          for (ii in 1:input$nums) {
            s1<-subset(selectrelation(),selectrelation()[,1]==ssdfdf[ii,1])
            print("s1")
            print(s1)
            n<-nrow(s1)
            lon<-ssdfdf[ii,2]
            lat<-ssdfdf[ii,3]
            set1<-s1[,2]
            print("set1")
            print(set1)
            dfset1<-as.data.frame(set1)
            colnames(dfset1)<-c("V1")
            infoset<-merge(dfset1,tweetsset$number,by="V1",all=FALSE)
            print("infoset")
            print(infoset)
            if(nrow(infoset)>0)
            {
              for (kk in 1:nrow(infoset)) {
                sumdistance<-sumdistance+(infoset[kk,2]-lon)^2+(infoset[kk,3]-lat)^2
           
              } 
             
            }
            print("sumdistance")
            print(sumdistance)
            distanceset<-c(distanceset,sumdistance/n)
          }
          dfdistancesett<<-as.data.frame(distanceset)
          
          
          # for (ii in 1:input$nums) {
          #   sumdistance<-0
          #   for (jj in 1:nrow(tweetsset$number)) {
          #    
          #       sumdistance<-sumdistance+(ssdfdf[ii,2]-tweetsset$number[jj,2])^2+(ssdfdf[ii,3]-tweetsset$number[jj,3])^2
          #     
          #    
          #    
          #   }
          #   sumdistance<-1/(sumdistance/nrow(tweetsset$number))*500
          #   
          #   distanceset<-c(distanceset,sumdistance)
          #   
          #   
          #   }
          
          dfdistanceset<-as.data.frame(distanceset,stringsAsFactors=FALSE)
          #print(isolate(dfdistanceset))
            mmap<-leafletProxy("psocialmeasure")%>%
            clearGroup("socialgroup")%>%
            addCircles(data=ssdfdf,ssdfdf[,2],ssdfdf[,3],radius=as.numeric(as.character(dfdistanceset[,1])),label=ssdfdf[,1],group = "socialgroup",color=lengendcolor())
         # print(isolate(dfdistanceset))
            lastdis<-as.numeric(as.character(dfdistanceset[,1]))/500
            tweetsset$centrality<<-cbind(ssdfdf[,1],ssdfdf[,2],ssdfdf[,3],lastdis)
        }
        
      })
      
      mmap
      
    }
  )
 
  


 
  output$usertrack1<-renderLeaflet(
    {
      
      
      
     
      linepoint1<-reactive(
        {
          if(input$nums>0)
            
          {
            pointline<-ssdfdf
          }
        
          pointline
            
        }
        
      )
      
     
      umap1<-leaflet("usertrack1")%>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
        addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
        addProviderTiles(providers$Esri.WorldImagery , group = "Esri.WorldImagery") %>%
        addProviderTiles(providers$Hydda , group = "Hydda") %>%
        addLegend("bottomright", 
                  colors =c("red","yellow", "blue", "green", "purple"),
                  labels= c("TSU",  "L-N","L-P","L-GC","L-GM"),
                  title= "LAT",
                  opacity = 10)%>%
       
      
        addLayersControl(
          baseGroups = c("Toner", "OpenStreetMap", "Esri.WorldImagery","Hydda"),
          
          
          options = layersControlOptions(collapsed =TRUE)
        )

      
      observe({
        
        
        output$comparenumber1<-renderText(
          {
            usernum<-paste(input$nums,"Locations posted",nrow(readyset),"tweets between",as.POSIXlt(tweetsset$begin+input$slider1[1]*3600, origin = "1970-01-01 00:00:00", tz = "UTC"),"and",as.POSIXlt(tweetsset$begin+input$slider1[2]*3600, origin = "1970-01-01 00:00:00", tz = "UTC"))
            usernum
          }
        )
        
        
        
       # uboundingbox1<-merge(ssdfdf,tweetsset$number,by="V1",all.x=T)
     #print("bounding")
     #print(uboundingbox1)
        timeset<-merge(tweetsset$number,ssdfdf,by="V1",all.y=T)
        timeset[,2]<-as.numeric(as.character(timeset[,2]))
        timeset[,3]<-as.numeric(as.character(timeset[,3]))
        timeset[,9]<-as.numeric(as.character(timeset[,9]))
        # print("timeset")
        # print(timeset)
        #print(begin+input$slider1[1]*3600)
        #print(begin+input$slider1[2]*3600)
        readyset<-subset(timeset,timeset[,9]>(tweetsset$begin+input$slider1[1]*3600))
        readyset<-subset(readyset,readyset[,9]<(tweetsset$begin+input$slider1[2]*3600))
        #readyset<-timeset[as.numeric(timeset[,4])>(begin+input$slider1[1]*3600) && as.numeric(timeset[,4])<(begin+input$slider1[2]*3600),]
        readyset[,2]<-as.numeric(as.character(readyset[,2]))
        readyset[,3]<-as.numeric(as.character(readyset[,3]))
        readyset[,9]<-as.numeric(as.character(readyset[,9]))
        readyset[,1]<-as.character(readyset[,1])
      
       
         # print("readyset")
         # print(readyset)
        if(nrow(readyset)>0)
        {
          split_data = lapply(unique(readyset$V1), function(x) {
            df = as.matrix(readyset[readyset$V1 == x, c("lon.x", "lat.x")])
            lns = Lines(Line(df), ID = x)
            return(lns)
          })
          
          data_lines<<- SpatialLines(split_data)
          
        }
        
        if(nrow(readyset)==0)
        {
          data_lines<<- SpatialPolygons(list())
        }
        umap2<-leafletProxy("usertrack1")%>%clearGroup("Tweets")
        if(nrow(readyset)>0 )
        {
          umap1<-leafletProxy("usertrack1")%>%clearGroup("Tweets")%>%
            clearGroup("Tweets")%>%
          addCircles(data=readyset,readyset[,2] ,readyset[,3], group = "Tweets",color ="red",radius=30,label=readyset[,1])

        }

      
         #print(uboundingbox)
        
        
           umap1<-leafletProxy("usertrack1",data=data_lines)%>%clearGroup(c("Movement","User Location","bounding_box1"))%>%
           addTiles()%>%addPolylines(group = "Movement",color = "red")%>%
          #addCircles(data=readyset,readyset[,2] ,readyset[,3], group = "Tweets",color ="red",radius=30,label=readyset[,1])%>%
          addCircles(data=linepoint1(),linepoint1()[,2] ,linepoint1()[,3], group = "User Location",color =lengendcolor(),radius=50,label=linepoint1()[,1])%>%
             addRectangles(
               lng1=timeset[,5], lat1=timeset[,6],
               lng2=timeset[,7], lat2=timeset[,8],
               fillColor = "transparent",group = "bounding_box1",label =timeset[,4])%>%
             addLayersControl(
               overlayGroups = c("Movement", "User Location","Tweets","bounding_box1"),
               baseGroups = c("Toner", "OpenStreetMap", "Esri.WorldImagery","Hydda"),
            
               options = layersControlOptions(collapsed =TRUE)
             )
           
            
           
       
        
        
              })
      
      
      
      
      umap1
      
    }
  )
  
  
  
  
  
  
  output$usertrack2<-renderLeaflet(
    {
     
      linepoint2<-reactive(
        {
          if(input$nums>0)
            
          {
            pointline<-ssdfdf
          }
          
          pointline
          
        }
      )
      
 

      
      umap2<-leaflet("usertrack2")%>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
        addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
        addProviderTiles(providers$Esri.WorldImagery , group = "Esri.WorldImagery") %>%
        addProviderTiles(providers$Hydda , group = "Hydda") %>%
        addLegend("bottomright", 
                  colors =c("red","yellow", "blue", "green", "purple"),
                  labels= c("SUT",  "L-N","L-P","L-GC","L-GM"),
                  title= "LAT",
                  opacity = 10)%>%
       
        addLayersControl(
          baseGroups = c("Toner", "OpenStreetMap", "Esri.WorldImagery","Hydda"),
         
          
          options = layersControlOptions(collapsed =TRUE)
        )
      
      

      observe({
        output$comparenumber2<-renderText(
          {
            
            usernum<-paste(paste(input$nums,"Locations posted",nrow(readyset),"tweets between",as.POSIXlt(tweetsset$begin+input$slider2[1]*3600, origin = "1970-01-01 00:00:00", tz = "UTC"),"and",as.POSIXlt(tweetsset$begin+input$slider2[2]*3600, origin = "1970-01-01 00:00:00", tz = "UTC")))
            usernum
          }
        )
        
        #uboundingbox2<-merge(ssdfdf,tweetsset$number,by="V1",all.x=T)
        #print(uboundingbox2)
          readyset<-c()
          timeset<-merge(tweetsset$number,ssdfdf,by="V1",all.y=T)
          timeset[,2]<-as.numeric(as.character(timeset[,2]))
          timeset[,3]<-as.numeric(as.character(timeset[,3]))
          timeset[,9]<-as.numeric(as.character(timeset[,9]))
          readyset<-subset(timeset,timeset[,9]>(tweetsset$begin+input$slider2[1]*3600))
          readyset<-subset(readyset,readyset[,9]<(tweetsset$begin+input$slider2[2]*3600))
          #readyset<-timeset[timeset[,4]>=values2$begin2 && timeset[,4]<=values2$end2,]
          readyset[,2]<-as.numeric(as.character(readyset[,2]))
          readyset[,3]<-as.numeric(as.character(readyset[,3]))
          readyset[,9]<-as.numeric(as.character(readyset[,9]))
          readyset[,1]<-as.character(readyset[,1])
          # print("readyset")
          # print(readyset)
          # turn into SpatialLines
          
          if(nrow(readyset)>0)
          {
            split_data = lapply(unique(readyset$V1), function(x) {
              df = as.matrix(readyset[readyset$V1 == x, c("lon.x", "lat.x")])
              lns = Lines(Line(df), ID = x)
              return(lns)
            })
            
            data_lines<- SpatialLines(split_data)
            
          }
          
          if(nrow(readyset)==0)
          {
            data_lines<- SpatialPolygons(list())
          }
          umap2<-leafletProxy("usertrack2")%>%clearGroup("Tweets2")
     if(nrow(readyset)>0)
          {
           umap2<-leafletProxy("usertrack2")%>%clearGroup("Tweets2")%>%
           addCircles(data=readyset,readyset[,2] ,readyset[,3], group = "Tweets2",color ="red",radius=30,label=readyset[,1])
            
          }
        umap2<-leafletProxy("usertrack2",data=data_lines)%>%clearGroup(c("Movement","User Location","bounding_box2"))%>%
            addTiles()%>%addPolylines(group ="Movement",color="black")%>%
         # addCircles(data=readyset,readyset[,2] ,readyset[,3], group = "Tweets",color ="red",radius=30,label=readyset[,1])%>%
            addCircles(data=linepoint2(),linepoint2()[,2] ,linepoint2()[,3], group = "User Location",color =lengendcolor(),radius=50,label=linepoint2()[,1])%>%
          addRectangles(
            lng1=timeset[,5], lat1=timeset[,6],
            lng2=timeset[,7], lat2=timeset[,8],
            fillColor = "transparent",group = "bounding_box2",label =timeset[,4])%>%
          addLayersControl(
              overlayGroups = c("Movement", "User Location","Tweets","bounding_box2"),
              baseGroups = c("Toner", "OpenStreetMap", "Esri.WorldImagery","Hydda"),
              options = layersControlOptions(collapsed =TRUE)
            )
          
 
      })
      
      
      
      
      umap2
      
      
      
      
    }
  )
  
  #usertime<<-get_timeline(ssdfdf[,1],n = 1)
  usertime<- reactive({
    pp<-get_timeline(ssdfdf[,1],n = 1)
    pp
  })
  
  
  output$pusertimeline<-renderLeaflet(
    {
      
      
     
      
      
      
      umap3<-leaflet("pusertimeline")%>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
        addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
        addProviderTiles(providers$Esri.WorldImagery , group = "Esri.WorldImagery") %>%
        addProviderTiles(providers$Hydda , group = "Hydda") %>%
        addLegend("bottomright", 
                  colors =c("black","yellow", "blue", "green", "purple"),
                  labels= c("Position-User Timeline",  "L-N","L-P","L-GC","L-GM"),
                  title= "LAT",
                  opacity = 10)%>%
        addCircles(data=ssdfdf,ssdfdf[,2],ssdfdf[,3],radius=50,label=ssdfdf[,1],group = "user location",color=lengendcolor())%>%
        addLayersControl(
          baseGroups = c("Toner", "OpenStreetMap", "Esri.WorldImagery","Hydda"),
          overlayGroups = c("user location"),
          options = layersControlOptions(collapsed =TRUE)
        )
      
      
   
     
      observe({
       
       userdf<-c()
      rr<<-c()
       for (ii in 1:nrow(usertime())) {
         c<-c()
         name<-c()
         place<-c()
         y<-c()
         k<-c()
         lon<-0
         lat<-0
         if(!is.na(usertime()[ii,41][[1]][[1]][1])==TRUE)
         {
           lon<-usertime()[ii,41][[1]][[1]][1]
           lat<-usertime()[ii,41][[1]][[1]][2]
           name<-usertime()[ii,4][[1]]
           place<-usertime()[ii,35][[1]]
           y<-cbind(name,lon,lat,place)
       
         }
         if(!is.na(usertime()[ii,42][[1]][[1]][1])==TRUE)
         {
           lon<-usertime()[ii,42][[1]][[1]][1]
           lat<-usertime()[ii,42][[1]][[1]][5]
           name<-usertime()[ii,4][[1]]
           place<-usertime()[ii,35][[1]]
           
         
           k<-cbind(name,lon,lat,place)
         
         }
         if(lon!=0)
         {
        c<-cbind(name,lon,lat,place)
        userdf<-rbind(c,userdf)
         }
        
         
       }
       rr<<-userdf
      
     umap3<-leafletProxy("pusertimeline")%>%
     clearGroup("user timeline")%>%
     addCircles(data=userdf,as.numeric(userdf[,2]),as.numeric(userdf[,3]),radius=1000,label=userdf[,1],group = "user timeline",color="black",stroke = FALSE, fillOpacity = 0.5)
      
     
     
     output$usertimenumber<-renderText(
       {
         usernum<-paste("There are",nrow(rr),"users have timeline position")
         usernum
       }
     )
     
     })
      
      umap3
      
    })
  output$areaselect<-renderLeaflet(
    {
      s<-character()
      
      
      umap4<-leaflet("areaselect")%>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
        addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
        addProviderTiles(providers$Esri.WorldImagery , group = "Esri.WorldImagery") %>%
        addProviderTiles(providers$Hydda , group = "Hydda") %>%
        addLayersControl(
          baseGroups = c("Toner", "OpenStreetMap", "Esri.WorldImagery","Hydda"),
          options = layersControlOptions(collapsed =TRUE)
        )%>%
        addLegend("bottomright", 
                  colors=c("green","red", "yellow", "blue", "orange","purple"),
                  labels= c("MOP", "MIP","IPM","OPM","IPS","OPS"),
                  title= "Status",
                  opacity = 10)
      observeEvent(input$confirm,{
        str<-strsplit(input$allinput,",")
        tweetsset$str<<-str[[1]]
        if(length(str[[1]])==3)
        {
         lon<-as.numeric(str[[1]][1])
         lat<-as.numeric(str[[1]][2])
         radius<-as.numeric(str[[1]][3])
         umap4<-leafletProxy("areaselect")%>%
           clearGroup("area")%>%
           addCircles(lng=lon,lat=lat,radius=radius,label="Circle",group = "area",color="red",fillColor = "transparent")
         #print(paste(lon,lat,radius))
         
        }
        if(length(str[[1]])==1)
        {
          uniqueboundingbox<-unique(tweetsset$number[,c(4,5,6,7,8)])
          print("uniqueboundingbox")
          print(uniqueboundingbox)
          k<-which(uniqueboundingbox[,1]==str[[1]][1])
          a<-uniqueboundingbox[k,2]
          b<-uniqueboundingbox[k,3]
          c<-uniqueboundingbox[k,4]
          d<-uniqueboundingbox[k,5]
          # print(uniqueboundingbox)
          # print(class(str[[1]][1]))
          # print(str[[1]][1])
          # print(k)
          # print(a)
          # print(b)
          # print(c)
          # print(d)

          umap4<-leafletProxy("areaselect")%>%
            clearGroup("area")%>%
            addRectangles(
              lng1=a, lat1=b,
              lng2=c, lat2=d,
              fillColor = "transparent"
              ,label="bounding_box",group = "area",color = "blue")
          #print("1")
        }
        if(length(str[[1]])==4)
        {
          x1<-as.numeric(str[[1]][1])
          y1<-as.numeric(str[[1]][2])
          x2<-as.numeric(str[[1]][3])
          y2<-as.numeric(str[[1]][4])
          umap4<-leafletProxy("areaselect")%>%
            clearGroup("area")%>%
            addRectangles(
              lng1=x1, lat1=y1,
              lng2=x2, lat2=y2,
              fillColor = "transparent"
            ,label="rectangle",group = "area")
          #print("4")
        } 
        if(length(str[[1]])==12)
        {
          numberstr<-as.numeric(str[[1]])
          #print(class(numberstr))
matrix1<-c(numberstr[1],numberstr[3],numberstr[5],numberstr[7],numberstr[9],numberstr[11],numberstr[2],numberstr[4],numberstr[6],numberstr[8],numberstr[10],numberstr[12])

          matrix2<-matrix(matrix1,ncol = 2)
      
          umap4<-leafletProxy("areaselect")%>%
            clearGroup("area")%>%
            addPolygons(data = matrix2,group = "area")
          print("12")
        }
      # print(str)
      # print(length(str[[1]]))
      # print(nrow(str))
      # print(class(str))
      #umap4<-leafletProxy("areaselect")%>%
      
      })
      
      
      
      
      observe({
        
        #Three places sent 10 messages between five and six o'clock
        output$areatweetsnumber<-renderText(
          {
            number<-paste(input$nums,"Locations posted",nrow(readyset),"tweets between",as.POSIXlt(tweetsset$begin+input$slider3[1]*3600+input$slider33[1]*60, origin = "1970-01-01 00:00:00", tz = "UTC"),"and",as.POSIXlt(tweetsset$begin+input$slider3[2]*3600+input$slider333[1]*60, origin = "1970-01-01 00:00:00", tz = "UTC"))
            number
          }
        )
        
   
        
        #uboundingbox2<-merge(ssdfdf,tweetsset$number,by="V1",all.x=T)
        #print(uboundingbox2)
        readyset<-c()
        timeset<-merge(tweetsset$number,ssdfdf,by="V1",all.y=T)
        timeset[,2]<-as.numeric(as.character(timeset[,2]))
        timeset[,3]<-as.numeric(as.character(timeset[,3]))
        timeset[,9]<-as.numeric(as.character(timeset[,9]))
        readyset<-subset(timeset,timeset[,9]>(tweetsset$begin+input$slider3[1]*3600+input$slider33[1]*60))
        readyset<-subset(readyset,readyset[,9]<(tweetsset$begin+input$slider3[2]*3600+input$slider333[1]*60))
        #readyset<-timeset[timeset[,4]>=values2$begin2 && timeset[,4]<=values2$end2,]
        readyset[,2]<-as.numeric(as.character(readyset[,2]))
        readyset[,3]<-as.numeric(as.character(readyset[,3]))
        readyset[,9]<-as.numeric(as.character(readyset[,9]))
        readyset[,1]<-as.character(readyset[,1])
        #print("readyset")
        #print(readyset)
        # turn into SpatialLines
        tweetsset$readyset<<-readyset
        if(nrow(readyset)>0)
        {
          split_data = lapply(unique(readyset$V1), function(x) {
            df = as.matrix(readyset[readyset$V1 == x, c("lon.x", "lat.x")])
            lns = Lines(Line(df), ID = x)
            return(lns)
          })
          
          data_lines<- SpatialLines(split_data)
          
        }
        
        if(nrow(readyset)==0)
        {
          data_lines<- SpatialPolygons(list())
        }
        umap4<-leafletProxy("areaselect")%>%clearGroup("Tweets4")
        if(nrow(readyset)>0)
        {
          umap4<-leafletProxy("areaselect")%>%clearGroup("Tweets4")%>%
            addCircles(data=readyset,readyset[,2] ,readyset[,3], group = "Tweets4",color ="red",radius=30,label=readyset[,1])
          
        }
        umap4<-leafletProxy("areaselect",data=data_lines)%>%clearGroup(c("Movement","User Location","bounding_box2"))%>%
          addTiles()%>%addPolylines(group ="Movement",color="red")%>%
          # addCircles(data=readyset,readyset[,2] ,readyset[,3], group = "Tweets",color ="red",radius=30,label=readyset[,1])%>%
          #addCircles(data=linepoint2(),linepoint2()[,2] ,linepoint2()[,3], group = "User Location",color =lengendcolor(),radius=50,label=linepoint2()[,1])%>%
          addLayersControl(
            overlayGroups = c("Movement", "Tweets4"),
            baseGroups = c("Toner", "OpenStreetMap", "Esri.WorldImagery","Hydda"),
            options = layersControlOptions(collapsed =TRUE)
          )
        
        
      })
      
      
      
      
      
      observeEvent(input$confirm2,{
        if(nrow(tweetsset$readyset)>1)
        {
        ###function
        Triangle <-function(x1,y1,x2,y2,x3,y3)
        {
          a<-sqrt((x1-x2)^2+(y1-y2)^2)
          b<-sqrt((x2-x3)^2+(y2-y3)^2)
          c<-sqrt((x3-x1)^2+(y3-y1)^2)
          s<-(a+b+c)/2
          area<-sqrt(s*(s-a)*(s-b)*(s-c))
          return(area)
        }
        hexagon<-function(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6)
        {
          a<-Triangle(x1,y1,x2,y2,x6,y6)
          b<-Triangle(x2,y2,x5,y5,x6,y6)
          c<-Triangle(x2,y2,x4,y4,x5,y5)
          d<-Triangle(x2,y2,x3,y3,x4,y4)
          area<-a+b+c+d
          return(area)
          
        }
        inout<-function(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6,lon,lat)
        {
          a<-Triangle (x1,y1,x2,y2,lon,lat)
          b<-Triangle (x2,y2,x3,y3,lon,lat)
          c<-Triangle (x3,y3,x4,y4,lon,lat)
          d<-Triangle (x4,y4,x5,y5,lon,lat)
          e<-Triangle (x5,y5,x6,y6,lon,lat)
          f<-Triangle (x6,y6,x1,y1,lon,lat)
          san<-a+b+c+d+e+f
          liu<-hexagon(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6)
          if(abs(san-liu)>0.001)
          {
            return(FALSE)
            
          }
          if(abs(san-liu)<=0.001)
          {
            return(TRUE)
          }
          
        }
        
        circle<-function(lon,lat,radius,x,y)
        {
          dis<-sqrt((x-lon)^2+(y-lat)^2)
          if(dis>radius)
          {
            return(FALSE)
          }
          if(dis<=radius)
          {
            return(TRUE)
          }
          
        }
        earthDist <- function (lon1, lat1,radius,lon2, lat2){
          rad <- pi/180
          a1 <- lat1 * rad
          a2 <- lon1 * rad
          b1 <- lat2 * rad
          b2 <- lon2 * rad
          dlon <- b2 - a2
          dlat <- b1 - a1
          a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
          c <- 2 * atan2(sqrt(a), sqrt(1 - a))
          R <- 6378.145
          d <- R * c
          dm<-d*1000
          if(dm>radius)
          {
            return(FALSE)
          }
          if(dm<=radius)
          {
            return(TRUE)
          }
          
          return(d)
        }
        rectangle<-function(x1,y1,x2,y2,x,y)
        {
          if(x>=x1 && x<=x2 && y<=y1 && y>=y2)
          {
            return(TRUE)
          }
          else {
            return(FALSE)
          }
          
        }
      
        if(input$areatype=="circle")
        {
         lon<-as.numeric(tweetsset$str[1])
         lat<-as.numeric(tweetsset$str[2])
         radius<-as.numeric(tweetsset$str[3])
          k<-tweetsset$readyset
          k$v9<-"unknow"
          for (ii in 1:nrow(k)) {
            
            k$v9[ii]<-circle(lon,lat,radius,k[ii,2],k[ii,3])
            #print(circle(lon,lat,radius,k[ii,2],k[ii,3]))
          }
          
          tweetsset$export<<-k
          #print(k)
          
        }
        if(input$areatype=="boundingbox")
        {
          k<-tweetsset$readyset
          k$v9<-"unknow"
          for(ii in 1:nrow(k))
          {
            if(k[ii,4]==tweetsset$str[1])
            {
              k$v9[ii]<-TRUE
            }
            if(k[ii,4]!=tweetsset$str[1])
            {
              k$v9[ii]<-FALSE
            }
            
            
          }
          tweetsset$export<<-k
          #print(k)
        }
        if(input$areatype=="hexagon")
        {
          k<-tweetsset$readyset
          k$v9<-"unknow"
          x1<-as.numeric(tweetsset$str[1])
          y1<-as.numeric(tweetsset$str[2])
          x2<-as.numeric(tweetsset$str[3])
          y2<-as.numeric(tweetsset$str[4])
          x3<-as.numeric(tweetsset$str[5])
          y3<-as.numeric(tweetsset$str[6])
          x4<-as.numeric(tweetsset$str[7])
          y4<-as.numeric(tweetsset$str[8])
          x5<-as.numeric(tweetsset$str[9])
          y5<-as.numeric(tweetsset$str[10])
          x6<-as.numeric(tweetsset$str[11])
          y6<-as.numeric(tweetsset$str[12])
          
          for (ii in 1:nrow(k)) {
            
            k$v9[ii]<-inout(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6,k[ii,2],k[ii,3])
            
          }
          tweetsset$export<<-k
          #print(k)
          
        }
        if(input$areatype=="rectangle")
        {
          k<-tweetsset$readyset
          k$v9<-"unknow"
          
          x1<-as.numeric(tweetsset$str[1])
          y1<-as.numeric(tweetsset$str[2])
          x2<-as.numeric(tweetsset$str[3])
          x2<-as.numeric(tweetsset$str[4])
          for (ii in 1:nrow(k)) {
            
            k$v9[ii]<-rectangle(x1,y1,x2,y2,k[ii,2],k[ii,3])
            
          }
          tweetsset$export<<-k
          #print(k)
          
          
        }
        
        #tweetsset$export$v10<-"unknow"
        #print(tweetsset$export)
        r<-c()
        rr<-c()
    for(jj in 1:nrow(tweetsset$export))   
    {
      c<-c()
      cc<-c()
         set<-subset(tweetsset$export,tweetsset$export[,1]==tweetsset$export[jj,1])
         print("set")
         print(set)
         if(nrow(set)==1)
         {
           cc<-c(set[1,1],set[1,4],set[1,4])
           if(set[1,14]=="TRUE")
           {
             status<-"in stayer"
             
           }
           if(set[1,14]=="FALSE")
           {
             status<-"out stayer"
           }
         }
         if(nrow(set)>1)
         {
           n<-nrow(set)
           cc<-c(set[1,1],set[1,4],set[n,4])
           
           if(set[1,14]=="TRUE" && set[n,14]=="FALSE")
           {
             status<-"move out"
           }
           if(set[1,14]=="FALSE" && set[n,14]=="TRUE")
           {
             status<-"move in"
           }
           if(set[1,14]=="TRUE" && set[n,14]=="TRUE")
           {
             status<-"in move"
           }
           if(set[1,14]=="FALSE" && set[n,14]=="FALSE")
           {
             status<-"out move"
           }
           
         }
         
         c<-c(unique(set[,1]),status)
         r<-rbind(c,r)
         rr<-rbind(rr,cc)
         
 
    }
        
        
      
      dfr<-as.data.frame(r)
      # print(dfr)
      # print(tweetsset$readyset)
      #timeset<-merge(tweetsset$number,ssdfdf,by="V1",all.y=T)
      change<-merge(dfr,tweetsset$readyset,by="V1",all.x=T)
      tweetsset$change<<-merge(dfr,tweetsset$readyset,by="V1",all.x=T)
      print("dfr")
      print(dfr)
      print("readyset")
      print(tweetsset$readyset)
      print(change)
      change[,2]<-as.character(change[,2])
      for(ii in 1:nrow(change))
      {
        if(change[ii,2]=="move out")
        {
          change[ii,2]<-"green"
          next()
        }
        if(change[ii,2]=="move in")
        {
          change[ii,2]<-"red"
          next()
        }
        if(change[ii,2]=="in move")
        {
          change[ii,2]<-"yellow"
          next()
        }
        if(change[ii,2]=="out move")
        {
          change[ii,2]<-"blue"
          next()
        }
        if(change[ii,2]=="in stayer")
        {
          change[ii,2]<-"orange"
          next()
        }
        if(change[ii,2]=="out stayer")
        {
          change[ii,2]<-"purple"
          next()
        }
     
        
      }
      print("after change")
      print(change)
      afterchange<-(unique(change[,c(1,2,3,4)]))
      umap4<-leafletProxy("areaselect")%>%clearGroup("Tweets4")%>%
        addCircles(data=afterchange,as.numeric(as.character(afterchange[,3])) ,as.numeric(as.character(afterchange[,4])), group = "Tweets4",color =afterchange[,2],radius=30,label=as.character(afterchange[,1]))
     

      tweetsset$out<<-unique(r[,c(1,2)])

      
       export2<-as.data.frame(tweetsset$out)
       print("tweetsset$out")
       print(export2)
        dfrr<-as.data.frame(rr)
        dfrr<-unique(dfrr[,c(1,2,3)])
        colnames(rr)<-c("V1","V2","V3")
        print(rr)
        print("export")
        export<-merge(dfrr,tweetsset$out,by="V1",all=T)
        tweetsset$centrality<-as.data.frame(tweetsset$centrality)
        colnames(tweetsset$centrality)<-c("V1","lon","lat","centrality")
        exportt<-merge(export,tweetsset$centrality,by="V1",all=T)
        colnames(exportt)<-c("screen_name","From","To","Status","Location lon","Location lat","Spatial Centrality")
        
        print(exportt)
        exporttt<-merge(exportt,information,by="screen_name",all.x=T)
        colnames(mentiontable)<-c("screen_name","mention degree in","mention degree out","mention betweeness")
        colnames(replytable)<-c("screen_name","reply_degree in","reply_degree out","reply_betweeness")
        exportttt<-merge(exporttt,mentiontable,by="screen_name",all.x=T)
        exporttttt<-merge(exportttt,replytable,by="screen_name",all.x=T)
      write.csv(exporttttt, file = "MyData.csv",row.names=FALSE)
        
        }
        
      })
      
      
      
      
      
      
 
      umap4
    
 
})
  output$notationtext<-renderText({
    "1.If you don't want to use the 'Collect Data' fu nction to pull data,you have to set the 'rectangle area',otherwise the 'Grid 
     Sampling' function will not work.
     2.Each step is based on last step,please use the application step by step."
 
    })
  
  
  output$notationtable<-renderTable({
    nmatrix<-matrix(nrow =12, ncol = 2)
    nmatrix[1,1]<-"abbreviation"  
    nmatrix[1,2]<-"full form"
    colnames(nmatrix)<-c("ABBREVIATION","FULL FORM")
    nmatrix[1,1]<-"L-N"
    nmatrix[1,2]<-"Location-Nday"
    nmatrix[2,1]<-"L-P"
    nmatrix[2,2]<-"Location-Plurality"
    nmatrix[3,1]<-"L-GC"
    nmatrix[3,2]<-"Location-Geometrical center"
    nmatrix[4,1]<-"L-GM"
    nmatrix[4,2]<-"Location-GemetricMedian"
    nmatrix[5,1]<-"TSU"
    nmatrix[5,2]<-"Tweet of Sampling User"
    nmatrix[6,1]<-"LAT"
    nmatrix[6,2]<-"Location Assessment Techniques"
    nmatrix[7,1]<-"MOP"
    nmatrix[7,2]<-"Move Out of Polygon"
    nmatrix[8,1]<-"MIP"
    nmatrix[8,2]<-"Move Into the Polygon"
    nmatrix[9,1]<-"IPM"
    nmatrix[9,2]<-"In the Polygon make Movement"
    nmatrix[10,1]<-"OPM"
    nmatrix[10,2]<-"Out of the Polygon make Movement"
    nmatrix[11,1]<-"IPS"
    nmatrix[11,2]<-"In the Polygon stay"
    nmatrix[12,1]<-"OPS"
    nmatrix[12,2]<-"Out of the Polygon stay"
    
    dfnmatrix<-as.data.frame(nmatrix)
    
    dfnmatrix
  
    
    

    })

  

}
shinyApp(ui,server)
