library(shiny)
library(leaflet)
library(dplyr)
library(rvest)
library(tidyverse)
library(shinyvalidate)
library(geosphere)
library(stringr)
library(shinythemes)
library(shinycssloaders)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(DT)
library(formattable)

setwd("C:/studies/Y3S1/DBA3702/Project/Submission") #change accordingly 

property = read.csv("US Property data Cleaned with ROI.csv")
state.vector = unique(property$State)


property = property %>% select(-X.1)%>%rename(id=X, latitude=lat, longitude=lon) %>%
  filter(Address != 'Address Not Disclosed')
repeats = data.frame(table(property$Full.Address)) %>% filter(Freq > 1)

for (i in 1:length(repeats$Var1)){
  address = repeats$Var1[i]
  rpt_addresses = property %>% filter(Full.Address == address)
  n_rpts = nrow(rpt_addresses)
  identifiers = paste0('(', 1:n_rpts, ')')
  ids = rpt_addresses$id
  property$Address[!is.na(match(property$id, ids))] = paste(rpt_addresses$Address, identifiers)
}

property$Address_Nice = paste(paste(property$Address, property$Cities,property$State, 
                          sep = ', '), property$Postal.Code)
prop.vector = property$Address_Nice

airbnb_df=read.csv("combined_airbnb.csv")
airbnb_df$price = as.numeric(gsub('[\\$,]', '', airbnb_df$price))

prop_id=function(Name){ ##get id of property by name
  id=property%>%filter(Address_Nice==Name)%>%pull(id)
  return(id)
}







ui <- fluidPage(
  navbarPage("watrbnb",theme = shinytheme("yeti"),
             tabPanel("App Guide", fluid = TRUE, icon = icon("fa-regular fa-circle-question"),
                      htmlOutput("Appintro")
                     ),
             
             #for investors 
             tabPanel("Investor view", fluid = TRUE, icon = icon("fa-light fa-money-bill-trend-up"),
                      tabsetPanel(
                        
                        #choosing properties
                        tabPanel("Property Selector",
                                 sidebarLayout(
                                   sidebarPanel(
                                     titlePanel("Property Filters"),
                                     
                                     radioButtons(
                                       inputId = "state",
                                       label = "State",
                                       choices = state.vector,
                                       selected =state.vector[1],
                                       inline = T
                                     ),
                                     sliderInput(inputId = "Price.Range",
                                                 label ='Investment Capital',
                                                 min = min(property$sale_price, na.rm=T),
                                                 max = max(property$sale_price, na.rm=T),
                                                 value = c(min(property$sale_price, na.rm=T),
                                                           max(property$sale_price, na.rm=T)),
                                                 dragRange = T),
                                     numericInput(inputId = "sqft",
                                                  label = "Minimum Size in Sqft:",
                                                  value = min(property$sqft, na.rm=T),
                                                  min = min(property$sqft, na.rm=T),
                                                  max = max(property$sqft, na.rm=T)),
                                     numericInput(inputId = "bathroom",
                                                  label = "Minimum Number of bathrooms:",
                                                  value = min(property$bath, na.rm=T),
                                                  min =  min(property$bath, na.rm=T),
                                                  max = max(property$bath, na.rm=T)),
                                     numericInput(inputId = "bedroom",
                                                  label = "Minimum Number of bedrooms:",
                                                  value = min(property$bed, na.rm=T),
                                                  min = min(property$bed, na.rm=T),
                                                  max = max(property$bed, na.rm=T)),
                                     selectizeInput(inputId = "Property",
                                                    label = "Name of Property chosen",
                                                    choices = NULL,
                                                    options=list(onInitialize = I('function() { this.setValue(""); }'),
                                                                 maxItems=1)) #to link with clicking mechanic
                                   ),
                                   mainPanel(
                                     tags$style(type="text/css",
                                                ".shiny-output-error { visibility: hidden; }",
                                                ".shiny-output-error:before { visibility: hidden; }"), #removes warnings/errors
                                     htmlOutput("move"), withSpinner(leafletOutput("plotPropertyMap")),DT::DTOutput(outputId = "dt_table", width = "100%")
                                   )
                                 )
                        ), #close property selector tab
                        
                        #airbnb analysis 
                        tabPanel("Airbnb Map view",
                                 sidebarLayout(
                                   sidebarPanel('Radius around property to view',
                                                sliderInput(inputId = "radius.inv",
                                                            label ='Search Radius',
                                                            min = 100,
                                                            max = 10000,
                                                            value = 1000)),
                                   mainPanel(
                                     tags$style(type="text/css",
                                                ".shiny-output-error { visibility: hidden; }",
                                                ".shiny-output-error:before { visibility: hidden; }"), #removes warnings/errors
                                     withSpinner(leafletOutput("plotBnbMap")), plotOutput("plotBox1")) 
                                 )
                        )#closes airbnb map tab
                        
                      )
             ), #closes investor view tab
             #for landlords 
             tabPanel("Landlord view", fluid = TRUE, icon = icon( "airbnb"),
                      sidebarLayout(
                        sidebarPanel("Search your property",
                                     sliderInput(inputId = "radius.ll",
                                                 label ='Radius around property to view',
                                                 min = 100,
                                                 max = 10000,
                                                 value = 1000),
                                     selectizeInput(inputId = "ll.id",
                                                    label = "Input your Airbnb's ID",
                                                    choices = NULL,
                                                    options=list(
                                                      onInitialize = I('function() { this.setValue(""); }'),
                                                                 maxItems=1),
                                                    )
                                     
                        ),
                        mainPanel(
                          tags$style(type="text/css",
                                     ".shiny-output-error { visibility: hidden; }",
                                     ".shiny-output-error:before { visibility: hidden; }"), #removes warnings/errors
                          withSpinner(leafletOutput("plotLandlordMap")), DT::DTOutput(outputId = "ll_table", width = "100%"),plotOutput("plotBox2"))
                      )
             ), #closes landlord view tab
             tabPanel("Notifications Signup", fluid = TRUE, icon = icon("fa-regular fa-envelope"),
                      "Please fill in your details below if you would like to receive notifications from us:",
                      radioButtons(
                        "identity",
                        label="What is your purpose for using this app:",
                        choices = c("I am an Investor","I am an existing Airbnb landlord")),
                      textInput("noti","Enter your email:"),actionButton("button","Submit", class = "btn-success")
             )#closes notification tab
  )
)



#Define server logic
server <- function(input,output, session){
  updateSelectizeInput(session, 'Property', choices = c(Choose='', prop.vector), server = TRUE)
  updateSelectizeInput(session, 'll.id', choices = airbnb_df$id, server = TRUE,selected=6413)

  
  ## app guide 
  output$Appintro = renderText("Welcome to watrbnb! This page contains useful information on how to use the app<br/><br/>
  <b>For Investors</b>, by accessing the investor view tab, you will be able to look at the information of various properties that is displayed on the map.<br/>
                              By using the filters, you can limit the amount of properties you wish to see, and the map will display the properties inside the table, where the intial view is ordered by descending ROI.<br/>
                              Once you have selected a property you are interested in, you can either click the marker on the map or the table to select your property, by deleting the Name of Property chosen you will be able to return to the previous view.<br/>
                              When a property is selected you will be prompted to go into the Airbnb Map view, this allows you to look at the details of the neighbouring Airbnb, which you may indicate based on a radius value.<br/>
                              Additionally a box plot will also be shown, comparing the Price per night per person of the neighbouring rival Airbnb grouped by the number of bedrooms in the Airbnb<br/><br/>
  <b>For current Airbnb landlords</b> you can navigate to the For Landlord tab where you can input your Airbnb's ID to view the competitor analysis of your Airbnb.<br/>
                              The map will allow you to look at the details of the neighbouring Airbnb, which you may indicate based on a radius value.<br/>
                              Additonally a box plot will also be shown, comparing the Price per night per person of the neighbouring rival Airbnb grouped by the number of bedrooms in the Airbnb.<br/> <br/>")
## Property selector
  property.filter<- reactive({
    if(isTruthy(input$Property)){
      frame = property%>%filter(Address_Nice==input$Property) #problem
      full_add = frame$Full.Address
      property %>% filter(Full.Address == full_add) 
    }
    else if(isTruthy(input$dt_table_rows_selected)){
     
      filtered.data <- property %>% filter(State == input$state) %>%
        filter(sale_price >= input$Price.Range[1] & sale_price<=input$Price.Range[2]) %>%
        filter(sqft>=input$sqft)%>%
        filter(bath >= input$bathroom) %>%
        filter(bed >= input$bedroom) %>% arrange(Address_Nice)
      prop_slctd = filtered.data %>% slice(input$dt_table_rows_selected)
      updateSelectizeInput(session, "Property", selected=prop_slctd$Address_Nice)
      prop_slctd
    }

    
    else{
      filtered.data <- property %>% filter(State == input$state) %>%
        filter(sale_price >= input$Price.Range[1] & sale_price<=input$Price.Range[2]) %>%
        filter(sqft>=input$sqft)%>%
        filter(bath >= input$bathroom) %>%
        filter(bed >= input$bedroom) %>% arrange(Address_Nice)
      updateSelectizeInput(session, "Property", choices=c(Choose='', filtered.data$Address_Nice))
      filtered.data
      }
  }) #filtered data

  output$plotPropertyMap <- renderLeaflet({
    if(isTruthy(property.filter())){
      prop.labels=reactive({sprintf("<strong>%s</strong><br/>Price: $%s<br/>ROI: %s<br/>Size in sqft: %s sqft<br/>Price per sqft: $%s/sqft<br/>Number of bathrooms: %s<br/>Number of bedrooms: %s",
                                    property.filter()[input$dt_table_rows_current,]$Address_Nice,
                                    format(round(property.filter()[input$dt_table_rows_current,]$sale_price,2), scientific=FALSE,big.mark = ",", nsmall=2L),
                                    percent(property.filter()[input$dt_table_rows_current,]$ROI),
                                    property.filter()[input$dt_table_rows_current,]$sqft,
                                    property.filter()[input$dt_table_rows_current,]$psf,property.filter()[input$dt_table_rows_current,]$bath,property.filter()[input$dt_table_rows_current,]$bed) %>%lapply(htmltools::HTML)})#labels on leaflet
      leaflet(property.filter()[input$dt_table_rows_current,]) %>% addTiles() %>% addMarkers(lng = ~longitude, lat = ~latitude,label=prop.labels())
    }
    else{
      leaflet()%>%addTiles()
    }
  })

  
  map_click_event <- observeEvent(input$plotPropertyMap_marker_click, {
    lat = input$plotPropertyMap_marker_click$lat
    lng = input$plotPropertyMap_marker_click$lng
    prop_clicked = property %>% filter(latitude==lat & longitude==lng)
    
    if (nrow(prop_clicked) == 1){
      updateSelectizeInput(session, 'Property', selected=as.character(prop_clicked$Address_Nice))
    }
    else if (nrow(prop_clicked) > 1){
      updateSelectizeInput(session, 'Property', selected=as.character(prop_clicked$Address_Nice[1]))
    }
  })
  
  #### Dynamic table for property selector
  
  #create the dynamic table 
  rv <- shiny::reactiveValues(
    df = reactive({property.filter()%>%select(id,Address_Nice,sale_price,ROI)%>%rename("ID"=id,"Address"=Address_Nice, "Sales price"=sale_price)}),
    dt_row = NULL,
    add_or_edit = NULL,
    edit_button = NULL,
    keep_track_id = reactive({nrow(property.filter()) + 1})
  )
  #render table 
  output$dt_table <- DT::renderDT(
    
    {datatable(rv$df(),selection = 'single',
               options = list(order=list(4,"desc")),)%>%
        formatPercentage(c("ROI"),2)} #ordering and changing to D
  )

  #generate a text to move to airbnb map
  output$move=renderText(
    if(isTruthy(input$Property)){
      sprintf('<font color="%s">%s</font>',"red","Property Selected, move to Airbnb Map View for Airbnb competitor analysis") 
      }
  )
  
  #Airbnb Analysis 
  output$plotBnbMap<- renderLeaflet({
    if(isTruthy(input$Property)){ #checking if there is any input in chosen property 
      
      select.prop=property%>%filter(id==prop_id(input$Property)) # data of selected property 
      
      #creating distance col in airbnb data 
      prop.points=c(select.prop$longitude,select.prop$latitude)
      airbnb_df_state = airbnb_df %>% filter(state_code==select.prop$State)
      
      mat=cbind(airbnb_df_state$longitude,airbnb_df_state$latitude)
      airbnb_df_zoom=cbind(airbnb_df_state,data.frame("Distance.prop"=distm(mat,prop.points)))
      
      distance =input$radius.inv
      airbnb_plot= airbnb_df_zoom%>%filter(Distance.prop<distance) %>% mutate(Price.per.person=price/accommodates)
     
      
      #creating labels and pop ups 
      Bnb.labels=sprintf("<strong>ID: %s, Name: %s</strong><br/>Price/night/pax: $%s<br/>Accommodates: %s<br/>Number of Bedrooms: %s<br/>Number of beds: %s",
                         airbnb_plot$id, airbnb_plot$name,
                         format(round(airbnb_plot$Price.per.person,2), scientific=FALSE,big.mark = ",", nsmall=2L),
                         airbnb_plot$accommodates,airbnb_plot$bedrooms,
                         airbnb_plot$beds) %>% lapply(htmltools::HTML)
      
      Bnb.links <- paste("<b>",paste0("<a href=",airbnb_plot$listing_url,">",airbnb_plot$name,"</a></b>"),sep = "")
      
      selected.prop.label=sprintf("<h3><b>Property for Investment</b></h3><br/><strong>%s</strong><br/>Price: $%s<br/>Size in sqft: %s sqft<br/>Price per sqft: $%s/sqft<br/>Number of bathrooms: %s<br/>Number of bedrooms: %s",
                                            select.prop$Address_Nice,
                                            format(round(select.prop$sale_price,2), scientific=FALSE,big.mark = ",", nsmall=2L),
                                            select.prop$sqft,
                                            select.prop$psf,select.prop$bath,select.prop$bed) %>%lapply(htmltools::HTML)
      if(nrow(airbnb_plot)!=0){
        map.airbnb=leaflet(airbnb_plot) %>% addTiles() %>% 
            addMarkers(lng = ~longitude, lat = ~latitude,label = Bnb.labels,popup =Bnb.links) %>%
            addAwesomeMarkers(icon = awesomeIcons(markerColor = "red"),lng=select.prop$longitude,lat=select.prop$latitude,label =selected.prop.label)
        output$plotBox1<- renderPlot({
          ggplot(airbnb_plot,aes(y=Price.per.person,x=as.factor(bedrooms)))+geom_boxplot()+labs(title = "Box plot of Price of Airbnbs per night per person grouped by the number of bedrooms",y="Price/night/person",x="Number of bedrooms ")
        })
      }
    
      
      else{map.airbnb=leaflet()%>%addTiles()}
    }
    else{map.airbnb=leaflet()%>%addTiles()}
    map.airbnb
    })
 
  

  
  ###Landlord Section
  
  ll.property.filter<- reactive({airbnb_df%>%filter(id==input$ll.id)})#filtered data
  ll.prop.labels=reactive({sprintf("<strong>%s</strong><br/>Price/night: $%s<br/>Accommodates: %s pax<br/>Bedrooms: %s<br/>Beds: %s",
                                paste0('ID: ', ll.property.filter()$id, ', Name: ', ll.property.filter()$name),
                                format(round(ll.property.filter()$price,2), scientific=FALSE,big.mark = ",", nsmall=2L),
                                ll.property.filter()$accommodates,ll.property.filter()$bedrooms,
                                ll.property.filter()$beds) %>%lapply(htmltools::HTML)}) #labels on leaflet

  
  llrv <- shiny::reactiveValues(
    df = reactive({ll.property.filter()%>%
        mutate(Price.per.person=round(price/accommodates,2))%>%
        select(id,location,state_code,name,price,accommodates,Price.per.person)%>%
        rename("ID"=id,"City"=location,"State"=state_code,"Price/Night"=price,"Number of people accomodates"=accommodates,"Price/Night/Person"=Price.per.person)}),
    dt_row = NULL,
    add_or_edit = NULL,
    edit_button = NULL,
    keep_track_id = reactive({nrow(ll.property.filter()) + 1})
  )
  
  #render table 
  output$ll_table <- DT::renderDT(
    {llrv$df()},
    escape = F,
    rownames = FALSE,
    options = list(processing = FALSE)
  )
  
  output$plotLandlordMap<- renderLeaflet({
    map_three <- leaflet(ll.property.filter()) %>% addTiles() %>% addMarkers(lng = ~longitude, lat = ~latitude,label=ll.prop.labels())
    if(isTruthy(input$ll.id)){ #checking if there is any input in chosen property 
      
      ll.select.prop=airbnb_df%>%filter(id==input$ll.id) # data of selected property 
      
      #creating distance col in airbnb data
      ll.airbnb_df_location = airbnb_df %>% filter(location==ll.select.prop$location)
      
      ll.prop.points=c(ll.select.prop$longitude,ll.select.prop$latitude)
      
      ll.mat = cbind(ll.airbnb_df_location$longitude,ll.airbnb_df_location$latitude)
      
      ll.airbnb_df_zoom=cbind(ll.airbnb_df_location,data.frame("Distance.prop"=distm(ll.mat,ll.prop.points)))
      ll.distance =input$radius.ll
      ll.airbnb_plot=ll.airbnb_df_zoom%>%filter(Distance.prop<ll.distance)%>%mutate(Price.per.person=price/accommodates)
      
      
      #creating labels and pop ups 
      ll.Bnb.labels=sprintf("<strong>ID: %s, Name: %s</strong><br/>Price/night/pax: $%s<br/>Accommodates: %s pax<br/>Number of Bedrooms: %s<br/>Number of beds: %s",
                                      ll.airbnb_plot$id, ll.airbnb_plot$name,
                                      format(round(ll.airbnb_plot$Price.per.person,2), scientific=FALSE,big.mark = ",", nsmall=2L),
                                      ll.airbnb_plot$accommodates,
                                      ll.airbnb_plot$bedrooms,ll.airbnb_plot$beds) %>% lapply(htmltools::HTML)
      ll.Bnb.links <- paste("<b>",paste0("<a href=",ll.airbnb_plot$listing_url,">",ll.airbnb_plot$name,"</a></b>"),sep = "")

      ll.selected.prop.label=sprintf("<h3><b>Your Property</b></h3><br/><strong>ID: %s, Name: %s</strong><br/>Price/night/pax: $%s<br/>Accommodates: %s sqft<br/>Number of Bedrooms: $%s/sqft<br/>Number of beds: %s",
                                               ll.select.prop$id, ll.select.prop$name,
                                               format(round(ll.select.prop$price/ll.select.prop$accommodates,2), scientific=FALSE,big.mark = ",", nsmall=2L),
                                               ll.select.prop$accommodates,ll.select.prop$bedrooms,
                                               ll.select.prop$beds) %>% lapply(htmltools::HTML)
      if(isTruthy(ll.airbnb_plot)){
        map_three<-leaflet(ll.airbnb_plot) %>% addTiles() %>% 
            addMarkers(lng = ~longitude, lat = ~latitude,label = ll.Bnb.labels,popup =ll.Bnb.links) %>%
            addAwesomeMarkers(icon = awesomeIcons(markerColor = "red"),lng=ll.select.prop$longitude,lat=ll.select.prop$latitude,label =ll.selected.prop.label)
        output$plotBox2<- renderPlot({
          ggplot(ll.airbnb_plot,aes(y=Price.per.person,x=as.factor(bedrooms)))+geom_boxplot()+labs(title="Box plot of Price of Airbnbs per night per person grouped by the number of bedrooms",y="Price/night/person",x="Number of bedrooms")
        })
      }
    }
    map_three
  })
  
  ##function of action button on notifications page
  observeEvent(input$button, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'T')
    showModal(modalDialog(
      title = "Your response have been recorded",
      "Thank you for subscribing to our notification list",
      easyClose = TRUE,
      footer = NULL
    ))
    updateTextInput(session,"noti",value=NA)
  })
  
}# close server


shinyApp(ui=ui, server=server)
