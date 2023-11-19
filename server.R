###### CIRIGHTS Analytics ######

library(shiny)
library(ggplot2)
library(validate)
library(plotly)
library(ggiraph)
library(RColorBrewer)
library(viridis)
library(stringr)


hrdata <- read.csv("./Data//cirights_v2_rolling_avg.csv")
mapdata<-read.csv("./Data/mapdata_nona_v2.csv")

function(input, output, session) {
  
  ############################# World Map #################################
  output$world_map<-renderPlotly({
  
  #subsetting data according to user input
  maptemp<-subset(mapdata, mapdata$year==input$yearworld)
    
    
  #creating a world map
  low <- min(maptemp$hrscorex2)
  high <- max(maptemp$hrscorex2)
    map_animated<-maptemp %>%
      ggplot(aes(long, lat, group=group, fill=hrscorex2,text=str_glue("{country}:{hrscorex2}")))+
      geom_polygon(color="white",linewidth=.01)+
      scale_fill_gradient(" ",low = "#000000",  high = "#0C7BDC",
                            breaks = c(low, high), 
                            labels = c("Low Respect", "High Respect"))+
      ggtitle(paste("World Human Rights Index", "\n", "(", paste(input$yearworld, ")")) )  +
      theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        plot.caption = element_text(size = 8, hjust = 1),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        rect=element_blank()) +
      coord_fixed(ratio = 1.3)
  #printing the map
   fig_1<-ggplotly(map_animated, tooltip = "text")
  fig_1
  
   })
  
  ############################ Regional Trends #################
  
  output$reg_map<-renderPlotly({
   
    ##Africa
    
    if(input$region=="2"){
      #subsetting data
      mapreg<-subset(mapdata, mapdata$unreg==2 & mapdata$year==input$yearreg)
      low <- min(mapreg$hrscorex2)
      high <- max(mapreg$hrscorex2)
      
      ##plot
      map_region<-mapreg %>%
        ggplot(aes(long, lat, group=group, fill=hrscorex2,text=str_glue("{country}:{hrscorex2}")))+
        geom_polygon(color="white",linewidth=.01)+
        scale_fill_gradient(" ",low = "#000000",  high = "#0C7BDC",
                            breaks = c(low, high), 
                            labels = c("Low Respect", "High Respect"))+
        ggtitle(paste("Human Rights Index: Africa", "\n", "(", paste(input$yearreg, ")")) )  +
        theme(
          plot.title = element_text(size = 12, hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5),
          plot.caption = element_text(size = 8, hjust = 1),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          rect=element_blank()) +
        coord_fixed(ratio = 1.3)
    }
    
    ##Oceania
    if(input$region=="9"){
      #subsetting data
      mapreg<-subset(mapdata, mapdata$unreg==9 & mapdata$year==input$yearreg)
      low <- min(mapreg$hrscorex2)
      high <- max(mapreg$hrscorex2)
      
      ##plot
      map_region<-mapreg %>%
        ggplot(aes(long, lat, group=group, fill=hrscorex2,text=str_glue("{country}:{hrscorex2}")))+
        geom_polygon(color="white",linewidth=.01)+
        scale_fill_gradient(" ",low = "#000000",  high = "#0C7BDC",
                            breaks = c(low, high), 
                            labels = c("Low Respect", "High Respect"))+
        ggtitle(paste("Human Rights Index: Oceania", "\n", "(", paste(input$yearreg, ")")) )  +
        theme(
          plot.title = element_text(size = 12, hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5),
          plot.caption = element_text(size = 8, hjust = 1),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          rect=element_blank()) +
        coord_fixed(ratio = 1.3)
    }
    
    ##Northern America
    
    if(input$region=="21"){
     
      #subsetting data
      mapreg<-subset(mapdata, mapdata$unreg==21 & mapdata$year==input$yearreg)
      #low <- min(mapdata$hrscorex2)
      #high <- max(mapdata$hrscorex2)
      
      ##plot
      map_region<-mapreg %>%
        ggplot(aes(long, lat, group=group, fill=hrscorex2,text=str_glue("{country}:{hrscorex2}")))+
        geom_polygon(color="white",linewidth=.01)+
        # scale_fill_gradient(" ",low = "#000000",  high = "#0C7BDC",
        #                     breaks = c(low, high), 
        #                     labels = c("Low Respect", "High Respect"))+
        ggtitle(paste("Human Rights Index: Northern America", "\n", "(", paste(input$yearreg, ")")) )  +
        theme(
          plot.title = element_text(size = 12, hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5),
          plot.caption = element_text(size = 8, hjust = 1),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          rect=element_blank(),
          legend.position="none")+
          coord_fixed(ratio = 1.3)
    }
    
    ##Asia
    
    if(input$region=="142"){
      #subsetting data
      mapreg<-subset(mapdata, mapdata$unreg==142 & mapdata$year==input$yearreg)
      low <- min(mapreg$hrscorex2)
      high <- max(mapreg$hrscorex2)
      
      ##plot
      map_region<-mapreg %>%
        ggplot(aes(long, lat, group=group, fill=hrscorex2,text=str_glue("{country}:{hrscorex2}")))+
        geom_polygon(color="white",linewidth=.01)+
        scale_fill_gradient(" ",low = "#000000",  high = "#0C7BDC",
                            breaks = c(low, high), 
                            labels = c("Low Respect", "High Respect"))+
        ggtitle(paste("Human Rights Index: Asia", "\n", "(", paste(input$yearreg, ")")) )  +
        theme(
          plot.title = element_text(size = 12, hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5),
          plot.caption = element_text(size = 8, hjust = 1),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          rect=element_blank()) +
        coord_fixed(ratio = 1.3)
    }
    
    ##Southern Europe
  
    if(input$region=="150"){
      #subsetting data
      mapreg<-subset(mapdata, mapdata$unreg==150 & mapdata$year==input$yearreg)
      low <- min(mapreg$hrscorex2)
      high <- max(mapreg$hrscorex2)
      
      ##plot
      map_region<-mapreg %>%
        ggplot(aes(long, lat, group=group, fill=hrscorex2,text=str_glue("{country}:{hrscorex2}")))+
        geom_polygon(color="white",linewidth=.01)+
        scale_fill_gradient(" ",low = "#000000",  high = "#0C7BDC",
                            breaks = c(low, high), 
                            labels = c("Low Respect", "High Respect"))+
        ggtitle(paste("Human Rights Index: Southern Europe", "\n", "(", paste(input$yearreg, ")")) )  +
        theme(
          plot.title = element_text(size = 12, hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5),
          plot.caption = element_text(size = 8, hjust = 1),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          rect=element_blank()) +
        coord_fixed(ratio = 1.3)
    }
    
    ##Latin America
    if(input$region=="419"){
      #subsetting data
      mapreg<-subset(mapdata, mapdata$unreg==419 & mapdata$year==input$yearreg)
      low <- min(mapreg$hrscorex2)
      high <- max(mapreg$hrscorex2)
      
      ##plot
      map_region<-mapreg %>%
        ggplot(aes(long, lat, group=group, fill=hrscorex2,text=str_glue("{country}:{hrscorex2}")))+
        geom_polygon(color="white",linewidth=.01)+
        scale_fill_gradient(" ",low = "#000000",  high = "#0C7BDC",
                            breaks = c(low, high), 
                            labels = c("Low Respect", "High Respect"))+
        ggtitle(paste("Human Rights Index: Latin America", "\n", "(", paste(input$yearreg, ")")) )  +
        theme(
          plot.title = element_text(size = 12, hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5),
          plot.caption = element_text(size = 8, hjust = 1),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          rect=element_blank()) +
        coord_fixed(ratio = 1.3)
    }
    
    fig_2<-ggplotly(map_region, tooltip = "text")
    fig_2

  })
  
  ############################# Physical Integrity Rights Raw scores  ######################################
  output$phint_raw_scores <- renderPlot({
    ##  Subset out the data that the user selects in the app
    ##
    temp1 <- subset(hrdata, 
                    hrdata$country == input$country & hrdata$year %in% seq(input$countryyearslider[1], 
                                                                           input$countryyearslider[2], 1))
    
    ##  Extrajudicial killings
    
    if(input$phys_int_type=='kill'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$kill),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$kill <- as.factor(temp1$kill)
      #For highlighting different scores
      highlight_df_1<-temp1[temp1$kill == "1", ]  
      highlight_df_2<-temp1[temp1$kill == "2", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=kill)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=kill), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=kill), color='green', size=3)+ 
        xlab("Years") + 
        ylab("Political Killings Score")+
        scale_y_discrete(labels=c("0" = "Practiced \n Frequently (0)", "1" = "Practiced\n Occasionally (1)", "2"="Not\n Reported (2)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Score of Political Killings", "\n", "(", paste(input$country, ":", sep=""), input$countryyearslider[1], "-",  input$countryyearslider[2], ")")) 
    }
    
    ## Dissappearance
    
    if(input$phys_int_type=='disap'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$disap),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$disap <- as.factor(temp1$disap)
      #For highlighting different scores
      highlight_df_1<-temp1[temp1$disap == "1", ]  
      highlight_df_2<-temp1[temp1$disap == "2", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=disap)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=disap), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=disap), color='green', size=3)+ 
        xlab("Years") + 
        ylab("Disappearance Score")+
        scale_y_discrete(labels=c("0" = "Occured \n Frequently (0)", "1" = "Occured\n Occasionally (1)", "2"="Not\n Reported (2)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Score of Disappearances", "\n", "(", paste(input$country, ":", sep=""), input$countryyearslider[1], "-",  input$countryyearslider[2], ")")) 
    }
    
    ##Political Imprisonment
    
    if(input$phys_int_type=='polpris'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$polpris),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$polpris <- as.factor(temp1$polpris)
      #For highlighting different scores
      highlight_df_1<-temp1[temp1$polpris == "1", ]  
      highlight_df_2<-temp1[temp1$polpris == "2", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=polpris)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=polpris), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=polpris), color='green', size=3)+ 
        xlab("Years") + 
        ylab("Political Imprisonmeny Score")+
        scale_y_discrete(labels=c("0" = "Yes, \n Many (0)", "1" = "Yes\n Few (1)", "2"="Not\n Reported (2)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Score of Political Imprisonment", "\n", "(", paste(input$country, ":", sep=""), input$countryyearslider[1], "-",  input$countryyearslider[2], ")")) 
    }
    
  ## Torture
    
    if(input$phys_int_type=='tort'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$tort),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$tort <- as.factor(temp1$tort)
      #For highlighting different scores
      highlight_df_1<-temp1[temp1$tort == "1", ]  
      highlight_df_2<-temp1[temp1$tort == "2", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=tort)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=tort), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=tort), color='green', size=3)+ 
        xlab("Years") + 
        ylab("Torture Score")+
        scale_y_discrete(labels=c("0" = "Practiced \n Frequently (0)", "1" = "Practiced\n Occasionally (1)", "2"="Not\n Reported (2)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Score of Torture", "\n", "(", paste(input$country, ":", sep=""), input$countryyearslider[1], "-",  input$countryyearslider[2], ")")) 
    }
    
    ##Brutality based mass atrocity - Intensity
    
    if(input$phys_int_type=='bbatrocity_intensity'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$bbatrocity_intensity),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$bbatrocity_intensity <- as.factor(temp1$bbatrocity_intensity)
      #For highlighting different scores
      highlight_df_1<-temp1[temp1$bbatrocity_intensity == "1", ]  
      highlight_df_2<-temp1[temp1$bbatrocity_intensity == "2", ]  
      highlight_df_3<-temp1[temp1$bbatrocity_intensity == "3", ]  
      
    
      
     plot <- ggplot(temp1, aes(x=year, y=bbatrocity_intensity)) +
        geom_point(color="darkgreen", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=bbatrocity_intensity), color='green',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=bbatrocity_intensity), color='orange', size=3)+ 
        geom_point(data=highlight_df_3, aes(x=year,y=bbatrocity_intensity), color='red', size=3)+ 
      
        xlab("Years") + 
        ylab("Mass Atrocity Intensity Score")+
        scale_y_discrete(labels=c("0"="No right \n scores 0","1" = "1 Right\n scores 0", "2" = "2 Rights\n score 0 ", "3"="3 Rights\n score 0 "))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Brutality-Based Mass Atrocity (Intensity)", "\n", "(", paste(input$country, ":", sep=""), input$countryyearslider[1], "-",  input$countryyearslider[2], ")")) 
    }
    
  
    print(plot)
  }) # physical integrity rights raw scores  plots end here
  
  ########################## Physical Integrity Rolling average ####################################
  
  output$phint_roll_avg <- renderPlot({
    ##  Subset out the data that the user selects in the app
    ##
    temp1 <- subset(hrdata, 
                    hrdata$country == input$country & hrdata$year %in% seq(input$countryyearslider[1], 
                                                                           input$countryyearslider[2], 1))
    
    ## Extrajudicidal killings
    ##
    if(input$phys_int_type=='kill'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_kill3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_kill3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of Political Killings Score", "\n", "(", paste(input$country, ":", sep=""), input$countryyearslider[1], "-",  input$countryyearslider[2], ")")) 
    }
    
    ## Dissappearance
    ##
    if(input$phys_int_type=='disap'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_disap3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_disap3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of Disappearances Score", "\n", "(", paste(input$country, ":", sep=""), input$countryyearslider[1], "-",  input$countryyearslider[2], ")")) 
    }
    
    ## Political Imprisonment
    ##
    if(input$phys_int_type=='polpris'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_polpris3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_polpris3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of Political Imprisonment Score", "\n", "(", paste(input$country, ":", sep=""), input$countryyearslider[1], "-",  input$countryyearslider[2], ")")) 
    }
    
    ##Torture
    ##
    if(input$phys_int_type=='tort'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_tort3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_tort3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of Torture Score", "\n", "(", paste(input$country, ":", sep=""), input$countryyearslider[1], "-",  input$countryyearslider[2], ")")) 
    }
    
    ##BB Based Mass Atrocity
    ##
    if(input$phys_int_type=='bbatrocity_intensity'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_bbatrocity_intensity3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_bbatrocity_intensity3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of Brutality-Based Mass Atrocity (Intensity) Score", "\n", "(", paste(input$country, ":", sep=""), input$countryyearslider[1], "-",  input$countryyearslider[2], ")")) 
    }
    
  
    print(plot)
    
  }) # physical integrity rights rolling average plots end here
  
  ########################## Empowerment Rights and Freedoms Raw Scores #####################################
  
  output$emp_raw_scores <-renderPlot({
    ##  Subset out the data that the user selects in the app
    ##
    temp1 <- subset(hrdata, 
                    hrdata$country == input$country_emp & hrdata$year %in% seq(input$countryyearslider_emp[1], 
                                                                           input$countryyearslider_emp[2], 1))
    #Validating data
    validate(
      need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
      )
    )
     ##  Freedom of Speech and Press
    
    if(input$emp_rights=='speech'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$speech),]
      # Convert column from numeric to factor variable
      temp1$speech <- as.factor(temp1$speech)
      #For highlighting different scores
      highlight_df_1<-temp1[temp1$speech == "1", ]  
      highlight_df_2<-temp1[temp1$speech == "2", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=speech)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=speech), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=speech), color='green', size=3)+ 
        xlab("Years") + 
        ylab("Media Censorship Score")+
        scale_y_discrete(labels=c("0" = "Complete\n Censorship (0)", "1" = "Some\n Censorship (1)", "2"="No\n Censorship (2)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Score of Media Censorhip (Freedom of Speech)", "\n", "(", paste(input$country_emp, ":", sep=""), input$countryyearslider_emp[1], "-",  input$countryyearslider_emp[2], ")")) }
     
    ##Freedom of religion
      if(input$emp_rights=='rel_free'){
        #removing missing values
        temp1 <- temp1[!is.na(temp1$rel_free),]
        #Validating data
        validate(
          need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
          )
        )
        # Convert column from numeric to factor variable
        temp1$rel_free <- as.factor(temp1$rel_free)
        #For highlighting different scores
        highlight_df_1<-temp1[temp1$rel_free == "1", ]  
        highlight_df_2<-temp1[temp1$rel_free == "2", ]  
        
        plot <- ggplot(temp1, aes(x=year, y=rel_free)) +
          geom_point(color="red", size=3)+
          geom_point(data=highlight_df_1, aes(x=year,y=rel_free), color='orange',size=3)+
          geom_point(data=highlight_df_2, aes(x=year,y=rel_free), color='green', size=3)+ 
          xlab("Years") + 
          ylab("Restrictions of religion")+
          scale_y_discrete(labels=c("0" = "Severe (0)", "1" = "Moderate (1)", "2"="Practically\n Absent (2)"))+ 
          theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
          ggtitle(paste("Score of Freedom of Religion", "\n", "(", paste(input$country_emp, ":", sep=""), input$countryyearslider_emp[1], "-",  input$countryyearslider_emp[2], ")")) }
    
    ##Freedom of domestic movement
    if(input$emp_rights=='dommov'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$dommov),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$dommov <- as.factor(temp1$dommov)
      #For highlighting different scores
      highlight_df_1<-temp1[temp1$dommov == "1", ]  
      highlight_df_2<-temp1[temp1$dommov == "2", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=dommov)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=dommov), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=dommov), color='green', size=3)+ 
        xlab("Years") + 
        ylab("Domestic movement restrictions")+
        scale_y_discrete(labels=c("0" = "Severe (0)", "1" = "Moderate (1)", "2"="Unrestricted (2)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Freedom of Domestic Movement Score", "\n", "(", paste(input$country_emp, ":", sep=""), input$countryyearslider_emp[1], "-",  input$countryyearslider_emp[2], ")")) }
    
    ##Freedom of Foreign Movement
    if(input$emp_rights=='formov'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$formov),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$formov <- as.factor(temp1$formov)
      #For highlighting different scores
      highlight_df_1<-temp1[temp1$formov == "1", ]  
      highlight_df_2<-temp1[temp1$formov == "2", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=formov)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=formov), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=formov), color='green', size=3)+ 
        xlab("Years") + 
        ylab("Foreign movement restrictions")+
        scale_y_discrete(labels=c("0" = "Severe (0)", "1" = "Moderate (1)", "2"="Unrestricted (2)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Freedom of Foreign Movement and Travel Score", "\n", "(", paste(input$country_emp, ":", sep=""), input$countryyearslider_emp[1], "-",  input$countryyearslider_emp[2], ")")) }
    
    ##Freedom of Assembly and Association
    if(input$emp_rights=='assn'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$assn),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$assn <- as.factor(temp1$assn)
      #For highlighting different scores
      highlight_df_2<-temp1[temp1$assn == "2", ]  
      highlight_df_1<-temp1[temp1$assn == "1", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=assn)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=assn), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=assn), color='green', size=3)+ 
        xlab("Years") + 
        ylab("Assembly and Association")+
        scale_y_discrete(labels=c("0" = "Severely\n Restricted (0)", "1" = "Limited (1)", "2"="Unrestricted (2)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Freedom of Assembly and Association Score", "\n", "(", paste(input$country_emp, ":", sep=""), input$countryyearslider_emp[1], "-",  input$countryyearslider_emp[2], ")")) }
    
    
      ##Electoral Self-Determination
    if(input$emp_rights=="elecsd"){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$elecsd),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$elecsd <- as.factor(temp1$elecsd)
      #For highlighting different scores
      highlight_df_2<-temp1[temp1$elecsd == "2", ]  
      highlight_df_1<-temp1[temp1$elecsd == "1", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=elecsd)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=elecsd), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=elecsd), color='green', size=3)+ 
        xlab("Years") + 
        ylab("Electoral Self-Determination")+
        scale_y_discrete(labels=c("0" = "Not\n Respected (0)", "1" = "Limited (1)", "2"="Generally\n Respected (2)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Electoral Self-Determination Score", "\n", "(", paste(input$country_emp, ":", sep=""), input$countryyearslider_emp[1], "-",  input$countryyearslider_emp[2], ")")) }
    
    ##Women's Political Rights
    if(input$emp_rights=="wopol"){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$wopol),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$wopol <- as.factor(temp1$wopol)
      #For highlighting different scores
      highlight_df_3<-temp1[temp1$wopol == "3", ]  
      highlight_df_2<-temp1[temp1$wopol == "2", ]  
      highlight_df_1<-temp1[temp1$wopol == "1", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=wopol)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=wopol), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=wopol), color='green', size=3)+ 
        geom_point(data=highlight_df_3, aes(x=year,y=wopol), color='darkgreen', size=3)+ 
        xlab("Years") + 
        ylab("Women's Political Rights")+
        scale_y_discrete(labels=c("0" = "None (0)", "1" = "law,\n no \npractice (1)", "2"="law,\n practice \nmoderate (2)", "3"="law and \npractice (3)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Women's Political Rights Score", "\n", "(", paste(input$country_emp, ":", sep=""), input$countryyearslider_emp[1], "-",  input$countryyearslider_emp[2], ")")) }
    
      ##Women's Social Rights (Law)
    if(input$emp_rights=="wosoc_l"){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$wosoc_l),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$wosoc_l <- as.factor(temp1$wosoc_l)
      #For highlighting different scores
      highlight_df_3<-temp1[temp1$wosoc_l == "3", ]  
      highlight_df_2<-temp1[temp1$wosoc_l == "2", ]  
      highlight_df_1<-temp1[temp1$wosoc_l == "1", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=wosoc_l)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=wosoc_l), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=wosoc_l), color='green', size=3)+ 
        geom_point(data=highlight_df_3, aes(x=year,y=wosoc_l), color='darkgreen', size=3)+ 
        xlab("Years") + 
        ylab("Women's Social Rights (Law)")+
        scale_y_discrete(labels=c("0" = "None (0)", "1" = "Some\n rights (1)", "2"="Nearly\n all rights (2)", "3"="All social \nrights (3)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Women's Social Rights (Law) Score", "\n", "(", paste(input$country_emp, ":", sep=""), input$countryyearslider_emp[1], "-",  input$countryyearslider_emp[2], ")")) }
    
    ##Women's Social Rights (Practice)
    if(input$emp_rights=="wosoc_p"){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$wosoc_p),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$wosoc_p <- as.factor(temp1$wosoc_p)
      #For highlighting different scores
      highlight_df_3<-temp1[temp1$wosoc_p == "3", ]  
      highlight_df_2<-temp1[temp1$wosoc_p == "2", ]  
      highlight_df_1<-temp1[temp1$wosoc_p == "1", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=wosoc_p)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=wosoc_p), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=wosoc_p), color='green', size=3)+ 
        geom_point(data=highlight_df_3, aes(x=year,y=wosoc_p), color='darkgreen', size=3)+ 
        xlab("Years") + 
        ylab("Women's Social Rights (Practice)")+
        scale_y_discrete(labels=c("0" = "Discrimination \nhigh  (0)", "1" = "Discrimination\n moderate (1)", "2"="Discrimination\n low (2)", "3"="No \ndiscrimination (3)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Women's Social Rights (Practice) Score", "\n", "(", paste(input$country_emp, ":", sep=""), input$countryyearslider_emp[1], "-",  input$countryyearslider_emp[2], ")")) }
    
    ##Women's Economic Rights 
    if(input$emp_rights=="wecon"){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$wecon),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$wecon <- as.factor(temp1$wecon)
      #For highlighting different scores
      highlight_df_3<-temp1[temp1$wecon == "3", ]  
      highlight_df_2<-temp1[temp1$wecon == "2", ]  
      highlight_df_1<-temp1[temp1$wecon == "1", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=wecon)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=wecon), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=wecon), color='green', size=3)+ 
        geom_point(data=highlight_df_3, aes(x=year,y=wecon), color='darkgreen', size=3)+ 
        xlab("Years") + 
        ylab("Women's Economic Rights")+
        scale_y_discrete(labels=c("0" = "None (0)", "1" = "law,\n no \npractice (1)", "2"="law,\n practice \nmoderate (2)", "3"="law and \npractice (3)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Women's Economic Rights Score", "\n", "(", paste(input$country_emp, ":", sep=""), input$countryyearslider_emp[1], "-",  input$countryyearslider_emp[2], ")")) }
    
    
      print(plot)
      
  })
  
  ####################### Empowerment Rights Rolling Average##############################
  
  output$emp_roll_avg <- renderPlot({
    ##  Subset out the data that the user selects in the app
    ##
    temp1 <- subset(hrdata, 
                    hrdata$country == input$country_emp & hrdata$year %in% seq(input$countryyearslider_emp[1], 
                                                                               input$countryyearslider_emp[2], 1))
    
    ## Freedom of speech
    ##
    if(input$emp_rights=='speech'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_speech3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_speech3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of Freedom of Speech Score", "\n", "(", paste(input$country_emp, ":", sep=""), input$countryyearslider_emp[1], "-",  input$countryyearslider_emp[2], ")")) 
    }
    
    ## Religious freedom 
    ##
    if(input$emp_rights=='rel_free'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_rel_free3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_rel_free3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of Religious Freedom Score", "\n", "(", paste(input$country_emp, ":", sep=""), input$countryyearslider_emp[1], "-",  input$countryyearslider_emp[2], ")")) 
    }
    
    ## Domestic Movement
    ##
    if(input$emp_rights=='dommov'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_dommov3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_dommov3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of Freedom of Domestic Movement Score", "\n", "(", paste(input$country_emp, ":", sep=""), input$countryyearslider_emp[1], "-",  input$countryyearslider_emp[2], ")")) 
    }
    
    ##Foreign Movement
    ##
    if(input$emp_rights=='formov'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_formov3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_formov3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of Freedom of Foreign Movement Score", "\n", "(", paste(input$country_emp, ":", sep=""), input$countryyearslider_emp[1], "-",  input$countryyearslider_emp[2], ")")) 
    }
    
    ##Association and assembly
    ##
    if(input$emp_rights=='assn'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_assn3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_assn3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of Freedom of Assembly and Association Score", "\n", "(", paste(input$country_emp, ":", sep=""), input$countryyearslider_emp[1], "-",  input$countryyearslider_emp[2], ")")) 
    }
    
    ##Electoral Freedom
    ##
    if(input$emp_rights=='elecsd'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_elecsd3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_elecsd3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of Electoral Self-Determination Score", "\n", "(", paste(input$country_emp, ":", sep=""), input$countryyearslider_emp[1], "-",  input$countryyearslider_emp[2], ")")) 
    }
    
    ## Women's Political Rights
    ##
    if(input$emp_rights=='wopol'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_wopol3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_wopol3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of Women's Political Rights Score", "\n", "(", paste(input$country_emp, ":", sep=""), input$countryyearslider_emp[1], "-",  input$countryyearslider_emp[2], ")")) 
    }
    
    ##Women's Social Rights Law (NAs)
    ##
    if(input$emp_rights=='wosoc_l'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_wosoc_l3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_wosoc_l3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of Women's Social Rights (Law) Score", "\n", "(", paste(input$country_emp, ":", sep=""), input$countryyearslider_emp[1], "-",  input$countryyearslider_emp[2], ")")) 
    }
    
    ##Women's Social Rights Practice (NAs)
    ##
    if(input$emp_rights=='wosoc_p'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_wosoc_p3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_wosoc_p3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of Women's Social Rights (Practice) Score", "\n", "(", paste(input$country_emp, ":", sep=""), input$countryyearslider_emp[1], "-",  input$countryyearslider_emp[2], ")")) 
    }
    
    ##Women's Economic Rights Practice
    ##
    if(input$emp_rights=='wecon'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_wecon3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_wecon3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of Women's Economic Rights (Practice) Score", "\n", "(", paste(input$country_emp, ":", sep=""), input$countryyearslider_emp[1], "-",  input$countryyearslider_emp[2], ")")) 
    }
    

    
    print(plot)
  })
  
  ##################### Workers' Rights Raw scores ######################################
  
  output$work_raw_scores<-renderPlot({
    ##  Subset out the data that the user selects in the app
    ##
    temp1 <- subset(hrdata, 
                    hrdata$country == input$country_work & hrdata$year %in% seq(input$countryyearslider_work[1], 
                                                                               input$countryyearslider_work[2], 1))
    
    ## The Right to Form Worker Unions (Law)
    
    if(input$workers_rights=='union_l'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$union_l),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$union_l <- as.factor(temp1$union_l)
      #For highlighting different scores
      highlight_df_1<-temp1[temp1$union_l == "1", ]  
      highlight_df_2<-temp1[temp1$union_l == "2", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=union_l)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=union_l), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=union_l), color='green', size=3)+ 
        xlab("Years") + 
        ylab("Score")+
        scale_y_discrete(labels=c("0" = "No\n Protection (0)", "1" = "Some\n Protection (1)", "2"="Full\n Protection (2)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("The Right to Form Worker Unions (Law) Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) }
    
    ## The Right to Form Worker Unions (Practice)
    
    if(input$workers_rights=='union_p'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$union_p),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$union_p <- as.factor(temp1$union_p)
      #For highlighting different scores
      highlight_df_1<-temp1[temp1$union_p == "1", ]  
      highlight_df_2<-temp1[temp1$union_p == "2", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=union_p)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=union_p), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=union_p), color='green', size=3)+ 
        xlab("Years") + 
        ylab("Score")+
        scale_y_discrete(labels=c("0" = "No\n Protection (0)", "1" = "Some\n Protection (1)", "2"="Full\n Protection (2)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("The Right to Form Worker Unions (Practice  ) Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) }
    
    ## The Right to Bargain Collectively (Law)
    ##
    
    if(input$workers_rights=='barg_l'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$barg_l),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$barg_l <- as.factor(temp1$barg_l)
      #For highlighting different scores
      highlight_df_1<-temp1[temp1$barg_l == "1", ]  
      highlight_df_2<-temp1[temp1$barg_l == "2", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=barg_l)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=barg_l), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=barg_l), color='green', size=3)+ 
        xlab("Years") + 
        ylab("Score")+
        scale_y_discrete(labels=c("0" = "No\n Protection (0)", "1" = "Some\n Protection (1)", "2"="Full\n Protection (2)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("The Right to Bargain Collectively (Law) Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) }
    
    ## The Right to Bargain Collectively (Practice)
    ##
    
    if(input$workers_rights=='barg_p'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$barg_p),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$barg_p <- as.factor(temp1$barg_p)
      #For highlighting different scores
      highlight_df_1<-temp1[temp1$barg_p == "1", ]  
      highlight_df_2<-temp1[temp1$barg_p == "2", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=barg_p)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=barg_p), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=barg_p), color='green', size=3)+ 
        xlab("Years") + 
        ylab("Score")+
        scale_y_discrete(labels=c("0" = "No\n Protection (0)", "1" = "Some\n Protection (1)", "2"="Full\n Protection (2)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("The Right to Bargain Collectively (Practice) Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) }
    
    
    ##The Right to be Free from Forced Labor (Law)
    ##
    
    if(input$workers_rights=='force_l'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$force_l),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$force_l <- as.factor(temp1$force_l)
      #For highlighting different scores
      highlight_df_1<-temp1[temp1$force_l == "1", ]  
      highlight_df_2<-temp1[temp1$force_l == "2", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=force_l)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=force_l), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=force_l), color='green', size=3)+ 
        xlab("Years") + 
        ylab("Score")+
        scale_y_discrete(labels=c("0" = "No\n Protection (0)", "1" = "Some\n Protection (1)", "2"="Full\n Protection (2)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("The Right to be Free from Forced Labor (Law) Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) }
    
    ##The Right to be Free from Forced Labor (Practice)
    ##
    
    if(input$workers_rights=='force_p'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$force_p),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$force_p <- as.factor(temp1$force_p)
      #For highlighting different scores
      highlight_df_1<-temp1[temp1$force_p == "1", ]  
      highlight_df_2<-temp1[temp1$force_p == "2", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=force_p)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=force_p), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=force_p), color='green', size=3)+ 
        xlab("Years") + 
        ylab("Score")+
        scale_y_discrete(labels=c("0" = "No\n Protection (0)", "1" = "Some\n Protection (1)", "2"="Full\n Protection (2)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("The Right to be Free from Forced Labor (Practice) Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) }
    
    
    ## Children's Rights (Law)
    ##
    
    if(input$workers_rights=='child_l'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$child_l),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$child_l <- as.factor(temp1$child_l)
      #For highlighting different scores
      highlight_df_1<-temp1[temp1$child_l == "1", ]  
      highlight_df_2<-temp1[temp1$child_l == "2", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=child_l)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=child_l), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=child_l), color='green', size=3)+ 
        xlab("Years") + 
        ylab("Score")+
        scale_y_discrete(labels=c("0" = "No\n Protection (0)", "1" = "Some\n Protection (1)", "2"="Full\n Protection (2)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("The Children's Right (Law) Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) }
    
    ## Children's Rights (Practice)
    ##
    
    if(input$workers_rights=='child_p'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$child_p),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$child_p <- as.factor(temp1$child_p)
      #For highlighting different scores
      highlight_df_1<-temp1[temp1$child_p == "1", ]  
      highlight_df_2<-temp1[temp1$child_p == "2", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=child_p)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=child_p), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=child_p), color='green', size=3)+ 
        xlab("Years") + 
        ylab("Score")+
        scale_y_discrete(labels=c("0" = "No\n Protection (0)", "1" = "Some\n Protection (1)", "2"="Full\n Protection (2)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("The Children's Right (Practice) Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) }
    
    
    ##The Right to a Minimum Wage (Law)
    ##
    
    if(input$workers_rights=='wage_l'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$child_p),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$wage_l <- as.factor(temp1$wage_l)
      #For highlighting different scores
      highlight_df_1<-temp1[temp1$wage_l == "1", ]  
      highlight_df_2<-temp1[temp1$wage_l == "2", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=wage_l)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=wage_l), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=wage_l), color='green', size=3)+ 
        xlab("Years") + 
        ylab("Score")+
        scale_y_discrete(labels=c("0" = "Not\n Established (0)", "1" = "Somewhat\n Established (1)", "2"="Fully\n Established (2)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("The Right to a Minimum Wage (Law) Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) }
    
    ##The Right to a Minimum Wage (Practice)
    ##
    
    if(input$workers_rights=='wage_p'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$child_p),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$wage_p <- as.factor(temp1$wage_p)
      #For highlighting different scores
      highlight_df_1<-temp1[temp1$wage_p == "1", ]  
      highlight_df_2<-temp1[temp1$wage_p == "2", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=wage_p)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=wage_p), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=wage_p), color='green', size=3)+ 
        xlab("Years") + 
        ylab("Score")+
        scale_y_discrete(labels=c("0" = "Not\n Established (0)", "1" = "Somewhat\n Established (1)", "2"="Fully\n Established (2)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("The Right to a Minimum Wage (Practice) Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) }
    
    
    ## The Right to Occupational Safety (Law)
    ##
    
    if(input$workers_rights=='safe_l'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$safe_l),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$safe_l <- as.factor(temp1$safe_l)
      #For highlighting different scores
      highlight_df_1<-temp1[temp1$safe_l == "1", ]  
      highlight_df_2<-temp1[temp1$safe_l == "2", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=safe_l)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=safe_l), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=safe_l), color='green', size=3)+ 
        xlab("Years") + 
        ylab("Score")+
        scale_y_discrete(labels=c("0" = "No\n Protection (0)", "1" = "Some\n Protection (1)", "2"="Full\n Protection (2)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("The Right to Occupational Safety (Law) Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) }
    
    
    ## The Right to Occupational Safety (Practice)
    ##
    
    if(input$workers_rights=='safe_p'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$safe_p),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$safe_p <- as.factor(temp1$safe_p)
      #For highlighting different scores
      highlight_df_1<-temp1[temp1$safe_p == "1", ]  
      highlight_df_2<-temp1[temp1$safe_p == "2", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=safe_p)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=safe_p), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=safe_p), color='green', size=3)+ 
        xlab("Years") + 
        ylab("Score")+
        scale_y_discrete(labels=c("0" = "No\n Protection (0)", "1" = "Some\n Protection (1)", "2"="Full\n Protection (2)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("The Right to Occupational Safety (Law) Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) }
    
    
    ##Reasonable Limitation on Working Hours (Law)
    ##
    
    if(input$workers_rights=='hour_l'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$hour_l),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$hour_l <- as.factor(temp1$hour_l)
      #For highlighting different scores
      highlight_df_1<-temp1[temp1$hour_l == "1", ]  
      highlight_df_2<-temp1[temp1$hour_l == "2", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=hour_l)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=hour_l), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=hour_l), color='green', size=3)+ 
        xlab("Years") + 
        ylab("Score")+
        scale_y_discrete(labels=c("0" = "No\n Protection (0)", "1" = "Some\n Protection (1)", "2"="Full\n Protection (2)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Reasonable Limitation on Working Hours (Law) Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) }
    
    ##Reasonable Limitation on Working Hours (Practice)
    ##
    
    if(input$workers_rights=='hour_p'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$hour_p),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$hour_p <- as.factor(temp1$hour_p)
      #For highlighting different scores
      highlight_df_1<-temp1[temp1$hour_p == "1", ]  
      highlight_df_2<-temp1[temp1$hour_p == "2", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=hour_p)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=hour_p), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=hour_p), color='green', size=3)+ 
        xlab("Years") + 
        ylab("Score")+
        scale_y_discrete(labels=c("0" = "No\n Protection (0)", "1" = "Some\n Protection (1)", "2"="Full\n Protection (2)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Reasonable Limitation on Working Hours (Practice) Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) }
    
    
    ##Human Trafficking (Law)
    ##
    
    if(input$workers_rights=='trafficking_l'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$trafficking_l),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$trafficking_l <- as.factor(temp1$trafficking_l)
      #For highlighting different scores
      highlight_df_1<-temp1[temp1$trafficking_l == "1", ]  
      highlight_df_2<-temp1[temp1$trafficking_l == "2", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=trafficking_l)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=trafficking_l), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=trafficking_l), color='green', size=3)+ 
        xlab("Years") + 
        ylab("Score")+
        scale_y_discrete(labels=c("0" = "No\n Protection (0)", "1" = "Some\n Protection (1)", "2"="Full\n Protection (2)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Human Trafficking (Law) Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) }
    
    ##Human Trafficking (Practice)
    ##
      if(input$workers_rights=='trafficking_p'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$trafficking_p),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
        temp1$trafficking_p <- as.factor(temp1$trafficking_p)
        #For highlighting different scores
        highlight_df_1<-temp1[temp1$trafficking_p == "1", ]  
        highlight_df_2<-temp1[temp1$trafficking_p == "2", ]  
        
        plot <- ggplot(temp1, aes(x=year, y=trafficking_p)) +
          geom_point(color="red", size=3)+
          geom_point(data=highlight_df_1, aes(x=year,y=trafficking_p), color='orange',size=3)+
          geom_point(data=highlight_df_2, aes(x=year,y=trafficking_p), color='green', size=3)+ 
          xlab("Years") + 
          ylab("Score")+
          scale_y_discrete(labels=c("0" = "No\n Protection (0)", "1" = "Some\n Protection (1)", "2"="Full\n Protection (2)"))+ 
          theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
          ggtitle(paste("Human Trafficking (Practice) Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) }
      
    ## Discrimination based on race/color
    ##
    
    # if(input$workers_rights=='discri_race'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$discri_race),]
    #   #Validating data
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   # Convert column from numeric to factor variable
    #   temp1$discri_race <- as.factor(temp1$discri_race)
    #   #For highlighting different scores
    #   highlight_df_1<-temp1[temp1$discri_race == "1", ]  
    #   highlight_df_2<-temp1[temp1$discri_race == "2", ]  
    #   highlight_df_99<-temp1[temp1$discri_race == "99", ]  
    #   
    #   plot <- ggplot(temp1, aes(x=year, y=discri_race)) +
    #     geom_point(color="red", size=3)+
    #     geom_point(data=highlight_df_1, aes(x=year,y=discri_race), color='orange',size=3)+
    #     geom_point(data=highlight_df_2, aes(x=year,y=discri_race), color='green', size=3)+ 
    #     geom_point(data=highlight_df_99, aes(x=year,y=discri_race), color='pink', size=3)+ 
    #     xlab("Years") + 
    #     ylab("Score")+
    #     scale_y_discrete(labels=c("99"="No\n Mention (99)","0" = "No\n Protection (0)", "1" = "Some\n Protection (1)", "2"="Full\n Protection (2)"))+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Discrimination Based on Race/Color Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) }
    # 
    # ##Discrimination based on gender (sex)
    # ##
    # if(input$workers_rights=='discri_gender'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$discri_gender),]
    #   #Validating data
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   # Convert column from numeric to factor variable
    #   temp1$discri_gender <- as.factor(temp1$discri_gender)
    #   #For highlighting different scores
    #   highlight_df_1<-temp1[temp1$discri_gender == "1", ]  
    #   highlight_df_2<-temp1[temp1$discri_gender == "2", ]  
    #   highlight_df_99<-temp1[temp1$discri_gender == "99", ]  
    #   
    #   plot <- ggplot(temp1, aes(x=year, y=discri_gender)) +
    #     geom_point(color="red", size=3)+
    #     geom_point(data=highlight_df_1, aes(x=year,y=discri_gender), color='orange',size=3)+
    #     geom_point(data=highlight_df_2, aes(x=year,y=discri_gender), color='green', size=3)+ 
    #     geom_point(data=highlight_df_99, aes(x=year,y=discri_gender), color='pink', size=3)+ 
    #     xlab("Years") + 
    #     ylab("Score")+
    #     scale_y_discrete(labels=c("99"="No\n Mention (99)","0" = "No\n Protection (0)", "1" = "Some\n Protection (1)", "2"="Full\n Protection (2)"))+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Discrimination Based on Gender (Sex) Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) }
    # 
    # 
    # ##Discrimination based on nationality
    # ##
    # if(input$workers_rights=='discri_nationality'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$discri_nationality),]
    #   #Validating data
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   # Convert column from numeric to factor variable
    #   temp1$discri_nationality <- as.factor(temp1$discri_nationality)
    #   #For highlighting different scores
    #   highlight_df_1<-temp1[temp1$discri_nationality == "1", ]  
    #   highlight_df_2<-temp1[temp1$discri_nationality == "2", ]  
    #   highlight_df_99<-temp1[temp1$discri_nationality == "99", ]  
    #   
    #   plot <- ggplot(temp1, aes(x=year, y=discri_nationality)) +
    #     geom_point(color="red", size=3)+
    #     geom_point(data=highlight_df_1, aes(x=year,y=discri_nationality), color='orange',size=3)+
    #     geom_point(data=highlight_df_2, aes(x=year,y=discri_nationality), color='green', size=3)+ 
    #     geom_point(data=highlight_df_99, aes(x=year,y=discri_nationality), color='pink', size=3)+ 
    #     xlab("Years") + 
    #     ylab("Score")+
    #     scale_y_discrete(labels=c("99"="No\n Mention (99)","0" = "No\n Protection (0)", "1" = "Some\n Protection (1)", "2"="Full\n Protection (2)"))+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Discrimination Based on Nationality (Citizenship,Origin, Migrant Worker) Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) }
    # 
    # ##Discrimination based on ethnicity
    # ##
    # if(input$workers_rights=='discri_ethnicity'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$discri_ethnicity),]
    #   #Validating data
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   # Convert column from numeric to factor variable
    #   temp1$discri_ethnicity <- as.factor(temp1$discri_ethnicity)
    #   #For highlighting different scores
    #   highlight_df_1<-temp1[temp1$discri_ethnicity == "1", ]  
    #   highlight_df_2<-temp1[temp1$discri_ethnicity == "2", ]  
    #   highlight_df_99<-temp1[temp1$discri_ethnicity == "99", ]  
    #   
    #   plot <- ggplot(temp1, aes(x=year, y=discri_ethnicity)) +
    #     geom_point(color="red", size=3)+
    #     geom_point(data=highlight_df_1, aes(x=year,y=discri_ethnicity), color='orange',size=3)+
    #     geom_point(data=highlight_df_2, aes(x=year,y=discri_ethnicity), color='green', size=3)+ 
    #     geom_point(data=highlight_df_99, aes(x=year,y=discri_ethnicity), color='pink', size=3)+ 
    #     xlab("Years") + 
    #     ylab("Score")+
    #     scale_y_discrete(labels=c("99"="No\n Mention (99)","0" = "No\n Protection (0)", "1" = "Some\n Protection (1)", "2"="Full\n Protection (2)"))+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Discrimination Based on Ethnicity Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) }
    # 
    # ##Discrimination based on religion (creed)
    # ##
    # if(input$workers_rights=='discri_religion'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$discri_religion),]
    #   #Validating data
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   # Convert column from numeric to factor variable
    #   temp1$discri_religion <- as.factor(temp1$discri_religion)
    #   #For highlighting different scores
    #   highlight_df_1<-temp1[temp1$discri_religion == "1", ]  
    #   highlight_df_2<-temp1[temp1$discri_religion == "2", ]  
    #   highlight_df_99<-temp1[temp1$discri_religion == "99", ]  
    #   
    #   plot <- ggplot(temp1, aes(x=year, y=discri_religion)) +
    #     geom_point(color="red", size=3)+
    #     geom_point(data=highlight_df_1, aes(x=year,y=discri_religion), color='orange',size=3)+
    #     geom_point(data=highlight_df_2, aes(x=year,y=discri_religion), color='green', size=3)+ 
    #     geom_point(data=highlight_df_99, aes(x=year,y=discri_religion), color='pink', size=3)+ 
    #     xlab("Years") + 
    #     ylab("Score")+
    #     scale_y_discrete(labels=c("99"="No\n Mention (99)","0" = "No\n Protection (0)", "1" = "Some\n Protection (1)", "2"="Full\n Protection (2)"))+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Discrimination Based on Religion (Creed) Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) }
    # 
    # ##Discrimination based on sexual orientation
    # ##
    # 
    # if(input$workers_rights=='discri_sexuality'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$discri_sexuality),]
    #   #Validating data
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   # Convert column from numeric to factor variable
    #   temp1$discri_sexuality <- as.factor(temp1$discri_sexuality)
    #   #For highlighting different scores
    #   highlight_df_1<-temp1[temp1$discri_sexuality == "1", ]  
    #   highlight_df_2<-temp1[temp1$discri_sexuality == "2", ]  
    #   highlight_df_99<-temp1[temp1$discri_sexuality == "99", ]  
    #   
    #   plot <- ggplot(temp1, aes(x=year, y=discri_sexuality)) +
    #     geom_point(color="red", size=3)+
    #     geom_point(data=highlight_df_1, aes(x=year,y=discri_sexuality), color='orange',size=3)+
    #     geom_point(data=highlight_df_2, aes(x=year,y=discri_sexuality), color='green', size=3)+ 
    #     geom_point(data=highlight_df_99, aes(x=year,y=discri_sexuality), color='pink', size=3)+ 
    #     xlab("Years") + 
    #     ylab("Score")+
    #     scale_y_discrete(labels=c("99"="No\n Mention (99)","0" = "No\n Protection (0)", "1" = "Some\n Protection (1)", "2"="Full\n Protection (2)"))+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Discrimination Based on Sexual Orientation Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) }
    # 
    # ##Discrimination based on HIV-AIDS
    # ##
    # 
    # if(input$workers_rights=='discri_AIDS'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$discri_AIDS),]
    #   #Validating data
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   # Convert column from numeric to factor variable
    #   temp1$discri_AIDS <- as.factor(temp1$discri_AIDS)
    #   #For highlighting different scores
    #   highlight_df_1<-temp1[temp1$discri_AIDS == "1", ]  
    #   highlight_df_2<-temp1[temp1$discri_AIDS == "2", ]  
    #   highlight_df_99<-temp1[temp1$discri_AIDS == "99", ]  
    #   
    #   plot <- ggplot(temp1, aes(x=year, y=discri_AIDS)) +
    #     geom_point(color="red", size=3)+
    #     geom_point(data=highlight_df_1, aes(x=year,y=discri_AIDS), color='orange',size=3)+
    #     geom_point(data=highlight_df_2, aes(x=year,y=discri_AIDS), color='green', size=3)+ 
    #     geom_point(data=highlight_df_99, aes(x=year,y=discri_AIDS), color='pink', size=3)+ 
    #     xlab("Years") + 
    #     ylab("Score")+
    #     scale_y_discrete(labels=c("99"="No\n Mention (99)","0" = "No\n Protection (0)", "1" = "Some\n Protection (1)", "2"="Full\n Protection (2)"))+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Discrimination Based on HIV-AIDS Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) }
    # 
    # 
    # ##Discrimination based on social origin
    # ##
    # 
    # if(input$workers_rights=='discri_socialorigin'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$discri_socialorigin),]
    #   #Validating data
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   # Convert column from numeric to factor variable
    #   temp1$discri_socialorigin <- as.factor(temp1$discri_socialorigin)
    #   #For highlighting different scores
    #   highlight_df_1<-temp1[temp1$discri_socialorigin == "1", ]  
    #   highlight_df_2<-temp1[temp1$discri_socialorigin == "2", ]  
    #   highlight_df_99<-temp1[temp1$discri_socialorigin == "99", ]  
    #   
    #   plot <- ggplot(temp1, aes(x=year, y=discri_socialorigin)) +
    #     geom_point(color="red", size=3)+
    #     geom_point(data=highlight_df_1, aes(x=year,y=discri_socialorigin), color='orange',size=3)+
    #     geom_point(data=highlight_df_2, aes(x=year,y=discri_socialorigin), color='green', size=3)+ 
    #     geom_point(data=highlight_df_99, aes(x=year,y=discri_socialorigin), color='pink', size=3)+ 
    #     xlab("Years") + 
    #     ylab("Score")+
    #     scale_y_discrete(labels=c("99"="No\n Mention (99)","0" = "No\n Protection (0)", "1" = "Some\n Protection (1)", "2"="Full\n Protection (2)"))+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Discrimination Based on Social Origin Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) }
    # 
    # 
    # ##Discrimination based on political beliefs
    # ##
    # 
    # if(input$workers_rights=='discri_polibelief'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$discri_polibelief),]
    #   #Validating data
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   # Convert column from numeric to factor variable
    #   temp1$discri_polibelief <- as.factor(temp1$discri_polibelief)
    #   #For highlighting different scores
    #   highlight_df_1<-temp1[temp1$discri_polibelief == "1", ]  
    #   highlight_df_2<-temp1[temp1$discri_polibelief == "2", ]  
    #   highlight_df_99<-temp1[temp1$discri_polibelief == "99", ]  
    #   
    #   plot <- ggplot(temp1, aes(x=year, y=discri_polibelief)) +
    #     geom_point(color="red", size=3)+
    #     geom_point(data=highlight_df_1, aes(x=year,y=discri_polibelief), color='orange',size=3)+
    #     geom_point(data=highlight_df_2, aes(x=year,y=discri_polibelief), color='green', size=3)+ 
    #     geom_point(data=highlight_df_99, aes(x=year,y=discri_polibelief), color='pink', size=3)+ 
    #     xlab("Years") + 
    #     ylab("Score")+
    #     scale_y_discrete(labels=c("99"="No\n Mention (99)","0" = "No\n Protection (0)", "1" = "Some\n Protection (1)", "2"="Full\n Protection (2)"))+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Discrimination Based on Political Beliefs Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) }
    # 
    # 
    # ##Discrimination based on disability
    # ##
    # 
    # if(input$workers_rights=='discri_disability'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$discri_disability),]
    #   #Validating data
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   # Convert column from numeric to factor variable
    #   temp1$discri_disability <- as.factor(temp1$discri_disability)
    #   #For highlighting different scores
    #   highlight_df_1<-temp1[temp1$discri_disability == "1", ]  
    #   highlight_df_2<-temp1[temp1$discri_disability == "2", ]  
    #   highlight_df_99<-temp1[temp1$discri_disability == "99", ]  
    #   
    #   plot <- ggplot(temp1, aes(x=year, y=discri_disability)) +
    #     geom_point(color="red", size=3)+
    #     geom_point(data=highlight_df_1, aes(x=year,y=discri_disability), color='orange',size=3)+
    #     geom_point(data=highlight_df_2, aes(x=year,y=discri_disability), color='green', size=3)+ 
    #     geom_point(data=highlight_df_99, aes(x=year,y=discri_disability), color='pink', size=3)+ 
    #     xlab("Years") + 
    #     ylab("Score")+
    #     scale_y_discrete(labels=c("99"="No\n Mention (99)","0" = "No\n Protection (0)", "1" = "Some\n Protection (1)", "2"="Full\n Protection (2)"))+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Discrimination Based on Disability Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) }
    # 
    # 
    # 
    print(plot)
  })
  
  
  ##################### Worker's Rights Rolling Average ###############################
  
  output$work_roll_average<-renderPlot({
    ##  Subset out the data that the user selects in the app
    ##
    temp1 <- subset(hrdata, 
                    hrdata$country == input$country_work & hrdata$year %in% seq(input$countryyearslider_work[1], 
                                                                               input$countryyearslider_work[2], 1))
    
    ## The Right to Form Worker Unions (Law)
    ##
    if(input$workers_rights=='union_l'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_union_l3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_union_l3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of Union Rights Score (Law)", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) 
    }
    
    ##The Right to Form Worker Unions (Practice)
    ##
    if(input$workers_rights=='union_p'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_union_p3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_union_p3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of Union Rights Score (Practice)", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) 
    }
    
    ##The Right to Bargain Collectively (Law)
    ##
    if(input$workers_rights=='barg_l'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_barg_l3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_barg_l3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of the Right to Bargain (Law)", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) 
    }
    
    ##The Right to Bargain Collectively (Practice)
    ##
    if(input$workers_rights=='barg_p'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_barg_p3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_barg_p3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of the Right to Bargain (Practice)", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) 
    }
    
    ##The Right to be Free from Forced Labor (Law)
    ##
    
    if(input$workers_rights=='force_l'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_force_l3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_force_l3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of the Forced Labor Score (Law)", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) 
    }
    
    ##The Right to be Free from Forced Labor (Practice)
    ##
    
    if(input$workers_rights=='force_p'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_force_p3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_force_p3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of the Forced Labor Score (Practice)", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) 
    }
    
    ##Children's rights (Law)
    ##
    if(input$workers_rights=='child_l'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_child_l3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_child_l3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of the Children's Rights Score (Law)", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) 
    }
    
    
    ##Children's rights (Practice)
    ##
    if(input$workers_rights=='child_p'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_child_p3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_child_p3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of the Children's Rights Score (Practice)", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) 
    }
    
    ##The Right to a Minimum Wage (Law)
    ##
    if(input$workers_rights=='wage_l'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_wage_l3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_wage_l3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of the Right to a Minimum Wage (Law)", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) 
    }
    
    ##The Right to a Minimum Wage (Practice)
    ##
    if(input$workers_rights=='wage_p'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_wage_p3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_wage_p3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of the Right to a Minimum Wage (Practice)", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) 
    }
    
    ##The Right to Occupational Safety (Law)
    ##
    if(input$workers_rights=='safe_l'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_safe_l3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_safe_l3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of the Right to Occupational Safety (Law)", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) 
    }
    
    ##The Right to Occupational Safety (Practice)
    ##
    if(input$workers_rights=='safe_p'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_safe_p3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_safe_p3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of the Right to Occupational Safety (Practice)", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) 
    }
    
    
    ##Reasonable Limitation on Working Hours (Law)
    ##
    if(input$workers_rights=='hour_l'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_hour_l3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_hour_l3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of the Reasonable Limitation on Working Hours (Law)", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) 
    }
    
    ##Reasonable Limitation on Working Hours (Practice)
    ##
    if(input$workers_rights=='hour_p'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_hour_p3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_hour_p3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of the Reasonable Limitation on Working Hours (Practice)", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) 
    }
    
    ##Human Trafficking (Law)
    ##
    if(input$workers_rights=='trafficking_l'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_trafficking_l3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_trafficking_l3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of the Human Trafficking (Law) Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) 
    }
    
    ##Human Trafficking (Practice)
    ##
    if(input$workers_rights=='trafficking_p'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_trafficking_p3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_trafficking_p3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of the Human Trafficking (Practice) Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) 
    }
    
    ##Discrimination based on race/color
    ##
    if(input$workers_rights=='discri_race'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_discri_race3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_discri_race3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of the Discrimination Based on Race/Color Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) 
    }
    
    
    ##Discrimination based on gender
    ##
    if(input$workers_rights=='discri_gender'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_discri_gender3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_discri_gender3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of the Discrimination Based on Gender Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) 
    }
    
    ##Discrimination based on nationality
    ##
    if(input$workers_rights=='discri_nationality'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_discri_nationality3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_discri_nationality3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of the Discrimination Based on Nationality Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) 
    }
    
    ##Discrimination based on ethnicity
    ##
    if(input$workers_rights=='discri_ethnicity'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_discri_ethnicity3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_discri_ethnicity3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of the Discrimination Based on Ethnicity Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) 
    }
    
    
    ##Discrimination based on religion (creed)
    ##
    if(input$workers_rights=='discri_religion'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_discri_religion3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_discri_religion3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of the Discrimination Based on Religion Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) 
    }
    
    ##Discrimination based on sexual orientation
    ##
    if(input$workers_rights=='discri_sexuality'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_discri_sexuality3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_discri_sexuality3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of the Discrimination Based on Sexual Orientation Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) 
    }
    
    
    ##Discrimination based on HIV-AIDS
    ##
    if(input$workers_rights=='discri_AIDS'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_discri_AIDS3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_discri_AIDS3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of the Discrimination Based on HIV-AIDS Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) 
    }
    
    ##Discrimination based on social origin
    ##
    if(input$workers_rights=='discri_socialorigin'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_discri_socialorigin3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_discri_socialorigin3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of the Discrimination Based on Social Origin Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) 
    }
    
    ##Discrimination based on political beliefs
    ##
    if(input$workers_rights=='discri_polibelief'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_discri_polibelief3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_discri_polibelief3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of the Discrimination Based on Political Beliefs Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) 
    }
    
    ##Discrimination based on disability
    ##
    if(input$workers_rights=='discri_disability'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_discri_disability3),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_discri_disability3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of the Discrimination Based on Disability Score", "\n", "(", paste(input$country_work, ":", sep=""), input$countryyearslider_work[1], "-",  input$countryyearslider_work[2], ")")) 
    }
    
   print(plot) 
  })
  
  #################### Justice Rights Raw scores ##################################
  
  output$justice_raw_scores<-renderPlot({
    ##  Subset out the data that the user selects in the app
    ##
    temp1 <- subset(hrdata, 
                    hrdata$country == input$country_justice & hrdata$year %in% seq(input$countryyearslider_justice[1], 
                                                                                input$countryyearslider_justice[2], 1))
    ## The Right to a Fair Trial (law)
    
    if(input$justice_rights=='trial_l'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$trial_l),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$trial_l <- as.factor(temp1$trial_l)
      #For highlighting different scores
      highlight_df_1<-temp1[temp1$trial_l == "1", ]  
      highlight_df_2<-temp1[temp1$trial_l == "2", ]  
      highlight_df_3<-temp1[temp1$trial_l == "3", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=trial_l)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=trial_l), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=trial_l), color='green', size=3)+ 
        geom_point(data=highlight_df_3, aes(x=year,y=trial_l), color='darkgreen', size=3)+ 
        xlab("Years") + 
        ylab("Score")+
        scale_y_discrete(labels=c("0" = "No\n Protect. (0)", "1" = "Very Limited\n Protect. (1)", "2"="Moderate\n Protect. (2)", "3"="Full\n Protect. (3)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("The Right to a Fair Trial (Law) Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) }
   
    
    ## The Right to a Fair Trial (Practice)
    
    if(input$justice_rights=='trial_p'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$trial_p),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$trial_p <- as.factor(temp1$trial_p)
      #For highlighting different scores
      highlight_df_1<-temp1[temp1$trial_p == "1", ]  
      highlight_df_2<-temp1[temp1$trial_p == "2", ]  
      highlight_df_3<-temp1[temp1$trial_p == "3", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=trial_p)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=trial_p), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=trial_p), color='green', size=3)+ 
        geom_point(data=highlight_df_3, aes(x=year,y=trial_p), color='darkgreen', size=3)+ 
        xlab("Years") + 
        ylab("Score")+
        scale_y_discrete(labels=c("0" = "No\n Protect. (0)", "1" = "Very Limited\n Protect. (1)", "2"="Moderate\n Protect. (2)", "3"="Full\n Protect. (3)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("The Right to a Fair Trial (Practice) Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) }
    
    
    ##Independence of the Judiciary
    ##
    
    if(input$justice_rights=='injud'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$injud),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # Convert column from numeric to factor variable
      temp1$injud <- as.factor(temp1$injud)
      #For highlighting different scores
      highlight_df_1<-temp1[temp1$injud == "1", ]  
      highlight_df_2<-temp1[temp1$injud == "2", ]  
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      plot <- ggplot(temp1, aes(x=year, y=injud)) +
        geom_point(color="red", size=3)+
        geom_point(data=highlight_df_1, aes(x=year,y=injud), color='orange',size=3)+
        geom_point(data=highlight_df_2, aes(x=year,y=injud), color='green', size=3)+ 
        xlab("Years") + 
        ylab("Score")+
        scale_y_discrete(labels=c("0" = "Not\n Independent (0)", "1" = "Partially\n Independent (1)", "2"="Generally\n Independent (2)"))+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Independence of the Judiciary Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) }
    
    
    # ##Torture in Prison
    # ##
    # 
    # if(input$justice_rights=='prison_torture'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$prison_torture),]
    #   #Validating data
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   # Convert column from numeric to factor variable
    #   temp1$prison_torture <- as.factor(temp1$prison_torture)
    #   #For highlighting different scores
    #   highlight_df_1<-temp1[temp1$prison_torture == "1", ]  
    #   highlight_df_2<-temp1[temp1$prison_torture == "2", ]  
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   plot <- ggplot(temp1, aes(x=year, y=prison_torture)) +
    #     geom_point(color="red", size=3)+
    #     geom_point(data=highlight_df_1, aes(x=year,y=prison_torture), color='orange',size=3)+
    #     geom_point(data=highlight_df_2, aes(x=year,y=prison_torture), color='green', size=3)+ 
    #     xlab("Years") + 
    #     ylab("Score")+
    #     scale_y_discrete(labels=c("0" = "No\n Respect  (0)", "1" = "Partial\n Respect (1)", "2"="Full\n Respect (2)"))+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Torture in Prison Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) }
    # 
    # 
    # 
    # ##Discrimination in Prison
    # ##
    # 
    # if(input$justice_rights=='prison_discriminate'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$prison_discriminate),]
    #   #Validating data
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   # Convert column from numeric to factor variable
    #   temp1$prison_discriminate <- as.factor(temp1$prison_discriminate)
    #   #For highlighting different scores
    #   highlight_df_1<-temp1[temp1$prison_discriminate == "1", ]  
    #   highlight_df_2<-temp1[temp1$prison_discriminate == "2", ]  
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   
    #   plot <- ggplot(temp1, aes(x=year, y=prison_discriminate)) +
    #     geom_point(color="red", size=3)+
    #     geom_point(data=highlight_df_1, aes(x=year,y=prison_discriminate), color='orange',size=3)+
    #     geom_point(data=highlight_df_2, aes(x=year,y=prison_discriminate), color='green', size=3)+ 
    #     xlab("Years") + 
    #     ylab("Score")+
    #     scale_y_discrete(labels=c("0" = "No\n Respect  (0)", "1" = "Partial\n Respect (1)", "2"="Full\n Respect (2)"))+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Discrimination in Prison Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) }
    # 
    # 
    # ##Rehabilitation in Prison
    # ##
    # 
    # if(input$justice_rights=='prison_rehabilitate'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$prison_rehabilitate),]
    #   #Validating data
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   # Convert column from numeric to factor variable
    #   temp1$prison_rehabilitate <- as.factor(temp1$prison_rehabilitate)
    #   #For highlighting different scores
    #   highlight_df_1<-temp1[temp1$prison_rehabilitate == "1", ]  
    #   highlight_df_2<-temp1[temp1$prison_rehabilitate == "2", ]  
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   
    #   plot <- ggplot(temp1, aes(x=year, y=prison_rehabilitate)) +
    #     geom_point(color="red", size=3)+
    #     geom_point(data=highlight_df_1, aes(x=year,y=prison_rehabilitate), color='orange',size=3)+
    #     geom_point(data=highlight_df_2, aes(x=year,y=prison_rehabilitate), color='green', size=3)+ 
    #     xlab("Years") + 
    #     ylab("Score")+
    #     scale_y_discrete(labels=c("0" = "No\n Respect  (0)", "1" = "Partial\n Respect (1)", "2"="Full\n Respect (2)"))+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Rehabilitation in Prison Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) }
    # 
    # 
    #  ##Overcrowding in Prison
    #  ##
    # 
    # if(input$justice_rights=='prison_overcrowd'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$prison_overcrowd),]
    #   #Validating data
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   # Convert column from numeric to factor variable
    #   temp1$prison_overcrowd <- as.factor(temp1$prison_overcrowd)
    #   #For highlighting different scores
    #   highlight_df_1<-temp1[temp1$prison_overcrowd == "1", ]  
    #   highlight_df_2<-temp1[temp1$prison_overcrowd == "2", ]  
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   
    #   plot <- ggplot(temp1, aes(x=year, y=prison_overcrowd)) +
    #     geom_point(color="red", size=3)+
    #     geom_point(data=highlight_df_1, aes(x=year,y=prison_overcrowd), color='orange',size=3)+
    #     geom_point(data=highlight_df_2, aes(x=year,y=prison_overcrowd), color='green', size=3)+ 
    #     xlab("Years") + 
    #     ylab("Score")+
    #     scale_y_discrete(labels=c("0" = "No\n Respect  (0)", "1" = "Partial\n Respect (1)", "2"="Full\n Respect (2)"))+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Rehabilitation in Prison Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) }
    # 
    # ##Separation in Prison
    # ##
    # 
    # if(input$justice_rights=='prison_separation'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$prison_separation),]
    #   #Validating data
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   # Convert column from numeric to factor variable
    #   temp1$prison_separation <- as.factor(temp1$prison_separation)
    #   #For highlighting different scores
    #   highlight_df_1<-temp1[temp1$prison_separation == "1", ]  
    #   highlight_df_2<-temp1[temp1$prison_separation == "2", ]  
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   
    #   plot <- ggplot(temp1, aes(x=year, y=prison_separation)) +
    #     geom_point(color="red", size=3)+
    #     geom_point(data=highlight_df_1, aes(x=year,y=prison_separation), color='orange',size=3)+
    #     geom_point(data=highlight_df_2, aes(x=year,y=prison_separation), color='green', size=3)+ 
    #     xlab("Years") + 
    #     ylab("Score")+
    #     scale_y_discrete(labels=c("0" = "No\n Respect  (0)", "1" = "Partial\n Respect (1)", "2"="Full\n Respect (2)"))+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Separation in Prison Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) }
    # 
    # ##Sanitation in Prison
    # ##
    # 
    # if(input$justice_rights=='prison_sanitation'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$prison_sanitation),]
    #   #Validating data
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   # Convert column from numeric to factor variable
    #   temp1$prison_sanitation <- as.factor(temp1$prison_sanitation)
    #   #For highlighting different scores
    #   highlight_df_1<-temp1[temp1$prison_sanitation == "1", ]  
    #   highlight_df_2<-temp1[temp1$prison_sanitation == "2", ]  
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   
    #   plot <- ggplot(temp1, aes(x=year, y=prison_sanitation)) +
    #     geom_point(color="red", size=3)+
    #     geom_point(data=highlight_df_1, aes(x=year,y=prison_sanitation), color='orange',size=3)+
    #     geom_point(data=highlight_df_2, aes(x=year,y=prison_sanitation), color='green', size=3)+ 
    #     xlab("Years") + 
    #     ylab("Score")+
    #     scale_y_discrete(labels=c("0" = "No\n Respect  (0)", "1" = "Partial\n Respect (1)", "2"="Full\n Respect (2)"))+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Sanitation in Prison Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) }
    # 
    # ##Food in Prison
    # ##
    # 
    # if(input$justice_rights=='prison_food'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$prison_food),]
    #   #Validating data
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   # Convert column from numeric to factor variable
    #   temp1$prison_food <- as.factor(temp1$prison_food)
    #   #For highlighting different scores
    #   highlight_df_1<-temp1[temp1$prison_food == "1", ]  
    #   highlight_df_2<-temp1[temp1$prison_food == "2", ]  
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   
    #   plot <- ggplot(temp1, aes(x=year, y=prison_food)) +
    #     geom_point(color="red", size=3)+
    #     geom_point(data=highlight_df_1, aes(x=year,y=prison_food), color='orange',size=3)+
    #     geom_point(data=highlight_df_2, aes(x=year,y=prison_food), color='green', size=3)+ 
    #     xlab("Years") + 
    #     ylab("Score")+
    #     scale_y_discrete(labels=c("0" = "No\n Respect  (0)", "1" = "Partial\n Respect (1)", "2"="Full\n Respect (2)"))+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Food in Prison Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) }
    # 
    # ##Healthcare in prison
    # ##
    # 
    # if(input$justice_rights=='prison_health'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$prison_health),]
    #   #Validating data
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   # Convert column from numeric to factor variable
    #   temp1$prison_health <- as.factor(temp1$prison_health)
    #   #For highlighting different scores
    #   highlight_df_1<-temp1[temp1$prison_health == "1", ]  
    #   highlight_df_2<-temp1[temp1$prison_health == "2", ]  
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   
    #   plot <- ggplot(temp1, aes(x=year, y=prison_health)) +
    #     geom_point(color="red", size=3)+
    #     geom_point(data=highlight_df_1, aes(x=year,y=prison_health), color='orange',size=3)+
    #     geom_point(data=highlight_df_2, aes(x=year,y=prison_health), color='green', size=3)+ 
    #     xlab("Years") + 
    #     ylab("Score")+
    #     scale_y_discrete(labels=c("0" = "No\n Respect  (0)", "1" = "Partial\n Respect (1)", "2"="Full\n Respect (2)"))+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Healthcare in Prison Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) }
    # 
    # 
    # ##Cleanliness in Prison
    # ##
    # 
    # if(input$justice_rights=='prison_clean'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$prison_clean),]
    #   #Validating data
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   # Convert column from numeric to factor variable
    #   temp1$prison_clean <- as.factor(temp1$prison_clean)
    #   #For highlighting different scores
    #   highlight_df_1<-temp1[temp1$prison_clean == "1", ]  
    #   highlight_df_2<-temp1[temp1$prison_clean == "2", ]  
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   
    #   plot <- ggplot(temp1, aes(x=year, y=prison_clean)) +
    #     geom_point(color="red", size=3)+
    #     geom_point(data=highlight_df_1, aes(x=year,y=prison_clean), color='orange',size=3)+
    #     geom_point(data=highlight_df_2, aes(x=year,y=prison_clean), color='green', size=3)+ 
    #     xlab("Years") + 
    #     ylab("Score")+
    #     scale_y_discrete(labels=c("0" = "No\n Respect  (0)", "1" = "Partial\n Respect (1)", "2"="Full\n Respect (2)"))+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Cleanliness in Prison Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) }
    # 
    # 
    # 
    # ##Family Access in Prison
    # ##
    # 
    # if(input$justice_rights=='prison_family'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$prison_family),]
    #   #Validating data
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   # Convert column from numeric to factor variable
    #   temp1$prison_family <- as.factor(temp1$prison_family)
    #   #For highlighting different scores
    #   highlight_df_1<-temp1[temp1$prison_family == "1", ]  
    #   highlight_df_2<-temp1[temp1$prison_family == "2", ]  
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   
    #   plot <- ggplot(temp1, aes(x=year, y=prison_family)) +
    #     geom_point(color="red", size=3)+
    #     geom_point(data=highlight_df_1, aes(x=year,y=prison_family), color='orange',size=3)+
    #     geom_point(data=highlight_df_2, aes(x=year,y=prison_family), color='green', size=3)+ 
    #     xlab("Years") + 
    #     ylab("Score")+
    #     scale_y_discrete(labels=c("0" = "No\n Respect  (0)", "1" = "Partial\n Respect (1)", "2"="Full\n Respect (2)"))+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Family Access in Prison Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) }
    # 
    # 
    # ##Record keeping in Prison
    # ##
    # 
    # if(input$justice_rights=='prison_records'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$prison_records),]
    #   #Validating data
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   # Convert column from numeric to factor variable
    #   temp1$prison_records <- as.factor(temp1$prison_records)
    #   #For highlighting different scores
    #   highlight_df_1<-temp1[temp1$prison_records == "1", ]  
    #   highlight_df_2<-temp1[temp1$prison_records == "2", ]  
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   
    #   plot <- ggplot(temp1, aes(x=year, y=prison_records)) +
    #     geom_point(color="red", size=3)+
    #     geom_point(data=highlight_df_1, aes(x=year,y=prison_records), color='orange',size=3)+
    #     geom_point(data=highlight_df_2, aes(x=year,y=prison_records), color='green', size=3)+ 
    #     xlab("Years") + 
    #     ylab("Score")+
    #     scale_y_discrete(labels=c("0" = "No\n Respect  (0)", "1" = "Partial\n Respect (1)", "2"="Full\n Respect (2)"))+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Record keeping in Prison Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) }
    # 
    # 
    # ##Commitment Order in Prison
    # ##
    # 
    # if(input$justice_rights=='prison_commitment'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$prison_commitment),]
    #   #Validating data
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   # Convert column from numeric to factor variable
    #   temp1$prison_commitment <- as.factor(temp1$prison_commitment)
    #   #For highlighting different scores
    #   highlight_df_1<-temp1[temp1$prison_commitment == "1", ]  
    #   highlight_df_2<-temp1[temp1$prison_commitment == "2", ]  
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   
    #   plot <- ggplot(temp1, aes(x=year, y=prison_commitment)) +
    #     geom_point(color="red", size=3)+
    #     geom_point(data=highlight_df_1, aes(x=year,y=prison_commitment), color='orange',size=3)+
    #     geom_point(data=highlight_df_2, aes(x=year,y=prison_commitment), color='green', size=3)+ 
    #     xlab("Years") + 
    #     ylab("Score")+
    #     scale_y_discrete(labels=c("0" = "No\n Respect  (0)", "1" = "Partial\n Respect (1)", "2"="Full\n Respect (2)"))+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Commitment Order in Prison Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) }
    # 
    # 
    # ##Central Authority in Prison
    # ##
    # 
    # if(input$justice_rights=='prison_central'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$prison_central),]
    #   #Validating data
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   # Convert column from numeric to factor variable
    #   temp1$prison_central <- as.factor(temp1$prison_central)
    #   #For highlighting different scores
    #   highlight_df_1<-temp1[temp1$prison_central == "1", ]  
    #   highlight_df_2<-temp1[temp1$prison_central == "2", ]  
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   
    #   plot <- ggplot(temp1, aes(x=year, y=prison_central)) +
    #     geom_point(color="red", size=3)+
    #     geom_point(data=highlight_df_1, aes(x=year,y=prison_central), color='orange',size=3)+
    #     geom_point(data=highlight_df_2, aes(x=year,y=prison_central), color='green', size=3)+ 
    #     xlab("Years") + 
    #     ylab("Score")+
    #     scale_y_discrete(labels=c("0" = "No\n Respect  (0)", "1" = "Partial\n Respect (1)", "2"="Full\n Respect (2)"))+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Central Authority in Prison Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) }
    # 

    print(plot) 
  })
  
  
  #################### Justice Rights Rolling Average ###################
  
  output$justice_roll_avg<-renderPlot({
    
    ##  Subset out the data that the user selects in the app
    ##
    temp1 <- subset(hrdata, 
                    hrdata$country == input$country_justice & hrdata$year %in% seq(input$countryyearslider_justice[1], 
                                                                                input$countryyearslider_justice[2], 1))
    
    ## The right to a fairl trial (law)
    ##
    if(input$justice_rights=='trial_l'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_trial_l3),]
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_trial_l3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of The Right to a Fail Trial Score (Law)", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) 
    }
    
    ## The right to a fairl trial (practice)
    ##
    if(input$justice_rights=='trial_p'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_trial_p3),]
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_trial_p3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of The Right to a Fail Trial Score (Practice)", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) 
    }
    
    ##Independence of the Judiciary
    ##
    if(input$justice_rights=='injud'){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$avg_injud3),]
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      #rolling average plot
      plot <- ggplot(temp1, aes(x=year, y=avg_injud3)) +
        geom_line()+
        xlab("Years") + 
        ylab("Rolling Average")+ 
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Rolling 3 year Average of The Independence of the Judiciary Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) 
    }
    
    # ##Torture in Prison
    # ##
    # 
    # if(input$justice_rights=='prison_torture'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$avg_prison_torture3),]
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   #rolling average plot
    #   plot <- ggplot(temp1, aes(x=year, y=avg_prison_torture3)) +
    #     geom_line()+
    #     xlab("Years") + 
    #     ylab("Rolling Average")+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Rolling 3 year Average of The Torture in Prison Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) 
    # }
    
    # ##Discrimination in Prison
    # ##
    # 
    # if(input$justice_rights=='prison_discriminate'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$avg_prison_discriminate3),]
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   #rolling average plot
    #   plot <- ggplot(temp1, aes(x=year, y=avg_prison_discriminate3)) +
    #     geom_line()+
    #     xlab("Years") + 
    #     ylab("Rolling Average")+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Rolling 3 year Average of The Discrimination in Prison Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) 
    # }
    
    # ##Rehabilitation in Prison
    # ##
    # 
    # if(input$justice_rights=='prison_rehabilitate'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$avg_prison_rehabilitate3),]
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   #rolling average plot
    #   plot <- ggplot(temp1, aes(x=year, y=avg_prison_rehabilitate3)) +
    #     geom_line()+
    #     xlab("Years") + 
    #     ylab("Rolling Average")+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Rolling 3 year Average of The Rehabilitation in Prison Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) 
    # }
    
    # ##Overcrowding in Prison
    # ##
    # 
    # if(input$justice_rights=='prison_overcrowd'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$avg_prison_overcrowd3),]
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   #rolling average plot
    #   plot <- ggplot(temp1, aes(x=year, y=avg_prison_overcrowd3)) +
    #     geom_line()+
    #     xlab("Years") + 
    #     ylab("Rolling Average")+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Rolling 3 year Average of The Overcrowding in Prison Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) 
    # }
    # 
    # ##Separation in Prison
    # ##
    # 
    # if(input$justice_rights=='prison_separation'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$avg_prison_separation3),]
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   #rolling average plot
    #   plot <- ggplot(temp1, aes(x=year, y=avg_prison_separation3)) +
    #     geom_line()+
    #     xlab("Years") + 
    #     ylab("Rolling Average")+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Rolling 3 year Average of The Separation in Prison Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) 
    # }
    # 
    # ##Sanitation in Prison
    # ##
    # 
    # if(input$justice_rights=='prison_sanitation'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$avg_prison_sanitation3),]
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   #rolling average plot
    #   plot <- ggplot(temp1, aes(x=year, y=avg_prison_sanitation3)) +
    #     geom_line()+
    #     xlab("Years") + 
    #     ylab("Rolling Average")+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Rolling 3 year Average of The Sanitation in Prison Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) 
    # }
    # 
    # ##Food in Prison
    # ##
    # 
    # if(input$justice_rights=='prison_food'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$avg_prison_food3),]
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   #rolling average plot
    #   plot <- ggplot(temp1, aes(x=year, y=avg_prison_food3)) +
    #     geom_line()+
    #     xlab("Years") + 
    #     ylab("Rolling Average")+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Rolling 3 year Average of The Food in Prison Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) 
    # }
    # 
    # ##Healthcare in Prison
    # ##
    # 
    # if(input$justice_rights=='prison_health'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$avg_prison_health3),]
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   #rolling average plot
    #   plot <- ggplot(temp1, aes(x=year, y=avg_prison_health3)) +
    #     geom_line()+
    #     xlab("Years") + 
    #     ylab("Rolling Average")+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Rolling 3 year Average of The Healthcare in Prison Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) 
    # }
    # 
    # ##Cleanliness in Prison
    # ##
    # 
    # if(input$justice_rights=='prison_clean'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$avg_prison_clean3),]
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   #rolling average plot
    #   plot <- ggplot(temp1, aes(x=year, y=avg_prison_clean3)) +
    #     geom_line()+
    #     xlab("Years") + 
    #     ylab("Rolling Average")+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Rolling 3 year Average of The Cleanliness in Prison Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) 
    # }
    # 
    # ##Family Access in Prison
    # ##
    # 
    # if(input$justice_rights=='prison_family'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$avg_prison_family3),]
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   #rolling average plot
    #   plot <- ggplot(temp1, aes(x=year, y=avg_prison_family3)) +
    #     geom_line()+
    #     xlab("Years") + 
    #     ylab("Rolling Average")+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Rolling 3 year Average of The Family Access in Prison Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) 
    # }
    # 
    # ##Record keeping in Prison
    # ##
    # 
    # if(input$justice_rights=='prison_records'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$avg_prison_records3),]
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   #rolling average plot
    #   plot <- ggplot(temp1, aes(x=year, y=avg_prison_records3)) +
    #     geom_line()+
    #     xlab("Years") + 
    #     ylab("Rolling Average")+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Rolling 3 year Average of The Record Keeping in Prison Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) 
    # }
    # 
    # ##Commitment Order in Prison
    # ##
    # 
    # if(input$justice_rights=='prison_commitment'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$avg_prison_commitment3),]
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   #rolling average plot
    #   plot <- ggplot(temp1, aes(x=year, y=avg_prison_commitment3)) +
    #     geom_line()+
    #     xlab("Years") + 
    #     ylab("Rolling Average")+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Rolling 3 year Average of The Commitment Order in Prison Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) 
    # }
    # 
    # ##Central Authority in Prison
    # ##
    # 
    # if(input$justice_rights=='prison_central'){
    #   #removing missing values
    #   temp1 <- temp1[!is.na(temp1$avg_prison_central3),]
    #   validate(
    #     need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
    #     )
    #   )
    #   #rolling average plot
    #   plot <- ggplot(temp1, aes(x=year, y=avg_prison_central3)) +
    #     geom_line()+
    #     xlab("Years") + 
    #     ylab("Rolling Average")+ 
    #     theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
    #     ggtitle(paste("Rolling 3 year Average of The Central Authority in Prison Score", "\n", "(", paste(input$country_justice, ":", sep=""), input$countryyearslider_justice[1], "-",  input$countryyearslider_justice[2], ")")) 
    # }

    print(plot)
  })
  
  output$summary_scores <- renderPlot({
    temp1 <- subset(hrdata, 
                    hrdata$country == input$country_summary & hrdata$year %in% seq(input$countryyearslider_summary[1], 
                                                                                   input$countryyearslider_summary[2], 1))
    ## The Right to a Fair Trial (law)
    
    if(input$summary_rights=="physint_sum"){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$physint_sum),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # # Convert column from numeric to factor variable
      # temp1$physint_sum <- as.factor(temp1$trial_l)
      # #For highlighting different scores
      # highlight_df_1<-temp1[temp1$trial_l == "1", ]  
      # highlight_df_2<-temp1[temp1$trial_l == "2", ]  
      # highlight_df_3<-temp1[temp1$trial_l == "3", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=physint_sum)) +
        geom_line()+
        xlab("Years") + 
        ylab("Score")+
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Physical Integrity Summary Score", "\n", "(", paste(input$country_summary, ":", sep=""), input$countryyearslider_summary[1], "-",  input$countryyearslider_summary[2], ")")) }
    
    
    if(input$summary_rights=="repression_sum"){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$repression_sum),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # # Convert column from numeric to factor variable
      # temp1$physint_sum <- as.factor(temp1$trial_l)
      # #For highlighting different scores
      # highlight_df_1<-temp1[temp1$trial_l == "1", ]  
      # highlight_df_2<-temp1[temp1$trial_l == "2", ]  
      # highlight_df_3<-temp1[temp1$trial_l == "3", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=repression_sum)) +
        geom_line()+
        xlab("Years") + 
        ylab("Score")+
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Repression Index", "\n", "(", paste(input$country_summary, ":", sep=""), input$countryyearslider_summary[1], "-",  input$countryyearslider_summary[2], ")")) }
    
    if(input$summary_rights=="civpol_sum"){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$civpol_sum),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # # Convert column from numeric to factor variable
      # temp1$physint_sum <- as.factor(temp1$trial_l)
      # #For highlighting different scores
      # highlight_df_1<-temp1[temp1$trial_l == "1", ]  
      # highlight_df_2<-temp1[temp1$trial_l == "2", ]  
      # highlight_df_3<-temp1[temp1$trial_l == "3", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=civpol_sum)) +
        geom_line()+
        xlab("Years") + 
        ylab("Score")+
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Civil and Political Rights Index", "\n", "(", paste(input$country_summary, ":", sep=""), input$countryyearslider_summary[1], "-",  input$countryyearslider_summary[2], ")")) }
    
    if(input$summary_rights=="workerrights_laws_sum"){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$workerrights_laws_sum),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # # Convert column from numeric to factor variable
      # temp1$physint_sum <- as.factor(temp1$trial_l)
      # #For highlighting different scores
      # highlight_df_1<-temp1[temp1$trial_l == "1", ]  
      # highlight_df_2<-temp1[temp1$trial_l == "2", ]  
      # highlight_df_3<-temp1[temp1$trial_l == "3", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=workerrights_laws_sum)) +
        geom_line()+
        xlab("Years") + 
        ylab("Score")+
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Worker Rights Laws", "\n", "(", paste(input$country_summary, ":", sep=""), input$countryyearslider_summary[1], "-",  input$countryyearslider_summary[2], ")")) }
    
    if(input$summary_rights=="workerrights_practices_sum"){
      #removing missing values
      temp1 <- temp1[!is.na(temp1$workerrights_practices_sum),]
      #Validating data
      validate(
        need(nrow(temp1)!=0, "Sorry, currently there is no data for this country, but we are working on it :) "
        )
      )
      # # Convert column from numeric to factor variable
      # temp1$physint_sum <- as.factor(temp1$trial_l)
      # #For highlighting different scores
      # highlight_df_1<-temp1[temp1$trial_l == "1", ]  
      # highlight_df_2<-temp1[temp1$trial_l == "2", ]  
      # highlight_df_3<-temp1[temp1$trial_l == "3", ]  
      
      plot <- ggplot(temp1, aes(x=year, y=workerrights_practices_sum)) +
        geom_line()+
        xlab("Years") + 
        ylab("Score")+
        theme(axis.title.y = element_text(size=14,face="bold"),axis.title.x = element_text(size=14,face="bold"),axis.text.x = element_text(size=14),axis.text.y = element_text( angle=90, vjust=1, hjust=.5,size=14 ))+
        ggtitle(paste("Worker Rights Practices", "\n", "(", paste(input$country_summary, ":", sep=""), input$countryyearslider_summary[1], "-",  input$countryyearslider_summary[2], ")")) }
    
    print(plot)   
  })
  
 
  
  
 ########################## Data Download ########################################### 
  ##  Download these data
  ## World
  output$downloadWorldData <- downloadHandler(
    filename = function() { paste("World","_",input$yearworld, 
                                  '.csv', 
                                  sep='') },
    content = function(file) {
      write.csv(subset(hrdata, hrdata$year == input$yearworld), file)
    })
  
  ## Region
  
  output$downloadRegionalData <- downloadHandler(
    filename = function() { paste(input$region,"_",
                                  input$yearreg,
                                  '.csv', 
                                  sep='') },
    content = function(file) {
      write.csv(subset(hrdata, hrdata$year == input$yearworld & hrdata$unreg==as.numeric(input$region)), file)
    })
  
  
  #Physical Integrity
  output$downloadPhysicalData <- downloadHandler(
    filename = function() { paste(input$country, "_", 
                                  input$countryyearslider[1], "-", 
                                  input$countryyearslider[2], 
                                  '.csv', 
                                  sep='') },
    content = function(file) {
      write.csv(subset(hrdata, hrdata$country == input$country), file)
    }
  )#physical integrity download ends here
  
  #Empowerment Rights
  output$ downloadEmpData <- downloadHandler(
    filename = function() { paste(input$country_emp, "_", 
                                  input$countryyearslider_emp[1], "-", 
                                  input$countryyearslider_emp[2], 
                                  '.csv', 
                                  sep='') },
    content = function(file) {
      write.csv(subset(hrdata, hrdata$country == input$country_emp), file)
    }
  )#empowerment rights download ends here
  
  #Workers' rights
  output$ downloadWorkData <- downloadHandler(
    filename = function() { paste(input$country_work, "_", 
                                  input$countryyearslider_work[1], "-", 
                                  input$countryyearslider_work[2], 
                                  '.csv', 
                                  sep='') },
    content = function(file) {
      write.csv(subset(hrdata, hrdata$country == input$country_work), file)
    }
  )#worker rights download ends here
  
  
  #Justice rights
  output$downloadJusticeData<-downloadHandler(
    filename = function() { paste(input$country_justice, "_", 
                                  input$countryyearslider_justice[1], "-", 
                                  input$countryyearslider_justice[2], 
                                  '.csv', 
                                  sep='') },
    content = function(file) {
      write.csv(subset(hrdata, hrdata$country == input$country_justice), file)
    })
  
  #Summary Scores
  output$downloadSummaryData<-downloadHandler(
    filename = function() { paste(input$country_summary, "_", 
                                  input$countryyearslider_summary[1], "-", 
                                  input$countryyearslider_summary[2], 
                                  '.csv', 
                                  sep='') },
    content = function(file) {
      write.csv(subset(hrdata, hrdata$country == input$country_summary), file)
    })
 
  
  
}