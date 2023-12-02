
library(shiny)
library(ggplot2)
library(validate)
library(ggiraph)
library(RColorBrewer)
library(viridis)
library(stringr)
pacman::p_load(spData, tmap, tmaptools, gifski, rnaturalearth, rnaturalearthdata,dplyr) 

urlhrdata<-"https://raw.githubusercontent.com/skipmark/CIRIGHTS/main/cirights_website_rolling_avg_final.csv"
hrdata <- read.csv(urlhrdata)
mapdata<-readRDS("./Data/cirights_website_12_2023_nona.rds")


navbarPage("CIRIGHTS Analytics",
           tabPanel("World Trends",
                    tmapOutput("world_map",height = '400', width = '100%'),
                    hr(),
                    fluidRow(
                      column(4, offset=5,
                             sliderInput("yearworld", "Choose year",min=2005, max=2021,value=2005,
                                         sep="",animate=TRUE),#sliderInput ends
                             downloadButton("downloadWorldData", "Download these data"),
                             helpText("Note: Downloads user-selected year.") )
                    )), #World trends end
           tabPanel("Regional Trends",
                    tmapOutput("reg_map",height = '400', width = '100%'),
                    hr(),
                    fluidRow(
                      column(4, offset=5,
                             selectInput("region", "Choose a region:",
                                         c("Africa" = "africa",
                                           "Oceania" = 'oceania',
                                           "Americas" = 'americas',
                                           "Asia & Middle East" = 'asia_middle_east',
                                           "Europe"="europe")), #selectInput ends
                             sliderInput("yearreg", "Choose year",min=2005, max=2021,value=2005,
                                         sep="", animate=TRUE),#sliderInput ends
                             downloadButton("downloadRegionalData", "Download these data"),
                             helpText("Note: Downloads user-selected year.") )
                    )

           ), #Regional trends end

           tabPanel("Physical Integrity Rights",
                    sidebarLayout(
                      sidebarPanel(##Select which country you want
                        selectInput("country", "Choose country:",
                                    choices=as.character(sort(unique(hrdata$country))),
                                    selected="Afghanistan"), #selectInput ends
                        #select a type of physical integrity rights you want
                        selectInput("phys_int_type", "Choose a category of Physical integrity rights:",
                                    c("Physical Integrity Index" = "physint_sum",
                                      "Political/Extrajudicial Killings" = 'kill',
                                      "Disappearance" = 'disap',
                                      "Political Imprisonment" = 'polpris',
                                      "Torture" = 'tort',
                                      "Brutality-based Mass Atrocity"="bbatrocity_intensity")), #selectInput ends
                        sliderInput("countryyearslider", "Choose year",
                                    min=2005, max=2021,
                                    value=c(2005, 2021),
                                    sep=""),#sliderInput ends
                        downloadButton('downloadPhysicalData', "Download these data"),
                        helpText("Note: Downloads user-selected country and year range.")
                      ),#sidebarPanel ends
                      #Create a spot for  plot
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Raw Country Scores",
                                   plotOutput("phint_raw_scores")), #raw scores for each year
                          tabPanel("Rolling Average", plotOutput("phint_roll_avg"),
                          ), #physical integrity rights end
                        )#tabsetPanel ends
                      )#mainPanel ends
                    ) #sidebarLayout ends
           ),#Physical Integrity Rights end here
           tabPanel("Empowerment Rights & Freedoms",
                    sidebarLayout(
                      sidebarPanel(##Select which country you want
                        selectInput("country_emp", "Choose country:",
                                    choices=as.character(sort(unique(hrdata$country))),
                                    selected="Afghanistan"), #selectInput ends
                        #select a type of empowerment rights you want
                        selectInput("emp_rights", "Choose a category of Empowerment rights:",
                                    c("Empowerment Rights Index" ="emp_rights_sum",
                                      "Freedom of Domestic Movement" = 'dommov',
                                      "Freedom of Foreign Movement" = 'formov',
                                      "Electoral Self-Determination"="elecsd",
                                      "Women's Political Rights"="wopol",
                                      "Women's Social Rights (Law)"="wosoc_l",
                                      "Women's Social Rights (Practice)"="wosoc_p",
                                      "Women's Economic Rights"="wecon")), #selectInput ends
                        sliderInput("countryyearslider_emp", "Choose year",
                                    min=2005, max=2021,
                                    value=c(2005, 2021),
                                    sep=""),#sliderInput ends
                        downloadButton('downloadEmpData', "Download these data"),
                        helpText("Note: Downloads user-selected country and year range.")
                      ),#sidebarPanel ends
                      #Create a spoty for line plot
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Raw Country Scores",
                                   plotOutput("emp_raw_scores")), #raw scores for each year
                          tabPanel("Rolling Average", plotOutput("emp_roll_avg"),
                          ), #demands ends
                        )#tabsetPanel ends
                      )#mainPanel ends
                    ) #sidebarLayout ends
           ), #Empowerment Rights and Freedoms end here
           tabPanel("Civil and Political Rights",
                    sidebarLayout(
                      sidebarPanel(##Select which country you want
                        selectInput("country_civpol", "Choose country:",
                                    choices=as.character(sort(unique(hrdata$country))),
                                    selected="Afghanistan"), #selectInput ends
                        #select a type of empowerment rights you want
                        selectInput("civpol_rights", "Choose a category of Empowerment rights:",
                                    c("Civil and Political Rights Index" ="civpol_sum",
                                       "Freedom of Speech and Press" = 'speech',
                                       "Freedom of Religion" = 'rel_free',
                                       "Freedom of Assembly and Association"="assn")), #selectInput ends
                        sliderInput("countryyearslider_civpol", "Choose year",
                                    min=2005, max=2021,
                                    value=c(2005, 2021),
                                    sep=""),#sliderInput ends
                        downloadButton('downloadCivPolData', "Download these data"),
                        helpText("Note: Downloads user-selected country and year range.")
                      ),#sidebarPanel ends
                      #Create a spoty for line plot
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Raw Country Scores",
                                   plotOutput("civpol_raw_scores")), #raw scores for each year
                          tabPanel("Rolling Average", plotOutput("civpol_roll_avg"),
                          ), #demands ends
                        )#tabsetPanel ends
                      )#mainPanel ends
                    ) #sidebarLayout ends
           ), #Civil and Political Rights end here
           tabPanel("Workers' Rights",
                    sidebarLayout(
                      sidebarPanel(##Select which country you want
                        selectInput("country_work", "Choose country:",
                                    choices=as.character(sort(unique(hrdata$country))),
                                    selected="Afghanistan"), #selectInput ends
                        #select a type of workers' rights you want
                        selectInput("workers_rights", "Choose a category of Workers' rights:",
                                    c("Worker Rights Practice Index" ="workerrights_practices_sum",
                                      "Worker Rights Laws Index" = "workerrights_laws_sum",
                                      "The Right to Form Worker Unions (Law)" = 'union_l',
                                      "The Right to Form Worker Unions (Practice)" = 'union_p',
                                      "The Right to Bargain Collectively (Law)"="barg_l",
                                      "The Right to Bargain Collectively (Practice)"="barg_p",
                                      "The Right to be Free from Forced Labor (Law)"="force_l",
                                      "The Right to be Free from Forced Labor (Practice)"="force_p",
                                      "Children's Rights (Law)"="child_l",
                                      "Children's Rights (Practice)"="child_p",
                                      "The Right to a Minimum Wage (Law)"="wage_l",
                                      "The Right to a Minimum Wage (Practice)"="wage_p",
                                      "The Right to Occupational Safety (Law)"="safe_l",
                                      "The Right to Occupational Safety (Practice)"="safe_p",
                                      "Reasonable Limitation on Working Hours (Law)"="hour_l",
                                      "Reasonable Limitation on Working Hours (Practice)"="hour_p",
                                      "Human Trafficking (Law)"="trafficking_l",
                                      "Human Trafficking (Practice)"="trafficking_p"
                                      # "Discrimination based on race/color"="discri_race",
                                      # "Discrimination based on gender (sex)"="discri_gender",
                                      # "Discrimination based on nationality (citizenship, origin, migrant workers)"="discri_nationality",
                                      # "Discrimination based on ethnicity"="discri_ethnicity",
                                      # "Discrimination based on religion (creed)"='discri_religion',
                                      # "Discrimination based on sexual orientation"= "discri_sexuality",
                                      # "Discrimination based on HIV-AIDS"='discri_AIDS',
                                      # "Discrimination based on social origin"="discri_socialorigin",
                                      # "Discrimination based on political beliefs"="discri_polibelief",
                                      # "Discrimination based on disability"="discri_disability"
                                    )), #selectInput ends
                        sliderInput("countryyearslider_work", "Choose year",
                                    min=2005, max=2021,
                                    value=c(2005, 2021),
                                    sep=""),#sliderInput ends
                        downloadButton('downloadWorkData', "Download these data"),
                        helpText("Note: Downloads user-selected country and year range.")
                      ),#sidebarPanel ends
                      #Create a spoty for line plot
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Raw Country Scores",
                                   plotOutput("work_raw_scores")),#raw scores for each year
                          tabPanel("Rolling Average", plotOutput("work_roll_average"),
                          ), #demands ends
                        )#tabsetPanel ends
                      )#mainPanel ends
                    ) #sidebarLayout ends
           ), #Workers' right section end here
          tabPanel("Justice Rights",
                   sidebarLayout(
                     sidebarPanel(##Select which country you want
                       selectInput("country_justice", "Choose country:",
                                   choices=as.character(sort(unique(hrdata$country))),
                                   selected="Afghanistan"), #selectInput ends
                       #select a type of justice rights you want
                       selectInput("justice_rights", "Choose a category of Justice rights:",
                                   c("Right to a Fair Trial (Law)"="trial_l",
                                     "Right to a Fair Trial (Practice)"="trial_p",
                                     "Independence of the Judiciary"="injud",
                                     "Human Rights NGO Freedom" ="ngo_freedom"
                                     # ,
                                     # "Torture in Prison"="prison_torture",
                                     # "Discrimination in Prison"="prison_discriminate",
                                     # "Rehabilitation in Prison"="prison_rehabilitate",
                                     # "Overcrowding in Prison"="prison_overcrowd",
                                     # "Separation in Prison"="prison_separation",
                                     # "Sanitation in Prison"="prison_sanitation",
                                     # "Food in Prison"="prison_food",
                                     # "Healthcare in Prison"="prison_health",
                                     # "Cleanliness in Prison"="prison_clean",
                                     # "Family Access in Prison"="prison_family",
                                     # "Record keeping in Prison"="prison_records",
                                     # "Commitment Order in Prison"="prison_commitment",
                                     # "Central Authority in Prison"="prison_central"

                                   )), #selectInput ends
                       sliderInput("countryyearslider_justice", "Choose year",
                                   min=2005, max=2021,
                                   value=c(2005, max=2021),
                                   sep=""),#sliderInput ends
                       downloadButton('downloadJusticeData', "Download these data"),
                       helpText("Note: Downloads user-selected country and year range.")
                     ),#sidebarPanel ends
                     #Create a spoty for line plot
                     mainPanel(
                       tabsetPanel(
                         tabPanel("Raw Country Scores",
                                  plotOutput("justice_raw_scores")), #raw scores for each year
                         tabPanel("Rolling Average", plotOutput("justice_roll_avg"),
                         ), #demands ends
                       )#tabsetPanel ends
                     )#mainPanel ends
                   ) #sidebarLayout ends
          )#Justice rights end here

          
          
)



