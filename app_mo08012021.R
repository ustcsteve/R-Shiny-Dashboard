library(tidyverse)
library(purrr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(shinythemes)
library(RColorBrewer)
library(ggthemes)
library(ggtext)


#https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-06-08/readme.md
#http://www.glfc.org/fishstocking/dbstruct.htm

#load data
#you may load from your own local drive
tuesdata <- tidytuesdayR::tt_load(2021, week = 24)
stocked <- tuesdata$stocked
df<-stocked

#remove unnecessary columns
df = df %>%
    select(c('YEAR','MONTH','DAY', 'LAKE','STATE_PROV','SPECIES','NO_STOCKED','AGEMONTH', 'WEIGHT', 'CONDITION'))

#remove decimals from condition variables
df$CONDITION = trunc(df$CONDITION)

#impute missing values
df$MONTH = with(df, impute(MONTH, median))
df$DAY = with(df, impute(DAY, median))
df$CONDITION = with(df, impute(CONDITION, median))

#view obs where day = 0 and change it to 1
subset(df, DAY==0)
df[df$DAY==0,'DAY'] = 1
subset(df, DAY==0)  

#convert the condition numeric variable to dummy
df$CONDITION = as.factor(df$CONDITION)

# write.csv(df,"stocked_clean.csv", row.names = FALSE)
# List of 6 lakes
lake_list<-list("Michigan" = "MI",
        "Superior"="SU",
        "Ontario" = "ON",
        "Erie"="ER", 
        "Huron"="HU", 
        "Saint Clair"="SC") 

# List of species
species_list<-list("Lake Trout"="LAT", "Rainbow Trout"="RBT","Brook Trout"= "BKT","Brown Trout"="BNT",
                   "Coho Salmon"="COS","Chinook Salmon"= "CHS", "Atlantic Salmon"= "ATS", "Tiger trout"="TRT",
                    "Splake"= "SPE", "Walleye"= "WAE", "Yellow Perch"= "YEP","Muskllunge"= "MUE", "Smallmoouth Bass"= "SMB",
                "Lake Sturgeon"="LAS", "Tiger Muskellunge"= "TIM", "Northern Pike"= "NOP", "Sturgeon"= "STN", "Lake Herring"= "LAH")

#List of Conditions
condition_list = list("Unknown"=0, "Excellent"=1, "Good"=2, "Fair"=3, "Bad"=4, "Very Bad"=5, "Mortality is Reported"=6, "Distressed or Sick"=7)


ui <- fluidPage(
    theme = shinytheme("cerulean"),
    titlePanel("Great Lakes Fish Stock"), # Application title

    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "lake", "Choose a lake:",
                        choices = lake_list),
            actionButton(inputId = "action", "Run"),
            hr(),
            h3("Date Range"),
            sliderInput(inputId = "YEAR", "Year:", min = 1950,  max = 2018, value = 2018, step = 1, sep=""),  

        ),
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Fish Stock Number", value = "fish_stock_number", plotOutput(outputId = "fish_number", height = "400px", width = "400px")),
                        tabPanel("Fish Stock Weight", value = "fish_stock_weight", plotOutput(outputId = "fish_weight", height = "400px", width = "400px")),
                        tabPanel("Fish Stock Condition", value = "fish_stock_condition", plotOutput(outputId = "fish_condition", height = "400px", width = "600px"))
            )
        )
    )
)

server <- function(input, output) {
    fish <- eventReactive( 
        input$action,{ 
            withProgress({
                setProgress(message = "Getting Fish...")
                df%>%filter(df$YEAR==input$YEAR)
            })
        }
    )
    
    output$fish_number<-renderPlot({
        v <- fish()
        pal <- brewer.pal(8,"Dark2")
        
        v %>% 
            filter(v$LAKE==input$lake)%>%
            ggplot(aes(x=SPECIES, y=NO_STOCKED, fill = STATE_PROV))+
            geom_bar(stat="identity")+
            labs(title = "Number of Fish Stocked",
                 x = "State Province", y = "Total Number of Fish Stocked")+
            theme_wsj() +
            theme(
                text = element_text(size = input$fontsize),
                axis.title = element_blank()
            ) +
            facet_wrap(~STATE_PROV, scales = "free_y")+
            theme(legend.position = "none")+
            theme(plot.title = element_markdown(size = 18, hjust = 0.5),
                  axis.title.x.bottom=element_text(size =14, face="bold", vjust=-5.0),
                  axis.title.y.left=element_text(size =14, face="bold", vjust=5.0),
                  plot.margin=unit(c(1,1,1,1), "cm"))
    })
    output$fish_weight<-renderPlot({
        v <- fish()
        pal <- brewer.pal(8,"Dark2")
        
        v %>% 
            filter(v$LAKE==input$lake)%>%
            ggplot(aes(x=SPECIES, y=WEIGHT, fill = STATE_PROV))+
            geom_bar(stat="identity")+
            labs(title = "Weight of Fished Stocked",
                 x = "State Province", y = "Weight")+
            theme_wsj() +
            theme(
                text = element_text(size = input$fontsize),
                axis.title = element_blank(),
            )+
            facet_wrap(~STATE_PROV, scales = "free_y")+
            theme(legend.position = "none")+
            theme(plot.title = element_markdown(size = 18, hjust = 0.5),
                  axis.title.x.bottom=element_text(size =14, face="bold", vjust=-5.0),
                  axis.title.y.left=element_text(size =14, face="bold", vjust=5.0),
                  plot.margin=unit(c(1,1,1,1), "cm"))
    })
    output$fish_condition<-renderPlot({
        v <- fish()
        pal <- brewer.pal(8,"Dark2")
        
        v %>% 
            filter(v$LAKE==input$lake)%>%
            ggplot(aes(x=SPECIES, y=NO_STOCKED, fill = CONDITION))+
            geom_bar(position= "dodge", stat="identity")+
            labs(title = "Condition of Fish Stocked",
                 x = "Condition", y = "Total Number of Fish Stocked")+
            theme_wsj() +
            theme(
                text = element_text(size = input$fontsize),
                axis.title = element_blank()
            )+
            facet_wrap(~STATE_PROV, scales = "free_y") +
            theme(legend.position = "bottom")+
            theme(plot.title = element_markdown(size = 18, hjust = 0.5),
                  axis.title.x.bottom=element_text(size =14, face="bold", vjust=-5.0),
                  axis.title.y.left=element_text(size =14, face="bold", vjust=5.0),
                  plot.margin=unit(c(1,1,1,1), "cm"))
    })
    output$fish_age<-renderPlot({
        v <- fish()
        pal <- brewer.pal(8,"Dark2")
        
        v %>% 
            filter(v$LAKE==input$lake)%>%
            ggplot(aes(x=SPECIES, y=NO_STOCKED, fill = AGEMONTH))+
            geom_bar(stat="identity")+
            labs(title = "Fish Condition in Great Lakes",
                 x = "State Province", y = "AgeMonth")+
            theme_wsj() +
            theme(
                text = element_text(size = input$fontsize),
                axis.title = element_blank(),
            )+
            facet_wrap(~STATE_PROV, scales = "free_y") +
            theme(legend.position = "top")+
            theme(plot.title = element_markdown(size = 18, hjust = 0.5))
    })
}

shinyApp(ui = ui, server = server)
