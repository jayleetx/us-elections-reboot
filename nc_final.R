#read in voter info
temp <- tempfile()
download.file("https://s3.amazonaws.com/dl.ncsbe.gov/data/ncvoter_Statewide.zip", temp)
nc_info <- read_tsv(unz(temp, "ncvoter_Statewide.txt"))
unlink(temp)

# tidy voter information 
nc_info_tidy <- select(nc_info, voter_reg_num, ncid, status_cd, zip_code, race_code, ethnic_code, party_cd, gender_code, birth_age, confidential_ind) %>%
  mutate(unique_id = paste(voter_reg_num, ncid, sep = "")) %>%
  select(-voter_reg_num, -ncid)

#read in voter history  
temp_his <- tempfile()
download.file("https://s3.amazonaws.com/dl.ncsbe.gov/data/ncvhis_Statewide.zip", temp_his)
nc_his <- read_tsv(unz(temp_his, "ncvhis_Statewide.txt"))
unlink(temp_his)

# tidy voter history 
nc_his_tidy <- select(nc_his, county_id, voter_reg_num, ncid, election_lbl, voting_method, voted_party_cd, pct_label) %>%
  mutate(unique_id = paste(voter_reg_num, ncid, sep = "")) %>%
  select(-voter_reg_num, -ncid)

# joining history and information
nc_voter <- inner_join(nc_his_tidy, nc_info_tidy, by = "unique_id")

# cleaning birth age column
nc_voter <- mutate(nc_voter, birth_age = as.integer(birth_age)) %>%
  filter(birth_age <= 100)

# subsetting data
set.seed(2017)
nc_voter_small <- sample_n(nc_voter, 10000)

# renaming columns
nc_voter_small <- rename(nc_voter_small, Age = birth_age, Gender = gender_code, Race = race_code, `Party Affiliation` = party_cd)


#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
#nc_voter_small_orig <- nc_voter_small
nc_voter_small_arrange <- nc_voter_small

nc_voter_small$election_lbl <- as.Date(nc_voter_small$election_lbl, "%m/%d/%Y")
nc_voter_small <- arrange(nc_voter_small, desc(election_lbl))
nc_voter_small_arrange$election_lbl <- as.character((nc_voter_small$election_lbl))


tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                 height: 750px;
                                 -webkit-column-count: 2; /* Chrome, Safari, Opera */ 
                                 -moz-column-count: 2;    /* Firefox */ 
                                 column-count: 2; 
                                 -moz-column-fill: balance;
                                 -column-fill: balance;
                                 } 
                                 ")) 
  ))
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"), tweaks, 
                
                # Application title
                title = "NC Elections",
                
                hr(),
                
                column(9,
                       plotOutput("distPlot"),
                       
                       hr(),
                       
                       fluidRow(
                         column(4,
                                h4("NC ELECTIONS EXPLORER"),
                                selectInput(inputId = "xvar", 
                                            label = "What would you like on the x-axis?:", 
                                            choices = names(select(nc_voter_small, Gender, Age, Race, `Party Affiliation`)),
                                            selected = "Age"
                                ),
                                conditionalPanel(condition = "input.xvar == 'Gender'",
                                                 checkboxGroupInput(inputId = "gender_x",
                                                                    label = strong("Select Gender(s) of Interest"),
                                                                    choices = unique(nc_voter_small$Gender),
                                                                    selected = unique(nc_voter_small$Gender)
                                                 )
                                                 
                                ),
                                conditionalPanel(condition = "input.xvar == 'Race'",
                                                 checkboxGroupInput(inputId = "race_x",
                                                                    label = strong("Select Race(s) of Interest"),
                                                                    choices = unique(nc_voter_small$Race)
                                                 )
                                ),
                                conditionalPanel(condition = "input.xvar == 'Party Affiliation'",
                                                 checkboxGroupInput(inputId = "party_x",
                                                                    label = strong("Select Party(ies) of Interest"),
                                                                    choices = unique(nc_voter_small$`Party Affiliation`)
                                                 )
                                )
                         ),
                         column(4,
                                checkboxInput(inputId = "color",
                                              label = strong("Click here to add color"),
                                              value = FALSE
                                ),
                                conditionalPanel(condition = "input.color == true",
                                                 selectInput(inputId = "colorvar",
                                                             label = "What would you like to map to color?:",
                                                             choices = c("",names(select(nc_voter_small, Gender, Race, `Party Affiliation`))),
                                                             selected = ""
                                                 ),
                                                 conditionalPanel(condition = "input.colorvar == 'Gender'",
                                                                  checkboxGroupInput(inputId = "gender_color",
                                                                                     label = strong("Select Gender(s) of Interest"),
                                                                                     choices = unique(nc_voter_small$Gender)
                                                                  )
                                                                  
                                                 ),
                                                 conditionalPanel(condition = "input.colorvar == 'Race'",
                                                                  checkboxGroupInput(inputId = "race_color",
                                                                                     label = strong("Select Race(s) of Interest"),
                                                                                     choices = unique(nc_voter_small$Race)
                                                                  )
                                                 ),
                                                 conditionalPanel(condition = "input.colorvar == 'Party Affiliation'",
                                                                  checkboxGroupInput(inputId = "party_color",
                                                                                     label = strong("Select Party(ies) of Interest"),
                                                                                     choices = unique(nc_voter_small$`Party Affiliation`)
                                                                  )
                                                 )
                                )
                         ),
                         column(4,
                                checkboxInput(inputId = "facet",
                                              label = strong("Click here to add small multiples"),
                                              value = FALSE
                                ),
                                conditionalPanel(condition = "input.facet == true",
                                                 selectInput(inputId = "facetvar",
                                                             label = "By what would you like to split into small multiples?:",
                                                             choices = c("",names(select(nc_voter_small, Gender, Race, `Party Affiliation`))),
                                                             selected = ""
                                                 ),
                                                 conditionalPanel(condition = "input.facetvar == 'Gender'",
                                                                  checkboxGroupInput(inputId = "gender_facet",
                                                                                     label = strong("Select Gender(s) of Interest"),
                                                                                     choices = unique(nc_voter_small$Gender)
                                                                  )
                                                                  
                                                 ),
                                                 conditionalPanel(condition = "input.facetvar == 'Race'",
                                                                  checkboxGroupInput(inputId = "race_facet",
                                                                                     label = strong("Select Race(s) of Interest"),
                                                                                     choices = unique(nc_voter_small$Race)
                                                                  )
                                                 ),
                                                 conditionalPanel(condition = "input.facetvar == 'Party Affiliation'",
                                                                  checkboxGroupInput(inputId = "party_facet",
                                                                                     label = strong("Select Party(ies) of Interest"),
                                                                                     choices = unique(nc_voter_small$`Party Affiliation`)
                                                                  )
                                                 )
                                )
                         )
                       ),
                       column(12,
                              conditionalPanel(condition = "(input.xvar == input.colorvar && input.colorvar != '') || (input.xvar == input.facetvar && input.facetvar != '') || (input.colorvar == input.facetvar && input.facetvar != '')",
                                               "*Edward Tufte would advise against mapping the same variable to two or more visual cues; this program may not function properly."
                              )
                       )
                ),
                column(3,
                       #checkboxGroupInput(inputId = "election",
                       #                   label = "Which election(s) are you interested in?:",
                       #                   choices = unique(nc_voter_small$election_lbl)
                       #)
                       tags$div(list(tags$label("Which elections are you interested in?"), 
                                     tags$div(class = "multicol", 
                                              checkboxGroupInput(inputId  = "elections", 
                                                                 label    = NULL, 
                                                                 choices  = unique(nc_voter_small_arrange$election_lbl),
                                                                 inline   = FALSE)
                                     )
                       )
                       )
                )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    xreact <- input$xvar
    facetreact <- input$facetvar
    colorreact <- input$colorvar
    nc_voter_small <- filter(nc_voter_small_arrange, election_lbl %in% input$elections)
    if (input$color) {
      if (input$facet) {
        if (facetreact == "Gender") {
          if (colorreact == "Gender") {
            if (xreact == "Gender") {
              filter(nc_voter_small, Gender %in% input$gender_x) %>%
                filter(Gender %in% input$gender_color) %>%
                filter(Gender %in% input$gender_facet) %>%
                ggplot(aes(x = Gender)) +
                geom_bar(stat = "count", aes(fill = Gender)) +
                facet_wrap(~Gender)
            }
            else if (xreact == "Race") {
              filter(nc_voter_small, Race %in% input$race_x) %>%
                filter(Gender %in% input$gender_color) %>%
                filter(Gender %in% input$gender_facet) %>%
                ggplot(aes(x = Race)) +
                geom_bar(stat = "count", aes(fill = Gender)) +
                facet_wrap(~Gender)
            }
            else if (xreact == "Party Affiliation") {
              filter(nc_voter_small, `Party Affiliation` %in% input$party_x) %>%
                filter(Gender %in% input$gender_color) %>%
                filter(Gender %in% input$gender_facet) %>%
                ggplot(aes(x = `Party Affiliation`)) +
                geom_bar(stat = "count", aes(fill = Gender)) +
                facet_wrap(~Gender)
            }
            else {
              filter(nc_voter_small, Gender %in% input$gender_color) %>%
                filter(Gender %in% input$gender_facet) %>%
                ggplot(aes(x = Age)) +
                geom_bar(stat = "count", aes(fill = Gender)) +
                facet_wrap(~Gender)
            }
          }
          # -------------------------------------------------------------
          else if (colorreact == "Race") {
            if (xreact == "Gender") {
              filter(nc_voter_small, Gender %in% input$gender_x) %>%
                filter(Race %in% input$race_color) %>%
                filter(Gender %in% input$gender_facet) %>%
                ggplot(aes(x = Gender)) +
                geom_bar(stat = "count", aes(fill = Race)) +
                facet_wrap(~Gender)
            }
            else if (xreact == "Race") {
              filter(nc_voter_small, Race %in% input$race_x) %>%
                filter(Race %in% input$race_color) %>%
                filter(Gender %in% input$gender_facet) %>%
                ggplot(aes(x = Race)) +
                geom_bar(stat = "count", aes(fill = Race)) +
                facet_wrap(~Gender)
            }
            else if (xreact == "Party Affiliation") {
              filter(nc_voter_small, `Party Affiliation` %in% input$party_x) %>%
                filter(Race %in% input$race_color) %>%
                filter(Gender %in% input$gender_facet) %>%
                ggplot(aes(x = `Party Affiliation`)) +
                geom_bar(stat = "count", aes(fill = Race)) +
                facet_wrap(~Gender)
            }
            else {
              filter(nc_voter_small, Race %in% input$race_color) %>%
                filter(Gender %in% input$gender_facet) %>%
                ggplot(aes(x = Age)) +
                geom_bar(stat = "count", aes(fill = Race)) +
                facet_wrap(~Gender)
            }
          }
          # -------------------------------------------------------------
          else {
            if (xreact == "Gender") {
              filter(nc_voter_small, Gender %in% input$gender_x) %>%
                filter(`Party Affiliation` %in% input$party_color) %>%
                filter(Gender %in% input$gender_facet) %>%
                ggplot(aes(x = Gender)) +
                geom_bar(stat = "count", aes(fill = `Party Affiliation`)) +
                facet_wrap(~Gender)
            }
            else if (xreact == "Race") {
              filter(nc_voter_small, Race %in% input$race_x) %>%
                filter(`Party Affiliation` %in% input$party_color) %>%
                filter(Gender %in% input$gender_facet) %>%
                ggplot(aes(x = Race)) +
                geom_bar(stat = "count", aes(fill = `Party Affiliation`)) +
                facet_wrap(~Gender)
            }
            else if (xreact == "Party Affiliation") {
              filter(nc_voter_small, `Party Affiliation` %in% input$party_x) %>%
                filter(`Party Affiliation` %in% input$party_color) %>%
                filter(Gender %in% input$gender_facet) %>%
                ggplot(aes(x = `Party Affiliation`)) +
                geom_bar(stat = "count", aes(fill = `Party Affiliation`)) +
                facet_wrap(~Gender)
            }
            else {
              filter(nc_voter_small, `Party Affiliation` %in% input$party_color) %>%
                filter(Gender %in% input$gender_facet) %>%
                ggplot(aes(x = Age)) +
                geom_bar(stat = "count", aes(fill = `Party Affiliation`)) +
                facet_wrap(~Gender)
            }
          }
        }
        # --------------------------------------------------------------------------------------------------------------------------------------------
        else if (facetreact == "Race") {
          if (colorreact == "Gender") {
            if (xreact == "Gender") {
              filter(nc_voter_small, Gender %in% input$gender_x) %>%
                filter(Gender %in% input$gender_color) %>%
                filter(Race %in% input$race_facet) %>%
                ggplot(aes(x = Gender)) +
                geom_bar(stat = "count", aes(fill = Gender)) +
                facet_wrap(~Race)
            }
            else if (xreact == "Race") {
              filter(nc_voter_small, Race %in% input$race_x) %>%
                filter(Gender %in% input$gender_color) %>%
                filter(Race %in% input$race_facet) %>%
                ggplot(aes(x = Race)) +
                geom_bar(stat = "count", aes(fill = Gender)) +
                facet_wrap(~Race)
            }
            else if (xreact == "Party Affiliation") {
              filter(nc_voter_small, `Party Affiliation` %in% input$party_x) %>%
                filter(Gender %in% input$gender_color) %>%
                filter(Race %in% input$race_facet) %>%
                ggplot(aes(x = `Party Affiliation`)) +
                geom_bar(stat = "count", aes(fill = Gender)) +
                facet_wrap(~Race)
            }
            else {
              filter(nc_voter_small, Gender %in% input$gender_color) %>%
                filter(Race %in% input$race_facet) %>%
                ggplot(aes(x = Age)) +
                geom_bar(stat = "count", aes(fill = Gender)) +
                facet_wrap(~Race)
            }
          }
          # -------------------------------------------------------------
          else if (colorreact == "Race") {
            if (xreact == "Gender") {
              filter(nc_voter_small, Gender %in% input$gender_x) %>%
                filter(Race %in% input$race_color) %>%
                filter(Race %in% input$race_facet) %>%
                ggplot(aes(x = Gender)) +
                geom_bar(stat = "count", aes(fill = Race)) +
                facet_wrap(~Race)
            }
            else if (xreact == "Race") {
              filter(nc_voter_small, Race %in% input$race_x) %>%
                filter(Race %in% input$race_color) %>%
                filter(Race %in% input$race_facet) %>%
                ggplot(aes(x = Race)) +
                geom_bar(stat = "count", aes(fill = Race)) +
                facet_wrap(~Race)
            }
            else if (xreact == "Party Affiliation") {
              filter(nc_voter_small, `Party Affiliation` %in% input$party_x) %>%
                filter(Race %in% input$race_color) %>%
                filter(Race %in% input$race_facet) %>%
                ggplot(aes(x = `Party Affiliation`)) +
                geom_bar(stat = "count", aes(fill = Race)) +
                facet_wrap(~Race)
            }
            else {
              filter(nc_voter_small, Race %in% input$race_color) %>%
                filter(Race %in% input$race_facet) %>%
                ggplot(aes(x = Age)) +
                geom_bar(stat = "count", aes(fill = Race)) +
                facet_wrap(~Race)
            }
          }
          # -------------------------------------------------------------
          else {
            if (xreact == "Gender") {
              filter(nc_voter_small, Gender %in% input$gender_x) %>%
                filter(`Party Affiliation` %in% input$party_color) %>%
                filter(Race %in% input$race_facet) %>%
                ggplot(aes(x = Gender)) +
                geom_bar(stat = "count", aes(fill = `Party Affiliation`)) +
                facet_wrap(~Race)
            }
            else if (xreact == "Race") {
              filter(nc_voter_small, Race %in% input$race_x) %>%
                filter(`Party Affiliation` %in% input$party_color) %>%
                filter(Race %in% input$race_facet) %>%
                ggplot(aes(x = Race)) +
                geom_bar(stat = "count", aes(fill = `Party Affiliation`)) +
                facet_wrap(~Race)
            }
            else if (xreact == "Party Affiliation") {
              filter(nc_voter_small, `Party Affiliation` %in% input$party_x) %>%
                filter(`Party Affiliation` %in% input$party_color) %>%
                filter(Race %in% input$race_facet) %>%
                ggplot(aes(x = `Party Affiliation`)) +
                geom_bar(stat = "count", aes(fill = `Party Affiliation`)) +
                facet_wrap(~Race)
            }
            else {
              filter(nc_voter_small, `Party Affiliation` %in% input$party_color) %>%
                filter(Race %in% input$race_facet) %>%
                ggplot(aes(x = Age)) +
                geom_bar(stat = "count", aes(fill = `Party Affiliation`)) +
                facet_wrap(~Race)
            }
          }
        }
        # -------------------------------------------------------------------------------------------------------------------------------
        else {
          if (colorreact == "Gender") {
            if (xreact == "Gender") {
              filter(nc_voter_small, Gender %in% input$gender_x) %>%
                filter(Gender %in% input$gender_color) %>%
                filter(`Party Affiliation` %in% input$party_facet) %>%
                ggplot(aes(x = Gender)) +
                geom_bar(stat = "count", aes(fill = Gender)) +
                facet_wrap(~`Party Affiliation`)
            }
            else if (xreact == "Race") {
              filter(nc_voter_small, Race %in% input$race_x) %>%
                filter(Gender %in% input$gender_color) %>%
                filter(`Party Affiliation` %in% input$party_facet) %>%
                ggplot(aes(x = Race)) +
                geom_bar(stat = "count", aes(fill = Gender)) +
                facet_wrap(~`Party Affiliation`)
            }
            else if (xreact == "Party Affiliation") {
              filter(nc_voter_small, `Party Affiliation` %in% input$party_x) %>%
                filter(Gender %in% input$gender_color) %>%
                filter(`Party Affiliation` %in% input$party_facet) %>%
                ggplot(aes(x = `Party Affiliation`)) +
                geom_bar(stat = "count", aes(fill = Gender)) +
                facet_wrap(~`Party Affiliation`)
            }
            else {
              filter(nc_voter_small, Gender %in% input$gender_color) %>%
                filter(`Party Affiliation` %in% input$party_facet) %>%
                ggplot(aes(x = Age)) +
                geom_bar(stat = "count", aes(fill = Gender)) +
                facet_wrap(~`Party Affiliation`)
            }
          }
          # -------------------------------------------------------------
          else if (colorreact == "Race") {
            if (xreact == "Gender") {
              filter(nc_voter_small, Gender %in% input$gender_x) %>%
                filter(Race %in% input$race_color) %>%
                filter(`Party Affiliation` %in% input$party_facet) %>%
                ggplot(aes(x = Gender)) +
                geom_bar(stat = "count", aes(fill = Race)) +
                facet_wrap(~`Party Affiliation`)
            }
            else if (xreact == "Race") {
              filter(nc_voter_small, Race %in% input$race_x) %>%
                filter(Race %in% input$race_color) %>%
                filter(`Party Affiliation` %in% input$party_facet) %>%
                ggplot(aes(x = Race)) +
                geom_bar(stat = "count", aes(fill = Race)) +
                facet_wrap(~`Party Affiliation`)
            }
            else if (xreact == "Party Affiliation") {
              filter(nc_voter_small, `Party Affiliation` %in% input$party_x) %>%
                filter(Race %in% input$race_color) %>%
                filter(`Party Affiliation` %in% input$party_facet) %>%
                ggplot(aes(x = `Party Affiliation`)) +
                geom_bar(stat = "count", aes(fill = Race)) +
                facet_wrap(~`Party Affiliation`)
            }
            else {
              filter(nc_voter_small, Race %in% input$race_color) %>%
                filter(`Party Affiliation` %in% input$party_facet) %>%
                ggplot(aes(x = Age)) +
                geom_bar(stat = "count", aes(fill = Race)) +
                facet_wrap(~`Party Affiliation`)
            }
          }
          # -------------------------------------------------------------
          else {
            if (xreact == "Gender") {
              filter(nc_voter_small, Gender %in% input$gender_x) %>%
                filter(`Party Affiliation` %in% input$party_color) %>%
                filter(`Party Affiliation` %in% input$party_facet) %>%
                ggplot(aes(x = Gender)) +
                geom_bar(stat = "count", aes(fill = `Party Affiliation`)) +
                facet_wrap(~`Party Affiliation`)
            }
            else if (xreact == "Race") {
              filter(nc_voter_small, Race %in% input$race_x) %>%
                filter(`Party Affiliation` %in% input$party_color) %>%
                filter(`Party Affiliation` %in% input$party_facet) %>%
                ggplot(aes(x = Race)) +
                geom_bar(stat = "count", aes(fill = `Party Affiliation`)) +
                facet_wrap(~`Party Affiliation`)
            }
            else if (xreact == "Party Affiliation") {
              filter(nc_voter_small, `Party Affiliation` %in% input$party_x) %>%
                filter(`Party Affiliation` %in% input$party_color) %>%
                filter(`Party Affiliation` %in% input$party_facet) %>%
                ggplot(aes(x = `Party Affiliation`)) +
                geom_bar(stat = "count", aes(fill = `Party Affiliation`)) +
                facet_wrap(~`Party Affiliation`)
            }
            else {
              filter(nc_voter_small, `Party Affiliation` %in% input$party_color) %>%
                filter(`Party Affiliation` %in% input$party_facet) %>%
                ggplot(aes(x = Age)) +
                geom_bar(stat = "count", aes(fill = `Party Affiliation`)) +
                facet_wrap(~`Party Affiliation`)
            }
          }          
        }
      }
      else {
        if (colorreact == "Gender") {
          if (xreact == "Gender") {
            filter(nc_voter_small, Gender %in% input$gender_color) %>%
              filter(Gender %in% input$gender_x) %>%
              ggplot(aes(x = Gender)) +
              geom_bar(stat = "count", aes(fill = Gender))
          }
          else if (xreact == "Race") {
            filter(nc_voter_small, Gender %in% input$gender_color) %>%
              filter(Race %in% input$race_x) %>%
              ggplot(aes(x = Race)) +
              geom_bar(stat = "count", aes(fill = Gender))              
          }
          else if (xreact == "Party Affiliation") {
            filter(nc_voter_small, Gender %in% input$gender_color) %>%
              filter(`Party Affiliation` %in% input$party_x) %>%
              ggplot(aes(x = `Party Affiliation`)) +
              geom_bar(stat = "count", aes(fill = Gender))  
          }
          else {
            filter(nc_voter_small, Gender %in% input$gender_color) %>%
              ggplot(aes(x = Age)) +
              geom_bar(stat = "count", aes(fill = Gender))                
          }
        }
        else if (colorreact == "Race") {
          if (xreact == "Gender") {
            filter(nc_voter_small, Race %in% input$race_color) %>%
              filter(Gender %in% input$gender_x) %>%
              ggplot(aes(x = Gender)) +
              geom_bar(stat = "count", aes(fill = Race))
          }
          else if (xreact == "Race") {
            filter(nc_voter_small, Race %in% input$race_color) %>%
              filter(Race %in% input$race_x) %>%
              ggplot(aes(x = Race)) +
              geom_bar(stat = "count", aes(fill = Race))              
          }
          else if (xreact == "Party Affiliation") {
            filter(nc_voter_small, Race %in% input$race_color) %>%
              filter(`Party Affiliation` %in% input$party_x) %>%
              ggplot(aes(x = `Party Affiliation`)) +
              geom_bar(stat = "count", aes(fill = Race))  
          }
          else {
            filter(nc_voter_small, Race %in% input$race_color) %>%
              ggplot(aes(x = Age)) +
              geom_bar(stat = "count", aes(fill = Race))                
          }
        }
        else {
          if (xreact == "Gender") {
            filter(nc_voter_small, `Party Affiliation` %in% input$party_color) %>%
              filter(Gender %in% input$gender_x) %>%
              ggplot(aes(x = Gender)) +
              geom_bar(stat = "count", aes(fill = `Party Affiliation`))
          }
          else if (xreact == "Race") {
            filter(nc_voter_small, `Party Affiliation` %in% input$party_color) %>%
              filter(Race %in% input$race_x) %>%
              ggplot(aes(x = Race)) +
              geom_bar(stat = "count", aes(fill = `Party Affiliation`))              
          }
          else if (xreact == "Party Affiliation") {
            filter(nc_voter_small, `Party Affiliation` %in% input$party_color) %>%
              filter(`Party Affiliation` %in% input$party_x) %>%
              ggplot(aes(x = `Party Affiliation`)) +
              geom_bar(stat = "count", aes(fill = `Party Affiliation`))  
          }
          else {
            filter(nc_voter_small, `Party Affiliation` %in% input$party_color) %>%
              ggplot(aes(x = Age)) +
              geom_bar(stat = "count", aes(fill = `Party Affiliation`))                
          }
        }
      }
    }
    else {
      if (input$facet) {
        if (facetreact == "Gender") {
          if (xreact == "Gender") {
            filter(nc_voter_small, Gender %in% input$gender_facet) %>%
              filter(Gender %in% input$gender_x) %>%
              ggplot(aes(x = Gender)) +
              geom_bar(stat = "count") +
              facet_wrap(~Gender)
          }
          else if (xreact == "Race") {
            filter(nc_voter_small, Gender %in% input$gender_facet) %>%
              filter(Race %in% input$race_x) %>%
              ggplot(aes(x = Race)) +
              geom_bar(stat = "count") +
              facet_wrap(~Gender)            
          }
          else if (xreact == "Party Affiliation") {
            filter(nc_voter_small, Gender %in% input$gender_facet) %>%
              filter(`Party Affiliation` %in% input$party_x) %>%
              ggplot(aes(x = `Party Affiliation`)) +
              geom_bar(stat = "count") +
              facet_wrap(~Gender)                
          }
          else {
            filter(nc_voter_small, Gender %in% input$gender_facet) %>%
              ggplot(aes(x = Age)) +
              geom_bar(stat = "count") +
              facet_wrap(~Gender)            
          }
        }
        else if (facetreact == "Race") {
          if (xreact == "Gender") {
            filter(nc_voter_small, Race %in% input$race_facet) %>%
              filter(Gender %in% input$gender_x) %>%
              ggplot(aes(x = Gender)) +
              geom_bar(stat = "count") +
              facet_wrap(~Race)
          }
          else if (xreact == "Race") {
            filter(nc_voter_small, Race %in% input$race_facet) %>%
              filter(Race %in% input$race_x) %>%
              ggplot(aes(x = Race)) +
              geom_bar(stat = "count") +
              facet_wrap(~Race)            
          }
          else if (xreact == "Party Affiliation") {
            filter(nc_voter_small, Race %in% input$race_facet) %>%
              filter(`Party Affiliation` %in% input$party_x) %>%
              ggplot(aes(x = `Party Affiliation`)) +
              geom_bar(stat = "count") +
              facet_wrap(~Race)                
          }
          else {
            filter(nc_voter_small, Race %in% input$race_facet) %>%
              ggplot(aes(x = Age)) +
              geom_bar(stat = "count") +
              facet_wrap(~Race)            
          }
        }
        else {
          if (xreact == "Gender") {
            filter(nc_voter_small, `Party Affiliation` %in% input$party_facet) %>%
              filter(Gender %in% input$gender_x) %>%
              ggplot(aes(x = Gender)) +
              geom_bar(stat = "count") +
              facet_wrap(~`Party Affiliation`)
          }
          else if (xreact == "Race") {
            filter(nc_voter_small, `Party Affiliation` %in% input$party_facet) %>%
              filter(Race %in% input$race_x) %>%
              ggplot(aes(x = Race)) +
              geom_bar(stat = "count") +
              facet_wrap(~`Party Affiliation`)            
          }
          else if (xreact == "Party Affiliation") {
            filter(nc_voter_small, `Party Affiliation` %in% input$party_facet) %>%
              filter(`Party Affiliation` %in% input$party_x) %>%
              ggplot(aes(x = `Party Affiliation`)) +
              geom_bar(stat = "count") +
              facet_wrap(~`Party Affiliation`)                
          }
          else {
            filter(nc_voter_small, `Party Affiliation` %in% input$party_facet) %>%
              ggplot(aes(x = Age)) +
              geom_bar(stat = "count") +
              facet_wrap(~`Party Affiliation`)            
          }
        }
      }
      else {
        if (xreact == "Gender") {
          filter(nc_voter_small, Gender %in% input$gender_x) %>%
            ggplot(aes(x = Gender)) +
            geom_bar(stat = "count")
        }
        else if (xreact == "Race") {
          filter(nc_voter_small, Race %in% input$race_x) %>%
            ggplot(aes(x = Race)) +
            geom_bar(stat = "count")
        }
        else if (xreact == "Party Affiliation") {
          filter(nc_voter_small, `Party Affiliation` %in% input$party_x) %>%
            ggplot(aes(x = `Party Affiliation`)) +
            geom_bar(stat = "count")
        }
        else {
          ggplot(nc_voter_small, aes(x = Age)) +
            geom_bar(stat = "count")
        }
      }
    }
  })
}


#for tomorrow, add race (server), add options for changing x and y, add map

# Run the application 
shinyApp(ui = ui, server = server)
