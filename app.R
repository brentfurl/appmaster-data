## GETTING CLOSE BUT THE PROBLEM IS ON THE LABELING SIDE FOR THE MOST PART.  After Envoy and Rooster close, I can make the changes to them.

library(shiny)
library(DT)
library(shinydashboard)
library(tidyverse)
library(forcats)
#library(lubridate)
library(qualtRics)

# these are the qualtrics api credentials
qualtrics_api_credentials(api_key = "z5sbsGjc9jsc7WxAfqeLDiOmNyuYLX4IZV97TsqX",
                          base_url = "deutser.co1.qualtrics.com",
                          install = TRUE,
                          overwrite = TRUE) # overwrite = TRUE stops the error saying credentials are there already
readRenviron("~/.Renviron")
    

source("data prep/vars.R")
# these contain the functions called in the app.  The above call functions which in turn called in the get_df main function
# source("data prep/get_df_rt.R")
# source("data prep/get_df_env.R")
source("data prep/get_df_NEW.R")

#----------------------------------------------------------------------------------------
surveys <- all_surveys() %>% 
    arrange(lastModified) %>% 
    filter(grepl('CPI', name)) %>%
    filter(isActive == TRUE) #%>%
    #select(name)

ui <- dashboardPage(
    dashboardHeader(title = "in progress CPI company counts"),
    dashboardSidebar(
        sidebarMenu(
            # menuItem("Cleaning filters", icon = icon("bar-chart-o"),
            selectInput("company","Companies", choices = c(surveys[["name"]])), #c("", names[[1]])),
            selectInput("filters","Data filters", choices = c("answered thru CPI", "raw"), selected = "answered thru CPI"),
            actionButton("action", "refresh data"),
            tags$style(type="text/css", "#downloadData {background-color:orange;color: black;font-family: Courier New; margin-left: 15px; margin-top: 15px}"),
            downloadButton('downloadData', 'Download')
        )
    ),
    dashboardBody(
        fluidRow(
            valueBoxOutput("count")
        ),
        fluidRow(
            DT::dataTableOutput("table")
        )
    )
)

#--------------------------------------------------------------------------------------

server <- function(input, output) {

    change_data <- eventReactive(c(input$company,input$action), {

        qual_surv_id <- surveys %>%
            filter(name == input$company) %>%
            select(id) %>%
            simplify() %>%
            first()

        raw <- fetch_survey(surveyID = qual_surv_id,
                            start_date = "2019-01-14",
                            label = TRUE,
                            force_request = TRUE)


        df_clean <- get_df_NEW(raw)
        return(df_clean)

    }, ignoreNULL = FALSE)

    # this variable holds the dataset before tranforming to count table.
    df_new <- reactive(change_data())
    # this creates the count table upon change to a filter (and from onload) from df_new()...also the table that will be exported to csv
    create_table_tib <- eventReactive(c(df_new(), input$filters), {

        demo_graph_loop <- list()
        df_demos <- df_new() %>%
            select_if(is.factor)
        demo_vars <- toupper(colnames(df_demos))

        # the below conidtions take the refreshed data held in the reactive function df_new() and creates the tables to display

        # if (input$filters == "raw") {
        #     for (d in 1:length(demo_vars)){
        # 
        #         n_demo_graphs <-
        #             #refresh_data() %>%
        #             df_new() %>%
        #             group_by(get(demo_vars[d])) %>%
        #             summarise(n = n()) %>%
        #             ungroup() %>%
        #             add_column(demo = eval(demo_vars[d]), .before=1) %>%
        #             rename(level = colnames(.)[2]) %>%
        #             #filter(level != "NA") %>%
        #             mutate(level = factor(level, order = FALSE))
        # 
        #         demo_graph_loop[[d]] <- n_demo_graphs
        #     }
        # 
        #     demo_graph_nums <- do.call(dplyr::bind_rows, demo_graph_loop)
        # 
        # } else {
            for (d in 1:length(demo_vars)){

                n_demo_graphs <-
                    #refresh_data() %>%
                    df_new() %>%
                    filter(extra_pos_positivity_5 != "NA") %>%
                    group_by(get(demo_vars[d])) %>%
                    summarise(n = n()) %>%
                    ungroup() %>%
                    add_column(demo = eval(demo_vars[d]), .before=1) %>%
                    rename(level = colnames(.)[2]) %>%
                    #filter(level != "NA") %>%
                    mutate(level = factor(level, order = FALSE))

                demo_graph_loop[[d]] <- n_demo_graphs
            }
            # this binds all the tibbles for each demo group and is the table output
            demo_graph_nums <- do.call(dplyr::bind_rows, demo_graph_loop)
        #}
        return(demo_graph_nums)
    }, ignoreNULL = FALSE)

    # this is the var to be called for rendering table and downloading the csv
    table_tib <- reactive(create_table_tib())
    #-------------------------------------- TOTAL COUNT -------------------------------------

    output$count <- renderValueBox({

        # the below conidtions take the refreshed data held in the reactive function df_new() and creates the total n to display

        if (input$filters == "raw") {
            #df_box <- refresh_data() %>%
            df_box <- df_new() %>%
                group_by(ALL) %>%
                summarise(count=n()) %>%
                select(count)  %>%
                valueBox(subtitle = "total count")

        } else
        {
            #df_box <- refresh_data() %>%
            df_box <- df_new() %>%
                filter(extra_pos_positivity_5 != "NA") %>%
                summarise(count=n()) %>%
                select(count) %>%
                valueBox(subtitle = "total count")
        }
    })

    #-------------------------- OUTPUT TABLE -------------------------------------------------------

    output$table <- DT::renderDataTable({

        df_table <- table_tib() %>%
            filter(demo != "ALL")
    },
    options = list(lengthMenu = c(10, 50))
    #     autoWidth = TRUE,
    #     columnDefs = list(list(width = '200px', targets = "level")))
    )
    #-----------------------------------------------------------DOWNLOAD DATA--------------------------------------------
    output$downloadData <- downloadHandler(
        filename = function() {
            paste0(input$company, "_", input$filters, ".csv")
        },

        content = function(file) {
            write_csv(table_tib(), file)

        }
    ) 
}

# Run the application 
shinyApp(ui = ui, server = server)


# IDEAS TO ADD
#### select from list of qualtrics
#### clean up to make next survey as simple as possible to get into shape
#### create the standardized scoring
## filters for duration and SD. Don't need to show them though.  just have the filters
### create a report for different filters even if not used
# button for filters saved/downloaded...to use for final data...includes raw n, filtered n, cleaned by demographs and reasons.
#### work on global database
# client dashboard