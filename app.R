library(shiny)
library(shinydashboard)
require(shinydashboardPlus)
require(DT)
library(data.table)
require(tidyr)
library(dplyr)

Data = fread(
    'C:\\Users\\SrikarManepalli\\Documents\\Walmart S&OP\\gdc_postgame_dashboard_ads_2202_0603.csv'
)
Data = Data %>% group_by(
    WM_YR_WK_ID,
    CALENDAR_DATE,
    BU,
    ACCTG_DEPT_NBR,
    ACCTG_DEPT_DESC,
    DEPT_CATEGORY_NBR,
    DEPT_CATEGORY_DESC
) %>% summarise(grs_aligned = sum(grs_aligned))

Data$CALENDAR_DATE <- as.Date(Data$CALENDAR_DATE, '%Y-%m-%d')

# BU Level forecast
BU_Level <- Data %>%
    group_by(WM_YR_WK_ID, CALENDAR_DATE, BU) %>%
    summarize(grs_aligned = sum(grs_aligned)) #%>%
# ungroup() %>%
# select(-WM_YR_WK_ID) %>%
# spread(CALENDAR_DATE,grs_aligned,fill = 0)%>%
# mutate(BU=ifelse(BU=='','OTHER',BU))

# Dept Level Forecast

Dept_Level = Data %>% group_by(CALENDAR_DATE, BU, ACCTG_DEPT_NBR, ACCTG_DEPT_DESC) %>%
    summarise(grs_aligned = sum(grs_aligned)) %>%
    spread(CALENDAR_DATE, grs_aligned, fill = 0) %>%
    ungroup() %>%
    mutate(BU = ifelse(BU == '', 'OTHER', BU)) %>%
    #mutate(AnomalyCal=apply(1,max))%>%
    data.table()


# Category Level Data
Category_Level = Data %>% group_by(CALENDAR_DATE, BU, ACCTG_DEPT_NBR, ACCTG_DEPT_DESC) %>%
    summarise(grs_aligned = sum(grs_aligned))
datatable(Trans_dept, editable = TRUE)



bu_indice_details <- function(mt, df) {
    rnames <- df$BU
    cnames <- colnames(df)
    
    rn <- rnames[mt[1, 1]]
    cn <- cnames[mt[1, 2]]
    
    return(list(rn, cn))
    
}

dept_indice_details <- function(mt, df) {
    rnames <- df$ACCTG_DEPT_DESC
    cnames <- colnames(df)
    
    rn <- rnames[mt[1, 1]]
    cn <- cnames[mt[1, 2]]
    
    return(list(rn, cn))
}

bu_deepdive <- function(mt, BU_df, start_date, end_date) {
    # rnames <- BU_df$BU
    
    selected_BU <- BU_df$BU[mt[1, 1]]
    # print(class(selected_BU))
    
    dept_forecast <-  Data %>% filter(BU == selected_BU) %>%
        filter(CALENDAR_DATE %in% seq(start_date, end_date, by = 'day')) %>%
        group_by(CALENDAR_DATE, BU, ACCTG_DEPT_NBR, ACCTG_DEPT_DESC) %>%
        summarise(grs_aligned = sum(grs_aligned)) %>%
        spread(CALENDAR_DATE, grs_aligned, fill = 0) %>%
        ungroup() %>%
        mutate(BU = ifelse(BU == '', 'OTHER', BU))#%>%
    #mutate(AnomalyCal=apply(1,max))%>%
    # data.table()
    
    # print(dim(dept_forecast))
    
    return(dept_forecast)
}

dept_deepdive <- function(indice_list){
    
    selected_dept <- indice_list[[1]]
    selected_date <- indice_list[[2]]
    
    # print(selected_dept)
    # print(selected_date)
    
    category_table <- Data %>% 
        filter(ACCTG_DEPT_DESC == selected_dept) %>%
        filter(CALENDAR_DATE == selected_date) %>%
        ungroup() %>% 
        select(DEPT_CATEGORY_DESC,grs_aligned) %>% 
        mutate(UPDT_GRS_ALIGNED = grs_aligned)
    
    return(category_table)
    
}

shinyApp(
    ui = dashboardPagePlus(
        header = dashboardHeaderPlus(enable_rightsidebar = TRUE,
                                     rightSidebarIcon = "gears"),
        sidebar = dashboardSidebar(disable = T),
        body = dashboardBody(fluidRow(
            box(box(
                dateRangeInput(
                    inputId = 'fcast_review_date_range',
                    label = 'Enter date range :',
                    start = '2020-02-22',
                    end = '2020-03-06',
                    autoclose = T
                ),
                width = 3
            ),
            # actionButton(inputId = "date_submit",label = 'Submit'),
            width = 12),
            tabsetPanel(
                type = 'tabs',
                tabPanel(
                    title = 'Forecast Review',
                    box(
                        title = 'BU Level Forecast',
                        DTOutput('BU_level'),
                        verbatimTextOutput('BU_level_indices'),
                        width = 12,
                        height = 600,
                        collapsible = F
                    ),
                    box(
                        title = 'Dept Level Forecast',
                        DTOutput('Dept_level'),
                        verbatimTextOutput('dept_level_indices'),
                        width = 12,
                        height = 800
                    )
                ),
                tabPanel(
                    title = 'Aligned Forecast',
                    box(
                        title = 'BU Level Forecast',
                        width = 12,
                        height = 600
                    ),
                    box(
                        title = 'Dept Level Forecast',
                        width = 12,
                        height = 600
                    ),
                    
                    actionButton(inputId = 'commit_changes', label = 'Commit')
                )
            )
            # box(title = 'BU Level Forecast',
            #     width = 12,
            #     height = 300),
            # box(title = 'Dept Level Forecast',
            #     width = 12,
            #     height = 300)
        )),
        rightsidebar = rightSidebar(
            background = "dark",
            box(title = 'Dept',
                verbatimTextOutput('anomaly_Dept_name')),
            box(title = 'Date',
                verbatimTextOutput('anomaly_date')),
            br(),
            br(),
            br(),
            br(),
            box(
                title = 'Category fcast Table',footer = 'Press CTRL + Enter to edit',
                DTOutput('category_table'),
                height = 600,
                width = 100
            ),
            actionButton(inputId = 'Adjustment_submit', label = 'Update'),
            width = 800
        ),
        title = "Right Sidebar"
    ),
    server = function(input, output, session) {
        df <- reactive(
            BU_Level %>%
                filter(
                    CALENDAR_DATE %in% seq(
                        input$fcast_review_date_range[1],
                        input$fcast_review_date_range[2],
                        by = "day"
                    )
                ) %>%
                ungroup() %>%
                select(-WM_YR_WK_ID) %>%
                spread(CALENDAR_DATE, grs_aligned, fill = 0) %>%
                mutate(BU = ifelse(BU == '', 'OTHER', BU)) %>% arrange(BU)
        )
        
        # dim(df())
        # print(input$fcast_review_date_range[1])
        
        output$BU_level <- DT::renderDataTable(df(), selection = list(mode = 'single', target = 'cell'))
        
        # output$BU_level_indices <- renderPrint(input$BU_level_cells_selected)
        # class(BU_level_indices)
        
        BU_indices <-
            reactive(as.matrix(unlist(
                bu_indice_details(input$BU_level_cells_selected, df())
            )))
        
        # print(BU_indices())
        
        output$BU_level_indices <- renderPrint(BU_indices())
        
        output$Dept_level <- DT::renderDataTable(
            bu_deepdive(
                input$BU_level_cells_selected,
                df(),
                input$fcast_review_date_range[1],
                input$fcast_review_date_range[2]
            ),
            selection = list(mode = 'single', target = 'cell')
        )
        
        
        dept_indices <-
            reactive(as.matrix(unlist(
                dept_indice_details(
                    input$Dept_level_cells_selected,
                    bu_deepdive(
                        input$BU_level_cells_selected,
                        df(),
                        input$fcast_review_date_range[1],
                        input$fcast_review_date_range[2]
                    )
                )
            )))
         
        output$dept_level_indices <- renderPrint(dept_indices())
        
        output$category_table <- DT::renderDT(
            dept_deepdive(dept_indices()),editable = 'column',server = TRUE,rownames = F
        )
        
        
        
        
    }
)
 