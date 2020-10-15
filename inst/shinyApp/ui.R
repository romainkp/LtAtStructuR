
jscode <- "
shinyjs.disableTab = function(name) {
var tab = $('.nav li a[data-value=' + name + ']');
tab.bind('click.tab', function(e) {
e.preventDefault();
return false;
});
tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
var tab = $('.nav li a[data-value=' + name + ']');
tab.unbind('click.tab');
tab.removeClass('disabled');
}
"

css <- "
.nav li a.disabled {
background-color: #aaa !important;
color: #333 !important;
cursor: not-allowed !important;
border-color: #aaa !important;
}"

shinyUI(fluidPage(
  
shinyjs::useShinyjs(),
shinyjs::extendShinyjs(text = jscode, functions = c("disableTab","enableTab")),
shinyjs::inlineCSS(css),
navbarPage("LtAtStructuR",id="navbarPage",
           tabPanel("Cohort", id = "Cohort",
                    sidebarLayout(
                      sidebarPanel(
                        fileInput("cohort.file", "Choose CSV File",
                                  multiple = FALSE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")),
                        # Horizontal line ----
                        tags$hr(),
                        # Variable selection
                        selectInput('cohort.IDvar', 'ID', choices = ''),
                        selectInput('cohort.index.date', 'Index date', choices = ''),
                        selectInput('cohort.EOF.date', 'End of follow-up date', choices = ''),
                        selectInput('cohort.EOF.type', 'End of follow-up reason', choices = ''),
                        selectInput('cohort.Y.name', 'Outcome', choices = ''),
                        selectInput('cohort.L0', 'All baseline covariates', choices = '', multiple=TRUE, selectize=TRUE),
                        # Horizontal line ----
                        tags$hr(),
                        selectInput('cohort.L0.timeIndep', 'Subset of time-independent covariates', choices = '', multiple=TRUE, selectize=TRUE),
                        #uiOutput("testui"),
                        tags$div(id = 'cohort_ui_test'),
                        # Horizontal line ----
                        tags$hr(),
                        uiOutput("covariateop"),
                        shinyjs::disabled(
                          actionButton("set.cohort.button","Set cohort")
                          )
                      ),
                      mainPanel(
                        DT::dataTableOutput("cohort.table"),
                        tags$div(id = 'cohort_r_template')
                      )
                    )
           ),
           tabPanel("Exposure", id = "Exposure",
                    sidebarLayout(
                      sidebarPanel(
                        fileInput("exposure.file", "Choose CSV File",
                                  multiple = FALSE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")),
                        # Horizontal line ----
                        tags$hr(),
                        # Variable selection
                        selectInput('exposure.IDvar', 'ID', choices = ''),
                        selectInput('exposure.start.date', 'Start date', choices = ''),
                        selectInput('exposure.end.date', 'End date', choices = ''),
                        selectInput('exposure.exp.level', "Exposure level", choices = ''),
                        textInput('exposure.exp.ref', 'Exposure reference'),
                        # Horizontal line ----
                        tags$hr(),
                        shinyjs::disabled(
                          actionButton("set.exposure.button","Set exposure")
                          )
                      ),
                      mainPanel(
                        DT::dataTableOutput("exposure.table"),
                        tags$div(id = 'exposure_r_template')
                      )
                    )
           ),
           tabPanel("Time-dependent covariates", id = "Covariates", value = "Covariates",
                    sidebarLayout(
                      sidebarPanel(
                        h4(textOutput("covariate.count")),
                        # Horizontal line ----
                        tags$hr(),
                        verbatimTextOutput("covariate.uploaded"),
                        #textOutput("covariate.uploaded"),
                        # Horizontal line ----
                        tags$hr(),
                        fileInput("covariate.file", "Choose CSV File",
                                  multiple = FALSE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")),
                        # Horizontal line ----
                        tags$hr(),
                        # Variable selection
                        selectInput('covariate.type', 'Type', c("","binary monotone increasing","interval","sporadic","indicator")),
                        selectInput('covariate.IDvar', 'ID', ''),
                        selectInput('covariate.L.date', 'Date of follow-up', ''),
                        selectInput('covariate.L.name', 'Covariate name', ''),
                        #selectInput('covariate.L.class', 'Covariate class', c("","numeric","character")),
                        selectInput('covariate.L.categorical', 'Categorical', c("",TRUE,FALSE)),
                        selectInput('covariate.L.impute', "Impute", c("","default","mean","mode","median")),
                        textInput('covariate.L.impute.default.level', "Impute default level"),
                        selectInput('covariate.L.acute.change', "Acute change", c("",TRUE,FALSE)),
                        # Horizontal line ----
                        tags$hr(),
                        shinyjs::disabled(
                          actionButton("set.covariate.button","Set covariate")
                          )
                      ),
                      mainPanel(
                        DT::dataTableOutput("covariate.table"),
                        tags$div(id = 'covariate_r_template')
                        #verbatimTextOutput("covariate_LtAtStructuR")
                      )
                    )
           ),
           tabPanel("Construct", id = "Construct",
                    sidebarLayout(
                      sidebarPanel(
                        # Variable selection
                        sliderInput("time.unit", "Time unit",
                                    min = 1, max = 100,
                                    value = 15),
                        selectInput('first.exp.rule', 'first_exp_rule', c(1,0)),
                        sliderInput("exp.threshold", "exp_threshold",
                                    min = 0, max = 1,
                                    value = 0.50, step = 0.01),
                        selectInput('format', 'Format', c("standard","MSM SAS macro")),
                        selectInput('dates', 'Dates', c(FALSE,TRUE)),
                        checkboxInput("m.cores", "Multiple Cores", value = FALSE),
                        tags$div(id = 'multiple_cores'),
                        # Horizontal line ----
                        tags$hr(),
                        actionButton("set.construct.button","Construct"),
                        # Horizontal line ----
                        tags$hr(),tags$div(id = 'download')
                        #downloadButton("downloadData", "Download")
                      ),
                      mainPanel(
                        DT::dataTableOutput("final.table"),
                        #verbatimTextOutput("final.table"),
                        #tags$div(id = 'construct_final_table'), ### ATTENTION: When I commented this out, the construct.code text began appearing. Why is it that with this on the test does not appear?
                        tags$div(id = 'construct_r_template')
                      )
                    )
                    )
           )
))