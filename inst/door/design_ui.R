## -----------------------------------------------------------
##
##          FUNCTIONS
##
## -----------------------------------------------------------

set_outcome <- function() {
    list(type     = "Binary",
         mean_trt = 0.5,
         mean_ctl = 0.5,
         sd       = 1,
         delta    = 0)
}

##-------------------------------------------------------------
##           UI FUNCTIONS
##-------------------------------------------------------------

##define the main tabset for beans
tab_main <- function() {
    tabsetPanel(type = "pills",
                id   = "mainpanel",
                tab_setting(),
                tab_data(),
                tab_door()
                )
}

## simulation setting
tab_setting <- function() {
    tabPanel("Setting",
             wellPanel(
                 fluidRow(
                     column(3,
                            numericInput("inSize",
                                         "Sample Size Per Arm",
                                         value = 20,
                                         min = 5, max = 1000, step = 5),

                            numericInput("inNOutcome",
                                         "Number of Responses",
                                         value = 1,
                                         min = 1, max = 10, step = 1)),
                     column(3,
                            selectInput("inOutInx",
                                        "Outcome Index",
                                        choices = c("Outcome 1" = 1)),

                            textInput("inOutName",
                                      "Outcome Name",
                                      "Y1"),

                            radioButtons("inOutType",
                                         "Outcome Type",
                                         choices = c("Binary", "Continuous"),
                                         selected = "Binary")),
                     column(3,
                            numericInput("inTrtMean",
                                         "Treatment Arm Mean",
                                         value = 0.5, step = 0.05),
                            numericInput("inCtlMean",
                                         "Control Arm Mean",
                                         value = 0.5, step = 0.05),

                            conditionalPanel(
                                condition = "input.inOutType == 'Continuous'",
                                numericInput("inSD",
                                             "SD",
                                             value = 1, min = 0),
                                numericInput("inDelta",
                                             "Delta for DOOR",
                                             value = 0)),
                            actionButton("inBtnSave", "Save")),
                     column(3)
                 )),
             wellPanel(
                 h4("Current Endpoints"),
                 DT::dataTableOutput("tblEndpoint")
             )
             )
}

tab_data <- function() {
    tabPanel("Data",
             wellPanel(
                 numericInput("inSeed",
                              "Simulation Random Seed",
                              value = 1000, width = "300px"),
                 actionButton("inBtnSimu", "Simulate Data")
             ),

             fluidRow(
                 column(6,
                        wellPanel(
                            h4("Treatment Arm"),
                            DT::dataTableOutput("tblTrt"))
                        ),
                 column(6,
                        wellPanel(
                            h4("Control Arm"),
                            DT::dataTableOutput("tblCtl"))
                        ))
             )
}

tab_door <- function() {
    tabPanel("DOOR",
             wellPanel(
                 h4("DOOR Comparison"),
                 DT::dataTableOutput("tblDoorComp")),
             ## wellPanel(
             ##     h4("DOOR Bootstrap"),
             ##     DT::dataTableOutput("tblDoorBS")),
             wellPanel(
                 h4("Analysis Results"),
                 DT::dataTableOutput("tblAnaTypical"))
             )
}

##-------------------------------------------------------------
##           DATA MANIPULATION: Setting
##-------------------------------------------------------------

get_outcome <- reactive({
    list(input$inOutName,
         input$inOutType,
         input$inTrtMean,
         input$inCtlMean,
         input$inSD,
         input$inDelta)
})

observeEvent(get_outcome(), {
    shinyjs::enable(id = "inBtnSave")
})

observeEvent(userLog$outcomes, {
    ## cat("-------------Outcome updated: \n")
    ## print(userLog$outcomes)
})

## update outcome settings
observeEvent(input$inNOutcome, {

    if (is.null(input$inNOutcome)) {
        return(NULL)
    }

    cur_lst <- userLog$outcomes
    n_lst   <- length(cur_lst)
    n_out   <- as.numeric(input$inNOutcome)

    if (n_out < length(cur_lst)) {
        cur_lst <- cur_lst[1:n_out]
    } else if (n_out > n_lst) {
        for (i in (n_lst + 1):n_out) {
            cur_val      <- set_outcome()
            cur_val$name <- paste("Y", i, sep = "")
            cur_lst[[i]] <- cur_val
        }
    }
    userLog$outcomes <- cur_lst

    ## update ui
    choices        <- 1:n_out
    names(choices) <- paste("Outcome", 1:n_out)

    updateSelectInput(inputId  = "inOutInx",
                      choices  = choices,
                      selected = 1)
})

## save input values
observeEvent(input$inBtnSave, {
    if (0 == input$inBtnSave) {
        return(NULL)
    }

    if (is.null(input$inOutInx)) {
        return(NULL)
    }

    cur_inx <- as.numeric(input$inOutInx)
    cur_val <- list(type     = input$inOutType,
                    mean_trt = input$inTrtMean,
                    mean_ctl = input$inCtlMean,
                    sd       = input$inSD,
                    delta    = input$inDelta,
                    name     = input$inOutName)

    userLog$outcomes[[cur_inx]] <- cur_val
    shinyjs::disable(id = "inBtnSave")
})

## update input values
observeEvent(input$inOutInx, {

    if (is.null(input$inOutInx)) {
        return(NULL)
    }

    if (0 == length(userLog$outcomes)) {
        return(NULL)
    }

    cur_inx <- as.numeric(input$inOutInx)
    cur_out <- userLog$outcomes[[cur_inx]]

    updateTextInput(inputId     = "inOutName",
                    value    = cur_out$name)

    updateRadioButtons(inputId  = "inOutType",
                       selected = cur_out$type)

    updateNumericInput(inputId = "inTrtMean",
                       value   = cur_out$mean_trt)

    updateNumericInput(inputId = "inCtlMean",
                       value   = cur_out$mean_ctl)

    updateNumericInput(inputId = "inSD",
                       value   = cur_out$sd)

    updateTextInput(inputId  = "inDelta",
                    value    = cur_out$delta)

})

get_tbl_endpoint <- reactive({
    si_door_outcomes_to_table(userLog$outcomes)
})

##-------------------------------------------------------------
##           DATA MANIPULATION: Simulation
##-------------------------------------------------------------

simu_data <- eventReactive(input$inBtnSimu, {
    if (0 == length(userLog$outcomes)) {
        return(NULL)
    }

    rst <- si_door_simu(input$inSize,
                        userLog$outcomes,
                        input$inSeed)
    rst
})

simu_data_trt <- reactive({
    dta <- simu_data()
    if (is.null(dta)) return(NULL)

    dta %>%
        filter(arm == "Treatment")
})

simu_data_ctl <- reactive({
    dta <- simu_data()
    if (is.null(dta)) return(NULL)

    dta %>%
        filter(arm == "Control")
})


##-------------------------------------------------------------
##           DOOR
##-------------------------------------------------------------
get_door_comparison <- reactive({

    id_trt <- input$tblTrt_rows_selected
    id_ctl <- input$tblCtl_rows_selected
    if (is.null(id_trt) |
        is.null(id_ctl) |
        0 == length(userLog$outcomes)) {
        return(NULL)
    }

    dta_trt <- simu_data_trt()
    dta_ctl <- simu_data_ctl()
    rst     <- si_door_rank_single(dta_trt[id_trt, ],
                                   dta_ctl[id_ctl, ],
                                   userLog$outcomes)$data

    colnames(rst) <- c("Outcome", "Delta", "Treatment PT",
                       "Control PT", "Trt vs. Ctl")

    rst
})


get_door_bs <- reactive({

    dta_trt <- simu_data_trt()
    dta_ctl <- simu_data_ctl()

    if (is.null(dta_trt) |
        is.null(dta_ctl) |
        0 == length(userLog$outcomes)) {
        return(NULL)
    }

    rst <- si_door_rank_bs(dta_trt, dta_ctl, userLog$outcomes)
    rst <- rst %>%
        mutate(stat = sum / n_trt - n_ctl / 2)

    estimate <- rst[1, "stat"]
    sd_est   <- sd(rst$stat)
    ci_l     <- estimate - 1.96 * sd_est
    ci_u     <- estimate + 1.96 * sd_est
    p_value  <- pnorm(estimate / sd_est)
    p_value  <- 2 * min(p_value, 1 - p_value)

    list(bs   = rst,
         door = data.frame(Outcome   = "DOOR",
                           Test      = "DOOR",
                           Mean_Trt  = NA,
                           Mean_Ctl  = NA,
                           Conf_Low  = ci_l,
                           Conf_High = ci_u,
                           p_Value   = p_value))
})
