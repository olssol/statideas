##-------------------------------------------------------------
##           UI FUNCTIONS
##-------------------------------------------------------------

##define the main tabset for beans
tab_main <- function() {
    tabsetPanel(type = "pills",
                id   = "mainpanel",
                tab_slides(),
                tab_demo_1(),
                tab_demo_2(),
                tab_demo_3(),
                tab_demo_4(),
                tab_demo_5(),
                tab_demo_6()
                )
}

tab_slides <- function() {
    tabPanel("Presentation",
             tags$iframe(src    = "slides.html",
                         style  = "border:none;width:100%",
                         height = 800),
             )
}

tab_demo_1 <- function() {
    tabPanel("Demo I",
             fluidRow(
                 column(4,
                        numericInput("inN",
                                     "Total Sample Size",
                                     value = 50),
                        numericInput("inTrtMean",
                                     "Treatment Group Mean (Control Mean = 0)",
                                     value = 0),
                        ## numericInput("inInc",
                        ##             "Increment Size",
                        ##             value = 10,
                        ##             min   = 1),
                        numericInput("inAlpha",
                                     "One-Sided Alpha Level",
                                     value = 0.025,
                                     min   = 0.01, max = 0.1),
                        ## actionButton("btnAdd",
                        ##              "Enroll Patients",
                        ##              width = "120px"),
                        numericInput("inSeed",
                                     "Random Seed",
                                     value = 0, min = 0),
                        actionButton("btnReset",
                                     "Reset",
                                     width = "120px")
                        ),

                 column(8,
                        h4("Enrollment"),
                        sliderInput("inNfirst", "", value = 5,
                                    min = 0,
                                    max = 50, step = 1,
                                    width = "100%"),
                        h4("By Arm"),
                        radioButtons("rdoArmOpt",
                                     "",
                                     choices = c("Individual Value" = "y",
                                                 "Cumulative Mean"  = "mean" )
                                    ),
                        plotlyOutput("pltArm",  height = "350px"),
                        h4("By Difference Between Two Arms"),
                        radioButtons("rdoDiffOpt",
                                     "",
                                     choices = c("Standardized Difference" = "zscore",
                                                 "Difference" = "difference",
                                                 "P Value" = "pvalue")
                                     ),
                        plotlyOutput("pltDiff", height = "350px"),
                        checkboxInput("chkThresh",
                                      "Show Rejection Threshold",
                                      value = FALSE)
                        )
                 )
             )
}

tab_demo_2 <- function() {
    tabPanel("Demo II",
             fluidRow(
                 column(4,
                        numericInput("inRep",
                                     "Number of Replications",
                                     value = 500,
                                     min   = 10),
                        numericInput("inN2",
                                     "Total Sample Size",
                                     value = 50),
                        numericInput("inTrtMean2",
                                     "Treatment Group Mean (Control Mean = 0)",
                                     value = 0),
                        numericInput("inNSel",
                                     "Number of Selected Replications",
                                     value = 500,
                                     min   = 1),
                        numericInput("inNInterim",
                                     "Number of Interim Analysis",
                                     value = 4, min = 0),
                        radioButtons("rdoBd",
                                     "Boundary",
                                     choices = c("No adjustment" = "NO",
                                                 "Pocock" = "P",
                                                 "O'Brien-Fleming" = "OF",
                                                 "Haybittle-Peto"  = "HP")),
                        actionButton("btnReset2",
                                     "Reset",
                                     width = "120px")
                        ),
                 column(8,
                        h4("Illustration"),
                        h6("Each line in the figure represents a replication of the trial."),
                        radioButtons("rdoTestOpt",
                                     "",
                                     choices = c("Difference" = "difference",
                                                 "Standardized Difference" = "zscore",
                                                 "P Value" = "pvalue")),
                        plotOutput("pltTest", height = "350px"),
                        h4("Proportion of Rejection"),
                        plotOutput("pltAlpha", height = "350px"),
                        tableOutput("tblTest"),
                        h4("Boundary Details"),
                        verbatimTextOutput("txtBd")
                        )
             ))
}

tab_demo_3 <- function() {
    tabPanel("Demo III",
             fluidRow(
                 column(4,
                        numericInput("inNInterim2",
                                     "Number of Interim Analysis",
                                     value = 5, min = 0),
                        checkboxGroupInput(
                            "chkDesigns",
                            "Designs",
                            choices = c("Pocock"          = "P",
                                        "O'Brien-Fleming" = "OF",
                                        "Haybittle-Peto"  = "HP",
                                        "Alpha Spending: Pocock" = "asP",
                                        "Alpha Spending: OF"     = "asOF"),
                            selected = c("P", "OF", "HP"))
                        ),
                 column(8,
                        h4("Comparison of designs"),
                        radioButtons("rdoRpact",
                                     "",
                                     choices = c("Boundaries"       = 1,
                                                 "Alpha"            = 3,
                                                 "Cumulative Alpha" = 4)),
                        plotOutput("pltBds", height = "350px")
                        )
             ))
}

tab_demo_4 <- function() {
    tabPanel("Demo IV",
             fluidRow(
                 column(4,
                        textInput("inInterFrac",
                                  "Interim Analysis Information Fraction",
                                  value = "0.3, 0.6, 1"),
                        radioButtons(
                            "rdoASBd",
                            "Designs",
                            choices = c("Alpha Spending: Pocock" = "asP",
                                        "Alpha Spending: OF"     = "asOF"))
                        ),
                 column(8,
                        h4("Illustration"),
                        plotOutput("pltASTest", height = "350px"),
                        h4("Proportion of Rejection"),
                        tableOutput("tblASTest"),
                        h4("Boundary Details"),
                        verbatimTextOutput("txtASBd")
                        )
             ))
}

tab_demo_5 <- function() {
    tabPanel("Demo V: Alpha Spent At IA",

             msg_box("In the Demo, we consider there is only one interim analysis.
                      We illustrate how alpha spent at the IA affects the nominal
                      alpha at the final analysis.",
                     type = "info"),

             msg_box("Remark: The same alpha spent at an earlier IA has a bigger
                     impact at the FA.",
                     type = "warning"),

             wellPanel(
                 h4("Specify Design Parameters"),

                 fluidRow(
                     column(3,
                            textInput("inInterFrac5",
                                      "Interim Analysis Information Fraction",
                                      value = "0.6, 1")),
                     column(3,
                            textInput("inNominalAlpha",
                                      "Nominal Alpha Level at Each Analysis",
                                      value = "0.02, NA")),

                     column(3,
                            numericInput("inAlpha5",
                                         "Total Alpha",
                                         value = 0.05,
                                         min   = 0,
                                         max   = 1,
                                         step  = 0.005)),

                     column(3,
                            numericInput("inPower5",
                                         "Study Power",
                                         value = 0.9,
                                         min   = 0,
                                         max   = 1,
                                         step  = 0.005))),

                 actionButton("btnCalc5",
                              "Get Study Design",
                              width = "150px")
             ),

             wellPanel(
                 tabsetPanel(
                     tabPanel(
                         "Study Design",
                         wellPanel(DTOutput("tblDesn5"))
                     ),

                     tabPanel(
                         "How Much Alpha Left for Final Analysis?",
                         wellPanel(
                             fluidRow(
                                 column(
                                     3,
                                     radioButtons(
                                         "inRdo5",
                                         "",
                                         choices = c("Under Null" = "type1",
                                                     "Power"      = "power" )
                                     )),
                                 column(
                                     3,
                                     radioButtons(
                                         "inRdoAna5",
                                         "",
                                         choices = c("Final Analysis Only" = "fa",
                                                     "At Interim Analysis"  = "ia",
                                                     "Interim and Final Analysis" = "fia"))
                                 ),
                                 column(3,
                                        uiOutput("uiChkbox5"))
                             ),
                             plotOutput("pltout5", height = "600px"),
                             sliderInput("inLim5",
                                         "",
                                         min   = 0.05,
                                         max   = 1,
                                         value = 0.25,
                                         step  = 0.05))
                     ),

                     tabPanel(
                         "Alpha at Final Analysis",
                         fluidRow(
                             column(3,
                                    numericInput("inIaAlpha5",
                                                 "Nominal Alpha at IA",
                                                 value = 0.02,
                                                 min = 0, max = 0.05, step = 0.005)),
                             column(9,
                                    sliderInput("inIaIf5",
                                                "Information Fraction at IA",
                                                value = c(0.05, 0.95),
                                                min = 0.05, max = 0.95,
                                                step = 0.05))
                         ),

                         plotlyOutput("pltoutCurve5", height = "600px"))
                 ))
)}

tab_demo_6 <- function() {
    tabPanel("Demo VI: Spending Functions",

             msg_box("In the Demo, we illustrate how different alpha spending
                      functions differ from each other.",
                      type = "info"),

             wellPanel(
                 h4("Specify Design Parameters"),
                 fluidRow(
                     column(3,
                            checkboxGroupInput(
                                "chkDesigns6",
                                "Select Alpha Spending Functions",
                                choices = c("Pocock Type"          = "asP",
                                            "O'Brien-Fleming Type" = "asOF",
                                            "Kim-Demets"           = "asKD",
                                            "User Defined"         = "asUser"),
                                selected = c("asOF", "asKD")),

                            textInput("inInterFrac6",
                                      "Interim Analysis Information Fraction",
                                      value = "0.4, 0.6,  0.9")),

                     column(3,
                            numericInput("inKDgamma6",
                                         "Kim-Demets Gamma",
                                         value = 3,
                                         min   = 0,
                                         max   = 20,
                                         step  = 1),

                            textInput("inCumuAlpha6",
                                      "User Defined: Cumulative Alpha Spending",
                                      value = "0.01, 0.02, 0.035"),

                            checkboxInput("inGsDes6",
                                          "Use gsDesign",
                                          value = FALSE)
                            ),

                     column(3,
                            numericInput("inAlpha6",
                                         "Total Alpha",
                                         value = 0.05,
                                         min   = 0,
                                         max   = 1,
                                         step  = 0.005),

                            numericInput("inPower6",
                                         "Study Power",
                                         value = 0.9,
                                         min   = 0,
                                         max   = 1,
                                         step  = 0.005)
                            ),

                     column(3,
                            numericInput("inSample6",
                                         "Study Sample Size",
                                         value = 500),

                            numericInput("inEnrollRate6",
                                         "Enrollment Rate (per Month)",
                                         value = 20),

                            numericInput("inMinFu6",
                                         "Minimum Follow-up Months",
                                         value = 6)
                            )),

                 actionButton("btnAdd6",
                              "Add Study Design",
                              width = "150px"),

                 actionButton("btnReset6",
                              "Reset",
                              width = "150px")

             ),

             wellPanel(
                 tabsetPanel(
                     tabPanel(
                         "Study Design",
                         wellPanel(DTOutput("tblDesn6"))
                     ),

                     tabPanel(
                         "Resource Saved",
                         wellPanel(DTOutput("tblOut6"))
                     ),

                     tabPanel(
                         "Illustration",
                         wellPanel(
                             radioButtons(
                                 "inRdo6",
                                 "",
                                 choices = c(
                                     "Nominal Alpha Spent",
                                     "Cumulative Alpha Spent",
                                     "Cumulative Power",
                                     "Z-score Boundary",
                                     "MDD"
                                 )),
                             plotlyOutput("pltout6", height = "600px")
                         )
                     )
             )))
}
