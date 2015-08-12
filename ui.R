library(shiny)
library(shinyAce)
library(psych)
library(CTT)
library(lattice)
library(latticeExtra)
library(beeswarm)



shinyUI(bootstrapPage(

    headerPanel("Item Analysis for Criterion-referenced Test"),


########## Adding loading message #########

tags$head(tags$style(type="text/css", "
#loadmessage {
position: fixed;
top: 0px;
left: 0px;
width: 100%;
padding: 10px 0px 10px 0px;
text-align: center;
font-weight: bold;
font-size: 100%;
color: #000000;
background-color: #CCFF66;
z-index: 105;
}
")),

conditionalPanel(condition="$('html').hasClass('shiny-busy')",
tags$div("Loading...",id="loadmessage")),

########## Added up untill here ##########


    mainPanel(
        tabsetPanel(

            tabPanel("Cut-score Indices",

                h2("Cut-score Indices"),

                h4("Single administration of a test"),


                p('Note: Input values must be separated by tabs. Copy and paste from Excel/Numbers.'),
                p('If you need to create 1-0 data first, use',
                a('Binary (1-0) Data Converter', href='https://langtest.shinyapps.io/biconv/', target="_blank"), '.'),

                p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>Your data needs to have the header (variable names) in the first row.</div></b>")),

                br(),

                strong('Option:'),
                checkboxInput("rownameCS", label = strong("The first columns of datasets contain case names."), value = T),

                br(),

                aceEditor("cutscore", value="varID\ti01\ti02\ti03\ti04\ti05\ti06\ti07\ti08\ti09\ti10\ti11\ti12\ti13\ti14\ti15\ti16\ti17\ti18\ti19\ti20\ti21\ti22\ti23\ti24\ti25\nStudent001\t0\t1\t0\t1\t1\t0\t0\t1\t0\t0\t0\t0\t0\t1\t0\t1\t0\t1\t0\t0\t1\t1\t0\t0\t1\nStudent002\t0\t1\t1\t0\t1\t1\t0\t1\t0\t0\t0\t0\t0\t1\t0\t0\t0\t0\t0\t0\t1\t0\t0\t1\t0\nStudent003\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t0\t0\t1\t1\t1\t1\t1\t0\t0\t0\t1\t1\nStudent004\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t0\t0\t1\t0\t1\t0\t0\t1\t1\t0\nStudent005\t0\t1\t0\t0\t1\t1\t0\t0\t0\t0\t0\t0\t1\t0\t1\t0\t1\t1\t1\t0\t0\t0\t1\t0\t1\nStudent006\t0\t1\t0\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t0\t0\t1\t1\t1\t0\t0\t0\t1\t1\t0\t1\nStudent007\t1\t0\t0\t1\t1\t1\t0\t1\t0\t0\t0\t1\t0\t1\t0\t0\t0\t1\t1\t0\t0\t1\t1\t1\t0\nStudent008\t0\t1\t0\t1\t1\t0\t0\t1\t1\t0\t1\t0\t0\t1\t0\t1\t1\t0\t0\t1\t0\t1\t1\t1\t0\nStudent009\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t0\t0\t0\t1\t0\t1\nStudent010\t0\t0\t0\t0\t1\t1\t0\t1\t0\t1\t0\t0\t0\t0\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\nStudent011\t1\t1\t0\t1\t0\t1\t0\t1\t0\t0\t0\t0\t1\t0\t0\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\nStudent012\t0\t0\t1\t0\t0\t0\t1\t1\t1\t0\t0\t1\t0\t0\t0\t0\t0\t0\t1\t0\t0\t0\t0\t1\t1\nStudent013\t0\t0\t0\t0\t0\t0\t0\t0\t1\t1\t0\t0\t0\t1\t1\t1\t1\t1\t0\t1\t1\t0\t0\t0\t0\nStudent014\t1\t1\t0\t1\t1\t1\t1\t1\t0\t0\t0\t0\t1\t0\t0\t1\t1\t1\t1\t0\t0\t0\t1\t1\t1\nStudent015\t1\t1\t1\t0\t1\t0\t0\t1\t1\t1\t0\t1\t1\t0\t0\t1\t1\t1\t0\t1\t0\t0\t1\t0\t1\nStudent016\t0\t1\t0\t0\t0\t1\t0\t1\t1\t1\t0\t0\t1\t0\t1\t1\t1\t1\t0\t0\t0\t0\t0\t1\t1\nStudent017\t0\t0\t1\t0\t1\t0\t0\t1\t1\t1\t1\t0\t0\t0\t0\t1\t1\t1\t0\t0\t1\t0\t0\t1\t0\nStudent018\t1\t1\t1\t0\t0\t0\t1\t1\t0\t0\t1\t1\t0\t1\t0\t0\t0\t1\t0\t0\t0\t0\t1\t1\t0\nStudent019\t1\t1\t1\t0\t0\t0\t0\t0\t1\t1\t0\t0\t0\t0\t1\t0\t0\t0\t0\t0\t0\t0\t1\t0\t0\nStudent020\t1\t1\t0\t0\t0\t1\t0\t0\t0\t0\t0\t1\t0\t0\t0\t1\t1\t0\t1\t1\t0\t0\t1\t1\t1\nStudent021\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t0\t0\t0\t1\t1\t1\nStudent022\t0\t0\t0\t0\t0\t0\t0\t0\t1\t1\t1\t0\t0\t0\t0\t1\t0\t1\t0\t0\t0\t0\t1\t0\t1\nStudent023\t0\t0\t0\t0\t0\t0\t1\t0\t0\t1\t0\t0\t0\t1\t0\t0\t1\t0\t0\t0\t0\t0\t0\t0\t0\nStudent024\t0\t1\t1\t0\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t1\t1\nStudent025\t1\t1\t0\t1\t0\t1\t1\t1\t0\t0\t0\t1\t0\t1\t1\t1\t1\t1\t0\t0\t0\t0\t1\t1\t0\nStudent026\t0\t1\t0\t1\t1\t1\t0\t1\t0\t0\t0\t1\t1\t0\t0\t1\t1\t1\t0\t1\t0\t0\t0\t1\t0\nStudent027\t1\t1\t1\t0\t1\t0\t1\t1\t1\t0\t0\t1\t0\t1\t0\t1\t0\t1\t1\t1\t0\t1\t0\t1\t0\nStudent028\t0\t0\t0\t1\t1\t0\t0\t1\t0\t0\t1\t1\t0\t0\t0\t1\t1\t0\t1\t0\t0\t1\t0\t1\t0\nStudent029\t0\t1\t0\t1\t0\t0\t0\t1\t1\t0\t0\t1\t1\t0\t0\t1\t1\t1\t1\t1\t0\t0\t0\t1\t0\nStudent030\t0\t0\t0\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t0\t0\t1\t1\t1\t0\t1\t0\t0\t0\t1\t0\nStudent031\t1\t1\t0\t0\t1\t0\t0\t1\t0\t0\t0\t1\t0\t1\t0\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\nStudent032\t0\t1\t1\t0\t0\t0\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t0\t1\t0\t0\t1\t1\t1\nStudent033\t1\t0\t0\t1\t1\t1\t0\t1\t0\t0\t0\t0\t0\t1\t0\t0\t0\t1\t0\t0\t0\t0\t1\t1\t0\nStudent034\t1\t1\t0\t1\t0\t0\t0\t1\t0\t1\t0\t1\t1\t0\t0\t1\t0\t1\t0\t1\t0\t0\t0\t1\t0\nStudent035\t0\t1\t0\t1\t1\t0\t0\t1\t0\t0\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t1\t0\nStudent036\t1\t0\t0\t0\t0\t0\t1\t1\t1\t0\t0\t1\t1\t0\t1\t0\t1\t1\t0\t0\t0\t0\t0\t0\t0\nStudent037\t0\t0\t0\t0\t1\t0\t0\t1\t1\t1\t0\t1\t0\t0\t0\t0\t1\t1\t0\t0\t0\t1\t0\t1\t1\nStudent038\t0\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t0\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\nStudent039\t0\t0\t1\t1\t0\t0\t0\t1\t0\t0\t1\t1\t0\t0\t1\t0\t0\t0\t0\t1\t0\t1\t1\t0\t0\nStudent040\t0\t0\t0\t1\t1\t0\t1\t0\t0\t0\t1\t1\t1\t0\t0\t1\t1\t1\t0\t0\t0\t1\t1\t0\t1\nStudent041\t0\t0\t0\t1\t0\t0\t0\t1\t0\t0\t0\t0\t0\t1\t0\t1\t0\t0\t1\t0\t0\t0\t0\t0\t0\nStudent042\t0\t1\t0\t1\t1\t0\t1\t1\t0\t1\t0\t0\t1\t1\t1\t0\t1\t1\t0\t0\t1\t0\t0\t1\t0\nStudent043\t1\t1\t0\t1\t0\t0\t1\t1\t1\t0\t0\t1\t0\t0\t1\t0\t1\t1\t0\t1\t0\t0\t0\t1\t1\nStudent044\t0\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t1\t1\t1\t0\t0\t1\nStudent045\t0\t1\t1\t1\t0\t1\t1\t1\t0\t1\t0\t1\t0\t0\t0\t0\t1\t1\t0\t0\t0\t1\t1\t0\t0\nStudent046\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\nStudent047\t1\t0\t1\t1\t1\t1\t1\t1\t0\t0\t0\t1\t1\t0\t1\t1\t1\t0\t1\t1\t0\t0\t1\t1\t1\nStudent048\t0\t0\t1\t0\t0\t0\t0\t1\t0\t0\t0\t0\t1\t0\t0\t0\t1\t0\t0\t1\t0\t0\t0\t1\t0\nStudent049\t0\t0\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\nStudent050\t1\t1\t1\t1\t1\t0\t0\t1\t0\t0\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\nStudent051\t0\t0\t0\t0\t0\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t0\t0\t0\t0\t1\t0\t1\nStudent052\t0\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t0\t1\t0\t0\t0\t0\t0\t0\t1\t1\t1\nStudent053\t0\t1\t0\t0\t1\t0\t0\t1\t0\t0\t0\t1\t1\t0\t1\t1\t1\t1\t0\t0\t0\t0\t0\t1\t0\nStudent054\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t1\t1\t0\t0\t0\t1\t0\t0\t1\t0\t0\t0\t0\nStudent055\t0\t1\t0\t1\t1\t1\t0\t0\t0\t1\t0\t0\t1\t0\t0\t1\t1\t1\t0\t0\t0\t1\t1\t1\t1\nStudent056\t0\t0\t1\t1\t1\t0\t0\t0\t1\t0\t0\t0\t0\t1\t0\t0\t1\t0\t0\t1\t0\t1\t1\t0\t1\nStudent057\t0\t1\t1\t0\t0\t0\t0\t1\t1\t0\t0\t1\t0\t0\t0\t0\t0\t1\t0\t0\t0\t0\t0\t1\t1\nStudent058\t0\t1\t0\t1\t0\t0\t0\t1\t1\t1\t0\t0\t0\t0\t0\t1\t1\t1\t0\t0\t0\t1\t1\t1\t1\nStudent059\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t0\t1\t0\t1\t0\t1\t1\t1\t1\t1\t1\t0\t0\t0\t0\nStudent060\t0\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t0\t0\t1\t0\t1\t0\t0\t0\t0\t1\t0\t0\nStudent061\t0\t1\t0\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\nStudent062\t0\t1\t0\t0\t1\t1\t1\t1\t1\t0\t0\t1\t0\t0\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t0\nStudent063\t1\t0\t1\t0\t1\t1\t1\t1\t1\t0\t0\t1\t0\t0\t0\t1\t1\t1\t0\t0\t0\t0\t1\t0\t1\nStudent064\t0\t1\t1\t0\t0\t1\t1\t0\t1\t0\t0\t1\t0\t1\t0\t1\t0\t0\t0\t0\t0\t0\t1\t1\t0\nStudent065\t1\t1\t0\t1\t1\t0\t0\t1\t1\t0\t0\t0\t0\t1\t1\t1\t0\t1\t1\t1\t0\t0\t1\t1\t1\nStudent066\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t0\t0\t1\t0\t0\t0\t1\t1\nStudent067\t0\t1\t1\t1\t0\t0\t1\t1\t0\t0\t0\t1\t0\t0\t0\t0\t1\t1\t0\t1\t0\t1\t1\t0\t1\nStudent068\t0\t0\t0\t1\t1\t0\t0\t0\t0\t0\t0\t0\t1\t0\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\t1\nStudent069\t0\t0\t1\t0\t0\t0\t0\t0\t0\t0\t1\t0\t0\t0\t1\t0\t1\t0\t0\t0\t0\t0\t1\t0\t1\nStudent070\t1\t1\t1\t0\t1\t1\t0\t1\t1\t0\t0\t1\t0\t0\t1\t1\t1\t1\t0\t0\t0\t1\t1\t1\t1\nStudent071\t1\t1\t0\t0\t1\t0\t1\t1\t0\t0\t0\t0\t1\t1\t0\t1\t1\t1\t0\t1\t0\t1\t1\t1\t0\nStudent072\t1\t1\t0\t1\t0\t0\t1\t1\t1\t0\t0\t1\t0\t1\t0\t1\t1\t1\t1\t1\t0\t0\t0\t1\t0\nStudent073\t1\t0\t0\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t0\t0\t0\t1\t1\t1\t0\t0\t0\t0\t0\t0\nStudent074\t0\t1\t1\t0\t1\t0\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t0\t0\t1\t1\t1\t1\nStudent075\t0\t0\t0\t0\t0\t0\t0\t1\t0\t1\t0\t1\t0\t0\t0\t0\t0\t1\t0\t0\t0\t0\t0\t0\t0\nStudent076\t0\t1\t0\t0\t1\t0\t1\t1\t1\t1\t1\t0\t0\t0\t1\t1\t0\t1\t0\t1\t0\t0\t1\t1\t0\nStudent077\t1\t0\t0\t0\t1\t0\t0\t1\t1\t1\t0\t1\t0\t0\t1\t0\t1\t1\t0\t0\t0\t0\t0\t0\t0\nStudent078\t0\t1\t0\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t0\t1\t0\t1\t0\t1\t0\t0\t1\t1\t1\nStudent079\t0\t1\t1\t0\t0\t1\t0\t1\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t1\t0\t0\t0\t0\nStudent080\t0\t0\t0\t0\t1\t1\t1\t1\t1\t0\t0\t1\t1\t0\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\nStudent081\t0\t0\t1\t1\t0\t1\t1\t1\t1\t1\t0\t0\t0\t1\t0\t1\t1\t0\t0\t0\t0\t0\t1\t1\t0\nStudent082\t0\t1\t0\t1\t1\t0\t1\t1\t1\t0\t0\t1\t0\t0\t0\t1\t1\t1\t0\t1\t0\t0\t1\t1\t1\nStudent083\t0\t0\t0\t0\t1\t0\t1\t0\t0\t0\t0\t1\t0\t0\t1\t0\t1\t0\t1\t0\t0\t0\t1\t0\t0\nStudent084\t1\t1\t0\t0\t1\t1\t0\t0\t0\t0\t0\t1\t0\t0\t0\t0\t1\t1\t0\t0\t0\t1\t1\t1\t1\nStudent085\t0\t0\t1\t0\t0\t1\t1\t0\t0\t0\t1\t1\t0\t1\t0\t0\t0\t1\t0\t0\t0\t1\t1\t0\t0\nStudent086\t0\t1\t1\t1\t1\t1\t0\t0\t0\t1\t0\t1\t1\t0\t0\t1\t1\t0\t0\t1\t0\t1\t0\t1\t0\nStudent087\t0\t0\t0\t1\t0\t1\t0\t1\t0\t0\t0\t1\t0\t1\t0\t0\t0\t1\t0\t0\t1\t0\t0\t1\t1\nStudent088\t0\t0\t0\t1\t0\t0\t0\t0\t1\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t1\t0\t1\t0\t0\nStudent089\t0\t1\t0\t0\t1\t1\t1\t1\t0\t0\t0\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t0\nStudent090\t0\t0\t0\t0\t0\t1\t1\t0\t1\t0\t0\t1\t0\t0\t0\t0\t1\t1\t0\t1\t0\t0\t0\t1\t1\nStudent091\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t1\t0\t0\t1\t0\t1\nStudent092\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t0\nStudent093\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t0\t1\t0\t0\t0\t1\t0\t1\t0\t0\t0\t1\t0\t1\t1\nStudent094\t0\t1\t0\t0\t1\t0\t1\t1\t1\t1\t0\t1\t0\t1\t0\t1\t0\t1\t1\t1\t0\t0\t1\t0\t1\nStudent095\t0\t0\t1\t0\t1\t1\t0\t0\t0\t0\t0\t0\t0\t0\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\nStudent096\t0\t0\t0\t0\t1\t1\t0\t1\t0\t0\t0\t0\t0\t1\t0\t0\t1\t1\t0\t1\t0\t0\t0\t0\t1\nStudent097\t0\t0\t0\t0\t1\t1\t1\t1\t1\t1\t0\t1\t0\t0\t0\t0\t0\t1\t0\t0\t0\t0\t1\t1\t0\nStudent098\t0\t1\t0\t0\t1\t1\t0\t1\t1\t0\t0\t1\t0\t1\t0\t0\t1\t1\t1\t1\t0\t1\t1\t0\t0\nStudent099\t0\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t0\t0\t1\t1\t1\nStudent100\t1\t1\t0\t0\t1\t1\t0\t1\t0\t0\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t0\t1\t0\t0\nStudent101\t0\t0\t0\t0\t1\t1\t1\t1\t1\t0\t0\t0\t0\t0\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\nStudent102\t0\t1\t1\t1\t1\t0\t1\t1\t1\t0\t0\t1\t1\t0\t0\t0\t1\t1\t1\t1\t0\t0\t1\t0\t1\nStudent103\t0\t0\t0\t0\t1\t0\t0\t1\t0\t1\t0\t1\t1\t0\t0\t1\t0\t1\t0\t0\t1\t0\t0\t0\t1\nStudent104\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t0\nStudent105\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t0\t1\t0\t1\t1\t1\t0\t0\t0\t0\t1\t0\t1\nStudent106\t0\t1\t1\t0\t1\t1\t0\t1\t0\t0\t0\t1\t0\t0\t0\t1\t1\t1\t1\t1\t0\t0\t0\t1\t1\nStudent107\t0\t1\t1\t1\t0\t0\t0\t0\t0\t0\t0\t0\t1\t0\t0\t1\t1\t1\t0\t0\t0\t1\t0\t0\t1\nStudent108\t0\t1\t0\t1\t1\t0\t0\t1\t1\t0\t0\t1\t1\t1\t1\t1\t0\t1\t0\t0\t0\t0\t1\t1\t1\nStudent109\t1\t1\t0\t1\t1\t0\t0\t1\t1\t1\t0\t1\t0\t0\t0\t1\t1\t1\t0\t1\t0\t1\t1\t1\t0\nStudent110\t1\t1\t0\t0\t1\t0\t1\t1\t0\t1\t0\t1\t0\t0\t0\t0\t0\t1\t1\t1\t0\t0\t0\t0\t0\nStudent111\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t1\t1\t0\t1\t0\t0\t0\t1\t0\nStudent112\t0\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t0\t1\t1\t1\t0\t0\t0\t1\t1\t0\t1\nStudent113\t1\t1\t0\t1\t1\t0\t1\t1\t0\t0\t0\t0\t0\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t1\t0\nStudent114\t1\t1\t0\t0\t1\t0\t0\t1\t1\t1\t0\t1\t0\t0\t0\t0\t1\t1\t1\t1\t0\t0\t1\t0\t1\nStudent115\t0\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t0\t0\nStudent116\t1\t1\t0\t0\t0\t1\t0\t1\t1\t1\t0\t1\t0\t0\t1\t1\t0\t1\t1\t1\t0\t0\t0\t1\t0\nStudent117\t1\t0\t0\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t0\nStudent118\t0\t1\t0\t1\t1\t0\t0\t1\t0\t1\t0\t0\t1\t0\t1\t1\t0\t1\t0\t1\t0\t0\t1\t1\t0\nStudent119\t1\t0\t0\t1\t0\t0\t1\t1\t0\t0\t0\t1\t0\t0\t0\t1\t0\t1\t0\t1\t0\t0\t0\t1\t1\nStudent120\t0\t1\t0\t1\t0\t1\t0\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t0\nStudent121\t0\t1\t1\t0\t1\t1\t1\t0\t0\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t0\t0\nStudent122\t0\t1\t0\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t0\t0\t0\t0\t1\t0\t0\t0\t0\t1\t0\t0\nStudent123\t0\t1\t0\t1\t0\t0\t1\t1\t0\t0\t1\t1\t0\t0\t0\t0\t0\t1\t1\t0\t0\t0\t1\t1\t1\nStudent124\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\nStudent125\t1\t1\t1\t0\t1\t0\t1\t0\t1\t1\t1\t0\t0\t0\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\nStudent126\t1\t1\t0\t0\t0\t1\t0\t1\t1\t0\t1\t1\t1\t0\t0\t0\t0\t1\t1\t1\t0\t0\t0\t1\t0\nStudent127\t1\t1\t1\t0\t0\t1\t1\t1\t0\t1\t0\t1\t1\t1\t0\t1\t0\t1\t0\t0\t0\t0\t1\t0\t0\nStudent128\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t0\t1\t0\t0\t0\t1\t0\t1\t1\t1\t0\t0\t0\t1\t0\nStudent129\t0\t0\t0\t1\t0\t0\t0\t0\t0\t0\t0\t1\t1\t1\t1\t1\t0\t1\t0\t0\t0\t0\t0\t0\t0\nStudent130\t0\t1\t0\t0\t1\t1\t0\t1\t1\t0\t1\t0\t0\t1\t1\t1\t0\t1\t1\t1\t0\t0\t1\t0\t1\nStudent131\t0\t0\t0\t1\t1\t0\t1\t0\t0\t0\t0\t0\t1\t0\t0\t0\t0\t0\t1\t0\t0\t0\t0\t0\t1", mode="r", theme="cobalt"),

                br(),
                br(),


                h3("Basic statistics and item statistics"),
                verbatimTextOutput("cutscore.ds.out"),

                p('Drop if: Cronbach alpha when the item is removed', br(),
                'r dropped: item-total correlation without the item', br(),
                'IF: item facility or item mean (proportion of those who answered the item correctly)', br(),
                'ID: item discrimination (upper 1/3 - lower 1/3)', br(),
                'rpbi: point-biserial correlation', br()
                ),

                br(),

                h3("Histogram"),

                plotOutput("distPlot2"),

                h3("Box plot with individual data points"),

                plotOutput("boxPlot2"),

                br(),

                h3("Total score and percentage (descending order)"),
                verbatimTextOutput("totalper.out"),

                br(),

                h3("Cut-score Indices"),
                numericInput("CutPoint", "Set the cut-score (in %):", 60),

                verbatimTextOutput("cutscoreInd.out"),

                p('Total IF: item facility or item mean of all test-takers', br(),
                'Pass IF: item facility of masters (over the cut-score)', br(),
                'Fail IF: item facility of non-masters (under the cut-score)', br(),
                'B: B-index (Pass IF — Fail IF)', br(),
                'A: Agreement statistic', a('(Brown & Hudson, 2002, p. 125)', href='http://bit.ly/1u76Txf', target="_blank"), br(),
                'Φ: Item phi', a('(Brown & Hudson, 2002, p. 126)', href='http://bit.ly/1u76Txf', target="_blank"), br()
                ),

                br(),

                h3("Comparison of masters and non-masters"),

                plotOutput("masternonmaster", height = "550px"),

                br(),
                br(),

                strong('R session info'),
                verbatimTextOutput("info22.out")

            ),






            tabPanel("Difference Index",

                h2("Difference Index"),

                h4("Two administrations of the test"),

                p('Note: Input values must be separated by tabs. Copy and paste from Excel/Numbers.'),
                p('If you need to create 1-0 data first, use',
                a('Binary (1-0) Data Converter', href='https://langtest.shinyapps.io/biconv/', target="_blank"), '.'),

                p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>Your data needs to have the header (variable names) in the first row.</div></b>")),

                br(),

                strong('Option:'),
                checkboxInput("rowname", label = strong("The first columns of datasets contain case names."), value = T),



                h4("Pretest"),

                aceEditor("pretest", value="varID\ti01\ti02\ti03\ti04\ti05\ti06\ti07\ti08\ti09\ti10\ti11\ti12\ti13\ti14\ti15\ti16\ti17\ti18\ti19\ti20\ti21\ti22\ti23\ti24\ti25\ti26\ti27\ti28\ti29\ti30\ti31\ti32\ti33\ti34\ti35\ti36\ti37\ti38\ti39\ti40\nStudent001\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t0\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t1\t1\t1\t0\t0\t0\t0\t0\t0\nStudent002\t1\t1\t1\t1\t1\t0\t0\t0\t0\t1\t1\t0\t0\t0\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t1\t1\t1\t0\t1\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\nStudent003\t1\t0\t1\t1\t1\t0\t0\t0\t1\t1\t1\t0\t0\t1\t0\t1\t1\t1\t1\t1\t0\t0\t1\t1\t0\t1\t0\t1\t0\t0\t0\t1\t1\t0\t0\t0\t0\t0\t0\t1\nStudent004\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t0\t1\t1\t0\t1\t0\t1\t0\t0\t0\t1\t1\t0\t1\t0\t1\t0\t0\t1\nStudent005\t1\t1\t0\t1\t1\t1\t0\t0\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t0\t0\t1\t0\t0\t0\t0\t1\t1\t0\t0\t0\t1\t1\t0\t1\nStudent006\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t0\t0\t1\t1\t0\t0\t0\t0\t0\t0\t0\t1\t1\t0\t0\t1\nStudent007\t1\t1\t1\t1\t0\t1\t1\t0\t0\t1\t1\t1\t1\t0\t0\t1\t1\t1\t0\t1\t0\t0\t1\t0\t1\t0\t1\t1\t1\t0\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\nStudent008\t0\t1\t1\t1\t1\t0\t0\t0\t1\t1\t0\t0\t0\t0\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t1\t0\t0\t0\t0\nStudent009\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t1\t0\t0\t1\t0\t1\t1\t1\t0\t0\t1\t1\t1\t0\t0\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t0\t0\t0\t1\nStudent010\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t0\t1\t0\t1\t0\t0\t1\t0\t1\t0\t0\t0\nStudent011\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t0\t0\t0\t0\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t0\t0\t0\t1\t0\t0\t0\t1\t0\t0\t1\t0\t0\t0\t0\t1\nStudent012\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t0\t1\t1\t0\t0\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t0\t1\t0\t0\t0\t1\t0\t1\t0\t1\t0\t0\t0\t1\t1\nStudent013\t0\t1\t0\t1\t1\t1\t0\t1\t0\t0\t1\t1\t0\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t0\t1\t0\t1\t0\t1\t1\t1\t0\t0\t0\t0\t1\nStudent014\t0\t1\t1\t0\t1\t0\t0\t0\t1\t0\t0\t0\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t0\t0\t0\t0\t0\t1\t0\t1\t0\t0\t0\t1\t0\t0\t0\nStudent015\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t0\t0\t1\t1\t0\t0\t1\t1\t0\t0\t0\t1\t0\t1\t0\t0\t1\t0\t1\t0\t0\t0\t1\t0\t1\t0\t0\t0\t0\t1\nStudent016\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t1\t1\t0\t0\t1\t1\t0\t1\t0\nStudent017\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t0\t0\t1\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t0\t0\t0\t0\t0\t1\t1\t1\t0\t0\t0\t0\t0\t1\nStudent018\t1\t0\t0\t1\t1\t1\t0\t0\t0\t1\t0\t1\t0\t0\t1\t1\t0\t0\t1\t1\t0\t1\t1\t0\t0\t0\t1\t1\t1\t0\t0\t0\t1\t0\t0\t0\t0\t0\t1\t0\nStudent019\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t0\t1\t0\t0\t0\t0\t1\t0\t1\t0\t1\t0\t0\t0\t0\t0\t0\t0\nStudent020\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t0\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t0\t1\t0\t0\t1\t0\t0\t0\t0\nStudent021\t0\t1\t1\t1\t1\t0\t0\t1\t0\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t0\t1\t1\t0\t1\t0\t0\t0\t1\t1\t0\t0\t1\t0\t1\t0\t0\t0\t0\t0\nStudent022\t1\t1\t1\t1\t1\t0\t0\t0\t0\t1\t0\t1\t0\t0\t0\t0\t1\t0\t0\t1\t0\t0\t1\t1\t1\t0\t1\t0\t0\t0\t0\t0\t0\t0\t1\t0\t0\t0\t0\t1\nStudent023\t1\t1\t0\t1\t1\t0\t0\t1\t0\t1\t1\t0\t0\t0\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t0\t1\t1\t1\t0\t0\t0\t1\t0\t0\nStudent024\t1\t1\t1\t1\t1\t0\t1\t0\t0\t1\t0\t0\t0\t0\t1\t1\t1\t0\t0\t1\t0\t1\t1\t0\t1\t0\t1\t0\t0\t1\t1\t1\t0\t0\t0\t0\t0\t0\t0\t0\nStudent025\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t0\t0\t1\t0\t1\t0\t1\t1\t1\t1\t0\t0\t0\t0\t0\t1\t0\t1\t1\t0\t1\nStudent026\t0\t0\t0\t1\t1\t0\t0\t0\t0\t1\t0\t0\t0\t0\t1\t0\t0\t1\t0\t1\t0\t0\t0\t0\t1\t0\t0\t1\t1\t0\t0\t1\t1\t0\t0\t1\t0\t1\t0\t1\nStudent027\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t0\t1\t0\t1\t0\t0\t0\t0\t1\t0\t1\nStudent028\t1\t0\t1\t1\t0\t1\t0\t0\t1\t1\t1\t1\t0\t0\t1\t1\t1\t0\t0\t1\t0\t0\t1\t0\t1\t1\t1\t0\t0\t1\t0\t0\t1\t1\t0\t0\t0\t1\t0\t0\nStudent029\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t0\t0\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t0\t0\t1\t0\t0\t1\t1\t1\t1\t0\t0\t1\t1\t0\nStudent030\t1\t1\t1\t1\t1\t0\t0\t0\t1\t0\t0\t0\t0\t0\t0\t0\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t0\t0\t1\t0\t1\t0\t0\t0\t0\t1\nStudent031\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t1\t0\t1\t0\t1\t1\t1\t1\t1\t0\t0\t0\t0\t1\t0\t0\t0\t0\t0\t0\t0\t0\t1\t0\t1\t0\t1\t0\nStudent032\t0\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t0\t1\t0\t0\t1\t0\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t0\t0\t0\t0\t0\t0\t0\t1\t0\t0\t0\t0\t0\nStudent033\t1\t0\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t0\t0\t0\t1\t1\t0\t1\t0\t1\t0\t0\t1\t0\t0\t0\t1\t0\nStudent034\t1\t1\t1\t0\t1\t1\t0\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t0\t1\t1\t0\t0\t0\t1\t1\t1\t1\t0\t0\t0\t0\t0\t1\t0\t0\t0\t1\nStudent035\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t0\t1\t0\t1\t1\t1\t0\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t0\t0\t0\t1\t0\t0\t0\t0\nStudent036\t1\t0\t1\t0\t1\t1\t0\t0\t0\t1\t0\t0\t0\t0\t1\t1\t1\t0\t1\t0\t0\t1\t1\t1\t0\t0\t1\t0\t0\t0\t1\t1\t0\t0\t0\t0\t0\t0\t0\t0\nStudent037\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t0\t1\t0\t0\t1\t1\t0\t0\t0\t1\t1\t1\t0\t0\t0\t1\t1\t0\t1\t0\nStudent038\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t0\t0\t1\t1\t0\t0\t0\t0\t1\t1\nStudent039\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t0\t1\t0\t1\t1\t1\t0\t1\t0\nStudent040\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t1\t0\t0\t0\t1\t0\t1\t0\t0\t1\t0\t1\t1\t1\t1\t0\t0\t0\t0\t1\t0\t1\t0\t0\t0\t0\t0\t0\t0\t1\nStudent041\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t0\t0\t1\t0\t0\t1\t1\t0\t0\t1\t1\t0\t0\t0\nStudent042\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t0\t0\t0\t1\t1\t0\t1\t0\t1\t0\t1\t0\t1\t1\t0\t1\t0\t0\t0\t1\t1\t0\t0\t0\t0\t1\nStudent043\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t0\t0\t1\t1\t0\t1\t0\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\t1\t1\t0\t0\t0\nStudent044\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t0\t1\nStudent045\t0\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t1\t0\t0\t0\t1\t1\t0\t1\t0\t1\t1\t1\t1\t0\t0\t0\t1\t1\t1\t0\t0\t1\t0\t1\t1\t0\t0\t0\nStudent046\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t0\t1\t1\t0\t1\t1\t0\t0\t1\t1\t0\t0\t0\t1\t0\t1\t1\t0\t1\t0\t0\t1\t1\nStudent047\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t0\t0\t1\t1\t1\t1\t1\t0\t1\t0\t0\t1\t0\t1\t0\t1\t1\t0\t0\t0\t0\t0\t1\t0\t0\t0\t1\nStudent048\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t0\t0\t1\t1\t1\t0\t1\t0\t1\t0\t0\t1\t0\t0\t0\t1\t1\t1\t0\t0\t1\t0\t1\t0\t0\t0\t1\nStudent049\t0\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t0\t1\t0\t0\t0\t1\t0\t0\t1\t1\t0\t1\t0\t0\t0\t0\t0\t0\t0\t0\t1\t0\t0\nStudent050\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t0\t1\t1\t0\t1\t1\t1\t1\t0\t0\t1\t0\t1\t0\t1\t0\t0\t0\t1\t0\t0\t0\t1\t0\t1\t0\nStudent051\t0\t0\t0\t1\t1\t1\t0\t0\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t1\t0\t1\t0\t1\t0\t1\t0\nStudent052\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t0\t1\t1\t0\t1\t1\t1\t0\t0\t1\t1\t0\t0\t0\t1\t0\t1\t1\t0\t1\t1\t0\t0\t1\t1\t0\t1\t0\t0\t1\nStudent053\t1\t1\t1\t0\t1\t0\t0\t0\t1\t0\t0\t1\t0\t0\t0\t1\t1\t0\t0\t0\t1\t1\t1\t0\t0\t0\t0\t0\t0\t1\t1\t1\t0\t0\t0\t0\t0\t0\t1\t0\nStudent054\t1\t0\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t0\t1\t1\t0\t1\t0\t1\t0\t1\t1\t1\t1\t1\t1\t0\t0\t0\t0\t1\t1\t0\t0\t0\t1\t0\t0\nStudent055\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t0\t0\t0\t0\t0\t0\t0\nStudent056\t1\t1\t1\t1\t1\t0\t1\t1\t0\t0\t0\t0\t1\t0\t0\t0\t1\t1\t0\t0\t0\t1\t1\t1\t1\t0\t0\t0\t0\t0\t0\t1\t0\t0\t0\t1\t0\t0\t1\t0\nStudent057\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t0\t0\t0\t1\t0\t0\t1\t0\t1\t0\nStudent058\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t0\t0\t0\t0\t1\t1\t0\t0\t0\t0\t1\nStudent059\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t0\t0\t0\t1\t1\t0\t1\t1\t1\t0\t1\nStudent060\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t0\t1\t0\t1\t0\t1\t1\t0\t0\t0\t1\t0\t1\t0\t0\t0\t0\t1\t0\t0\t0\t0\t0\t0\t1\t0\t0\t0\t1\t1\nStudent061\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\nStudent062\t1\t0\t0\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t0\t0\t1\t1\t0\t0\t1\t0\t0\t0\t0\t0\t1\t1\t1\t0\t0\t0\t0\t1\t0\t0\t0\t0\t0\t1\t1\nStudent063\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\nStudent064\t1\t1\t0\t1\t0\t0\t0\t0\t1\t1\t0\t0\t1\t0\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t0\t0\t1\t0\t0\t0\t0\t0\t1\nStudent065\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t0\t1\t1\t1\t0\t1\t0\t1\t0\t0\t1\t0\t0\t0\t0\nStudent066\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t0\t1\t1\t1\t0\t0\t1\t1\t1\t1\t0\t1\t0\t1\t0\t0\t0\t0\t0\t0\t1\t0\t0\t1\t1\t0\t1\nStudent067\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t0\t0\t1\t1\t0\t1\t0\nStudent068\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t0\t0\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t0\t0\t0\t0\t0\t0\t1\t0\t1\t1\t0\t1\t0\t1\t0\t0\t1\nStudent069\t1\t1\t1\t0\t1\t1\t0\t0\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t0\t0\t1\t0\t1\t1\t0\t0\t1\t1\t0\t1\t1\nStudent070\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t0\t1\t0\t1\t0\t1\t1\t1\t1\t0\t1\t0\t1\t0\t1\t0\t0\t0\t0\t0\t1\t0\t0\t0\t0\t0\t0\t1\nStudent071\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t1\t0\t0\t1\t1\t1\t1\t1\t0\t0\t1\t0\t0\t0\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t0\t0\t0\t0\t1\nStudent072\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t0\t1\t0\t1\t0\t1\t1\t0\t0\t0\t1\t1\t1\t0\t0\t0\t1\nStudent073\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t0\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t0\t1\t1\t1\t0\t1\t1\t1\t1\t1\nStudent074\t0\t1\t0\t0\t1\t1\t1\t1\t0\t1\t0\t1\t0\t0\t0\t1\t1\t0\t1\t0\t0\t1\t0\t1\t0\t0\t0\t0\t1\t0\t0\t0\t1\t0\t0\t0\t1\t0\t0\t1\nStudent075\t1\t1\t0\t0\t1\t0\t0\t0\t1\t1\t0\t0\t0\t0\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t0\t0\t0\t0\t0\t0\t1\t1\t0\t1\nStudent076\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t1\t1\t0\t0\t0\t0\t0\t0\t0\nStudent077\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t1\t0\t0\t1\t1\t1\t1\t0\t1\t1\t1\t0\t0\t1\t1\t0\t0\t0\t0\t0\t0\t0\t1\t1\t1\t0\t0\t1\t0\t1\nStudent078\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t0\t0\t1\t0\t1\t0\t0\t1\t0\t0\t0\t1\t1\t1\t1\t0\t0\t0\nStudent079\t0\t1\t0\t1\t1\t1\t0\t1\t0\t1\t1\t0\t0\t0\t1\t1\t1\t1\t1\t1\t0\t0\t0\t0\t1\t1\t1\t0\t0\t0\t1\t0\t1\t1\t1\t1\t0\t0\t0\t1\nStudent080\t0\t1\t0\t1\t1\t1\t0\t0\t0\t1\t0\t1\t0\t0\t0\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t0\t0\t0\t1\t0\t0\t0\t1\t0\t0\t0\nStudent081\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\nStudent082\t1\t0\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t0\t1\t1\t1\t1\t0\t0\t1\t0\t1\t0\t1\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t0\t1\t0\t1\nStudent083\t1\t1\t1\t1\t1\t0\t0\t0\t1\t0\t1\t0\t0\t0\t0\t1\t1\t1\t0\t1\t0\t0\t0\t0\t0\t1\t1\t0\t0\t1\t0\t0\t0\t1\t1\t1\t1\t0\t0\t0\nStudent084\t1\t1\t0\t1\t1\t1\t0\t0\t1\t1\t0\t1\t0\t0\t0\t0\t0\t0\t1\t0\t0\t0\t1\t0\t1\t0\t1\t0\t0\t1\t0\t0\t0\t1\t1\t0\t1\t0\t1\t0\nStudent085\t0\t0\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t0\t0\t1\t1\t0\t1\t1\t0\t0\t0\nStudent086\t1\t0\t0\t1\t1\t1\t0\t1\t0\t1\t1\t1\t0\t0\t0\t1\t0\t0\t0\t0\t1\t0\t1\t0\t0\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\t1\t0\t0\t0\t1\nStudent087\t1\t0\t0\t1\t1\t0\t1\t0\t1\t0\t1\t0\t0\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t0\t0\t0\t0\t1\t1\t0\t1\t0\t1\t0\nStudent088\t1\t1\t1\t0\t1\t0\t0\t0\t0\t1\t0\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t1\t0\t0\t1\nStudent089\t1\t1\t0\t1\t1\t1\t1\t0\t1\t0\t0\t1\t0\t1\t0\t1\t1\t0\t1\t0\t0\t1\t0\t1\t0\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t1\t0\t0\nStudent090\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t0\t1\t0\t1\t1\t1\t0\t1\t1\t0\t0\t1\t0\t0\t0\t1\t1\t0\t1\t0\t0\t1\t1\t0\t0\t0\t0\t0\t0\nStudent091\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t0\t1\t0\t0\t1\t1\t0\t0\t0\t0\t1\t0\t1\t0\t1\nStudent092\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t0\t0\t1\t0\t1\t1\t1\t0\t1\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t1\t0\t1\t1\t0\t0\nStudent093\t0\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t0\t1\t1\t1\t0\t0\t1\t0\t1\t0\t1\t0\t0\t0\t1\t1\t0\t1\t0\t0\t0\t0\t0\t1\t0\t0\t0\nStudent094\t0\t1\t1\t0\t1\t0\t0\t0\t1\t1\t1\t0\t1\t0\t0\t0\t1\t1\t0\t0\t1\t1\t0\t1\t1\t1\t0\t1\t0\t0\t0\t0\t1\t1\t1\t0\t0\t0\t0\t1\nStudent095\t1\t0\t0\t1\t1\t0\t1\t1\t0\t1\t0\t0\t0\t0\t0\t1\t1\t0\t0\t1\t1\t0\t1\t1\t1\t1\t1\t0\t0\t1\t0\t0\t0\t1\t1\t1\t0\t1\t0\t0\nStudent096\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t0\t1\t0\t0\t1\t0\t0\t0\t1\nStudent097\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t0\t0\t1\t0\t0\t1\t1\t1\t0\t0\t1\t0\t1\t0\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t1\t0\nStudent098\t1\t0\t0\t1\t1\t1\t1\t0\t0\t1\t0\t0\t0\t0\t0\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t0\t1\t0\t1\t0\t0\t0\t0\t1\t1\t0\t0\t0\t0\t1\nStudent099\t1\t0\t1\t1\t1\t1\t0\t1\t1\t0\t0\t1\t0\t0\t0\t0\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t0\t0\t1\t0\t0\t0\t1\t0\t0\t0\t1\t1\t0\t0\nStudent100\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t0\t0\t0\t0\t1\t0\t0\t0\t1\t0\t1\t1\t0\t0\t0\t1\t1\t0\t0\t0\t0\t1\nStudent101\t0\t1\t0\t1\t1\t1\t1\t0\t0\t1\t1\t0\t0\t0\t0\t0\t1\t1\t1\t1\t0\t1\t0\t0\t1\t1\t1\t0\t1\t0\t0\t0\t1\t0\t1\t0\t1\t0\t0\t0\nStudent102\t1\t0\t1\t1\t1\t0\t1\t0\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t1\nStudent103\t1\t1\t1\t0\t1\t0\t0\t1\t1\t1\t0\t1\t0\t1\t0\t1\t1\t0\t0\t0\t0\t0\t0\t1\t0\t0\t1\t0\t0\t0\t0\t1\t1\t0\t0\t0\t0\t1\t0\t0\nStudent104\t1\t1\t0\t0\t0\t0\t0\t1\t0\t1\t1\t0\t1\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\nStudent105\t0\t0\t0\t1\t1\t1\t0\t0\t1\t0\t0\t1\t1\t0\t0\t1\t1\t0\t0\t0\t1\t0\t1\t0\t0\t1\t1\t0\t1\t0\t0\t0\t0\t0\t1\t0\t0\t0\t0\t0\nStudent106\t0\t1\t1\t1\t1\t0\t0\t0\t0\t1\t1\t0\t1\t0\t0\t1\t1\t0\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t0\t0\t1\t0\t1\t1\t1\t0\t0\t0\nStudent107\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t0\t0\t0\t1\t1\t1\nStudent108\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t0\t0\t1\t1\t0\t1\t1\t1\t0\t0\t1\t0\t0\t0\t1\t1\t1\t0\t0\t0\t1\t0\t0\nStudent109\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t0\t0\t1\t1\t1\t0\t0\t1\t0\t0\t0\t0\t0\t0\t1\t0\t0\t0\t1\t1\t0\nStudent110\t1\t0\t1\t0\t1\t1\t0\t0\t1\t1\t1\t0\t1\t0\t0\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t0\t0\t0\t0\t0\t0\t1\t0\t0\t1\t0\t0\t0\t1\t0\nStudent111\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t0\t0\t0\t1\t0\t1\t1\t1\t0\t1\t1\t0\t1\t0\t0\t0\t1\t1\t1\t0\t1\t0\t0\t0\t0\t0\t0\t1\nStudent112\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t0\t1\t0\t0\t0\t0\t0\t0\t0\t1\t1\t1\t0\t0\t1\nStudent113\t1\t1\t1\t1\t1\t1\t0\t0\t0\t0\t0\t1\t1\t0\t0\t1\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t0\t0\t0\t1\t0\t1\t0\t1\t0\t0\nStudent114\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t0\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t0\t0\t0\t0\t1\t0\t1\t1\t1\nStudent115\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t0\t0\t0\t0\t0\t0\t1\t0\t0\t0\t0\t0\nStudent116\t1\t0\t1\t1\t1\t1\t1\t0\t0\t1\t1\t0\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t0\t1\t1\t0\t0\t0\t0\t0\t0\t0\nStudent117\t0\t1\t0\t1\t1\t1\t0\t0\t1\t1\t1\t0\t1\t0\t0\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t0\t0\t0\t0\t0\t1\t0\t1\t0\t0\t0\t1\t0\t1\t1\nStudent118\t1\t0\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t0\t0\t1\t1\t0\t1\t0\t0\t1\t0\t0\t1\t0\t0\t1\t1\t1\t0\t0\t0\t1\t0\t1\t1\t0\t1\t0\nStudent119\t1\t1\t0\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t1\t1\t0\t0\t0\t1\t0\t0\t1\t0\t0\t1\t0\t0\nStudent120\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t1\t1\t0\t1\t0\t0\t1\t1\t0\t0\t0\t1\t1\t0\t0\t0\t0\t0\t0\nStudent121\t0\t1\t1\t1\t1\t0\t0\t1\t0\t0\t0\t1\t0\t0\t1\t0\t1\t1\t1\t1\t1\t0\t0\t1\t0\t0\t0\t0\t0\t1\t0\t0\t1\t1\t1\t0\t1\t0\t1\t1\nStudent122\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t0\t0\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t0\t1\t0\t1\t0\t0\t0\t0\t0\t1\t0\t0\t0\t0\t1\nStudent123\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t0\t1\t1\t0\t1\t1\t1\t0\t0\t0\t1\t1\t0\t0\t0\t1\t1\t0\nStudent124\t1\t1\t0\t1\t1\t1\t0\t0\t0\t1\t0\t0\t0\t0\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\nStudent125\t1\t1\t0\t1\t1\t0\t0\t0\t1\t1\t1\t0\t1\t0\t0\t0\t1\t0\t0\t1\t1\t1\t0\t0\t0\t0\t0\t1\t0\t1\t0\t0\t1\t0\t0\t0\t0\t0\t0\t0\nStudent126\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t1\t1\t1\t0\t0\t1\t0\t0\t0\t1\nStudent127\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t0\t0\t0\t0\t0\t1\t0\t0\t1\t1\t0\t1\t0\t1\nStudent128\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t0\t1\t0\t1\t0\t1\t1\t0\t0\t0\t1\t0\t0\t0\t0\t1\t0\t0\t0\t1\t0\t0\t1\t0\t0\t0\t0\t0\t0\nStudent129\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t0\t0\t1\t0\t0\t1\t1\t1\t0\t1\t0\t0\t1\t0\t1\t1\t0\t1\t0\t0\t1\t0\t1\t0\t1\t0\t0\t1\t0\t1\nStudent130\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t0\t1\t0\t0\t0\t1\t1\t0\t0\t1\t1\t1\t1\t0\t0\nStudent131\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t0\t0\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\nStudent132\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t0\t0\t0\t0\t0\t1\t0\t0\t0\t0\t0\t0\t0\t1\nStudent133\t1\t1\t0\t1\t0\t0\t0\t0\t0\t1\t1\t0\t0\t0\t0\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t0\t1\t0\t0\t0\t0\t0\t1\t1\t0\t0\t0\t1\t0\t1\nStudent134\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t0\t0\t1\t1\t1\t1\t0\t0\t0\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t0\t1\nStudent135\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t0\t1\t0\t1\t1\t0\t0\t1\t1\t1\t1\t1\nStudent136\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t0\t0\t1\t1\t0\t0\t1\t0\t0\t0\t0\t0\t1\t0\t1\t0\t0\t0\t0\t1\t1\t1\t0\t0\t1\t1\t1\nStudent137\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t0\t1\t0\t1\t0\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\nStudent138\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t0\t1\t1\t0\t0\t1\t1\t1\t0\t0\t1\t0\nStudent139\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t0\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t0\t1\t0\t0\t0\t0\t1\t0\t1\t0\t0\t1\nStudent140\t1\t0\t1\t1\t1\t1\t1\t0\t0\t1\t1\t0\t0\t0\t0\t1\t1\t1\t1\t0\t1\t0\t0\t0\t1\t1\t1\t1\t0\t0\t0\t1\t1\t0\t0\t1\t0\t0\t0\t0\nStudent141\t0\t0\t1\t1\t1\t1\t0\t0\t1\t1\t1\t0\t0\t0\t1\t1\t1\t1\t1\t1\t0\t1\t0\t0\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t0\t0\t0\t1\nStudent142\t1\t1\t1\t0\t1\t0\t0\t0\t1\t1\t1\t0\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t0\t0\t0\t0\t1\t0\t0\t0\t1\t0\t1\t0\nStudent143\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t0\t0\t1\t0\t1\t0\t1\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\nStudent144\t1\t1\t0\t1\t1\t1\t0\t0\t0\t0\t0\t0\t0\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t0\t1\t1\t0\t1\t0\t0\t0\t0\t0\t0\t0\t0\nStudent145\t1\t0\t1\t1\t1\t1\t1\t0\t0\t1\t0\t0\t0\t0\t0\t1\t0\t0\t0\t0\t0\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t1\t1\t0\t0\t0\t0\t1\nStudent146\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t0\t0\t1\t1\t0\t1\t0\t1\t1\t0\t0\t1\t1\t0\t0\t1\t0\t1\t0\t1\t0\t1\t0\t0\t1\t1\t1\nStudent147\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t0\t1\t0\t0\t0\t1\t0\t0\t1\t1\t1\t0\t0\t1\t0\t0\t0\t1\t1\t0\t1\t1\t0\t1\t1\t0\t0\t0\t1\nStudent148\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t0\t0\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t1\t0\t0\t1\t0\t0\t1\t1\t0\t1\t0\t0\t1\nStudent149\t1\t1\t0\t1\t1\t1\t0\t1\t0\t1\t1\t0\t1\t0\t0\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t1\t1\t1\t1\t0\t1\t0\t0\t0\nStudent150\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t1\t1\t0\t0\t1\t1\t0\nStudent151\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t0\t0\t1\t1\t0\t0\t0\t0\t0\t1\t0\nStudent152\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t0\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\nStudent153\t1\t1\t1\t1\t1\t0\t0\t0\t1\t0\t1\t0\t0\t1\t0\t1\t1\t0\t1\t1\t0\t0\t1\t0\t0\t0\t1\t0\t1\t0\t1\t0\t0\t1\t0\t1\t1\t0\t1\t0\nStudent154\t1\t0\t1\t0\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t1\t1\t0\t0\t1\t0\t1\t0\t1\t0\t0\t1\t0\t1\t0\t1\t0\t0\t0\t0\t0\t0\nStudent155\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t0\t1\t0\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t0\t1\t0\t0\t1\t1\t1\t0\t1\t0\t1\t0\nStudent156\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t0\t1\t1\t0\t0\t1\t1\t0\t0\t1\t1\nStudent157\t1\t1\t1\t1\t1\t0\t0\t1\t1\t0\t1\t1\t0\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t0\t0\t1\t1\t0\t1", mode="r", theme="cobalt"),

                br(),

                h4("Posttest"),

                aceEditor("posttest", value="varID\ti01\ti02\ti03\ti04\ti05\ti06\ti07\ti08\ti09\ti10\ti11\ti12\ti13\ti14\ti15\ti16\ti17\ti18\ti19\ti20\ti21\ti22\ti23\ti24\ti25\ti26\ti27\ti28\ti29\ti30\ti31\ti32\ti33\ti34\ti35\ti36\ti37\ti38\ti39\ti40\nStudent001\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t0\nStudent002\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t0\t0\t0\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t0\t0\t1\t1\nStudent003\t1\t1\t1\t1\t1\t0\t0\t0\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t0\t0\t1\t1\t1\t0\t0\t0\t0\t0\t1\t0\t1\t0\t0\t0\t1\nStudent004\t1\t1\t0\t1\t1\t0\t1\t0\t1\t0\t0\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t0\t0\t0\t1\t0\t0\t1\t0\t0\t0\t0\t1\t0\t1\nStudent005\t0\t1\t0\t1\t1\t1\t1\t0\t1\t1\t0\t0\t0\t0\t0\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t0\t1\t1\t1\t0\t0\t1\t1\t0\t0\nStudent006\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t0\t0\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t0\t0\t0\t1\t1\nStudent007\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t1\t1\t1\t0\nStudent008\t1\t0\t1\t0\t1\t0\t0\t1\t0\t0\t0\t1\t0\t0\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t0\t0\t0\t0\t1\t0\t0\t1\t0\t0\t0\t1\t0\t0\t0\nStudent009\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t1\t1\t1\t1\t0\t0\t1\t0\t1\t1\t1\t1\t0\t1\t0\t0\t1\t1\t0\t1\t1\t0\t1\t0\t1\nStudent010\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t0\t0\t0\t0\t1\t0\t1\t1\nStudent011\t1\t0\t0\t1\t1\t0\t0\t1\t0\t1\t0\t0\t1\t0\t0\t0\t0\t1\t1\t1\t0\t1\t1\t0\t0\t0\t0\t1\t0\t0\t0\t0\t1\t0\t0\t0\t0\t0\t0\t1\nStudent012\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t0\t1\t0\t1\t0\t1\t0\t1\t0\t0\t0\t0\t0\t1\t0\nStudent013\t0\t0\t0\t1\t1\t1\t1\t1\t0\t1\t1\t0\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t0\t1\t1\t1\t0\t1\t1\t0\t0\t0\t0\t0\t0\nStudent014\t1\t1\t0\t1\t1\t0\t1\t0\t0\t1\t1\t0\t0\t1\t0\t1\t0\t0\t1\t0\t1\t1\t0\t0\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t1\nStudent015\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t0\t0\t0\t1\t1\t1\t0\t0\t0\t0\t1\nStudent016\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t1\t0\t1\t1\t1\t1\t1\t1\nStudent017\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t0\t1\t1\t1\t0\t0\t0\t0\t1\t1\t1\nStudent018\t0\t0\t0\t1\t1\t1\t1\t1\t1\t0\t0\t0\t0\t1\t0\t1\t1\t1\t0\t1\t1\t0\t1\t0\t0\t0\t1\t1\t1\t0\t1\t0\t0\t1\t0\t0\t0\t1\t0\t0\nStudent019\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t0\t1\t0\t0\t1\t1\t1\t1\t0\t0\t0\t1\t1\t1\t0\t1\t1\t0\t1\t0\t0\t0\t1\t1\t1\t0\t0\t0\t0\t1\nStudent020\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t0\t1\t0\t1\t0\t1\nStudent021\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t0\t0\t1\t0\t1\t0\t1\t0\t0\t1\t1\t0\t1\t0\t1\t0\t0\t0\t0\t0\t0\t0\t1\t0\t0\t0\t0\t0\nStudent022\t1\t0\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t0\t0\t1\t1\t1\t1\t0\t1\t0\t0\t1\t0\t1\t1\t1\t0\t1\t1\t0\t1\t0\t0\t0\t1\t0\t0\t0\t1\nStudent023\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t1\t1\t1\t1\t1\t0\t0\t0\t0\t1\t0\t1\nStudent024\t1\t0\t1\t1\t1\t0\t0\t1\t0\t1\t1\t0\t0\t0\t1\t1\t1\t1\t0\t0\t1\t1\t1\t0\t1\t0\t1\t0\t1\t0\t0\t0\t1\t0\t0\t0\t1\t1\t0\t1\nStudent025\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t0\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t0\t0\nStudent026\t1\t1\t0\t1\t1\t1\t1\t0\t0\t1\t0\t0\t1\t0\t0\t1\t1\t1\t1\t0\t0\t0\t1\t0\t1\t1\t1\t0\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\t1\t1\nStudent027\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t0\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t0\t1\t1\t1\t0\t0\t0\t0\t0\t0\t1\nStudent028\t1\t0\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t1\t1\t1\t1\t0\t1\t1\t1\t0\t0\t1\t0\t0\t1\t1\t1\t1\t0\t0\t0\t0\t1\t1\t0\t0\t1\nStudent029\t1\t1\t0\t1\t1\t1\t0\t1\t0\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t0\t0\t0\t0\t1\t0\t1\t1\t0\t1\t1\t1\nStudent030\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t0\t0\t0\t0\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t0\t0\t1\t0\t1\t1\t1\t0\t0\t1\nStudent031\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t0\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t0\t1\t1\t0\t0\t1\nStudent032\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t0\t1\t0\t1\t1\t0\t0\t1\t0\t1\t1\t0\t0\t0\t1\nStudent033\t1\t1\t0\t1\t1\t1\t0\t0\t1\t1\t1\t0\t0\t0\t0\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t0\t1\t0\t1\nStudent034\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t1\t1\t1\t0\t0\t1\t1\t1\t0\t1\t0\t0\t1\t1\t0\t0\t0\t1\t0\t1\t0\t0\t1\nStudent035\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t0\t1\t1\t1\t1\t0\t0\t1\t1\t0\t0\t1\t0\t0\t0\nStudent036\t1\t1\t1\t1\t1\t1\t0\t0\t0\t1\t0\t0\t0\t0\t0\t1\t1\t0\t1\t0\t0\t1\t1\t1\t0\t1\t1\t0\t1\t0\t1\t1\t0\t0\t0\t0\t1\t1\t0\t1\nStudent037\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t1\t1\t1\t1\t0\t0\t0\t1\t1\t0\t0\t1\nStudent038\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t0\t0\t1\t1\t1\t1\t1\nStudent039\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\nStudent040\t1\t1\t1\t1\t0\t1\t0\t0\t1\t1\t1\t0\t0\t1\t1\t1\t1\t0\t1\t1\t0\t0\t1\t0\t0\t0\t1\t1\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\nStudent041\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t0\t0\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t0\t1\t1\t1\t0\t0\t1\t0\t1\t0\t0\t1\t0\t0\t1\nStudent042\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t0\t1\nStudent043\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t0\t1\t1\t0\t1\t0\t0\nStudent044\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\nStudent045\t1\t1\t1\t1\t1\t0\t1\t0\t0\t1\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t1\t0\t0\t1\t0\t0\t0\t1\t1\t0\t0\t0\t0\t0\t0\t1\t1\t0\t0\t1\nStudent046\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t1\t1\t1\t0\t1\t1\t1\t0\t0\t1\t1\t0\t0\t1\t0\t0\t1\t1\t1\t0\t0\t0\t0\t0\nStudent047\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t0\t0\t0\t0\t1\t0\t1\t0\t1\t0\t0\t0\t0\t1\nStudent048\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t0\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t0\t1\t0\t0\t1\t0\t0\t0\t0\t0\t0\t1\nStudent049\t1\t0\t1\t1\t1\t0\t0\t1\t0\t1\t0\t0\t0\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t0\t0\t0\t1\t1\t0\nStudent050\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t0\t0\t1\t1\nStudent051\t1\t0\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t0\t1\t1\t0\t1\t0\t1\t0\t1\t0\t1\t1\t0\t0\t1\t0\t0\t1\nStudent052\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t0\t0\t0\t0\t1\t1\nStudent053\t1\t1\t1\t0\t1\t0\t0\t0\t0\t0\t1\t1\t0\t0\t0\t1\t1\t0\t0\t1\t0\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t1\t0\t0\t0\t0\t0\t0\t1\nStudent054\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t0\t0\t1\t1\t0\t1\t1\t0\t1\t0\t0\t0\t1\t0\t1\t0\t0\t0\t0\t1\nStudent055\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t0\t1\t0\t0\t0\t0\t0\t0\t1\nStudent056\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t0\t1\t0\t0\t1\t0\t0\t0\t0\t1\t1\t1\t0\t0\t0\t0\t1\t1\t1\t1\t0\t0\t1\t0\t1\nStudent057\t1\t0\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t1\t1\t1\t0\t1\t0\t0\t0\t1\t1\t0\t0\t0\t0\t0\t1\nStudent058\t0\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t0\t0\t1\t1\t1\t0\t1\t0\t0\nStudent059\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t0\t1\t0\t0\t1\nStudent060\t1\t1\t0\t1\t1\t1\t0\t0\t0\t1\t0\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t0\t0\t0\t0\t1\t1\nStudent061\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\nStudent062\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t0\t0\t0\t1\t1\t0\t1\t1\t0\t0\t1\t1\t0\t0\t1\t0\t1\t0\t0\t1\t0\t0\t0\t0\t1\t1\t1\nStudent063\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t1\t0\t1\t0\t1\t1\t1\t1\t0\t1\t0\t0\t1\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\nStudent064\t1\t0\t1\t1\t1\t1\t0\t1\t0\t1\t0\t0\t1\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t0\t0\t0\t0\t0\t1\t0\t0\t1\t0\t0\t1\nStudent065\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\t0\t0\t1\t1\t1\t1\t0\t1\nStudent066\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\nStudent067\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t0\t0\t1\t1\t1\t0\t0\nStudent068\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\nStudent069\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t0\t1\t1\t0\t1\t1\t0\t1\t1\t1\nStudent070\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t0\t0\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t0\t0\t1\t0\t0\t0\t0\t1\t0\t0\t0\t1\nStudent071\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t0\t1\t0\t1\t0\t1\t0\t0\t1\nStudent072\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t0\t1\t0\t0\t0\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t0\t0\t0\t1\nStudent073\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t1\nStudent074\t0\t1\t0\t0\t1\t1\t0\t1\t0\t1\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t0\t1\t1\t1\t0\t0\t1\t0\t0\t0\t0\t1\t1\t0\t1\nStudent075\t1\t0\t1\t1\t1\t0\t0\t1\t0\t1\t0\t0\t0\t0\t0\t1\t1\t0\t1\t1\t0\t0\t0\t0\t1\t0\t1\t0\t1\t1\t0\t0\t1\t0\t1\t0\t0\t1\t0\t1\nStudent076\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t0\t0\t1\t1\t1\t1\t0\t0\t1\t0\t0\t1\nStudent077\t1\t1\t1\t1\t1\t0\t0\t1\t1\t0\t1\t1\t0\t0\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t1\t0\t1\t0\t1\t0\t0\t1\t0\t1\nStudent078\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t0\t0\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t0\t1\t0\t0\t0\t1\nStudent079\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t1\t0\t0\t0\t0\t0\t1\t1\t1\t1\t0\t0\t1\t1\t0\t0\t0\t0\t0\t0\t1\t0\t0\t0\t1\t1\t0\t0\t0\t1\nStudent080\t1\t1\t1\t1\t1\t1\t0\t0\t0\t1\t0\t1\t1\t0\t0\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t0\t0\t0\t0\t1\t1\t0\t0\t1\t1\t0\t1\nStudent081\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t0\t1\t0\t0\t0\t0\t0\t1\nStudent082\t1\t0\t0\t1\t1\t0\t1\t0\t1\t0\t1\t0\t0\t0\t1\t0\t1\t0\t0\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t0\t0\t1\t0\t0\t1\t0\t1\t0\t1\nStudent083\t1\t1\t0\t0\t0\t1\t0\t0\t1\t1\t0\t0\t0\t0\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t0\t1\t0\t1\t1\nStudent085\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t1\t0\t0\t0\t0\t1\nStudent086\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t0\t1\t1\t1\t1\t1\t0\t0\t0\t1\t1\t1\t1\t1\t0\t0\t0\t0\t1\t0\t1\t0\t1\t1\t0\t1\nStudent087\t1\t0\t1\t1\t1\t1\t1\t1\t0\t0\t0\t1\t0\t0\t0\t0\t1\t1\t0\t0\t1\t1\t0\t0\t1\t1\t1\t0\t1\t0\t0\t0\t1\t1\t0\t0\t1\t0\t0\t1\nStudent088\t0\t1\t1\t1\t0\t1\t1\t1\t0\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t0\t0\t0\t1\t0\t0\t0\t1\t0\t0\t0\t1\nStudent089\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t0\t1\t1\t0\t0\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t0\t0\t0\t1\t0\t0\t0\t0\t0\t0\t1\nStudent090\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t0\t0\t1\t1\t0\t1\t0\t0\t1\t1\t0\t1\t0\t1\t1\t1\nStudent091\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t0\t0\t0\t1\t1\t0\t0\t0\t1\t0\t0\t1\t0\t0\nStudent092\t1\t1\t1\t1\t1\t0\t0\t1\t1\t0\t0\t0\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t0\t0\t0\t1\t0\t0\t0\t0\t1\t0\t1\t0\t1\nStudent093\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t0\t1\t1\t1\t1\t1\t1\t0\t0\t0\t0\t0\t0\t1\t1\t0\t1\t0\t0\t0\t1\t0\t1\t0\t1\t0\t1\nStudent094\t1\t1\t0\t0\t1\t1\t0\t1\t1\t1\t1\t0\t0\t0\t0\t1\t1\t1\t0\t0\t0\t0\t1\t1\t1\t0\t0\t0\t0\t1\t0\t0\t1\t0\t0\t0\t0\t0\t0\t1\nStudent095\t1\t0\t0\t1\t1\t0\t1\t1\t0\t1\t1\t1\t0\t0\t1\t0\t1\t1\t0\t1\t1\t0\t1\t0\t1\t1\t1\t0\t1\t0\t0\t1\t1\t0\t0\t0\t0\t0\t0\t1\nStudent096\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t0\t1\t0\t1\t0\t0\t0\t1\t0\t0\t0\nStudent097\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t0\t0\t0\t1\t0\t0\t1\t0\t0\t1\t0\t1\t0\t0\t0\t0\t0\t0\t1\nStudent098\t1\t1\t0\t0\t1\t0\t0\t1\t0\t1\t0\t0\t0\t0\t1\t1\t1\t0\t1\t0\t1\t0\t0\t1\t0\t0\t0\t0\t0\t1\t0\t0\t0\t0\t1\t0\t0\t1\t0\t1\nStudent099\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t0\t0\t1\t0\t0\t1\t1\t0\t1\nStudent100\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t0\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t0\t0\t1\t1\t0\t1\t1\t1\t0\t0\t0\t0\t1\nStudent101\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t0\t0\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t0\t1\t0\t1\t1\t0\t1\t1\t0\t0\t0\t1\t0\t1\t0\t0\t0\nStudent102\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t0\t0\t0\t0\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\nStudent103\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t0\t0\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t0\t1\t1\t0\t0\t0\t0\t0\t1\t1\t1\t1\t1\t1\nStudent104\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t0\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t1\t0\t0\t0\t1\nStudent105\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t1\t0\t1\t0\t0\t1\t1\t1\t0\t0\t1\t0\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t0\t0\t0\t0\t0\t0\t0\nStudent106\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t0\t0\t0\t1\t1\t0\t0\t0\t0\t1\t0\t1\nStudent107\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\nStudent108\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t0\t1\t0\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t0\t1\t0\t0\t1\t0\t0\t0\t0\t0\t0\t1\nStudent109\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t0\t1\t0\t1\t0\t1\t1\t1\t1\t1\t0\t1\t0\t0\t1\t0\t0\t0\t1\t0\t1\t0\t1\nStudent110\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t1\t1\t0\t0\t0\t0\t1\t1\t1\t0\t1\t1\t1\t0\t0\nStudent111\t1\t0\t1\t1\t1\t0\t1\t0\t0\t0\t1\t0\t1\t0\t0\t0\t0\t1\t1\t1\t1\t0\t0\t1\t1\t0\t0\t0\t1\t1\t0\t0\t0\t0\t0\t0\t0\t0\t0\t1\nStudent112\t1\t1\t1\t1\t0\t1\t0\t1\t0\t1\t0\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t1\t0\t1\t1\t0\t1\t0\t1\t1\t0\t1\t0\t1\t1\nStudent113\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t0\t0\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t0\t1\t1\t0\t0\t1\t0\t0\t0\t0\t1\t0\t1\t0\t1\nStudent114\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t1\t1\t0\t1\t1\t0\t1\t1\t1\t0\t0\t0\t0\t0\t1\t0\t0\t1\t0\t0\t0\t1\t0\t1\nStudent115\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t1\t1\t0\t1\t0\t0\t1\t1\t1\nStudent116\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\nStudent117\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t0\t0\t0\t1\t0\t0\t0\t0\t0\t1\t0\t1\t0\t0\t1\nStudent118\t1\t0\t0\t0\t1\t0\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t1\t1\t0\t0\t0\t1\t0\t0\t0\t1\t0\t0\nStudent119\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t0\t0\t0\t1\t1\t1\t0\t1\t0\t0\t1\t0\t0\t0\t0\t1\nStudent120\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\nStudent121\t1\t0\t1\t1\t1\t0\t1\t1\t0\t1\t1\t0\t0\t0\t0\t1\t1\t0\t1\t1\t0\t0\t1\t1\t0\t0\t0\t0\t0\t0\t0\t0\t1\t1\t0\t1\t1\t1\t0\t1\nStudent122\t1\t0\t0\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t1\t1\t1\t1\nStudent123\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t1\t0\t0\t0\t0\t1\t1\t1\nStudent124\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t0\t1\t1\t0\t0\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t0\t1\t0\t1\t0\t0\t1\t1\t0\t1\nStudent125\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t0\t0\t0\t0\t0\t1\t1\t0\t0\t1\t0\t0\t1\t0\t0\t0\t1\nStudent126\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\nStudent127\t1\t1\t1\t0\t1\t1\t0\t1\t0\t1\t1\t1\t0\t1\t0\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t1\t0\t0\t0\t0\t1\t1\t1\t1\nStudent128\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t0\t1\t1\t1\t0\t0\t1\t1\t0\t0\t1\t0\t1\t0\t1\t0\t1\t0\t1\t1\t0\t0\t0\t1\t0\t1\nStudent129\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t0\t1\t0\t1\nStudent130\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t0\t0\t0\t1\t0\t0\t0\t0\t0\t0\t1\t0\t1\nStudent132\t1\t0\t0\t1\t1\t0\t1\t0\t1\t0\t0\t0\t0\t1\t0\t0\t1\t0\t1\t1\t1\t0\t0\t0\t1\t0\t0\t0\t1\t0\t0\t1\t0\t1\t1\t0\t0\t0\t0\t0\nStudent133\t0\t1\t0\t1\t1\t0\t0\t1\t0\t1\t0\t0\t0\t0\t0\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t0\t1\t0\t1\nStudent134\t1\t1\t0\t0\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t0\t1\t1\t1\t0\t1\t0\t1\t1\t1\t0\t1\t1\t0\t0\t0\t1\t0\t1\nStudent135\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t0\t0\t1\t1\t0\t1\nStudent136\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t1\t0\t1\t0\t0\t1\t0\t0\t1\t0\t0\t1\t0\t1\nStudent137\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t0\t0\t0\t1\t0\t1\t1\t0\t1\t1\t0\t0\t0\t0\t0\t0\t1\t0\t0\nStudent138\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t0\t0\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t1\nStudent139\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t0\t1\t0\t0\t1\t0\t1\t1\t1\t1\t0\t1\nStudent140\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t0\t1\t0\t0\t1\t1\t1\t0\t1\t0\t1\t0\t1\t0\t1\t1\t1\t0\t0\t0\t1\t0\t0\t0\t1\t1\t0\t1\nStudent141\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t0\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t1\t0\t1\t1\t0\t1\t0\t1\t1\t0\t1\nStudent142\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t0\t0\t1\t0\t0\t0\t1\t1\t1\t1\nStudent143\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\nStudent144\t1\t1\t1\t1\t1\t1\t0\t0\t0\t1\t0\t0\t0\t0\t1\t1\t0\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t0\t0\t0\t1\t0\t0\t0\t0\t1\t1\t0\t0\t1\nStudent145\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t0\t0\t1\t1\t1\t0\t0\t1\t1\t0\t1\t1\t1\t1\t1\t1\t0\t0\t0\t0\t1\t0\t0\t0\t1\t0\t1\nStudent146\t1\t0\t1\t1\t1\t1\t1\t0\t1\t0\t0\t0\t1\t0\t0\t1\t1\t0\t1\t0\t1\t1\t1\t1\t1\t0\t0\t0\t1\t1\t0\t0\t0\t0\t0\t0\t1\t0\t0\t0\nStudent147\t1\t0\t0\t1\t0\t0\t1\t0\t1\t0\t0\t0\t0\t0\t0\t1\t0\t1\t1\t0\t0\t1\t0\t1\t0\t0\t0\t0\t0\t1\t0\t0\t1\t0\t1\t0\t0\t0\t0\t1\nStudent148\t1\t0\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t0\t0\t1\t0\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t0\t1\t0\t1\t0\t1\t0\t0\t1\t1\t1\t0\t0\nStudent149\t1\t1\t0\t1\t1\t0\t1\t1\t0\t1\t1\t0\t0\t0\t0\t1\t0\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t0\t1\t0\t0\t0\t1\t1\t1\t0\t1\t1\nStudent150\t0\t1\t1\t1\t1\t1\t1\t1\t0\t0\t0\t0\t1\t0\t0\t0\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t0\t1\t0\t1\t1\t1\t1\t0\t0\t0\t1\t1\t1\nStudent151\t1\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t0\t0\t1\t1\t1\t0\t1\t1\t1\t0\t1\t0\t1\t0\t0\t1\nStudent152\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t1\t0\t1\t0\t0\t1\t0\t0\t1\t1\nStudent153\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t0\t0\t0\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t1\t0\t1\t0\t0\t1\t0\t1\t0\t1\t0\t1\nStudent154\t0\t0\t1\t1\t1\t0\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t0\t0\t1\t0\t0\t0\t0\t0\t1\t0\t1\t0\t0\t0\t0\t0\t0\t1\t0\t0\nStudent155\t0\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t1\t1\t0\t0\t0\t1\t1\t1\t0\t1\t1\t0\t1\t1\t1\t0\t1\t1\t0\t0\t0\t0\t1\t1\t0\t0\t1\t0\t0\nStudent156\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t1\t1\t0\t0\t0\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t1\t0\t0\t1\t1\t0\t1\t1\t1\t1\t0\t1\t1\t1\nStudent157\t1\t1\t1\t1\t1\t1\t1\t1\t0\t1\t1\t1\t0\t0\t0\t0\t1\t0\t1\t1\t0\t1\t0\t1\t1\t0\t1\t1\t1\t1\t0\t0\t1\t1\t1\t1\t1\t1\t0\t1", mode="r", theme="twilight"),

                br(),
                br(),


                h3("Basic statistics and item statistics"),
                verbatimTextOutput("prepost.ds.out"),

                p('Drop if: Cronbach alpha when the item is removed', br(),
                'r dropped: item-total correlation without the item', br(),
                'IF: item facility or item mean (proportion of those who answered the item correctly)', br(),
                'ID: item discrimination (upper 1/3 - lower 1/3)', br(),
                'rpbi: point-biserial correlation', br()
                ),

                br(),

                h3("Difference index (DI)"),
                verbatimTextOutput("DI.out"),

                p('IF = Item facility (proportion of those who answered the item correctly)', br(),
                'DI = Difference index (Posttest IF - Pretest IF)', br(),
                'IF DI is > .10, the item is appropriate for criterion-referenced test.'
                ),

                br(),

                h3("Change (gain) score"),
                verbatimTextOutput("gain.out"),

                p('Gain: Post — Pre', br(),
                'Exp.Post: Expected posttest score = M.post + r * (SD.post/SD.pre) * (X - M.pre)', br(),
                '±RTM: Post — Exp.Post (If the value is positive, the gain is over regression to the mean (RTM) effect.)', br(),
                'Adj.Post: Asjusted (corrected) posttest score', a('(Bonate, 2000)', href='http://bit.ly/1m3n37a', target="_blank"), br()
                ),

                br(),

                h3("Gain score reliability"),
                verbatimTextOutput("gain.rel.out"),
                p('If these values are low, you should not use the gain score as an indicator for subsequent analysis.'),

                br(),
                br(),

                br(),

                h3("Overlayed histograms"),

                plotOutput("distPlot"),

                br(),

                h3("Box plots with individual data points"),

                plotOutput("boxPlot", width="80%"),

                br(),

                h3("Changes of the individual data"),

                plotOutput("indvPlot", width="70%"),

                br(),

                h3("Scatterplot"),

                plotOutput("correlPlot", width="70%", height="500px"),

                br(),

                h3("Paired t-test"),
                verbatimTextOutput("t.out"),

                br(),

                h3("Effect size indices"),
                verbatimTextOutput("es.out"),
                p(a('(Borenstein et al., 2009)', href='http://www.meta-analysis.com/downloads/Meta-analysis%20Effect%20sizes%20based%20on%20means.pdf', target="_blank"), br()),

                br(),
                br(),

                strong('R session info'),
                verbatimTextOutput("info.out")

                ),





            tabPanel("About",

            strong('Note'),
            p('This web application is developed with',
            a("Shiny.", href="http://www.rstudio.com/shiny/", target="_blank"),
            ''),

            br(),

            strong('List of Packages Used'), br(),
            code('library(shiny)'),br(),
            code('library(shinyAce)'),br(),
            code('library(psych)'),br(),
            code('library(CTT)'),br(),
            code('library(lattice)'),br(),
            code('library(latticeExtra)'),br(),
            code('library(beeswarm)'),br(),

            br(),

            strong('Code'),
            p('Source code for this application is based on',
            a('"The handbook of Research in Foreign Language Learning and Teaching" (Takeuchi & Mizumoto, 2012)', href='http://mizumot.com/handbook/', target="_blank"), 'and',
            a("MacR.", href="https://sites.google.com/site/casualmacr/", target="_blank")),

            p('The code for this web application is available at',
            a('GitHub.', href='https://github.com/mizumot/criterion', target="_blank")),

            p('If you want to run this code on your computer (in a local R session), run the code below:',
            br(),
            code('library(shiny)'),br(),
            code('runGitHub("criterion","mizumot")')
            ),

            br(),

            strong('Acknowledgment'),
            p('I thank',
            a("Dr. Takaaki Kumazawa", href="https://twitter.com/TakaakiKumazawa", target="_blank"),
            'for his support and feedback to create this web application.'),

            br(),

            strong('Recommended'),
            p('To learn more about R, I suggest this excellent and free e-book (pdf),',
            a("A Guide to Doing Statistics in Second Language Research Using R,", href="http://cw.routledge.com/textbooks/9780805861853/guide-to-R.asp", target="_blank"),
            'written by Dr. Jenifer Larson-Hall.'),

            p('Also, if you are a cool Mac user and want to use R with GUI,',
            a("MacR", href="https://sites.google.com/site/casualmacr/", target="_blank"),
            'is defenitely the way to go!'),

            br(),

            strong('Author'),
            p(a("Atsushi MIZUMOTO,", href="http://mizumot.com", target="_blank"),' Ph.D.',br(),
            'Associate Professor of Applied Linguistics',br(),
            'Faculty of Foreign Language Studies /',br(),
            'Graduate School of Foreign Language Education and Research,',br(),
            'Kansai University, Osaka, Japan'),

            br(),

            a(img(src="http://i.creativecommons.org/p/mark/1.0/80x15.png"), target="_blank", href="http://creativecommons.org/publicdomain/mark/1.0/"),

            p(br())

            )
        )
    )

))