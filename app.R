library(shiny)
library(cowplot)
library(ggplot2)
library(tidyverse)



# googlesheets4::gs4_auth(path = '.secrets/enduring-coil-349821-3832e38b5b43.json')
# 
# df_phyto <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1JGXLAvG_U9dWn7Ya_NorUU3mcgpi7aX2uanszfhXP8c/edit?sharingaction=ownershiptransfer#gid=641716790",
#                                       sheet = "occurrence_temp") %>%
#   mutate(day=as.Date(day))
# 
# df_envir <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1JGXLAvG_U9dWn7Ya_NorUU3mcgpi7aX2uanszfhXP8c/edit?sharingaction=ownershiptransfer#gid=641716790",
#                                       sheet = "extended_temp")%>%
#   mutate(day=as.Date(day))

load("historical_data.Rdata")

locationID_select <- c("MDIBL_Dock_22G", "Bar_Harbor_Town_Pier_22D", "Bass_Harbor_21B")

# scaleFUN <- function(x) sprintf("%.2e", x)
theme_set(
  theme(text=element_text(family="Times"),
        panel.background = element_rect(fill = NA),
        panel.border = element_rect(colour = "black", fill=NA, size=0.75),
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        plot.title = element_text(size=16),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16, colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.ticks = element_line(size = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = 'top',
        # legend.key = element_rect(fill = NA,size = 0.25),
        legend.background = element_blank(),
        legend.key.width =unit(0.25, units = "cm"),
        legend.key.height =unit(0.25, units = "cm"),
        axis.title.x = element_blank(),
        # axis.title.y = element_blank(),
        axis.text.x = element_text( angle=-45, hjust = 0.2, vjust = 1),
        legend.text.align= 0,
        plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))
)


# Define UI for application that draws two line plots
ui = fluidPage(
  titlePanel("MDIBL HAB monitoring"),
  selectInput( 
    inputId = "locationID_sub",
    label = "Select sampling site",
    choices = locationID_select
  ),
  sliderInput(inputId = "Order",
              label = "Date range",
              min = as.Date(min(df_phyto$day)),
              max = as.Date(max(df_phyto$day)),
              value = c(as.Date(min(df_phyto$day)),
                        as.Date(max(df_phyto$day))
              )),
  sidebarLayout(position= "left",
                sidebarPanel(
                  checkboxInput("donum1", "Make environmental plot", value = T),
                  checkboxInput("donum2", "Make phytoplankton plot", value = T),
                  selectizeInput("data1",
                                 "Select environmental data:",
                                 choices = c("Water temperature (Celcius)"="water_temp",
                                             "Dissolved oxygen (ppm)"= "DOavg_ppm",
                                             "Salinity(1e-3)"= "salinity_ppt",
                                             "Transparency (m)"="transparency_depth_mean",
                                             "Biological oxygen demand (ppm)"="bod"),
                                 selected = "water_temp",
                                 multiple = TRUE),
                  selectizeInput("data2",
                                 "Select phytoplankton data:",
                                 choices = c("Alexandrium (cells/L)" = "Alexandrium",
                                             "Dinophysis spp (cells/L)"= "Dinophysis_spp",
                                             "Dinophysis acuminata (cells/L)"= "Dinophysis_acuminata",
                                             "Dinophysis norvegica (cells/L)"= "Dinophysis_norvegica",
                                             "Gonyaulax (cells/L)"="Gonyaulax",
                                             "Karenia (cells/L)"="Karenia",
                                             "Margalefidinium polykrikoides (cells/L)"="Margalefidinium_polykrikoides",
                                             "Prorocentrum spp (cells/L)"="Prorocentrum_spp",
                                             "Prorocentrum lima (cells/L)"="Prorocentrum_lima",
                                             "Scrippsiella (cells/L)"="Scrippsiella",
                                             "Pseudo nitzschia spp"="Pseudo_nitzschia_spp",
                                             "Pseudo nitzschia delicatissima (cells/L)"="Pseudo_nitzschia_small",
                                             "Pseudo nitzschia seriata (cells/L)"="Pseudo_nitzschia_large",
                                             "Other phytoplankton (cells/L)"="Other_phytoplankton",
                                             "Other Diatoms (cells/L)"="Other_diatoms",
                                             "Other Dinoflagellates (cells/L)"="Other_dinoflagellates",
                                             "Total Phytoplankton (cells/L)" = "Total_phytoplankton"),
                                 selected = "Alexandrium",
                                 multiple = TRUE)
                ),
                mainPanel(
                  plotOutput(outputId="plotgraph", height="600px")
                )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ### Filter by date
  env_plot <- reactive({
    if(is.null(input$locationID_sub)){
      return(df_envir)}
    else{
      df_envir <- df_envir %>%
        filter(locationID == input$locationID_sub)
    }
    
    df_envir <- df_envir[as.Date(df_envir$day) >= input$Order[1] & as.Date(df_envir$day) <= input$Order[2] ,]
    plot.env <- df_envir[df_envir$measurementType %in% input$data1, ]
    if (!input$donum1) return(NULL)
    ggplot(plot.env) +
      geom_point(aes(x = day, y = measurementValue, colour = measurementType), na.rm = T) +
      labs (x = "Time", y = "Values", title = " ") +
      # scale_colour_discrete(name = "measurementType")+
      geom_line(data=plot.env[!is.na(plot.env$measurementValue),], aes(x = day, y = measurementValue, colour = measurementType))#+
      # scale_y_continuous(labels = scaleFUN)
  })
  
  phyto_plot <- reactive({
    if(is.null(input$locationID_sub)){
      return(df_phyto)}
    else{
      df_phyto <- df_phyto %>%
        filter(locationID == input$locationID_sub)
    }
    df_phyto <- df_phyto[as.Date(df_phyto$day) >= input$Order[1] & as.Date(df_phyto$day) <= input$Order[2] ,]
    plot.phyto <- df_phyto[df_phyto$organismName %in% input$data2, ]
    
    phyto_plot <-ggplot(plot.phyto) +
      geom_point(aes(x = day, y = organismQuantity, colour = organismName), na.rm = T) +
      labs (x = "Time", y = "Cells per L", title = " ") +
      # scale_colour_discrete(name = "organismName")+
      geom_line(data=plot.phyto[!is.na(plot.phyto$organismQuantity),], aes(x = day, y = organismQuantity, colour = organismName))#+
      # scale_y_continuous(labels = scaleFUN)
  })
  
  output$plotgraph = renderPlot({
    
    ptlist <- list(env_plot(),phyto_plot())
    wtlist <- c(input$wt1,input$wt2)
    # remove the null plots from ptlist and wtlist
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete]
    wtlist <- wtlist[to_delete]
    if (length(ptlist)==0) return(NULL)
    
    cowplot::plot_grid(plotlist = ptlist, ncol=1, align ="v")
    # gridExtra::grid.arrange(grobs=ptlist,widths=wtlist,nrow=length(ptlist))
  })
}




# Run the application
shinyApp(ui = ui, server = server)



