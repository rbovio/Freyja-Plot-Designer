## app.R ##
library(shinydashboard)
library(stringr)
library(ggplot2)
library(gtools)
library(Polychrome)
library(periscope)
library(DT)
library(pals)
library(dplyr)
library(tibble)

###########################################################
########################## UI #############################
###########################################################


ui <- dashboardPage(
    
    ## HEADER CONTENT ##
    dashboardHeader(title = "Freyja Plot Design"), # end of Dashboard Header
    
    
    
    ## SIDEBAR CONTENT ##
    dashboardSidebar(
        sidebarMenu(
            menuItem("Inputs", tabName = "inputs_outputs", icon = icon("dashboard")),
            # Input: Select a file ----
            fileInput("tsvs",
                      label="Upload TSV files",
                      multiple = TRUE),
            selectInput("color_palette", "Choose a color palette:",
                        choices = c("alphabet", 
                                    "alphabet2", 
                                    "cols25",
                                    "glasbey",
                                    "kelly",
                                    "polychrome",
                                    "stepped",
                                    "stepped2",
                                    "stepped3",
                                    "okabe",
                                    "tableau20",
                                    "tol",
                                    "tol.groundcover")),
            sliderInput("width", "Sample Width:", min = 0, max = 10, value = 2, step = 0.1),
            checkboxInput("horizontal_lines", "Horizontal lines", TRUE),
            numericInput("plot_height", "Plot Height:", 400),
            numericInput("plot_width", "Plot Width:", 1500)
            
        ) # end of sidebar menu
    ), # end of sidebar
    
    
    
    
    ## BODY CONENT ##
    
    dashboardBody(
                    fluidRow(
                        tabBox(
                            title = textOutput("site1_name"),
                            id = "denton_tabset",
                            width = 12,
                            height = "600px",
                            tabPanel(
                                "Plot 1",
                                box(plotOutput("denton_plot1"), width = 12),
                                downloadButton("downloadDataDentonPlot1", paste0("Download Plot"))
                            ),
                            tabPanel(
                                "Plot 2",
                                box(plotOutput("denton_plot2"), width = 12),
                                downloadButton("downloadDataDentonPlot2", paste0("Download Plot"))
                            ),
                        )
                    ),
                    fluidRow(
                        tabBox(
                            title = textOutput("site2_name"),
                            id = "waco_tabset",
                            width = 12,
                            height = "600px",
                            tabPanel(
                                "Plot 1",
                                box(plotOutput("waco_plot1"), width = 12),
                                downloadButton("downloadDataWacoPlot1", paste0("Download Plot"))
                            ),
                            tabPanel(
                                "Plot 2",
                                box(plotOutput("waco_plot2"), width = 12),
                                downloadButton("downloadDataWacoPlot2", paste0("Download Plot"))
                            ),
                        )
                    ),
                    fluidRow(
                        downloadButton("downloadDataTable", "Download Table"),
                        box(div(DT::dataTableOutput("contents"), style = "font-size:75%"), width = 12)
                    )
    ) # end of Dashboard body
    
) # end of UI



###########################################################
######################## SERVER ###########################
###########################################################

server <- function(input, output) {
    
    ######################### REACTIVE DATA FOR TABLE ##############################
    
    # plot1_name_reactive = reactive({
    #     if(input$tsvs$name[1] == "dentonfreyja_aggregate.tsv"){
    #         name1 = "Denton"
    #     }
    #     else{name1 = "FDA 1"
    #     }
    #     return(name1)
    # })
    # 
    # plot2_name_reactive = reactive({
    #     if(input$tsvs$name[2] == "wacofreyja_aggregate.tsv"){
    #         name2 = "Waco"
    #     }
    #     else{name2 = "FDA 2"
    #     }
    #     return(name2)
    # })
    
    plot1_name_reactive = reactive({
        if(str_detect(input$tsvs$name[1], "denton") == TRUE){
            name1 = "Denton"
        }
        else{name1 = "FDA 1"
        }
        return(name1)
    })
    
    plot2_name_reactive = reactive({
        if(str_detect(input$tsvs$name[2], "waco") == TRUE){
            name2 = "Waco"
        }
        else{name2 = "FDA 2"
        }
        return(name2)
    })
    
    
    
    output$site1_name = renderText({
        plot1_name_reactive()
    })

    output$site2_name = renderText({
        plot2_name_reactive()
    })
    
    
    datasetInputTable <- reactive({
        req(input$tsvs)
        data_denton = read.csv(input$tsvs$datapath[1], sep = "\t")
        # edit ID column
        # add zero to months with only one digit
        data_denton$X = stringr::str_replace(string = data_denton$X, pattern = "([0-9]{4})-([0-9]{1}-)", replacement = "\\1-0\\2")
        # remove text
        data_denton$X = stringr::str_replace(string = data_denton$X, pattern = "(-|_)[A-Z].*", replacement = "")
        # add zero to days with only one digits
        data_denton$X = stringr::str_replace(string = data_denton$X, pattern = "([0-9]{4})-([0-9]{2})-([0-9]{1}$)", replacement = "\\1-\\2-0\\3")
        # remove "[", "]", "(", ")"
        data_denton$summarized = stringr::str_remove_all(string = data_denton$summarized, pattern = ("\\)|\\(|\\[|\\]|\\'"))
        # create vector of all variants in the dataset
        variant_denton_list = c()
        for(i in 1:nrow(data_denton)){
            variant_denton = data_denton$summarized[i]
            variant_denton = stringr::str_remove_all(string = variant_denton, pattern = ("\\)|\\(|\\[|\\]|\\'"))
            variant_denton = as.data.frame(stringr::str_split(string = variant_denton, pattern = ","))
            row_odd <- seq_len(nrow(variant_denton)) %% 2 
            data_row_variant_denton <- variant_denton[row_odd == 1, ]
            variant_denton_list = c(variant_denton_list, data_row_variant_denton)
        }
        variant_denton = sort(unique(stringr::str_remove_all(string = variant_denton_list, pattern = (" "))))
        
        ########
        # WACO #
        ########
        data_waco = read.csv(input$tsvs$datapath[2], sep = "\t")
        data_waco$X = stringr::str_replace(string = data_waco$X, pattern = "([0-9]{4})-([0-9]{1}-)", replacement = "\\1-0\\2")
        data_waco$X = stringr::str_replace(string = data_waco$X, pattern = "(-|_)[A-Z].*", replacement = "")
        data_waco$X = stringr::str_replace(string = data_waco$X, pattern = "([0-9]{4})-([0-9]{2})-([0-9]{1}$)", replacement = "\\1-\\2-0\\3")
        data_waco$summarized = stringr::str_remove_all(string = data_waco$summarized, pattern = ("\\)|\\(|\\[|\\]|\\'"))
        
        variant_waco_list = c()
        for(i in 1:nrow(data_waco)){
            variant_waco = data_waco$summarized[i]
            variant_waco = stringr::str_remove_all(string = variant_waco, pattern = ("\\)|\\(|\\[|\\]|\\'"))
            variant_waco = as.data.frame(stringr::str_split(string = variant_waco, pattern = ","))
            row_odd <- seq_len(nrow(variant_waco)) %% 2 
            data_row_variant_waco <- variant_waco[row_odd == 1, ]
            variant_waco_list = c(variant_waco_list, data_row_variant_waco)
        }
        variant_waco = sort(unique(stringr::str_remove_all(string = variant_waco_list, pattern = (" "))))
        
        
        # compare variants between sites
        all_variants = c(intersect(variant_denton, variant_waco),
                         setdiff(variant_denton, variant_waco),
                         setdiff(variant_waco, variant_denton))
        
        
        
        # create compiled dataframe 
        data_compiled1 =  data.frame(matrix(nrow = 0, ncol = length(all_variants)+2))
        data_compiled2 =  data.frame(matrix(nrow = 0, ncol = length(all_variants)+2))
        # colnames(data_compiled) = as.character(c("Date","Site",all_variants))
        
        
        # DENTON temporary dataframe
        for(i in 1:nrow(data_denton)){
            date_denton = data_denton$X[i]
            variant_denton = data_denton$summarized[i]
            variant_denton = stringr::str_remove_all(string = variant_denton, pattern = ("\\)|\\(|\\[|\\]|\\'"))
            variant_denton = as.data.frame(stringr::str_split(string = variant_denton, pattern = ", "))
            variant_denton = as.data.frame(stringr::str_replace_all(string = variant_denton[,1], pattern = " ", replacement = ""))
            row_odd <- seq_len(nrow(variant_denton)) %% 2 
            data_row_variant_denton <- variant_denton[row_odd == 1, ]
            data_row_value_denton <- variant_denton[row_odd == 0, ]  
            tmp_data_denton =  data.frame(matrix(nrow = 1, ncol = length(data_row_variant_denton)+2))
            names(tmp_data_denton) = c("Date", "Site", data_row_variant_denton)
            tmp_data_denton[1,] = c(data_denton$X[i], plot1_name_reactive(), as.numeric(data_row_value_denton))
            data_compiled1 = smartbind(data_compiled1, tmp_data_denton)
        }
        data_compiled1 = data_compiled1[order(as.Date(data_compiled1$Date)),]
        data_compiled1 = add_column(data_compiled1, ID = seq(1:nrow(data_compiled1)), .before = "Date")
        # data_compiled1$ID = seq(1:nrow(data_compiled1))
        
        # WACO temporary dataframe
        for(i in 1:nrow(data_waco)){
            date_waco = data_waco$X[i]
            variant_waco = data_waco$summarized[i]
            variant_waco = stringr::str_remove_all(string = variant_waco, pattern = ("\\)|\\(|\\[|\\]|\\'"))
            variant_waco = as.data.frame(stringr::str_split(string = variant_waco, pattern = ", "))
            variant_waco= as.data.frame(stringr::str_replace_all(string = variant_waco[,1], pattern = " ", replacement = ""))
            row_odd <- seq_len(nrow(variant_waco)) %% 2 
            data_row_variant_waco <- variant_waco[row_odd == 1, ]
            data_row_value_waco <- variant_waco[row_odd == 0, ]  
            tmp_data_waco =  data.frame(matrix(nrow = 1, ncol = length(data_row_variant_waco)+2))
            names(tmp_data_waco) = c("Date", "Site", data_row_variant_waco)
            tmp_data_waco[1,] = c(data_waco$X[i], plot2_name_reactive(), as.numeric(data_row_value_waco))
            data_compiled2 = smartbind(data_compiled2, tmp_data_waco)
        }
        data_compiled2 = data_compiled2[order(as.Date(data_compiled2$Date)),]
        data_compiled2 = add_column(data_compiled2, ID = seq(1:nrow(data_compiled2)), .before = "Date")
        # data_compiled2$ID = seq(1:nrow(data_compiled2))
        
        data_compiled = smartbind(data_compiled1, data_compiled2)
        
        # convert NAs to zero and reset row numbers
        data_compiled[is.na(data_compiled)] <- 0
        rownames(data_compiled) = NULL
        
        data_compiled$Date = as.Date(data_compiled$Date)
        data_compiled <- data_compiled %>% mutate_at(all_variants, as.numeric)
        
        is.num <- sapply(data_compiled, is.numeric)
        data_compiled[is.num] <- lapply(data_compiled[is.num], round, 5)
        
        data_to_rearrange = data_compiled[,c(4: length(data_compiled))]
        data_to_rearrange = data_to_rearrange %>% select(order(colnames(data_to_rearrange)))
        data_compiled = cbind(data_compiled[,c(1,2,3)], data_to_rearrange)
        # 
        # display the table
        return(data_compiled)
        
    }) # end of reactive datasetInput
    
    
    ######################### RENDER DATA TABLE ##############################
    
    # -1 means no pagination; the 2nd element contains menu labels
    output$contents <- DT::renderDataTable(
        DT::datatable(
            datasetInputTable(), options = list(
                autoWidth = TRUE,
                columnDefs = list(list(width = '50px', targets = c(1,2,3,4,5,6,7,8))),
                searching = FALSE,
                lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
                pageLength = 50
            )
        )
    )
    
    
    
    
    
    
    
    ############### REACTIVE DATA FOR PLOTS ######################
    
    # DATA TABLE
    
    datasetInputPlot = reactive({
        new_data_compiled = data.frame(ID = NA,
                                       Date = NA,
                                       Site = NA, 
                                       Variant = NA,
                                       Value = NA)
        new_data_compiled$Date = as.Date(new_data_compiled$Date)
        variant_names = colnames(datasetInputTable())[-c(1:3)]
        for(i in 1:nrow(datasetInputTable())){
            tmp_df = data.frame(ID = datasetInputTable()$ID[i],
                                Date = rep(datasetInputTable()$Date[i], length(variant_names)),
                                Site = rep(datasetInputTable()$Site[i], length(variant_names)),
                                Variant = variant_names,
                                Value = as.numeric(datasetInputTable()[i,-c(1:3)]))
            new_data_compiled = rbind(new_data_compiled, tmp_df)
        }
        new_data_compiled = new_data_compiled[-1,]
        return(new_data_compiled)
    })
    
    # COLOR PALETTE
    
    color_palette = reactive({
        switch(input$color_palette,
               "alphabet" = alphabet(n = length(unique(datasetInputPlot()$Variant))),
               "alphabet2" = alphabet2(n = length(unique(datasetInputPlot()$Variant))),
               "cols25" = cols25(n = length(unique(datasetInputPlot()$Variant))),
               "glasbey" = glasbey(n = length(unique(datasetInputPlot()$Variant))),
               "kelly" = kelly(n = length(unique(datasetInputPlot()$Variant))), 
               "polychrome" = polychrome(n = length(unique(datasetInputPlot()$Variant))),
               "stepped" = stepped(n = length(unique(datasetInputPlot()$Variant))),
               "stepped2" = stepped2(n = length(unique(datasetInputPlot()$Variant))), 
               "stepped3" = stepped3(n = length(unique(datasetInputPlot()$Variant))),
               "okabe" = okabe(n = length(unique(datasetInputPlot()$Variant))),
               "tableau20" = tableau20(n = length(unique(datasetInputPlot()$Variant))), 
               "tol" = tol(n = length(unique(datasetInputPlot()$Variant))),
               "tol.groundcover" = tol.groundcover(n = length(unique(datasetInputPlot()$Variant)))
        )
    })
    
    # WIDTH
    
    width_reactive = reactive({
        return(input$width)
    })

    # PLOT SIZE
    
    plot_height_reactive = reactive({
        return(input$plot_height)
    })
    
    plot_width_reactive = reactive({
        return(input$plot_width)
    })
    
    
    # DENTON PLOT 1
    
    denton_reactive_plot = reactive({
        denton_plot_data = subset(datasetInputPlot(), Site == plot1_name_reactive())
        waco_plot_data = subset(datasetInputPlot(), Site == plot2_name_reactive())
        max_date = max(c(denton_plot_data$Date,waco_plot_data$Date))
        min_date = min(c(denton_plot_data$Date,waco_plot_data$Date))
        denton_plot1 = ggplot(denton_plot_data, aes(fill=Variant, y=Value, x=Date, width = width_reactive())) + 
            geom_bar(position="fill", stat="identity") +
            scale_fill_manual(values = as.vector(color_palette())) +
            scale_x_date(limits = c(min_date,max_date)) +
            ggtitle(plot1_name_reactive()) +
            geom_hline(yintercept=c(0.25,0.5,0.75), linetype=rep("dashed",3), color = rep("grey",3)) +
            ylab("Variant Prevalence") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black")) 

        
        denton_plot1_no_h_line = ggplot(denton_plot_data, aes(fill=Variant, y=Value, x=Date, width = width_reactive())) +
            geom_bar(position="fill", stat="identity") +
            scale_fill_manual(values = as.vector(color_palette())) +
            scale_x_date(limits = c(min_date,max_date)) +
            ggtitle(plot1_name_reactive()) +
            # geom_hline(yintercept=c(0.25,0.5,0.75), linetype=rep("dashed",3), color = rep("grey",3)) +
            ylab("Variant Prevalence") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"))
        
        if(input$horizontal_lines == TRUE){
            return(denton_plot1)
        }
        else{
            return(denton_plot1_no_h_line)
        }

    })
    
    # DENTON PLOT 2
    
    denton_reactive_plot2 = reactive({
        denton_plot_data = subset(datasetInputPlot(), Site == plot1_name_reactive())
        waco_plot_data = subset(datasetInputPlot(), Site == plot2_name_reactive())
        max_date = max(c(denton_plot_data$Date,waco_plot_data$Date))
        min_date = min(c(denton_plot_data$Date,waco_plot_data$Date))
        denton_plot_data$ID = as.factor(denton_plot_data$ID)
        denton_plot_data$Date = as.factor(denton_plot_data$Date)
        denton_plot2 = ggplot(denton_plot_data, aes(fill=Variant, y=Value, x=Date, width = width_reactive())) + 
            geom_bar(position="fill", stat="identity") +
            scale_fill_manual(values = as.vector(color_palette())) +
            # scale_x_date(limits = c(min_date,max_date)) +
            ggtitle(plot1_name_reactive()) +
            geom_hline(yintercept=c(0.25,0.5,0.75), linetype=rep("dashed",3), color = rep("grey",3)) +
            ylab("Variant Prevalence") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black")) 
        
        denton_plot2_no_h_line = ggplot(denton_plot_data, aes(fill=Variant, y=Value, x=Date, width = width_reactive())) + 
            geom_bar(position="fill", stat="identity") +
            scale_fill_manual(values = as.vector(color_palette())) +
            # scale_x_date(limits = c(min_date,max_date)) +
            ggtitle(plot1_name_reactive()) +
            # geom_hline(yintercept=c(0.25,0.5,0.75), linetype=rep("dashed",3), color = rep("grey",3)) +
            ylab("Variant Prevalence") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black")) 
        
        if(input$horizontal_lines == TRUE){
            return(denton_plot2)
        }
        else{
            return(denton_plot2_no_h_line)
        }
    })
    
    # WACO PLOT 1
    
    waco_reactive_plot = reactive({
        denton_plot_data = subset(datasetInputPlot(), Site == plot1_name_reactive())
        waco_plot_data = subset(datasetInputPlot(), Site == plot2_name_reactive())
        max_date = max(c(denton_plot_data$Date,waco_plot_data$Date))
        min_date = min(c(denton_plot_data$Date,waco_plot_data$Date))
        waco_plot1 = ggplot(waco_plot_data, aes(fill=Variant, y=Value, x=Date, width = width_reactive())) + 
            geom_bar(position="fill", stat="identity") +
            scale_fill_manual(values = as.vector(color_palette())) +
            scale_x_date(limits = c(min_date,max_date)) +
            geom_hline(yintercept=c(0.25,0.5,0.75), linetype=rep("dashed",3), color = rep("grey")) +
            ggtitle(plot2_name_reactive()) +
            ylab("Variant Prevalence") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black")) 
        
        waco_plot1_no_h_line = ggplot(waco_plot_data, aes(fill=Variant, y=Value, x=Date, width = width_reactive())) + 
            geom_bar(position="fill", stat="identity") +
            scale_fill_manual(values = as.vector(color_palette())) +
            scale_x_date(limits = c(min_date,max_date)) +
            # geom_hline(yintercept=c(0.25,0.5,0.75), linetype=rep("dashed",3), color = rep("grey")) +
            ggtitle(plot2_name_reactive()) +
            ylab("Variant Prevalence") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black")) 
        
        if(input$horizontal_lines == TRUE){
            return(waco_plot1)
        }
        else{
            return(waco_plot1_no_h_line)
        }
    })
    
    waco_reactive_plot2 = reactive({
        denton_plot_data = subset(datasetInputPlot(), Site == plot1_name_reactive())
        waco_plot_data = subset(datasetInputPlot(), Site == plot2_name_reactive())
        max_date = max(c(denton_plot_data$Date,waco_plot_data$Date))
        min_date = min(c(denton_plot_data$Date,waco_plot_data$Date))
        waco_plot_data$ID = as.factor(waco_plot_data$ID)
        waco_plot_data$Date = as.factor(waco_plot_data$Date)
        waco_plot2 = ggplot(waco_plot_data, aes(fill=Variant, y=Value, x=Date, width = width_reactive())) + 
            geom_bar(position="fill", stat="identity") +
            scale_fill_manual(values = as.vector(color_palette())) +
            # scale_x_date(limits = c(min_date,max_date)) +
            ggtitle(plot2_name_reactive()) +
            geom_hline(yintercept=c(0.25,0.5,0.75), linetype=rep("dashed",3), color = rep("grey",3)) +
            ylab("Variant Prevalence") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black")) 
        
        waco_plot2_no_h_line = ggplot(waco_plot_data, aes(fill=Variant, y=Value, x=Date, width = width_reactive())) + 
            geom_bar(position="fill", stat="identity") +
            scale_fill_manual(values = as.vector(color_palette())) +
            # scale_x_date(limits = c(min_date,max_date)) +
            ggtitle(plot2_name_reactive()) +
            # geom_hline(yintercept=c(0.25,0.5,0.75), linetype=rep("dashed",3), color = rep("grey",3)) +
            ylab("Variant Prevalence") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black")) 
        
        if(input$horizontal_lines == TRUE){
            return(waco_plot2)
        }
        else{
            return(waco_plot2_no_h_line)
        }
    })
    ############### RENDER DENTON & WACO PLOT ######################
    output$denton_plot1 = renderPlot({
        denton_reactive_plot()
    })
    
    output$denton_plot2 = renderPlot({
        denton_reactive_plot2()
    })
    
    
    output$waco_plot1 = renderPlot({
        waco_reactive_plot()
    })
    
    output$waco_plot2 = renderPlot({
        waco_reactive_plot2()
    })
    
    
    
    
    
    ########################## DOWNLOAD #################################### 
    
    output$downloadDataTable <- downloadHandler(
        filename = function() {
            paste("freyja_covid_ww_variant_prevalence_table.csv", sep = "")
        },
        content = function(file) {
            write.csv(datasetInputTable(), file, row.names = FALSE)
        }
    )
    
    output$downloadDataDentonPlot1 <- downloadHandler(
        filename = function() {
            paste("freyja_covid_ww_variant_prevalence_plot_",plot1_name_reactive(),"_1.png", sep = "")
        },
        content = function(file) {
            png(file, height = plot_height_reactive(), width = plot_width_reactive())
            plot(denton_reactive_plot())
            dev.off()
        }
    )
    
    output$downloadDataDentonPlot2 <- downloadHandler(
        filename = function() {
            paste("freyja_covid_ww_variant_prevalence_plot_",plot1_name_reactive(),"_2.png", sep = "")
        },
        content = function(file) {
            png(file, height = plot_height_reactive(), width = plot_width_reactive())
            plot(denton_reactive_plot2())
            dev.off()
        }
    )
    
    output$downloadDataWacoPlot1 <- downloadHandler(
        filename = function() {
            paste("freyja_covid_ww_variant_prevalence_plot_",plot2_name_reactive(),"_1.png", sep = "")
        },
        content = function(file) {
            png(file, height = plot_height_reactive(), width = plot_width_reactive())
            plot(waco_reactive_plot())
            dev.off()
        }
    )
    
    output$downloadDataWacoPlot2 <- downloadHandler(
        filename = function() {
            paste("freyja_covid_ww_variant_prevalence_plot_",plot2_name_reactive(),"_2.png", sep = "")
        },
        content = function(file) {
            png(file, height = plot_height_reactive(), width = plot_width_reactive())
            plot(waco_reactive_plot2())
            dev.off()
        }
    )
    
    
} # end of server

###########################################################
######################## SHINY ############################
###########################################################

shinyApp(ui, server)
