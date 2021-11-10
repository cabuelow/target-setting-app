# Packages ----

library(shiny)  
library(shinythemes)
library(tidyverse)
library(sf)
library(tmap)
library(bslib)
library(pals)
library(qs)
library(data.table)
shinyOptions(bslib = TRUE)
thematic::thematic_shiny()

# Loading data ----

mang <- read.csv('data/mangrove-capped.csv') %>% arrange(Country)
seag <- read.csv('data/seagrass-capped.csv')
countries <- st_read('data/UIA_World_Countries_Boundaries/UIA_World_Countries_Boundaries.shp')
countries$Country <- recode(countries$Country, `Russian Federation` = 'Russia', Curacao = 'Curaco',
                            `Brunei Darussalam` = 'Brunei',`Timor-Leste` = 'East Timor',
                            `Côte d'Ivoire` = 'Ivory Coast',`Congo DRC` = 'Democratic Republic of the Congo',
                            Philippines = 'Phillippines',`Saint Martin` = 'Saint-Martin',
                            `Saint Vincent and the Grenadines` = 'Saint Vincent', `US Virgin Islands` = 'Virgin Islands')
seag.sf <- read.csv('data/seag.sf.csv')
prot.rates.s <- qread('data/seagrass-protection-rates-all.qs')
rest.rates.s <- qread('data/seagrass-restoration-rates-all.qs')
rest.rates.m <- qread('data/mangrove-restoration-rates-all.qs')
prot.rates.m <- qread('data/mangrove-protection-rates-all.qs')
seag.proj <- qread('data/seagrass-projections-all.qs') %>% mutate(PR = as.character(PR), RR = as.character(RR))
mang.proj <- qread('data/mangrove-projections-all.qs') %>% mutate(PR = as.character(PR), RR = as.character(RR))

# Wrangle data ----

mang.country <- c('Global', as.character(unique(mang.proj$country)))
seag.country <- c('Global', sort(as.character(unique(seag.sf$SOVEREIGN1))))

# Color palettes

colp <- data.frame( palette = c(rep(1,9), 
                                rep(2,9), 
                                rep(3,9),
                                rep(4,9)), 
                    palette = c(rep('stevens.greenblue',9), 
                                rep('stevens.purplegold',9), 
                                rep('stevens.bluered',9),
                                rep('stevens.pinkgreen',9)), 
                   hex = c(stevens.greenblue(n=9), 
                           stevens.purplegold(n=9), 
                           stevens.bluered(n=9), 
                           stevens.pinkgreen(n=9)))

# ui.R ----

# Define UI for application
ui <- fluidPage(#theme=shinytheme("sandstone"),
                theme=shinytheme("darkly"),
                fluidRow(
                    column(12,
                           h1("Targeting coastal ecosystem recovery", align = 'center'))),
                fluidRow(column(12, align = 'center',
                                conditionalPanel("$('#map').hasClass('recalculating')", 
                                                 tags$div('Loading ... ', style = "font-size:25px;")))),
                fluidRow(
                  column(3, div(style = "height:50px"),
                         h3("Set conservation targets", align = 'center'),
                    fluidRow(
                        column(4, offset = 2, div(style = "height:20px"),
                               selectInput("sliderPr", label = "Protect by 2030 (%)", 
                                           choices = seq(0,90,10), 
                                           selected = '0')),
                        column(4, offset = 0, div(style = "height:20px"),
                               selectInput("sliderRr", label = "Restore by 2050 (%)", 
                                           choices = seq(0,90,10), 
                                           selected = '0'))),
                    fluidRow(
                      column(12, offset = 2, style='margin-top:20px;',
                             radioButtons("colour", label = h4("Choose map colour"),
                                                choices = list("Green-blue" = 1, "Purple-gold" = 2, "Blue-red" = 3, "Pink-green" = 4),
                                                selected = 1))),
                    fluidRow(
                      column(5, offset = 2, h4('Map legend', align = 'center'))),
                       fluidRow(
                        column(1, offset = 2, div(style = "height:10px"), 
                               conditionalPanel(condition = "input.colour == 1", img(src='legend-bg.png', height = '120px', width = '120px')),
                               conditionalPanel(condition = "input.colour == 2", img(src='legend-pgold.png', height = '120px', width = '120px')),
                               conditionalPanel(condition = "input.colour == 3", img(src='legend-br.png', height = '120px', width = '120px')),
                               conditionalPanel(condition = "input.colour == 4", img(src='legend-pg.png', height = '120px', width = '120px')),
                               ))),
                        column(5,
                               fluidRow(column(12), h3('Mangroves', align = 'center')),
                               fluidRow(
                                   column(12,
                                          tmapOutput('map'))),
                                   fluidRow(column(12)),
                                   fluidRow(column(12), h3('Seagrass', align = 'center')),
                                   fluidRow(
                                       column(12,
                                              tmapOutput('map2')))),
                        column(3, style='padding-left:50px;',
                               fluidRow(column(12), div(style = "height:50px"), h3('Choose a country', align = 'center')),
                               fluidRow(
                                 column(12,style='margin-top:0px;',
                                        selectInput("var", label = h4(""), 
                                                    choices = mang.country, 
                                                    selected = 'Global'))),
                               fluidRow(
                                        plotOutput('mang.prop', width = '50%')),
                               fluidRow(
                                 column(12,style='margin-top:-60px;',
                                        selectInput("var2", label = h4(""), 
                                                    choices = seag.country, 
                                                    selected = 'Global'))),
                               fluidRow(
                                        plotOutput('seag.prop', width = '50%')))))
                               
# Define server logic
server <- function(input, output) {
    
    # get data
    
    seag2 <- reactive({
      
      seag.projsub <- setDT(seag.proj)[RR == as.character(as.numeric(input$sliderRr)/100) & 
                                         PR == as.character(as.numeric(input$sliderPr)/100)]
    }) 
    
    mang2 <- reactive({
      
      mang.projsub <- setDT(mang.proj)[RR == as.character(as.numeric(input$sliderRr)/100) & 
                                         PR == as.character(as.numeric(input$sliderPr)/100)]

    })
    
    seag.p <- reactive({
      
      prot.rates.s <- setDT(prot.rates.s)[RR == as.character(as.numeric(input$sliderRr)/100) & 
                                            PR == as.character(as.numeric(input$sliderPr)/100)]
      
    })
    
    seag.r <- reactive({
      
      rest.rates.s <- setDT(rest.rates.s)[RR == as.character(as.numeric(input$sliderRr)/100) & 
                                            PR == as.character(as.numeric(input$sliderPr)/100)]

    })
    
    mang.p <- reactive({
      
      prot.rates.m <- setDT(prot.rates.m)[RR == as.character(as.numeric(input$sliderRr)/100) & 
                                            PR == as.character(as.numeric(input$sliderPr)/100)]

    })
    
    mang.r <- reactive({
      
      rest.rates.m <- setDT(rest.rates.m)[RR == as.character(as.numeric(input$sliderRr)/100) & 
                                            PR == as.character(as.numeric(input$sliderPr)/100)]

    })
    
    # plotting 
    
    output$map <- renderTmap({

        prot.rates.m <- mang.p()
        rest.rates.m <- mang.r()
        
        mang.P <- prot.rates.m %>% 
          rename(Country = country) %>% 
          left_join(mang, by = 'Country') %>% 
          mutate(area_historical = Forest_ha + Deforested_ha,
                 prot.total_ha = prot_ha_yr*(2030-2023)) %>% 
          mutate(prot.prop = (prot.total_ha/area_historical))
        
        mang.R <- rest.rates.m %>% 
          rename(Country = country) %>% 
          left_join(mang, by = 'Country') %>% 
          mutate(area_historical = Forest_ha + Deforested_ha,
                 rest.total_ha = rest_ha_yr*(2050-2023)) %>% 
          mutate(rest.prop = (rest.total_ha/area_historical))
        
        mang2 <- select(mang.P, Country, prot.prop) %>% 
          left_join(select(mang.R, Country, rest.prop), by = 'Country')
        
        class1 <- mang2 %>% 
          mutate(rowid = seq(1, nrow(.), 1)) %>% 
          arrange(prot.prop) %>% 
          mutate(cut = ntile(prot.prop, n = 3)) %>% 
          mutate(cut = ifelse(prot.prop == 0, 1, cut)) %>% 
          arrange(rowid)
        
        class2 <- mang2 %>% 
          mutate(rowid = seq(1, nrow(.), 1)) %>% 
          arrange(rest.prop) %>% 
          mutate(cut = ntile(rest.prop, n = 3)) %>% 
          mutate(cut = ifelse(rest.prop == 0, 1, cut)) %>% 
          arrange(rowid)
        
        if(input$sliderPr > 0 & input$sliderRr == 0){
          mang2$cut <- class1$cut
        }else if(input$sliderPr == 0 & input$sliderRr > 0){
          mang2$cut <- class2$cut
        }else{
          mang2$cut <- factor(c(class1$cut + 3 * (class2$cut - 1)))
        }
        
        world.mang <- countries %>% 
            inner_join(mang2, by = 'Country') %>% 
          mutate(prot.prop = round(prot.prop, 4),
                 rest.prop = round(rest.prop, 4)) %>% 
          rename(`Proportion protected` = prot.prop,
                 `Proportion restored` = rest.prop)
        
        col.pal <- filter(colp, palette == input$colour)[,3]
        
        tmap_mode('view')
        
        if(input$sliderRr == 0 & input$sliderPr > 0){
          pal = col.pal[c(1:3)]
        }else if(input$sliderPr == 0 & input$sliderRr > 0){
          pal = col.pal[c(1,4,7)]
        }else{
          index <- as.numeric(sort(as.character(unique(world.mang$cut))))
          pal <- col.pal[index]
        }
        
        if(input$var == 'Global'){
          tm_shape(world.mang[,-c(1,3,4,5)]) +
            tm_polygons('cut', legend.show = F,
                        palette = pal, alpha =0.8,
                        popup.vars=c(
                          "Proportion protected",
                          "Proportion restored")) +
            tm_basemap(leaflet::providers$CartoDB.DarkMatter)

        }else{
          tm_shape(filter(world.mang[,-c(1,3,4,5)], Country == input$var)) +
            tm_polygons('cut', legend.show = F,
                        palette = pal,alpha =0.2,
                        popup.vars=c(
                          "Proportion protected",
                          "Proportion restored")) +
            tm_basemap(leaflet::providers$CartoDB.DarkMatter)
          }
    })
    
    output$map2 <- renderTmap({
        
      prot.rates.s <- seag.p()
      rest.rates.s <- seag.r()
        
        sea.R <- rest.rates.s %>% 
          rename(study_site = site) %>% 
          left_join(select(seag.sf, study_site, SOVEREIGN1, area_start_X50., max_area)) %>% 
          mutate(max_area = ifelse(area_start_X50. > max_area, area_start_X50., max_area)) %>% 
          group_by(SOVEREIGN1) %>%
          summarise(rest_ha_yr = sum(rest_ha_yr), max_area = sum(max_area)) %>%
          mutate(rest.prop = (rest_ha_yr*(2050-2023)/max_area))
        
        sea.P <- prot.rates.s %>% 
          rename(study_site = site) %>% 
          full_join(select(data.frame(seag.sf), study_site, SOVEREIGN1, area_start_X50., max_area)) %>% 
          mutate(max_area = ifelse(area_start_X50. > max_area, area_start_X50., max_area),
                 prot_ha_yr = ifelse(is.na(prot_ha_yr), 0, prot_ha_yr)) %>% 
          group_by(SOVEREIGN1) %>%
          summarise(prot_ha_yr = sum(prot_ha_yr), max_area = sum(max_area)) %>%
          mutate(prot.prop = (prot_ha_yr/max_area))
        
        sea2 <- sea.R %>% 
          full_join(select(sea.P, SOVEREIGN1, prot_ha_yr, prot.prop), by = 'SOVEREIGN1') %>% 
          mutate(prot.prop = ifelse(is.na(prot.prop), 0, prot.prop)) %>% 
          rename(Country = SOVEREIGN1)
        
        class1 <- sea2 %>% 
          mutate(rowid = seq(1, nrow(.), 1)) %>% 
          arrange(prot.prop) %>% 
          mutate(cut = ntile(prot.prop, n = 3)) %>% 
          mutate(cut = ifelse(prot.prop == 0, 1, cut)) %>% 
          arrange(rowid)
        
        class2 <- sea2 %>% 
          mutate(rowid = seq(1, nrow(.), 1)) %>% 
          arrange(rest.prop) %>% 
          mutate(cut = ntile(rest.prop, n = 3)) %>% 
          mutate(cut = ifelse(rest.prop == 0, 1, cut)) %>% 
          arrange(rowid)
        
        if(input$sliderPr > 0 & input$sliderRr == 0){
          sea2$cut <- class1$cut
        }else if(input$sliderPr == 0 & input$sliderRr > 0){
          sea2$cut <- class2$cut
        }else{
          sea2$cut <- factor(c(class1$cut + 3 * (class2$cut - 1)))
        }
        
        world.seag <- countries %>% 
            inner_join(sea2, by = 'Country') %>% 
          mutate(prot.prop = round(prot.prop, 4),
                 rest.prop = round(rest.prop, 4)) %>% 
          rename(`Proportion protected` = prot.prop,
                 `Proportion restored` = rest.prop)
        
        col.pal <- filter(colp, palette == input$colour)[,3]
        
        if(input$sliderRr == 0 & input$sliderPr > 0){
          pal <- col.pal[c(1:3)]
        }else if(input$sliderPr == 0 & input$sliderRr > 0){
          pal <- col.pal[c(1,4,7)]
        }else{
          index <- as.numeric(sort(as.character(unique(world.seag$cut))))
          pal <- col.pal[index]
        }
        
        tmap_mode('view')
        
        if(input$var2 == 'Global'){
        tm_shape(world.seag[,-c(1,3,4,5)]) +
          tm_polygons('cut', legend.show = F,
                      palette = pal,alpha =0.8,
                      popup.vars=c(
                        "Proportion protected",
                        "Proportion restored")) +
            tm_basemap(leaflet::providers$CartoDB.DarkMatter)
        }else{
          tm_shape(filter(world.seag[,-c(1,3,4,5)], Country == input$var2)) +
            tm_polygons('cut', legend.show = F, 
                        palette = pal, alpha =0.2,
                        popup.vars=c(
                          "Proportion protected",
                          "Proportion restored")) +
            tm_basemap(leaflet::providers$CartoDB.DarkMatter)
        }
        
    })
    
    output$mang.prop <- renderPlot({
        
        if(input$var == 'Global'){
        mang.projsub <- mang2()
        
        mang.global2 <- mang.projsub %>% 
            group_by(year, state) %>% 
            summarise(area_ha = sum(area_ha, na.rm = T)) %>% 
            pivot_wider(names_from = state, values_from = area_ha) %>% 
            mutate(state_total = PD+PF+UD+UF+Unrest) %>% 
            pivot_longer(cols = PD:Unrest, names_to = 'state', values_to = 'area_ha') %>% 
            mutate(prop = area_ha/state_total) %>% 
            droplevels()
        
       g <- ggplot() +
            geom_ribbon(data = mang.global2 %>% 
                            select(year, state, prop) %>% # prop Unprotected Forested
                            pivot_wider(names_from = state, values_from = prop), 
                        aes(x = as.integer(year), ymin=UD+PD+PF+UF, ymax=UF+PF+UD+PD+Unrest, fill="brown3"), alpha =0.75) +
            geom_ribbon(data = mang.global2 %>% 
                            select(year, state, prop) %>% # prop Protected forest
                            pivot_wider(names_from = state, values_from = prop), 
                        aes(x = as.integer(year), ymin=PD+PF+UF, ymax=UD+PD+PF+UF, fill="cadetblue2"), alpha =0.75) +
            geom_ribbon(data = mang.global2 %>%
                            select(year, state, prop) %>% # prop protected deforested
                            pivot_wider(names_from = state, values_from = prop), 
                        aes(x = as.integer(year), ymin=PF+UF,ymax=PD+PF+UF, fill="darkcyan"), alpha =0.75) +
            geom_ribbon(data = mang.global2 %>%
                            select(year, state, prop) %>% # prop unprodected deforested
                            pivot_wider(names_from = state, values_from = prop), 
                        aes(x = as.integer(year), ymin=UF,ymax=PF+UF, fill="lightgoldenrod2"), alpha =0.75) +
            geom_ribbon(data = mang.global2 %>%
                            select(year, state, prop) %>% # prop unrestorable
                            pivot_wider(names_from = state, values_from = prop), 
                        aes(x = as.integer(year), ymin=0,ymax=UF, fill="seagreen"), alpha =0.75) +
         scale_fill_identity(guide="legend", labels = c('Unrestorable', 'Unprotected deforested', 'Protected deforested',
                                 'Protected forest','Unprotected forest'), name = '') +
            ylab('Proportion') +
            xlab('Year') +
            geom_vline(xintercept = 2050, linetype = 'longdash', alpha = 0.5) +
            geom_vline(xintercept = 2030, linetype = 'longdash', alpha = 0.5) +
            xlim(c(2023, 2070))
       g + theme(axis.text = element_text(size = 12),
                 axis.title = element_text(size = 12),
                 legend.text = element_text(size = 12))

       }
        else{
            mang.projsub <- mang2()
            
            mang.global2 <- mang.projsub %>% 
                filter(country == input$var) %>% 
                group_by(year, state) %>% 
                summarise(area_ha = sum(area_ha, na.rm = T)) %>% 
                pivot_wider(names_from = state, values_from = area_ha) %>% 
                mutate(state_total = PD+PF+UD+UF+Unrest) %>% 
                pivot_longer(cols = PD:Unrest, names_to = 'state', values_to = 'area_ha') %>% 
                mutate(prop = area_ha/state_total) %>% 
                droplevels()
            
            g <- ggplot() +
                geom_ribbon(data = mang.global2 %>% 
                                select(year, state, prop) %>% # prop Unprotected Forested
                                pivot_wider(names_from = state, values_from = prop), 
                            aes(x = as.integer(year), ymin=UD+PD+PF+UF, ymax=UF+PF+UD+PD+Unrest, fill="brown3"), alpha =0.75) +
                geom_ribbon(data = mang.global2 %>% 
                                select(year, state, prop) %>% # prop Protected forest
                                pivot_wider(names_from = state, values_from = prop), 
                            aes(x = as.integer(year), ymin=PD+PF+UF, ymax=UD+PD+PF+UF, fill="cadetblue2"), alpha =0.75) +
                geom_ribbon(data = mang.global2 %>%
                                select(year, state, prop) %>% # prop protected deforested
                                pivot_wider(names_from = state, values_from = prop), 
                            aes(x = as.integer(year), ymin=PF+UF,ymax=PD+PF+UF, fill="darkcyan"), alpha =0.75) +
                geom_ribbon(data = mang.global2 %>%
                                select(year, state, prop) %>% # prop unprodected deforested
                                pivot_wider(names_from = state, values_from = prop), 
                            aes(x = as.integer(year), ymin=UF,ymax=PF+UF, fill="lightgoldenrod2"), alpha =0.75) +
                geom_ribbon(data = mang.global2 %>%
                                select(year, state, prop) %>% # prop unrestorable
                                pivot_wider(names_from = state, values_from = prop), 
                            aes(x = as.integer(year), ymin=0,ymax=UF, fill="seagreen"), alpha =0.75) +
              scale_fill_identity(guide="legend", labels = c('Unrestorable', 'Unprotected deforested', 'Protected deforested',
                                                             'Protected forest','Unprotected forest'), name = '') +
                ylab('Proportion') +
                xlab('Year') +
                geom_vline(xintercept = 2050, linetype = 'longdash', alpha = 0.5) +
                geom_vline(xintercept = 2030, linetype = 'longdash', alpha = 0.5) +
                xlim(c(2023, 2070)) 
            g + theme(axis.text = element_text(size = 12),
                      axis.title = element_text(size = 12),
                      legend.text = element_text(size = 12))
        }
        
   }, height = 300, width = 490)
    
    output$seag.prop <- renderPlot({
        
        if(input$var2 == 'Global'){
        seag.projsub <- seag2()
        
        seag.global2 <- seag.projsub %>% 
            group_by(year, state) %>% 
            summarise(area_ha = sum(area_ha, na.rm = T)) %>%
            pivot_wider(names_from = state, values_from = area_ha) %>% 
            mutate(state_total = M + LM) %>% 
            pivot_longer(cols = M:LM, names_to = 'state', values_to = 'area_ha') %>% 
            mutate(prop = area_ha/state_total) %>% 
            droplevels()
        
       g <- ggplot() +
            geom_ribbon(data = seag.global2 %>%
                            select(year, state, prop) %>% # prop unprodected deforested
                            pivot_wider(names_from = state, values_from = prop), 
                        aes(x = as.integer(year), ymin=M,ymax=LM+M, fill="cadetblue2"), alpha =0.75) +
            geom_ribbon(data = seag.global2 %>%
                            select(year, state, prop) %>% # prop unrestorable
                            pivot_wider(names_from = state, values_from = prop), 
                        aes(x = as.integer(year), ymin=0,ymax=M, fill="seagreen"), alpha =0.75) +
            scale_fill_identity(guide = 'legend',labels = c('Lost Meadow', 
                                                            'Meadow'),
                                name = '') +
            ylab('Proportion') +
            xlab('Year') +
            xlim(c(2023,2070)) +
            geom_vline(xintercept = 2050, linetype = 'longdash', alpha = 0.5) +
            geom_vline(xintercept = 2030, linetype = 'longdash', alpha = 0.5)
       g + theme(axis.text = element_text(size = 12),
                 axis.title = element_text(size = 12),
                 legend.text = element_text(size = 12))

       }
        else{
            
            seag.projsub <- seag2()
            
            seag.global2 <- seag.projsub %>% 
              rename(study_site = site) %>% 
              left_join(select(seag.sf, -year), by = 'study_site') %>% 
              filter(SOVEREIGN1 == input$var2) %>% 
              group_by(year, state) %>% 
              summarise(area_ha = sum(area_ha, na.rm = T)) %>%
              pivot_wider(names_from = state, values_from = area_ha) %>% 
              mutate(state_total = M + LM) %>% 
              pivot_longer(cols = M:LM, names_to = 'state', values_to = 'area_ha') %>% 
              mutate(prop = area_ha/state_total) %>% 
              droplevels()
            
            g <- ggplot() +
                geom_ribbon(data = seag.global2 %>%
                                select(year, state, prop) %>% # prop unprodected deforested
                                pivot_wider(names_from = state, values_from = prop), 
                            aes(x = as.integer(year), ymin=M,ymax=LM+M, fill="cadetblue2"), alpha =0.75) +
                geom_ribbon(data = seag.global2 %>%
                                select(year, state, prop) %>% # prop unrestorable
                                pivot_wider(names_from = state, values_from = prop), 
                            aes(x = as.integer(year), ymin=0,ymax=M, fill="seagreen"), alpha =0.75) +
                scale_fill_identity(guide = 'legend',labels = c('Lost Meadow', 
                                                                'Meadow'), name = '') +
                ylab('Proportion') +
                xlab('Year') +
                xlim(c(2023,2070)) +
                geom_vline(xintercept = 2050, linetype = 'longdash', alpha = 0.5) +
                geom_vline(xintercept = 2030, linetype = 'longdash', alpha = 0.5)
            
            g + theme(axis.text = element_text(size = 12),
                      axis.title = element_text(size = 12),
                      legend.text = element_text(size = 12))

        }
    }, height = 300, width = 440)
}

# Run the application 
shinyApp(ui = ui, server = server)

