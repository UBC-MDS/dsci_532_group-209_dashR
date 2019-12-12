library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(tidyverse)
library(plotly)

app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")
df <- read_csv("data/merged_data_clean.csv")


# Selection components
continentDropdown <- dccDropdown(
  id = "continent",
  options = list(
    list('label' = 'World', 'value' = 'World'),
    list('label' = 'Asia', 'value' = 'Asia'),
    list('label' = 'Europe', 'value' = 'Europe'),
    list('label' = 'Africa', 'value' = 'Africa'),
    list('label' = 'Americas', 'value' = 'Americas'),
    list('label' = 'Oceania', 'value' = 'Oceania')
  ),
  value = "World"
)

alcoholDropdown <- dccDropdown(
  id = "alcohol",
  options=list(
    list("label" = "Wine", "value" = "wine"),
    list("label" = "Beer", "value" = "beer"),
    list("label" = "Spirits", "value" = "spirit")
    ),
  value = "beer"
)

########################################################################
# Plot function goes here
make_scatter <- function(world_region = "World", drink = "Beer") {
    if(str_to_lower(world_region) == 'world') {
      plot_scatter_world(drink)
    } else{
    
    xint = df %>% 
      filter(region == str_to_title(world_region)) %>% 
      summarise(xint = max(total_servings) / 2) %>% 
      pull() %>% 
      round()
    if(drink == 'spirit'){
      drink = 'spirits'
    }
    col <- paste('prop_',str_to_lower(drink),sep = '') 
    col_to_select <- match(col,colnames(df))
    
    p <- df %>% 
      filter(region == str_to_title(world_region)) %>% 
      select(region, total_servings, col_to_select, sub_region, country) %>% 
      
      ggplot(.,aes_string(x = "total_servings", y = col, color = "sub_region", text = 'country')) +
      geom_point(size = 3, alpha = 0.75) +
      geom_vline(xintercept = xint, color = 'black', linetype = 'dashed') +
      geom_hline(yintercept = 0.5, color = 'black', linetype = 'dashed') +
      labs(x = "Total Alcohol Servings per Person",
           y = paste('Proportion of',str_to_title(drink), 'Consumed per Person'),
           title = paste("Total Alcohol Servings vs Proportion of",
                         str_to_title(drink),'Consumed in', str_to_title(world_region)),
           color = "Sub-Region") + 
      scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1), limits = c(-0.1, 1.1)) +
      annotate(geom = 'text', label = paste('Heavy drinkers, love', str_to_title(drink)), x = ((xint*2) - 75), y = 1.1, size = 5, color = 'red')+
      annotate(geom = 'text', label = 'Heavy Drinkers, like all types of Alcohol ',  x = ((xint*2) - 100), y = -0.1, size = 5, color = 'red')+
      annotate(geom = 'text', label = paste('Love' , str_to_title(drink), "but Not Big Drinkers"), x = 100, y = 1.1,size = 5, color = 'red')+
      annotate(geom = 'text', label = 'Light dinkers, love all types of Alcohol', x = 100, y = -0.1,size = 5, color = 'red')+
      expand_limits(x = 0, y = 0) +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p, width = 1400, height = 800, tooltip = c("text") )
    
  }
  
}


plot_scatter_world <- function(drink) {
  if(drink == 'spirit'){
    drink = 'spirits'
  }
  col <- paste('prop_',str_to_lower(drink),sep = '')
  
  p <- ggplot(df, aes_string(x = "total_servings", y = col, color = "region", text = "country" )) +
    geom_point(size = 3, alpha = 0.75) + 
    geom_vline(xintercept = 348, color = 'black', linetype = 'dashed', alpha = 0.5) +
    geom_hline(yintercept = 0.5, color = 'black', linetype = 'dashed', alpha = 0.5) +
    labs(x = "Total Alcohol Servings per Person",
         y = paste('Proportion of',str_to_title(drink), 'Consumed per Person'),
         title = paste("Total Alcohol Servings vs Proportion of",str_to_title(drink),'Consumed in the World'),
         color = 'Region') + 
    scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1), limits = c(-0.1, 1.1)) +
    annotate(geom = 'text', label = paste('Heavy drinkers, love', str_to_title(drink)), x = (695 - 75), y = 1.1, size = 5, color = 'red')+
    annotate(geom = 'text', label = 'Heavy Drinkers, like all types of Alcohol ',  x = (695 - 100), y = -0.1, size = 5, color = 'red')+
    annotate(geom = 'text', label = paste('Love' , str_to_title(drink), "but Not Big Drinkers"), x = 100, y = 1.1,size = 5, color = 'red')+
    annotate(geom = 'text', label = 'Light dinkers, love all types of Alcohol', x = 100, y = -0.1,size = 5, color = 'red')+
    expand_limits(x = 0, y = 0) +
    theme(plot.title = element_text(hjust = 0.5))
  ggplotly(p, width = 1400, height = 800, tooltip = c("text"))
}


colors <- tibble(wine = 'red',
                 spirit = 'blue',
                 beer = 'yellow')
top_n_countries <- function (continent = "World", alcohol = 'beer', n=20) {
  title_continent = continent
  if(n<0){
    first_word <- 'Bottom'
  } else{
    first_word <- "Top"
  }
  
  if(continent == "World"){
    continent = unique(df$region)
    title_continent = "World"
  }
  df <- df %>%
    select(matches(alcohol),
           matches("country"),
           matches("region")) %>% 
    filter(region %in% continent)
  p <- df %>% 
    top_n(n = n, wt = !!sym(names(df[2]))) %>% 
    arrange(desc(!!sym(names(df[2])))) %>% 
    ggplot(aes(x = reorder(!!sym(names(df[5])), !!sym(names(df[2]))),
               y = !!sym(names(df[2])),
               fill = !!sym(names(df[2])),
               text = paste("Proportion: ", round(!!sym(names(df[2])), 2)))) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_y_continuous(limits = c(0, 1)) +
    labs(title = paste(first_word, "20 Countries that Love", str_to_title(alcohol), "in", title_continent),
         x = '',
         y = 'Proportion Consumed') +
    scale_fill_gradient(low = "white",
                        high = sym(colors[[alcohol]]),
                        limits = c(0, 1)) +
    theme(legend.position = "none")
  

  ggplotly(p, width = 1200, height = 600, tooltip = c('text'))

}



########################################################################

# Define the graph as a dash component using generated figure
graph <- dccGraph(
  id = 'alcohol-graph',
  figure=make_scatter()
)
bar_chart <- dccGraph(
  id = 'bar_graph',
  figure=top_n_countries()
)

bottom_20_bar_chart <- dccGraph(
  id = 'bot20_bar-graph',
  figure=top_n_countries(n=-20)
)

# app layout
app$layout(
    htmlDiv(
        list(
            htmlH1('Which Countries are Beer-lovers, Wine-lovers, or Spirit-lovers?'),
            htmlDiv(list(
              htmlP(
                "The following dashboard provides a visual overview on the proportion of \
                global alcohol consumption across beer, wine and spirits in 2010. \
                Users can simultaneously adjust the geographic location and specific \
                alcohol type of their choice. The horizontal bar chart on the right of the \
                map dynamically updates as different geographies and alcohol types are selected."
                ),
              htmlP(
                "Note: Proportions are calculated as a ratio of total servings for a specific type of drink \
                divided by the total servings of all drinks in the country. As a result, countries with low total servings \
                of alchohol may have unusually high ratios as shown in the case of Saudi Arabia."
            ))),
            dccMarkdown("Data Source: [FiveThirtyEight](https://github.com/fivethirtyeight/data/tree/master/alcohol-consumption)"),
            htmlIframe(height=15, width=10, style=list(borderWidth = 0)), #space
            #selection components
            htmlLabel('Select a region:'),
            continentDropdown,
            htmlLabel('Select an alcohol type:'),
            alcoholDropdown,
            bar_chart,
            bottom_20_bar_chart,
            graph
        )
    )
)

# Callbacks for interactivity
app$callback(
  #update figure of alcohol-graph
  output=list(id = 'alcohol-graph', property='figure'),
  #based on values of continent and alcohol components
  params=list(input(id = 'continent', property='value'),
              input(id = 'alcohol', property='value')),
  # translate list of params into function arguments
  function(continent_value, alcohol_value) {
    make_scatter(continent_value, alcohol_value)
  })

app$callback(
  #update figure of alcohol-graph
  output=list(id = 'bar_graph', property='figure'),
  #based on values of continent and alcohol components
  params=list(input(id = 'continent', property='value'),
              input(id = 'alcohol', property='value')),
  # translate list of params into function arguments
  function(continent_value, alcohol_value) {
    top_n_countries(continent_value, alcohol_value)
  })

app$callback(
  #update figure of alcohol-graph
  output=list(id = 'bot20_bar-graph', property='figure'),
  #based on values of continent and alcohol components
  params=list(input(id = 'continent', property='value'),
              input(id = 'alcohol', property='value')),
  # translate list of params into function arguments
  function(continent_value, alcohol_value) {
    top_n_countries(continent_value, alcohol_value, -20)
  })

app$run_server(host = "0.0.0.0", port = Sys.getenv('PORT', 8050))
