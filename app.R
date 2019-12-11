library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(tidyverse)
library(plotly)

app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

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
make_graph <- function(region = "World", alcohol = "Beer") {
    # code goes here
}
########################################################################

# Define the graph as a dash component using generated figure
graph <- dccGraph(
  id = 'alcohol-graph',
  figure=make_graph()
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
            htmlLabel('Select a continent:'),
            continentDropdown,
            htmlLabel('Select an alcohol type:'),
            alcoholDropdown,
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
    make_graph(continent_value, alcohol_value)
  })

app$run_server()
