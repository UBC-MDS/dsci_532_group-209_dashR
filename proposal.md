# Proposal

## Section 1: Motivation and Purpose

According to a [study](https://www.alliedmarketresearch.com/beer-market) conducted by Allied Market Research, the global beer market on its own is projected to reach a valuation of nearly $700 billion by 2025. Despite the massive global market for alcoholic beverages, the industry is extremely competitive with large corporations dominating the majority of consumer market share. Amid this aggressive competition, the companies who best understand their customers and modify their marketing strategies accordingly will reap a majority of the rewards.

We believe harnessing the power of global alcohol consumption statistics integrated into a visual app can provide tangible value for companies operating in this industry. Marketing managers and corporate executives can use our app to assess which geographic regions are the most relevant for product expansion. Our app will save users time with raw data analysis by providing visuals of key alcohol consumption statistics within the beer, wine and spirits categories.


## Section 2: Description of the data

Our group has chosen to use a dataset describing the alcoholic consumption of 193 countries in the year 2010. We sourced the data from FiveThirtyEight ([source](https://github.com/fivethirtyeight/data/tree/master/alcohol-consumption)), who used it for an article they wrote titled, ["Dear Mona Followup: Where Do People Drink The Most Beer, Wine And Spirits?"](https://fivethirtyeight.com/features/dear-mona-followup-where-do-people-drink-the-most-beer-wine-and-spirits/).

FiveThirtyEight collected the data from the "World Health Organisation, Global Information System on Alcohol and Health (GISAH), 2010." WHO tracks this data as part of its global monitoring of alcohol consumption, alcohol-related harm and policy responses. According to FiveThirtyEight, WHO collected the data using "government records as well as statistics from the alcohol industry and the United Nation’s Food and Agriculture Organization database."

The raw dataset has 5 variables and 193 observations. The variables are:

- `country`
- `beer_servings`
- `spirit_servings`
- `wine_servings`
- `total_litres_of_pure_alcohol`

The dataset contains one observation for each country. The 4 associated variables describe the average alcohol consumption per capita for those age 15 in each country in the year 2010.

|Variable|Description|
|---|---|
|`beer_servings`|average number of **cans of beer** consumed per person in 2010|
|`spirit_servings`|average number of **shots of spirits** consumed per person in 2010|
|`wine_servings`|average number of **glasses of wine** consumed per person in 2010|
|`total_litres_of_pure_alcohol`|average **liters of pure alcohol** consumed per person in 2010|

We will also be using a secondary dataset called `ISO-3166-Countries-with-Regional-Codes` ([source](https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes)) to help with the creation of our visuals. We will merge two columns from this data source to our dataset on alcoholic consumption: `region` and `sub-region`.

Finally, we will derive 4 new variables that will help uncover new insights about alcoholic consumption that are often missed by other analysis of the same dataset. For example, most analysis focuses on countries that consume the most beer / wine / spirits, but we can use the `prop_*` variables described below to uncover other trends, like countries that *love* a particular alcohol type more than others.

|Variable|Description|
|---|---|
|`total_servings`|the total of `beer_servings`, `spirit_servings`, and `wine_servings`|
|`prop_beer`|the proportion of `total_servings` that comes from `beer_servings` |
|`prop_spirit`|the proportion of `total_servings` that comes from `spirit_servings` |
|`prop_wine`|the proportion of `total_servings` that comes from `wine_servings` |

## Section 3: Research questions and usage scenarios

**Research questions:**
1. Which countries could be considered "beer-lovers", "wine-lovers", or "spirit-lovers"?
2. Are there any interesting trends or patterns between the type of alcohol consumed and the geographic region?

**Usage scenarios:**

Usage scenario #1:
> Jenny is a marketing executive at a vodka company. Her company is looking to expand globally, but she doesn't have the budget to compete with the big players. She wants to be able to explore global spirit consumption and identify countries that her competition may have neglected. When Jenny logs into the 'Alcohol Consumption' app, she can use the alcohol type drop-down to select "spirits". She will see three visuals: two bar charts ranking the top- and bottom-20 countries by percentage of consumption from "spirits", and a scatterplot showing percentage spirit consumption vs. total alcohol consumption. From the bar charts, Jenny can immediately skim the country names to get an idea of some potential markets. She notices that the underserved markets of Haiti, India, and Liberia are higher on the list than she expected. If she wants to dig deeper, she can scroll down to the scatterplot. Now there is an added dimension -- total alcohol consumption -- that lets her identify which of these countries drink a high percentage of spirits AND drink a lot of alcohol overall. If Jenny scrolls over a point, a tooltip pops up with some key facts about that countries consumption. Once she's narrowed down to a few viable options, she could use the output from the app as part of her pitch to rest of the executive team.

Usage scenario #2:
> Pranav is a business development manager at a global real estate investment firm. The company invests in airports around the world, and is trying to identify the best advertising opportunities for billboards in the terminals. Pranav logs into the app to investigate what type of alcohol companies might be open to advertising in at the airports in each country. First, he uses the "region" drop-down to select Africa. Second, he uses the "alcohol type" drop-down to select "beer". Third, he scrolls to the scatterplot and focuses on the upper right quadrant, which shows countries that drink a lot of alcohol overall, most of which comes from beer. He notices that Namibia is very near the upper right. Pranav scrolls his mouse over the country and a tooltip pops up showing some quick stats. In 2010, 99.5% of consumption came from beer, and Namibia ranks #1 in the world as the msot "beer-loving" country. Pranav is sold, and starts looking up the contact information for the biggest brewers in Namibia to advertise at the airport.
