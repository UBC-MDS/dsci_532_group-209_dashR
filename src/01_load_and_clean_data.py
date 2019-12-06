# Load packages
import pandas as pd

# Load datasets
df = pd.read_csv("../data/drinks_raw.csv")
df_location = pd.read_csv("../data/country_codes_raw.csv")

# Clean geography dataset country names so that they can be merged
df_location = df_location.replace({'name':
                                   {'Antigua and Barbuda': 'Antigua & Barbuda',
                                    'Bolivia (Plurinational State of)': 'Bolivia',
                                    'Bosnia and Herzegovina': 'Bosnia-Herzegovina',
                                    'Brunei Darussalam': 'Brunei',
                                    "Côte d'Ivoire": "Cote d'Ivoire",
                                    'Czechia': 'Czech Republic',
                                    "Korea (Democratic People's Republic of)": 'North Korea',
                                    'Congo, Democratic Republic of the': 'DR Congo',
                                    'Iran (Islamic Republic of)': 'Iran',
                                    "Lao People's Democratic Republic": 'Laos',
                                    'Micronesia (Federated States of)': 'Micronesia',
                                    'Korea, Republic of': 'South Korea',
                                    'Moldova, Republic of': 'Moldova',
                                    'Saint Kitts and Nevis': 'St. Kitts & Nevis',
                                    'Saint Lucia': 'St. Lucia',
                                    'Saint Vincent and the Grenadines': 'St. Vincent & the Grenadines',
                                    'Sao Tome and Principe': 'Sao Tome & Principe',
                                    'Eswatini': 'Swaziland',
                                    'Syrian Arab Republic': 'Syria',
                                    'North Macedonia': 'Macedonia',
                                    'Trinidad and Tobago': 'Trinidad & Tobago',
                                    'United Kingdom of Great Britain and Northern Ireland': 'United Kingdom',
                                    'Tanzania, United Republic of': 'Tanzania',
                                    'United States of America': 'USA',
                                    'Venezuela (Bolivarian Republic of)': 'Venezuela',
                                     'Viet Nam': 'Vietnam'
                                   }}).rename(columns={'name': 'country'})

# merge datasets together
df = df.merge(df_location[['country', 'country-code', 'region', 'sub-region']], how='left')

# add new columns for plotting
df['total_servings'] = df.iloc[:, 1:4].sum(axis=1)
df['prop_wine'] = df['wine_servings'] / df['total_servings']
df['prop_beer'] = df['beer_servings'] / df['total_servings']
df['prop_spirits'] = df['spirit_servings'] / df['total_servings']
# Global Rank of Drink
df['rank_wine'] = df['prop_wine'].rank(ascending = False)
df['rank_beer'] = df['prop_beer'].rank(ascending = False)
df['rank_spirit'] = df['prop_spirits'].rank(ascending = False)
# Relative ranks of drink based on region
df['relative_rank_wine'] = df.groupby('region')['prop_wine'].rank(ascending = False)
df['relative_rank_beer'] = df.groupby('region')['prop_beer'].rank(ascending = False)
df['relative_rank_spirit'] = df.groupby('region')['prop_spirits'].rank(ascending = False)

# rename 'country-code' to 'id'
df = df.rename(columns={'country-code':'id', 'sub-region':'sub_region'})

df.to_csv("../data/merged_data_clean.csv")