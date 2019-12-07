## Reflection On Previous Milestone

In our second milestone [reflection](https://github.com/UBC-MDS/dsci_532_group-209/blob/master/team_reflection.md) we discussed several actions which we could take to further enhance our dashboard. These improvements included adjusting the colour gradient of the legend to dynamically adjust as users clicked on different countries. Another idea was to add the national flag for the countries along the horizontal bar chart on the side. Due to time limitations and prioritization of other key issues our group was unable to implement these enhancements in our dashboard.

## Overview of Changes Made

After receiving similar feedback from other groups, we found that when users selected “North America” under the interactive cell, the map displayed was not representative of the true geographic region. As a result, we manually adjusted the coordinates in the plot function to focus only on North American countries. Furthermore, many groups suggested that the definition of the alcohol proportions in our plot tool-tip was not clear and somewhat misleading. We addressed this issue by including a clear definition in the description of our dashboard to provide better context. As well, we received feedback from Alejandra (our TA), which we addressed this week. First, we updated our app description in the README file to accurately describe the updated functionalities and visualizations in our app. As well, based on Alejandra's suggestion, we made our bar chart wider and added an x axis label for the chart.

In regards to our strategy for maintenance of this dashboard, we decided to first make the changes necessary to address the feedback we received from our peers and Alejandra, summarized above. Then, we focused on improving the readability of our code in the app.py file, by adding more detailed docstrings to our functions, adding more comments to explain what the code is doing, and editing the code to be in PEP8 format. Improving readability of our code was important to us, as we wanted to ensure that users will understand how we created our app easily, and that if in the future we want to improve the app, we can easily understand what we did, and how we can improve it. 

The overview of the tasks we completed this week was communicated over Github and [documented](https://github.com/UBC-MDS/dsci_532_group-209/issues/27) for future reference. This allowed each group member to track the various changes that were to be made in the master project repository.

## Overview of Feedback and Reflection:

All of the groups provided us with praise and constructive criticism for various aspects of our dashboard. Firstly all of the groups enjoyed the sleek aesthetic layout of our dashboard. Many individuals mentioned that they enjoyed working with the dashboard due to its ease of use and simple interface. Another positive remark was the layout of the adjustable cells for both the geographic region and type of alcohol hence allowing greater interactivity. A common criticism for our dashboard pertained to the definition of alcohol proportions. All of the users were confused about this definition and how to interpret the numerical value of proportion.

This feedback exercise was valuable in allowing us to receive suggestions that we would have not otherwise considered. For example, to us the definition of a proportion was clear, however, due to this “curse of knowledge” all of the individuals who viewed our dashboard for the first time were not able to understand the context of that number.

There were several suggestions which were less helpful in improving our overall project. Some users suggested advanced features such as zooming within the map to have even more interactivity. Another bonus suggestion was adding another chart to allow for comparisons with a country and assess the ranking of a nation’s preference for alcoholic beverages. Although these are both interesting recommendations we were not able to pursue them due to our limited time constraint.


## Wishlist Features and Bug-Fixes:

Our top priority for bug fixes was focused on changing the coordinates of the North American map and including a definition of what we meant by proportions. These two issues were the primary complaints among all of the groups and hindered their ability to understand the context of the dashboard. The other bonus features suggested such as the ability to zoom in and out of the map would have been a useful addition as users would not be required to manually adjust the interactive cells. Additionally, when time permits we would consider implementing another suggestion to show a chart of intracountry alcohol consumption across various types of drinks.
