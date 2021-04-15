# Exploratory Data Analysis of Airbnb properties in Berlin

This repository contains the files for the project "Exploratory Data Analysis of Airbnb properties in Berlin" for the course Statistical Programming Languages at the Humboldt-University of Berlin in the Winter Term 2018/19.

## Description

Airbnb (airbnb.com) is a famous website allowing private people and commercial entities to rent out part of their spaces. Many studies have already been conducted on price determinants for the hotel industry, like the ones named in [[1]](#1), and for Airbnb properties ([[1]](#1)), but none so far specific for the city of Berlin. 
I therefore attempted an exploratory analysis of Airbnb listings in Berlin and, in particular, of their price. For this purpose linear regression on price was also run and properties were clustered. Because of its interactiveness and flexibility a ShinyApp was built for this project in order to enable the final user a chance to a first-hand analysis of the data.

The seminar paper can be read [here](https://github.com/silvia-ventoruzzo/SPL-WISE-2018/blob/master/SeminarPaper/SilviaVentoruzzo_SPL_WS1819.pdf) and the ShinyApp [here](https://silviav.shinyapps.io/airbnb-berlin/).

## Data
The data for this project was downloaded from [InsideAirbnb](http://insideairbnb.com/get-the-data.html) (stand: 14. January 2019). 

## Content

The project is divided into the following parts:
1. Data preparation:
   - Construction of polygon for Berlin's VBB transportation zones using polygons of Berlin's neighborhoods and train stations
   - Calculation of further Airbnb listings' attributes, such as number of stations and main attractions inside a certain area around them
2. Exploratory Data Analysis:
   - Descriptive statistics and distribution plots
3. Price analysis:
   - Correlation of properties' other features with their price
   - Linear regression with price as dependent variable
4. Clustering:
   - k-Means clustering of Airbnb listings
5. ShinyApp:
   - Autonomous development of a ShinyApp to visualize and try different combinations of steps 2-4

## References
<a id="1">[1]</a> Wang, D. and J. L. Nicolau (2017): Price determinants of sharing economy based accommodation rental: A study of listings from 33 cities on Airbnb. com, International Journal of Hospitality Management, 62, 120–131.


