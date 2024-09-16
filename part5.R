## Copy and paste it into a rmd file

```{r part5}
## 1.plotted rating in the y axis and  review date on the x axis using ggplot2.
plot1 <- ggplot(chocolate, aes( x = review_date, y = rating)) +
  geom_point()
print(plot1)

## 2. added a lowess smooth line by adding the geom_smooth function
plot2 <- ggplot(chocolate, aes( x = review_date, y = rating))+
  geom_point() +
  geom_smooth()
print(plot2)

## 3. used facets to compare between different company locations
plot3 <- ggplot(chocolate, aes( x = review_date, y = rating))+
  geom_point() +
  geom_smooth() +
  facet_grid(.~company_location)
print(plot3)

## 4. customizing the geometric points with different colors, sizes
plot4 <- ggplot(chocolate, aes( x = review_date, y = rating, color = "brown2" ))+
  geom_point( size = 2) +
  geom_smooth() +
  facet_grid(.~company_location)
print(plot4)

## 5. modifying the geom smooth by adding different line width, type and regression method
plot5 <- ggplot(chocolate, aes( x = review_date, y = rating, color = "brown2" ))+
  geom_point( size = 2) +
  geom_smooth( linewidth = 5, linetype = 5, method = "lm"  ) +
  facet_grid(.~company_location)
print(plot5)

## 6. modifying the labels and giving a chart title name 
plot6 <- ggplot(chocolate, aes( x = review_date, y = rating, color = "brown2" ))+
  geom_point( size = 2) +
  geom_smooth( linewidth = 5, linetype = 5, method = "lm"  ) +
  facet_grid(.~company_location)+
  labs(title = "Rating on different Review date", x = "Review date", y = "Rating" )
print(plot6)

## 7. setting a theme from the Themepark package
remotes::install_github("MatthewBJane/ThemePark")
library("ThemePark")
plot7 <- ggplot(chocolate, aes( x = review_date, y = rating, color = "brown2" ))+
  geom_point( size = 2) +
  geom_smooth( linewidth = 5, linetype = 5, method = "lm"  ) +
  facet_grid(.~company_location) +
  labs(title = "Rating on different Review date", x = "Review date", y = "Rating" ) +
  theme_gameofthrones()
print(plot7)

```