x = NULL
y = NULL

i = 48
print(stateCoords$State[i])

  r = stateCoords[i,]
  x1 = rep(r$Longitude, r$catTweets1)
  y1 = rep(r$Latitude, r$catTweets1)
  x = c(x, x1)
  y = c(y, y1)


x = x + rnorm(length(x), sd = .2)
y = y + rnorm(length(y), sd = .2)

points = data.frame(x = x, y = y)


# color the first 20 points (IL) blue and the 2nd 20 (CA) red
col = c(rep("blue", 20), rep("red", 20))

# add the points to the map
m2 = m + geom_point(data = points,  aes(x = x, y = y), size = 1, alpha = 1, color = "red")

# plot the map with the points
plot(m2)

