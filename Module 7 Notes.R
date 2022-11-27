for any one exp at any angle, and for both exp have 12 angles, take the max of all maxes and then divide all maxes by the ultimate max
which polynomial fits best? 
  then find max of best fitting model

poly.m2 <- lm(normF~poly(ang,2)) #second order
poly.m3 <- lm(normF~poly(ang,3)) #third order
poly.m4 <- lm(normF~poly(ang,4)) #fourth order
# in the "poly...." is the info

#the for loop is a little different thant the one we will neeed
