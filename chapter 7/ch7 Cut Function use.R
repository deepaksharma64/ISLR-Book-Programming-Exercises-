df=data.frame(year=sample(1990:2000,100,replace = TRUE),users=sample(134:10000,100))
table(df$year)
df$yearbucket=cut(df$year,c(1993,1997,1999))
names(df)
table(df$yearbucket)
df$yearbycket
df
