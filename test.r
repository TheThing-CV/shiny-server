m <- t(combn(levels(iris$Species), 2))
list(as.vector(m[1, ]), as.vector(m[2, ]), as.vector(m[3, ]))

l <- list()

for (variable in 1:nrow(m)) {
  l[[variable]] <- as.vector(m[variable, ])
}

l

levels(iris$Species)
