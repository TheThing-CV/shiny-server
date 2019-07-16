m <- t(combn(c('s1', 'ver', 'vi', 'rrr'), 2))
list(as.vector(m[1, ]), as.vector(m[2, ]), as.vector(m[3, ]))

for (variable in 1:nrow(m)) {
  print(as.vector(m[variable, ]))
}