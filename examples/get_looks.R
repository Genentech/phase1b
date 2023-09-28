# argument input for get_looks
dist <- get_distance(c(10, 30))

get_looks(dist, c(10, 30), c(10, 30))

# get_distance when nnE is different nnF
nnE <- c(10, 20)
nnF <- c(15, 20)
nn <- sort(unique(c(nnE, nnF)))
dist <- get_distance(nn)
get_looks(dist, nnE, nnF)

# argument input for get_looks, three element numeric
dist <- get_distance(c(10, 20, 30))

get_looks(dist, c(10, 20, 30), c(10, 20, 30))
