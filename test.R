steps <- 10

s <- steps + 1
level <- 0
s <- level

for (i in 1:steps) {
    s[i+1] <- s[i] + runif(0,1)
}

print(s)