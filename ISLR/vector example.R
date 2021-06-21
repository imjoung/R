c(1,7:9)
c(1:5, 10.5, "next")

x<-1:4
names(x) <- letters[1:4]
x
c(x)
as.vector(x)

11 <- list(A=1, c="C")

c(11, d=1:3)

c(11, d=list(1:3))

c(list(A=c(B=1)), recursive=TRUE)

c(options(), recursive=TRUE)
c(list(A=c(B=1,c=2), B=C(E=7)),recursive=TRUE)

Sys.setlocale("LC_ALL","ko_KR,UTF=8")
par(family="AppleGothic")

