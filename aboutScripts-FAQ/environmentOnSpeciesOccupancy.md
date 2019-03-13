Gaussian vs quadratic term
================
Javiera Rudolph
March 5, 2019

``` r
# Environment -------------------------------------------------------------
# The effect of the environment on the local performance and colonization

S_f <- function(E, u_c, s_c) {
  R <- ncol(u_c)
  N <- nrow(E)
  D <- ncol(E)
  S <- matrix(1, nr = N, nc = R)
  for(i in 1:D){
    optima <- matrix(u_c[i,],nrow = N,ncol = R,byrow = TRUE)
    breadth <- matrix(s_c[i,],nrow = N,ncol = R,byrow = TRUE)^2
    S <- S*exp(-(E[,i]-optima)^2 / breadth)
  }
  return(S)
}
```

![](environmentOnSpeciesOccupancy_files/figure-markdown_github/unnamed-chunk-2-1.png)

Same data, different visualization:

![](environmentOnSpeciesOccupancy_files/figure-markdown_github/unnamed-chunk-3-1.png)

Change the response to a quadratic function instead. I used a vertex form, *y* = *a*(*x* − *h*)+*k*, for which I defined the vertex to be at x = niche optima and y = maximum occupancy(1). The roots of the parabola are set to the minimum and maximum niche values based on the niche spread.

``` r
# Environment -------------------------------------------------------------
# The effect of the environment on the local performance and colonization

S_f_v2 <- function(E, u_c, s_c) {
  R <- ncol(u_c)
  N <- nrow(E)
  D <- ncol(E)
  S <- matrix(1, nr = N, nc = R)
  for(i in 1:D){
    optima <- matrix(u_c[i,],nrow = N,ncol = R,byrow = TRUE)
    breadth <- matrix(s_c[i,],nrow = N,ncol = R,byrow = TRUE)
    S <- S * ((-1 / (breadth/2)^2) * (E[,i] - optima)^2 + 1)
  }
  return(S)
}
```

Plot

![](environmentOnSpeciesOccupancy_files/figure-markdown_github/unnamed-chunk-5-1.png)

![](environmentOnSpeciesOccupancy_files/figure-markdown_github/unnamed-chunk-6-1.png)

Try to superimpose both curves:

![](environmentOnSpeciesOccupancy_files/figure-markdown_github/unnamed-chunk-7-1.png) ![](environmentOnSpeciesOccupancy_files/figure-markdown_github/unnamed-chunk-8-1.png)

![](environmentOnSpeciesOccupancy_files/figure-markdown_github/unnamed-chunk-9-1.png)

![](environmentOnSpeciesOccupancy_files/figure-markdown_github/unnamed-chunk-10-1.png)

![](environmentOnSpeciesOccupancy_files/figure-markdown_github/unnamed-chunk-11-1.png)

![](environmentOnSpeciesOccupancy_files/figure-markdown_github/unnamed-chunk-12-1.png)

![](environmentOnSpeciesOccupancy_files/figure-markdown_github/unnamed-chunk-13-1.png)

![](environmentOnSpeciesOccupancy_files/figure-markdown_github/unnamed-chunk-14-1.png)
