# nimble-virtual-2020

Materials for the virtual NIMBLE workshop, June 3-5 2020.

All materials for the workshop are here. If you're familiar with Git/Github, you already know how to get all the materials on your computer. If you're not, simply click [here](https://github.com/nimble-training/nimble-virtual-2020/archive/master.zip).

Get started [here](https://htmlpreview.github.io/?https://github.com/nimble-training/nimble-virtual-2020/blob/master/overview_slides.html) with logistical information and an outline of the workshop content.

Location: Zoom (URL TBD)
Time: 9 am - 2 pm California time (noon - 5 pm EDT)

## Installing NIMBLE

NIMBLE is an R package on CRAN, so in general it will be straightforward to install as with any R package, but you do need a compiler and related tools on your system.  

In summary, here are the steps.

1. Install compiler tools on your system. [https://r-nimble.org/download](https://r-nimble.org/download) has more details on how to install *Rtools* on Windows and how to install the command line tools of *Xcode* on a Mac. Note that if you have packages requiring a compiler (e.g., *Rcpp*) on your computer, you should already have the compiler tools installed.

2. Install the *nimble* package from CRAN in the usual fashion for an R package. More details (including troubleshooting tips) can also be found in Section 4 of the [NIMBLE manual](https://r-nimble.org/html_manual/cha-installing-nimble.html).

3) To test that things are working please run the following code  in R:

```
library(nimble)
code <- nimbleCode({
  y ~ dnorm(0,1)
})
model <- nimbleModel(code)
cModel <- compileNimble(model)
```

Note that to run on Windows under R 4.0, you will need our most recent version, 0.9.1.

If that runs without error, you're all set. If not, please see the troubleshooting tips and email me directly if you can't get things going.  