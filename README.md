
<!-- README.md is generated from README.Rmd. Please edit that file -->

# domedselectivity

<!-- badges: start -->

<!-- badges: end -->

The goal of domedselectivity is to allow the user to explore the
workings of the selectivity pattern 24 (size) and 20 (age) from Stock
Synthesis 3.3+. This is not a published shiny app but rather the R file
that gives rise to such an app. To use it one needs to copy the
‘shiny-domed.R’ file out of the R folder and keep it somewhere
convenient on your own machine. If you open this in RStudio then, at the
top right of the source pane you will see a tiny green arrow next to
‘Run App’. There are options in a drop-down menu under that label, but
press the ‘Run App’ and the shiny app with appear.

domedselectivity uses the shiny and DT packages, so they will also need
to be installed.

You can enter specific values into the parameter boxes or use the up and
down arrows within each box to alter the values. With each new value the
plot should update.

This shiny app was only put together because I could never get the the
one on the NOAA web-site to work for more than a few seconds before it
started going a little crazy.

You can either download this repository or a zip file by using the Green
\[\<\> Code\] button, or just open shiny-domed.R in the R sub-directory
and directly copy the R code and paste it into your own file. The
shiny-domed.R file is the only functional part of this repository.

Why not just publish the app and be done with it?

By having the code used in front of you the potential for using a
‘black-box’ is greatly reduced. Should you wish, you can explore the
code, and really see how it all works. That would be far better than
taking my word for it that I have made no mistakes! You could even
modify the whole thing to explore and plot a function of your own
choosing.

It would be a good idea to read or visit:

1)  Methot, R.D, and C.R Wetzel (2013) Stock synthesis: A biological and
    statistical framework for fish stock assessment and fishery
    management. *Fisheries Research* **142**:86-99. see **supplementary
    materials** p8-9, equations A.1.30 - A.1.35,
2)  Methot, R.D., Wetzel, C.R., Taylor, I.G., Doering, K.L., Perl, E.F.,
    and K.F Johnson (2025) Stock Synthesis User Manual Version
    3.30.23.2, April 2025. see especially pages 148 - 149.
3)  I have removed the hyperlinks in the following so just copy them and
    paste them into a browser.
4)  <https://>nmfs-ost.github.io/ss3-website/index.html, to access
    latest SS33 Manual and executable
5)  <https://>github.com/nmfs-ost/ss3-user-examples, to access the
    example SS33 files used here.
