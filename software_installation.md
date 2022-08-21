---
layout: page
title: R and RStudio installation instructions
nav_exclude: true
---

In this class, we will use R and RStudio for data analysis, visualization, and reports. R is the statistical software, while RStudio is an environment that makes it easy to manage and use that software. Below are instructions for installing R and RStudio (steps 1--5). Please email me if you have any questions or issues!

1. Install R: go to [this link](https://mirror.las.iastate.edu/CRAN/), and choose the download option for your operating system.
    a. Windows: Click **Download R for Windows**, then click the **install R for the first time** button.
    b. macOS: Click **Download R for macOS**. Under *Latest release*, choose the package corresponding to your macOS version.
    c. Linux: Click **Download R for Linux**, then select your distro and follow the installation instructions (you will install via the command line).
2. Install RStudio: go to [this link](https://www.rstudio.com/products/rstudio/download/), and click the **Download** button under RStudio Dekstop. Then, click **Download** on the following screen.
3. Open RStudio (the icon is a white R in a blue circle). You should see something like the following:
![](rstudio_image.jpeg)
4. We'll get acquainted with RStudio in class. For now, let's just check that it works. The panel to the bottom left of the screen is called the **console**. Next to the `>`, type `16 * 7` and hit **Enter** (or **return** on Mac). You should get `112`.
5. Finally, we'll do one more setup step. In the console, copy and paste the following lines of code, and hit enter.

```R
install.packages("tinytex")
tinytex::install_tinytex()
```

It may take a few minutes to run. If using macOS, you may need to enter your password in the installation process.
