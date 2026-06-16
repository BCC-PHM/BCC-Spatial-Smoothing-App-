# BCC Spatial Smoothing App

Created by: [Chung Au-Yeung](https://github.com/ChungHim5d) </br>
Population Health Management</br>
Birmingham City Council

## Introduction

### What is Birmingham Spatial Smoothing App?

The Birmingham Spatial Smoothing App is a shiny app designed to support analysts at Birmingham City Council in producing **small-area health estimates** using Bayesian spatial smoothing models. 
The app aims to remove the need to write model code directly. Users can supply their own data set and shape file through a guided interface, and the app will fits the model based on user's decision. 
The app ultimately returns smoothed estimates and downloadable map visuals as a one-stop service. 

The underlying models are built on the Besag-York-Mollié 2 (BYM2) framework implemented via R-INLA, and account for spatial autocorrelation between neighbouring areas. It implies that 
areas with limited data or even no data borrow strengths from their neighbours rather than relying solely on local counts.


## Model Selection Guide

The app supports four model types depending on your data structure and outcome measure. Use the decision guide below to identify the right model before running the app.


### Decide the appropriate model following the below guidance

<div class="figure" style="text-align: center">

<img src="fig/read_me_images/spatial_model_selection_flowchart_trimmed.svg" alt="Model selection flowchart" width="100%" />
<p class="caption">
Model selection flowchart
</p>

</div>

## Installation & Setup
Please follow the [release page](https://github.com/BCC-PHM/BCC-Spatial-Smoothing-App-/releases/tag/v1.0) to download the latest release of the app. The requirements of using this 
app are listed in the same release page. 


## How to Use the App

### Step 1 — Upload your data

Prepare your input file (.csv or .xlsx) and **zipped shape file** and upload them to the appropriate upload spaces specified in the app. 


