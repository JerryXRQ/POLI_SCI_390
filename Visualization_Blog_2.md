# Visualization Blog 2 - Uncertainties

## Accounting for Uncertainties in Data

<p align="center">
<img src="Figures/VisBlog2/avg_daily_cases_with_confid.png" width="800" />
</p>

Since the yearly changes in emission should be relatively small in a ten-year interval, we can treat each annual emission value as a separate measurement and use the standard deviation and sample size to derive standard error. Then, we will scale the standard error to find the 95% confidence interval, which is more commonly used than the other options. We can visualize the confidence interval this way as the interval is a region around the average we found.

<p align="center">
<img src="Figures/VisBlog2/avg_emission_with_confid.png" width="800" />
</p>

Since the daily cases should be relatively uniform in a month and surges usually take place in a period of 2 or more months, we can treat each daily case total as a separate measurement and use the standard deviation and sample size to derive standard error. Similarly, we can derive and display the confidence interval as discussed above.

[Click Here to Return to Homepage](README.md)
