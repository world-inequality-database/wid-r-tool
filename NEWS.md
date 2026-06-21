# CHANGES TO wid-r-tool suggested in 2026-06-20 fork by Spencer Graves

## Motivation: 

I cloned wid-r-tool on 20206-06-18 and encountered a problem immediately. I ran:

devtools::document();
roxygen2::roxygenize();
devtools::test();
devtools::check()

document();roxygenize();test() ran with no complaints, but check() reported:

WARNING
  Documented arguments not in \usage in Rd file 'environment.Rd':
    ‘areas’ ‘sixlet’

ALSO, NAMESPACE included export(download_wid) but NOT 
export(get_variables_areas); export(get_data_variables); 
export(get_metadata_variables). 

I assume that get_variables_areas, get_data_variables, and 
get_metadata_variables should also be exported, but environment should NOT be 
exported. The man subdirectory also included 
check_ages.Rd, check_areas.Rd, check_indicators.Rd, check_perc.Rd, check_pop.Rd, 
and check_years.Rd, so I exported those as well; otherwise user could see them 
in the documentation but could not access then without, e.g., wid::check_ages. 

I converted base_api_url to a function and added enviroment as an argument to 
it with optional values c("prod", "dev"), with the first being the default.
And I added environment as a new final argument to all the functions that use 
base_api_url. 

I addec "#' @export" immediately prior to the definitions for all the functions 
that I thought should be exported, as suggested above. 

## WHO AM I

I've been contributing packages to CRAN since 2006 and GitHub since at least 
2014 but roxygen2 only since last October.

## MY INTEREST:

I have a PhD in statistics. My experience with data that are all greater than 
zero like income, wealth, populations, etc., is that they tend to be 
lognormally distributed to a first order of approximation, with log-Student's t 
likely getting a better fit when the upper tail tends to follow a power law. 
(The upper tail of a Student's t distribution is asympototic to a power law.) 

There is also a log-skewed-t distribution, and one can add spline (piecewise 
polynomial) adjustments to a log-density function, as described in section 
5.4.3 on "Probability Density Functions" in my book with Ramsay and Hooker on, 
"Functional Data Analysis with R and Matlab". (If you would like a pdf of that 
book, I will happily provide such; you can request it from "Spencer Graves 3" 
on ResearchGate.)

I don't know without trying how well this will work, but I think it likely 
that we could get a good fit to much of the WID data using a log-Student's with 
a constant value for the degrees of freedom shared by different countries. To 
the extent that that will work, the standard deviation of the logarithms will 
become a better summary of inequality than the Gini coefficient.

My time to work on this is limited, but I'm keenly interested in estimating 
what the increase in inequality costs the bottom 99 percent of the population 
assuming the same increase in the average over time, especially in the US, 
where I've lived most of my life to date.

Thanks for your work on this package.
Sincerely, Spencer Graves, PhD, m: +1-408-655-4567
