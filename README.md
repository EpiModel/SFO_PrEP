# Population Impact and Efficiency of Improvements to HIV PrEP Under Conditions of High ART Coverage among San Francisco Men Who Have Sex with Men

This repository holds the source code to reproduce the analysis featured in our HIV transmission model evaluating improvements to PrEP and ART among men who have sex with men in San Francisco.

## Paper and engine

> Le Guillou A, Buchbinder S, Scott H, Liu A, Havlir D, Scheer S, Jenness SM. Population Impact and Efficiency of Improvements to HIV PrEP Under Conditions of High ART Coverage Among San Francisco Men Who Have Sex With Men. _J Acquir Immune Defic Syndr._ 2021; 88(4): 340-347. [doi:10.1097/QAI.0000000000002781](https://doi.org/10.1097/QAI.0000000000002781)

The EpiModelHIV engine for this paper is release [`SFPrEP-JAIDS`](https://github.com/EpiModel/EpiModelHIV/releases/tag/SFPrEP-JAIDS) of EpiModel/EpiModelHIV. Install it with:

```r
remotes::install_github("EpiModel/EpiModelHIV@SFPrEP-JAIDS")
```

ARTnetData is a private package. It is needed only to re-estimate the networks. The paper's networks were estimated with ARTnet 2.5.0, which is no longer public, so `renv.lock` installs ARTnet 2.9.3, the first version in the public [ARTnet](https://github.com/EpiModel/ARTnet) repository. Networks re-estimated with it may not match the paper's exactly.

![Figure 1](inst/Fig1.jpeg)

## Abstract

#### Background

Key components of Ending the HIV Epidemic (EHE) plan include increasing HIV antiretroviral therapy (ART) and HIV preexposure prophylaxis (PrEP) coverage. One complication to addressing this service delivery challenge is the wide heterogeneity of HIV burden and healthcare access across the U.S. It is unclear how the effectiveness and efficiency of expanded PrEP will depend on different baseline ART coverage.

#### Methods

We used a network-based model of HIV transmission for men who have sex with men (MSM) in San Francisco. Model scenarios increased varying levels of PrEP coverage relative under current empirical levels of baseline ART coverage and two counterfactual levels. We assessed the effectiveness of PrEP with the cumulative percent of infections averted (PIA) over the next decade and efficiency with the number needed to treat (NNT) by PrEP required
to avert one HIV infection.

#### Results

In our projections, only the highest levels of combined PrEP and ART coverage achieved the EHE goals. Increasing PrEP coverage up to 75% showed that PrEP effectiveness was higher at higher baseline ART coverage with the PIA ranging from 61% in the lowest baseline ART coverage population to 75% in the highest ART coverage. The efficiency declined with increasing ART (NNT range from 41 to 113).

#### Conclusions

Improving both PrEP and ART coverage would have a synergistic impact on HIV prevention even in a high baseline coverage city like San Francisco. Efforts should focus on narrowing the implementation gaps to achieve higher levels of PrEP retention and ART sustained viral suppression.

## Model Code

These models are written and executed in the R statistical software language. To run these files, it is necessary to use the correct version of our epidemic modeling software, [EpiModel](http://epimodel.org/), and our extension package specifically for modeling HIV/STI transmission dynamics among MSM,
[EpiModelHIV](http://github.com/statnet/EpiModelHIV).

In R, load the necessary packages with the following command:

```
install.packages("renv")
renv::init()
```

Once `renv` has finished initializing, restart R.

### ARTnet Data Access

To restore the full package set from `renv.lock`, you will need a GitHub personal access token with access to the private ARTnetData repository. It should be set either in "~/.bashrc":

```
export GITHUB_PAT="<your github private access token>"
```

or in "~/.Renviron":

```
GITHUB_PAT="<your github private access token>"
```
