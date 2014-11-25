\name{empiricalFDR.DESeq2-package}
\alias{empiricalFDR.DESeq2-package}
\alias{empiricalFDR.DESeq2}
\docType{package}
\title{
Simulation-based False Discovery Rate in RNA-seq
}
\description{
Auxiliary functions for the DESeq2 package to simulate read counts according to the null hypothesis (i.e., with empirical sample size factors, per-gene total counts and dispersions, but without effects of predictor variables) and to compute the empirical false discovery rate.
}
\details{
\tabular{ll}{
Package: \tab empiricalFDR.DESeq2\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-11-25\cr
License: \tab GPL-3\cr
}
The key function is simulateCounts, which takes a fitted DESeq2 data object as an input and returns a simulated data object with the same sample size factors, total counts and dispersions for each gene as in real data, but without the effect of predictor variables. Functions fdrTable, fdrBiCurve and empiricalFDR compare the DESeq2 results obtained for the real and simulated data, compute the empirical false discovery rate (the ratio of the number of differentially expressed genes detected in the simulated data and their number in the real data) and plot the results. 
}
\author{
Mikhail V. Matz
Maintainer: Mikhail V. Matz <matz@utexas.edu>
}
\references{
R. M. Wright, G. V. Aglyamova, E. Meyer and M. V. Matz (2015) Local and systemic gene expression responses to a white-syndrome-like disease in a reef-bulding coral, Acropora hyacinthus. 
}

\examples{

dds = makeExampleDESeqDataSet(betaSD=1, n=100)
dds = DESeq(dds)
sims = simulateCounts(dds)
sims = DESeq(sims)
res = results(dds)
sim.res=results(sims)

# how similar is the simulation to real data? 
plot(sizeFactors(sims)~sizeFactors(dds))
plot(log(dispersions(sims),10)~log(dispersions(dds),10))

# computing and plotting empirical FDR
fdrt = fdrTable(res$pvalue,sim.res$pvalue)
fdrBiCurve(fdrt,maxLogP=4,main="DEG discovery rates")
efdr = empiricalFDR(fdrt,plot=TRUE,main="False discovery rate")

# how many genes pass empirical 0.1 FDR cutoff?
table(res$pvalue<efdr)

# how many genes pass multiplicity-corrected 0.1 FDR cutoff?
table(res$padj<0.1)


}
