\name{empiricalFDR}
\alias{empiricalFDR}
\title{
Computing the p-value cutoff to achieve a given FDR.
}
\description{
This function calculates the cutoff at which a particular false discovery rate is observed using loess smoothing and interpolation.
}
\usage{
empiricalFDR(fdr.table, FDR = 0.1, maxLogP = 5, plot = FALSE, span = 0.1, ...)
}
\arguments{
  \item{fdr.table}{
The output of fdrTable(): a dataframe listing p-value cutoffs and the number of null hypothesis rejections at each cutoff in the real and simulated datasets.
}
  \item{FDR}{
The target false discovery rate.
}
  \item{maxLogP}{
Maximal negative decimal logarithm of the p-value for plotting (the default, 5, implies the data for p-values better than 10e-5 will not be plotted)
}
  \item{plot}{
Whether to produce a plot.
}
  \item{span}{
span parameter for loess smoothing.
}
  \item{\dots}{
Additional plotting parameters.
}
}
\value{
The function returns a single value, which is the p-value cutoff at which the target FDR is observed. With plot = TRUE, also plots the observed experimental FDRs with the loess smoother.
}
\references{
R. M. Wright, G. V. Aglyamova, E. Meyer and M. V. Matz (2015) Local and systemic gene expression responses to a white-syndrome-like disease in a reef-bulding coral, Acropora hyacinthus. 
}
\author{
Mikhail V. Matz
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
\keyword{ false discovery rate }
\keyword{ RNA-seq }
\keyword{ DESeq2 }
\keyword{ simulation }
