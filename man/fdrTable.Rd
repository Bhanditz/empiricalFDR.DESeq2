\name{fdrTable}
\alias{fdrTable}
\title{
Computes false discovery rates for a series of p-value cutoffs.
}
\description{
Given vectors of p-values from real data and data simulated under a null hypothesis, produces a table listing the number of null hypothesis rejections under a range of p-value cutoffs in real and simulated data.
}
\usage{
fdrTable(real.p, sim.p)
}
\arguments{
  \item{real.p}{
Vector of p-values in the real data.
}
  \item{sim.p}{
Vector of p-values in the simulated data.
}
}
\value{
The function returns a dataframe listing p-value cutoffs, in steps of 0.1 on the decimal log scale, the number of null hypothesis rejections at each cutoff in real and simulated datasets, and their ratio (the false discovery rate).
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
