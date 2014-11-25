\name{simulateCounts}
\alias{simulateCounts}
\title{
Simulating RNA-seq read counts
}
\description{
This function takes a fitted DESeq2 data object as an input and returns a simulated data object with the same sample size factors, total counts and dispersions for each gene as in real data, but without the effect of predictor variables.
}
\usage{
simulateCounts(deseq.object)
}
\arguments{
  \item{deseq.object}{
DESeq2 data object, with estimated size factors and dispersions (output of DESeq() function).
}
}
\details{
For each gene, the total counts are randomly resampled into different samples. The estimated per-gene dispersions are understood as the suare of the coefficient of variation and used to simulate random deviations in per-sample assignment probability. The probabilities of per-sample assignment are also weighted by the empirical sample size factors.
}
\value{
DESeq2 data object
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
