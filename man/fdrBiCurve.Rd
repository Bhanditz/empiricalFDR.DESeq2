\name{fdrBiCurve}
\alias{fdrBiCurve}
\title{
Plots the numbers of null hypothesis rejections
}
\description{
Plots the numbers of null hypothesis rejections in real data and data simulated under null hypothesis (false positives)
}
\usage{
fdrBiCurve(fdr.table, maxLogP = 5, ...)
}
\arguments{
  \item{fdr.table}{
The output of fdrTable(): a dataframe listing p-value cutoffs and the number of null hypothesis rejections at each cutoff in the real and simulated datasets.
}
  \item{maxLogP}{
Maximal negative decimal logarithm of the p-value for plotting (the default, 5, implies the data for p-values better than 10e-5 will not be plotted)
}
  \item{\dots}{
Additional plotting parameters.
}
}
\value{
The plot is designed to ascertain that the number of discoveries in real data (black line) indeed exceeds the number of false positives (red line) across the range of p-value cutoffs. The grey dotted line is the number of discoveries expected under uniform distribution of p-values.
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
