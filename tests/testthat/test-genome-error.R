testthat::test_that("Test error for wrong class of genome", {
    Hsapiens <- "wrong genome"
    SHH <- GRanges(seqnames="chr7",
                ranges=IRanges(start=155799980,end=155812463,names="SHH"),
                strand="-")
    SHH_promoter <- promoters(SHH, upstream=2000, downstream=200)
    CAATbox <- "GGCCAATCA"
    expect_error(SeqCountGenome(Hsapiens,SHH_promoter,CAATbox))
})
