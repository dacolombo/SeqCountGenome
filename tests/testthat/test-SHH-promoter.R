testthat::test_that("Test presence of CAAT-box in human SHH promoter", {
    Hsapiens <- BSgenome.Hsapiens.UCSC.hg38::BSgenome.Hsapiens.UCSC.hg38
    SHH <- GRanges(seqnames="chr7",
                    ranges=IRanges(start=155799980,end=155812463,names="SHH"),
                    strand="-")
    SHH_promoter <- promoters(SHH, upstream=2000, downstream=200)
    CAATbox <- "GGCCAATCA"
    result <- 1
    names(result) <- "GGCCAATCA"
    expect_equal(SeqCountGenome(Hsapiens,SHH_promoter,CAATbox),result)
})
