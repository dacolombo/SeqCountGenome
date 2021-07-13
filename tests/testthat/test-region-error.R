testthat::test_that("Test error for wrong class of genome", {
    Hsapiens <- BSgenome.Hsapiens.UCSC.hg38::BSgenome.Hsapiens.UCSC.hg38
    SHH_promoter <- "wrong region"
    CAATbox <- "GGCCAATCA"
    expect_error(SeqCountGenome(Hsapiens,SHH_promoter,CAATbox))
})