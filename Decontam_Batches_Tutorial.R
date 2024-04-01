asv_df <- read.delim("/Users/jrabasc/Desktop/github/Decontam_Validation/dada2_output/dada2_feature_table/table.tsv", check.names=FALSE)
rownames(asv_df) <- asv_df[, 1]
asv_df <- asv_df[, -1]
asv_df<-t(asv_df)
numero_df <- as.matrix(sapply(asv_df, as.numeric))

metadata_df<-read.csv(file = "/Users/jrabasc/Desktop/github/Decontam_Validation/dada2_output/metadata_non_HMS_samples_removed.csv", check.names=FALSE)
rownames(metadata_df) <- metadata_df[, 1]

meta_data_cols <-function(asv_df, metadata_df, control.col){
  control_vec<-c()
  index<-0
  for (id in colnames(metadata_df)) {
    index=index+1
    if(id == control.col){
      control_vec<-metadata_df[,c(index)]
    }
  }
  # We need to align the metadata to the asv table
  mapped_ids <- match( rownames(asv_df),rownames(metadata_df))
  control_vec <- control_vec[mapped_ids]
  # drop sample IDs not in the asv table
  control_vec <- na.omit(control_vec)
  return(control_vec)
}


control_vec <- meta_data_cols(asv_df, metadata_df, "exp_sample_type")
true_false_control_vec<-grepl("control",control_vec)
prev_contam <- isContaminant(asv_df, neg=true_false_control_vec, threshold=0.1, detailed=TRUE, normalize=TRUE, method='prevalence')


