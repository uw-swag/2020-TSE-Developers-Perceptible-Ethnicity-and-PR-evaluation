############# COMPARING MODELS ###########
##########################################

model_base_EU <- readRDS('models_submitter/base_model_EU.rds')
print(summary(model_base_EU), correlation=FALSE)

model_submitter_EU <- readRDS('models_submitter/model_submitter_EU.rds')
print(summary(model_submitter_EU), correlation=FALSE)
anova(model_submitter_EU,model_base_EU,test="Chisq")

model_submitter_EU_NS <- readRDS('models_submitter/model_submitter_EU_NS.rds')
print(summary(model_submitter_EU_NS), correlation=FALSE)
anova(model_base_EU,model_submitter_EU_NS,test="Chisq")

model_submitter_EU_NoAssociation <- readRDS('models_submitter/model_subbmitter_EU_NoAssociation.rds')
print(summary(model_submitter_EU_NS), correlation=FALSE)
anova(model_submitter_EU_NS,model_submitter_EU_NoAssociation,test="Chisq")

model_submitter_EU_Interaction <- readRDS('model_submitter_EU_InteractionSI.rds')
print(summary(model_submitter_EU_Interaction), correlation=FALSE)
anova(model_submitter_EU_NS,model_submitter_EU_NoAssociation,test="Chisq")



