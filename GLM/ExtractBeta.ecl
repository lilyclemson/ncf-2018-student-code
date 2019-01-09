IMPORT $ AS GLMmod;
IMPORT GLMmod.Types AS Types;
IMPORT ML_Core.Types AS Core_Types;

// convenience aliases
id_betas := GLMmod.Constants.id_betas;
id_betas_coef := GLMmod.Constants.id_betas_coef;
id_betas_SE := GLMmod.Constants.id_betas_SE;
id_base := GLMmod.Constants.id_base;
base_ind_vars := GLMmod.Constants.base_ind_vars;
Model_Coef := Types.Model_Coef;
/**
  * Extract the beta values form the model dataset.
  *
  * @param mod_ds the model as returned from GetModel.
  * @return the beta values as Model_Coef records, with zero as the constant
  * term.
  * @see Types.Model_Coef
  */
EXPORT ExtractBeta(DATASET(Core_Types.Layout_Model) mod_ds):=FUNCTION
  full := GLMmod.ExtractBeta_full(mod_ds);
  rslt := PROJECT(full, Model_Coef);
  RETURN rslt;
END;