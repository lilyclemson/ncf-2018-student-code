IMPORT $ AS GLMMod;
IMPORT GLMMod.Constants;
IMPORT GLMMod.IRLS;
IMPORT GLMMod.Family;
IMPORT ML_Core.Interfaces;
IMPORT ML_Core.Types AS Types;

//Aliases
NumericField := Types.NumericField;
Layout_Model := Types.Layout_Model;

/**
  * Main GLM regression module. Performs regressions using iteratively 
  * re-weighted least squares (IRLS).
  *
  * @param X The observed explanatory values in NumericField format.
  * @param Y The observed values the model aims to fit in NumericField format.
  * @param fam (Optional) A module defining the type of regression to perform.  Default
  *             = Gaussian (i.e. ordinary least squares).
  * @param weights (Optional) A set of observation weights (one per dependent value), in
  *                 NumericField format.  Default = equal weights.
  * @param max_iter (Optional) Maximum number of iterations to try.  Default = 200.
  * @param epsilon (Optional) The minimum change in the Beta value estimate to continue.
  * @param ridge (Optional) A value to populate a diagonal matrix that is added to
  * a matrix help assure that the matrix is invertible.
  * @see ML_Core.Types.NumericField
  */

 EXPORT GLM(
  DATASET(NumericField) X       = DATASET([], NumericField),
  DATASET(NumericField) Y       = DATASET([], NumericField),
  Family.FamilyInterface fam    = Family.Gaussian,
  DATASET(NumericField) weights = DATASET([], NumericField),
  UNSIGNED max_iter             = 200,
  REAL8 epsilon                 = Constants.default_epsilon,
  REAL8 ridge                   = Constants.default_ridge) :=
                                                      MODULE(Interfaces.IRegression())

  /**
    * Calculate a model to fit the observation data to the observed values.
    * @return The encoded model in Layout_Model format.
    * @see ML_Core.Types.Layout_Model
    */
  EXPORT DATASET(Types.Layout_Model) GetModel :=
    IRLS.GetModel(X, Y, fam, max_iter, epsilon, ridge, weights, TRUE);

  /**
    * Predict the observations using models trained by the GetModel function.
    *
    * @param newX Observations to be predicted.
    * @param model The model as returned from GetModel.
    * @return Predictions in NumericField format.
    * @see ML_Core.Tyeps.NumericField
    */
   EXPORT DATASET(NumericField) Predict(
    DATASET(NumericField) newX,
    DATASET(Layout_Model) model) :=
    GLMMod.Predict(GLMMod.ExtractBeta(model), newX, fam);

END;
