# GLM
The GLM bundle is an implementation of [Generalized Linear Models](https://en.wikipedia.org/wiki/Generalized_linear_model) for HPCC.

## Description
The GLM bundle is capable of building GLMs with the following error distributions (+ link functions):

* Gaussian (Identity)
* Gamma (Inverse)
* Inverse Gaussian (Inverse Square)
* Poisson (Log)
* Binomial/Bernoulli (Logit)
* Quasipoisson (Log)
* Quasibinomial (Logit)

Methods for the fitting or models, scoring/prediction, and calculation of fit metrics (log-likelihood and deviance) are included in the bundle for each family above. Models are returned with relevant hypothesis testing metrics (standard errors and p-values).

In addition, for binomial models, which can be applied to classification, functions are supplied for classification (maximum probability) and calculation of confusion matrices.

## Simple usage

#### Without observation weights

```
IMPORT GLM;
IMPORT GLM.Family;
IMPORT GLM.Datasets.HeartScale;
IMPORT ML_Core;
IMPORT ML_Core.Types as Types;

// Pull in the HeartScale dataset (included in bundle repository)
heartScaleDS := HeartScale.Content;

// Convert dataset to the standard NumericField format used by HPCC ML algorithms
ML_Core.ToField(heartScaleDS,heartScaleDS_NF);

// Get predictor data
X_int := heartScaleDS_NF(number <> 1);
X := PROJECT(X_int, TRANSFORM(
  Types.NumericField,
  SELF.number := LEFT.number - 1, SELF := LEFT));

// Get binomial response column
Y_int := heartScaleDS_NF(number = 1);
Y_Binomial := PROJECT(Y_int, TRANSFORM(
  Types.NumericField,
  SELF.value := IF(LEFT.value < 0, 0.0, 1.0), SELF := LEFT));

BinomialSetup := GLM.GLM(X, Y_binomial, Family.Binomial);
BinomialMdl := BinomialSetup.GetModel();
BinomialPreds := BinomialSetup.Predict(X, BinomialMdl);
BinomialDeviance := GLM.Deviance_Detail(Y_Binomial, BinomialPreds, BinomialMdl, Family.Binomial);

OUTPUT(GLM.ExtractBeta_full(BinomialMdl), NAMED('Model'));
OUTPUT(BinomialPreds, NAMED('Preds'));
OUTPUT(BinomialDeviance, NAMED('Deviance'));
```

#### With observation weights

```
IMPORT GLM;
IMPORT GLM.Family;
IMPORT GLM.Datasets.HeartScale;
IMPORT ML_Core;
IMPORT ML_Core.Types as Types;

// Pull in the HeartScale dataset (included in bundle repository)
heartScaleDS := HeartScale.Content;

// Convert dataset to the standard NumericField format used by HPCC ML algorithms
ML_Core.ToField(heartScaleDS,heartScaleDS_NF);

// Get predictor data
X_int := heartScaleDS_NF(number <> 1);
X := PROJECT(X_int, TRANSFORM(
  Types.NumericField,
  SELF.number := LEFT.number - 1, SELF := LEFT));

// Get binomial response column
Y_int := heartScaleDS_NF(number = 1);
Y_Binomial := PROJECT(Y_int, TRANSFORM(
  Types.NumericField,
  SELF.value := IF(LEFT.value < 0, 0.0, 1.0), SELF := LEFT));

// Make weights (in this case all equal to 1)
Types.NumericField makeWeights(Types.NumericField y, INTEGER c) := TRANSFORM
  SELF.value := 1.0;
  SELF := y;
END;
weights := PROJECT(Y_Binomial, makeWeights(LEFT, COUNTER));

BinomialSetup := GLM.GLM(X, Y_binomial, Family.Binomial, weights);
BinomialMdl := BinomialSetup.GetModel();
BinomialPreds := BinomialSetup.Predict(X, BinomialMdl);
BinomialDeviance := GLM.Deviance_Detail(Y_Binomial, BinomialPreds, BinomialMdl, Family.Binomial);

OUTPUT(GLM.ExtractBeta_full(BinomialMdl), NAMED('Model'));
OUTPUT(BinomialPreds, NAMED('Preds'));
OUTPUT(BinomialDeviance, NAMED('Deviance'));
```