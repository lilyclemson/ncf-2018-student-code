IMPORT ML_Core AS Core;

PI := 3.141592653589793238462643;
LargeValue := 999999999999999;
SmallValue := 0.0000000000001;
/**
  * Definitions of supported families of Linear Models.
  * <p>Currently supported families are:<ul>
  *   <li>Binomial</li>
  *   <li>Quasibinomial</li>
  *   <li>Poisson</li>
  *   <li>Quasipoisson</li>
  *   <li>Gamma</li>
  *   <li>Gaussian</li>
  *   <li>InvGaussian</li></ul>
  *
  * <p>In addition, FamilyInterface defines the interface needed
  * to add new families.
  * <p>Adding new families is fairly straightforward and involves
  * overlaying a set of scalar functions that define the computations
  * for that family.  See FamilyInterface below.
  */
EXPORT Family := MODULE
  // Define interface
  /**
    * Defines the interface to create new GLM Regression Families.
    * Each family defines a series of eleven attributes that
    * describe the computations for that family within the overall
    * GLM model.
    */
  EXPORT FamilyInterface := INTERFACE
    /**
      * This function defines the linkage between output of the linear
      * function on independent data and the dependent data.
      * @param m The output from the linear function (i.e. the mean)
      * @return The value to be compared to the dependent data.
      */
    EXPORT REAL8 link(REAL8 m);
    /**
      * The Mean function is the inverse of the link function.  It
      * maps the expected value of the dependent variable to the
      * expected linear result.
      *
      * @param v The expected value of the dependent variable.
      * @return The expected output from the linear function.
      */
    EXPORT REAL8 mu  (REAL8 v);           // mean function
    /**
      * The derivative of the output of the linear function with
      * respect to the expected value of the dependent variable.
      * @param m The value of the output.
      * @return The derivative at m.
      */
    EXPORT REAL8 deta(REAL8 m);           // derivative of the linear predictor (eta) w.r.t. mu
    /**
      * The variance as a function of the output value.  This is
      * used for heteroskedastic distributions, otherwise 1.
      * @param m The value of the output.
      * @return The expected variance when output is at m
      */
    EXPORT REAL8 var (REAL8 m);           // variance function
    /**
      * Initialization transform sets the initial value for Betas
      * when running Iteratively Re-weighted Least Squares (IRLS).
      * @param y the dependent value.
      * @param w the current weight.
      * @return the initial weight value to use.
      */
    EXPORT REAL8 init(REAL8 y, REAL8 w);  // initialization transformation
    /**
      * Log Likelihood function.
      *
      * @param y The dependent variable.
      * @param m The output value.
      * @param disp The dispersion factor
      * @return The log likelihood of seeing m given y.
      */
    EXPORT REAL8 ll  (REAL8 y,            // log-likelihood function
                      REAL8 m,
                      REAL8 disp);
    /**
      * The string representation of the mu function (see mu above) for use
      * in LUCI. See LUCI guide for formatting of this ECL string.
      * @returns An ECL string representation of the mu function.
      */
    EXPORT STRING mu_LUCI;                // String representation of mean function
    /**
      * Flag indicating whether the error distribution should be
      * adjusted for over-dispersion or under-dispersion.
      */
    EXPORT BOOLEAN dispersion;            // Flag indicating if errors should be adjusted for 
                                          // over- under-dispersion
    /**
      * The minimum and maximum cardinality (i.e. number of unique
      * values) for dependent data.
      * @return SET([min_cardinality, max_cardinality])
      */
    EXPORT SET OF UNSIGNED4 cardinality;
    /**
      * The range of values that the dependent data can take.
      *
      * @return SET([min_value, max_value])
      */
    EXPORT SET OF REAL8 values;
    /**
      * Flag that indicates that the dependent variables can only
      * take Integer values.  If FALSE, then REAL values are
      * supported.
      * @return Boolean indicating if output is restricted to Integer
      *         values.
      */
    EXPORT BOOLEAN isInteger;
  END;
  /**
    * The Binomial Regression Family models the response (dependent variable(s))
    * as a series of Bernoulli Trials, of one of two disjoint outcomes.
    *
    * <p>It is appropriate for modeling a binary result such as success / fail or
    * true / false, which is typical in binary classification problems.
    */
  EXPORT Binomial := MODULE(FamilyInterface)
    /**
      * @internal
      */
    EXPORT REAL8 link(REAL8 m) := LN(m / (1.0 - m));
    /**
      * @internal
      */
    EXPORT REAL8 mu  (REAL8 v) := 1.0 / (1.0 + EXP(-v));
    /**
      * @internal
      */
    EXPORT REAL8 deta(REAL8 m) := 1.0 / (m * (1.0 - m));
    /**
      * @internal
      */
    EXPORT REAL8 var (REAL8 m) := m * (1.0 - m);
    /**
      * @internal
      */
    EXPORT REAL8 init(REAL8 y, REAL8 w = 1.0) := (w * y + 0.5) / (w + 1.0);
    /**
      * @internal
      */
    EXPORT REAL8 ll  (REAL8 y, REAL8 m, REAL8 disp = 0.0) := y * LN(m) + (1.0 - y) * LN(1.0 - m);
    /**
      * @internal
      */
    EXPORT STRING mu_LUCI := '1.0 / (1.0 + EXP(-Raw_point))';
    /**
      * @internal
      */
    EXPORT BOOLEAN dispersion := FALSE;
    /**
      * @internal
      */
    EXPORT SET OF UNSIGNED4 cardinality := [2, LargeValue];
    /**
      * @internal
      */
    EXPORT SET OF REAL8 values := [0.0, 1.0];
    /**
      * @internal
      */
    EXPORT BOOLEAN isInteger := FALSE;
  END;
  /**
    * The Quasibinomial Regression Family is similar to the Binomial family
    * (see Binomial above) except that it adjusts for situations where the variance
    * of the distribution is greater or less than anticipated by the Binomial
    * Distribution.  This is known as over-dispersion or under-dispersion.
    * <p>The results are adjusted based on the dispersion of the data to better model
    * the observations in these situations.
    */
  EXPORT Quasibinomial := MODULE(FamilyInterface)
    /**
      * @internal
      */
    EXPORT REAL8 link(REAL8 m) := LN(m / (1.0 - m));
    /**
      * @internal
      */
    EXPORT REAL8 mu  (REAL8 v) := 1.0 / (1.0 + EXP(-v));
    /**
      * @internal
      */
    EXPORT REAL8 deta(REAL8 m) := 1.0 / (m * (1.0 - m));
    /**
      * @internal
      */
    EXPORT REAL8 var (REAL8 m) := m * (1.0 - m);
    /**
      * @internal
      */
    EXPORT REAL8 init(REAL8 y, REAL8 w = 1.0) := (w * y + 0.5) / (w + 1.0);
    /**
      * @internal
      */
    EXPORT REAL8 ll  (REAL8 y, REAL8 m, REAL8 disp) := disp * (y * LN(m) + (1.0 - y) * LN(1.0 - m));
    /**
      * @internal
      */
    EXPORT STRING mu_LUCI := '1.0 / (1.0 + EXP(-Raw_point))';
    /**
      * @internal
      */
    EXPORT BOOLEAN dispersion := TRUE;
    /**
      * @internal
      */
    EXPORT SET OF UNSIGNED4 cardinality := [2, LargeValue];
    /**
      * @internal
      */
    EXPORT SET OF REAL8 values := [0.0, 1.0];
    /**
      * @internal
      */
    EXPORT BOOLEAN isInteger := FALSE;
  END;
  /**
    * Poisson Regression is generally used to model count data,
    * where the dependent variable is a positive (or zero) integer.
    * <p>It is also known as a log-linear model in that the logarithm
    * of the dependent variables is assumed to be linear.
    */
  EXPORT Poisson := MODULE(FamilyInterface)
    /**
      * @internal
      */
    EXPORT REAL8 link(REAL8 m) := LN(m);
    /**
      * @internal
      */
    EXPORT REAL8 mu  (REAL8 v) := EXP(v);
    /**
      * @internal
      */
    EXPORT REAL8 deta(REAL8 m) := 1.0 / m;
    /**
      * @internal
      */
    EXPORT REAL8 var (REAL8 m) := m;
    /**
      * @internal
      */
    EXPORT REAL8 init(REAL8 y, REAL8 w = 1.0) := y + 0.1;
    /**
      * @internal
      */
    EXPORT REAL8 ll  (REAL8 y, REAL8 m, REAL8 disp = 0.0) := y * LN(m) - m - LN(Core.Math.gamma(y + 1.0));
    /**
      * @internal
      */
    EXPORT STRING mu_LUCI := 'EXP(Raw_point)';
    /**
      * @internal
      */
    EXPORT BOOLEAN dispersion := FALSE;
    /**
      * @internal
      */
    EXPORT SET OF UNSIGNED4 cardinality := [2, LargeValue];
    /**
      * @internal
      */
    EXPORT SET OF REAL8 values := [0, LargeValue];
    /**
      * @internal
      */
    EXPORT BOOLEAN isInteger := TRUE;
  END;
  /**
    * Quasipoisson Regression is similar to Poisson Regression (see
    * Poisson above) except that it adjusts for situations where the variance
    * of the distribution is greater or less than anticipated by the Poisson
    * Distribution.  This is known as over-dispersion or under-dispersion.
    * <p>The results are adjusted based on the dispersion of the data to better model
    * the observations in these situations.
    */
  EXPORT Quasipoisson := MODULE(FamilyInterface)
    /**
      * @internal
      */
    EXPORT REAL8 link(REAL8 m) := LN(m);
    /**
      * @internal
      */
    EXPORT REAL8 mu  (REAL8 v) := EXP(v);
    /**
      * @internal
      */
    EXPORT REAL8 deta(REAL8 m) := 1.0 / m;
    /**
      * @internal
      */
    EXPORT REAL8 var (REAL8 m) := m;
    /**
      * @internal
      */
    EXPORT REAL8 init(REAL8 y, REAL8 w = 1.0) := y + 0.1;
    /**
      * @internal
      */
    EXPORT REAL8 ll  (REAL8 y, REAL8 m, REAL8 disp) := disp * (y * LN(m) - m - LN(Core.Math.gamma(y + 1.0)));
    /**
      * @internal
      */
    EXPORT STRING mu_LUCI := 'EXP(Raw_point)';
    /**
      * @internal
      */
    EXPORT BOOLEAN dispersion := TRUE;
    /**
      * @internal
      */
    EXPORT SET OF UNSIGNED4 cardinality := [2, LargeValue];
    /**
      * @internal
      */
    EXPORT SET OF REAL8 values := [0, LargeValue];
    /**
      * @internal
      */
    EXPORT BOOLEAN isInteger := FALSE;
  END;
  /**
    * Gamma Regression is used to model continuous,non-negative,
    * data with a right-skew.  Such data exhibits heteroskedacity,
    * (i.e. inconsistent variance across the range).  The gamma
    * regression assumes that the variance is near constant on a
    * log scale.  Various types of financial and insurance data
    * often have these characteristics.
    */
  EXPORT Gamma := MODULE(FamilyInterface)
    /**
      * @internal
      */
    EXPORT REAL8 link(REAL8 m) := 1.0 / m;
    /**
      * @internal
      */
    EXPORT REAL8 mu  (REAL8 v) := 1.0 / v;
    /**
      * @internal
      */
    EXPORT REAL8 deta(REAL8 m) := - 1.0 / POWER(m, 2);
    /**
      * @internal
      */
    EXPORT REAL8 var (REAL8 m) := POWER(m, 2);
    /**
      * @internal
      */
    EXPORT REAL8 init(REAL8 y, REAL8 w = 1.0) := y;
    /**
      * @internal
      */
    EXPORT REAL8 ll  (REAL8 y, REAL8 m, REAL8 disp) := -1 / disp * (y / m + LN(m) + (disp - 1) * LN(y) + LN(disp) + disp * LN(Core.Math.gamma(1 / disp)));
    /**
      * @internal
      */
    EXPORT STRING mu_LUCI := '1.0 / Raw_point';
    /**
      * @internal
      */
    EXPORT BOOLEAN dispersion := TRUE;
    /**
      * @internal
      */
    EXPORT SET OF UNSIGNED4 cardinality := [2, LargeValue];
    /**
      * @internal
      */
    EXPORT SET OF REAL8 values := [SmallValue, LargeValue];
    /**
      * @internal
      */
    EXPORT BOOLEAN isInteger := FALSE;
  END;
  /**
    * Gaussian Regression is equivalent to Ordinary Least Squares (OLS)
    * regression.  It assumes that the error term is Normally distributed.
    */
  EXPORT Gaussian := MODULE(FamilyInterface)
    /**
      * @internal
      */
    EXPORT REAL8 link(REAL8 m) := m;
    /**
      * @internal
      */
    EXPORT REAL8 mu  (REAL8 v) := v;
    /**
      * @internal
      */
    EXPORT REAL8 deta(REAL8 m) := 1.0;
    /**
      * @internal
      */
    EXPORT REAL8 var (REAL8 m) := 1.0;
    /**
      * @internal
      */
    EXPORT REAL8 init(REAL8 y, REAL8 w = 1.0) := y;
    /**
      * @internal
      */
    EXPORT REAL8 ll  (REAL8 y, REAL8 m, REAL8 disp) := - LN(2 * PI * disp) / 2 - 1/(2 * disp) * POWER(y - m, 2);
    /**
      * @internal
      */
    EXPORT STRING mu_LUCI := 'Raw_point';
    /**
      * @internal
      */
    EXPORT BOOLEAN dispersion := TRUE;
    /**
      * @internal
      */
    EXPORT SET OF UNSIGNED4 cardinality := [2, LargeValue];
    /**
      * @internal
      */
    EXPORT SET OF REAL8 values := [-LargeValue, LargeValue];
    /**
      * @internal
      */
    EXPORT BOOLEAN isInteger := FALSE;
  END;
  /**
    * Inverse Gaussian Regression aka Wald Regression is similar
    * to the Gamma Regression in that it is used to model
    * continuous, positive heteroskedastic data.  It differs
    * from the Gamma Regression assumptions in that it has a
    * wider tail (i.e. more frequent occurence of higher numbers).
    * The variance is assumed to be proportional to the cube of
    * the mean.
    */
  EXPORT InvGauss := MODULE(FamilyInterface)
    /**
      * @internal
      */
    EXPORT REAL8 link(REAL8 m) := 1.0 / POWER(m, 2);
    /**
      * @internal
      */
    EXPORT REAL8 mu  (REAL8 v) := 1.0 / SQRT(v);
    /**
      * @internal
      */
    EXPORT REAL8 deta(REAL8 m) := - 2.0 / POWER(m, 3);
    /**
      * @internal
      */
    EXPORT REAL8 var (REAL8 m) := POWER(m, 3);
    /**
      * @internal
      */
    EXPORT REAL8 init(REAL8 y, REAL8 w = 1.0) := y;
    /**
      * @internal
      */
    EXPORT REAL8 ll  (REAL8 y, REAL8 m, REAL8 disp) := - 1 / 2 * (POWER(y - m, 2) / (y * m * disp) + LN(disp * POWER(y, 3)) + LN(2 * PI));
    /**
      * @internal
      */
    EXPORT STRING mu_LUCI := '1.0 / SQRT(Raw_point)';
    /**
      * @internal
      */
    EXPORT BOOLEAN dispersion := TRUE;
    /**
      * @internal
      */
    EXPORT SET OF UNSIGNED4 cardinality := [2, LargeValue];
    /**
      * @internal
      */
    EXPORT SET OF REAL8 values := [SmallValue, LargeValue];
    /**
      * @internal
      */
    EXPORT BOOLEAN isInteger := FALSE;
  END;
END;