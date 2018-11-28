IMPORT $ AS GLMmod;
IMPORT GLMmod.Types;
IMPORT GLMmod.IRLS;
IMPORT GLMmod.Family;
IMPORT ML_Core.Types AS Core_Types;

//aliases
NumericField := Core_Types.NumericField;
Model_Coef := Types.Model_Coef;

/**
 * Calculate the score using the appropriate mean function and the
 * the supplied beta coefficients.
 * @param coef the model beta coefficients.
 * @param independents the observations.
 * @param fam module defining the error distribution and link of the dependents.
 * @return the prediction value.
 */
EXPORT DATASET(NumericField)
      Predict(DATASET(Model_Coef) coef,
            DATASET(NumericField) independents,
            Family.FamilyInterface fam) := FUNCTION

  // Get definition of mean function for current distribution
  REAL8 mu(REAL8 v) := Fam.mu(v);

  // Make a list of records and start the Y value with the constant
  rids := TABLE(independents, {wi, id}, wi, id, MERGE);
  NumericField base(RECORDOF(rids) y, Model_Coef b0) := TRANSFORM
    SELF.value := b0.w;
    SELF.number := b0.dep_nom;
    SELF := y;
  END;
  // constant term
  term0 := JOIN(rids, coef(ind_col=0), LEFT.wi=RIGHT.wi,
                base(LEFT,RIGHT), LOOKUP, MANY);
  // terms 1 through k
  NumericField mult(NumericField x, Model_Coef b) := TRANSFORM
    SELF.wi := x.wi;
    SELF.id := x.id;
    SELF.number := b.dep_nom;
    SELF.value := x.value*b.w;
  END;
  termk := JOIN(independents, coef(ind_col > 0),
                LEFT.wi=RIGHT.wi AND LEFT.number=RIGHT.ind_col,
                mult(LEFT,RIGHT), LOOKUP, MANY);
  //check performance, may have a significant distribution cost
  grp_terms := GROUP(SORT(term0+termk, wi, id, number), wi, id, number);
  NumericField roll_t(NumericField nf, DATASET(NumericField) nfs):=TRANSFORM
    SELF.value := mu(SUM(nfs, value));
    SELF := nf;
  END;
  m := ROLLUP(grp_terms, GROUP, roll_t(LEFT, ROWS(LEFT)));
  RETURN UNGROUP(m);
END;