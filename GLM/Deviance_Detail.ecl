IMPORT ML_Core;
IMPORT ML_Core.Types AS Core_Types;
IMPORT $ AS GLMmod;
IMPORT GLMmod.Types AS Types;
IMPORT GLMmod.IRLS;
IMPORT GLMmod.Family;
IMPORT GLMmod.Constants;

// aliases
AnyField     := Core_Types.AnyField;
NumericField := Core_Types.NumericField;
DiscreteField:= Core_Types.DiscreteField;
Obs_Deviance := Types.Observation_Deviance;
Layout_Model := Core_Types.Layout_Model;
t_work_item  := Core_Types.t_work_item;
t_RecordID   := Core_Types.t_RecordID;
t_FieldNumber:= Core_Types.t_FieldNumber;
t_FieldReal  := Core_Types.t_FieldReal;
t_Discrete   := Core_Types.t_Discrete;
/**
  * Deviance detail report. <p>Provides deviance information 
  * for each observation.
  *
  * <p>Analysis of Deviance is analogous to the Analysis of Variance (ANOVA) used in
  * least-squares modeling, but adapted to the general linear model (GLM).
  *
  * @param dependents original dependent records for the model.
  * @param predicts the predicted values of the response variable.
  * @param model the fitted model object as returned from GetModel.
  * @param fam a module defining the error distribution and link of the dependents
  * @return the deviance information by observation and the log likelihood
  * of the predicted result in Observation_Deviance format.
  * @see Types.Observation_Deviance
  */
EXPORT DATASET(Types.Observation_Deviance)
       Deviance_Detail(DATASET(NumericField) dependents,
                 DATASET(NumericField) predicts,
                 DATASET(Layout_Model) model,
                 Family.FamilyInterface fam) := FUNCTION

  // Get definition of LL function for current distribution
  REAL8 ll(REAL8 y, REAL8 m, REAL8 disp) := fam.ll(y, m, disp);

  // Get dispersion parameter for each model
  disp_mdl := model(id = Constants.id_dispersion);

  // Join dispersion onto predictions
  NF_withDisp := RECORD(NumericField)
    REAL8 disp;
  END;
  NF_withDisp joinDispersion(NumericField p, Layout_Model d) := TRANSFORM
    SELF.disp := d.value;
    SELF := p;
  END;
  predsWithDisp := JOIN(predicts, disp_mdl, LEFT.wi = RIGHT.wi, joinDispersion(LEFT, RIGHT));

  // match up prediction to actual
  Obs_Dev_withDisp := RECORD(Obs_Deviance)
    REAL8 disp;
  END;
  Obs_Dev_withDisp pred_v_act(NumericField act, NF_withDisp prd):=TRANSFORM
    sgn := IF(act.value < prd.value, -1, 1);
    disp := prd.disp;
    SELF.model := prd.number;
    SELF.actual := act.value;
    SELF.predicted := prd.value;
    SELF.mod_ll := ll(act.value, prd.value, disp);
    SELF.mod_dev_component := 2 * (ll(act.value, act.value, disp) - SELF.mod_ll);
    SELF.mod_dev_residual := SQRT(SELF.mod_dev_component) * sgn;
    SELF.disp := disp;
    SELF := prd;
    SELF := [];
  END;
  avp := JOIN(dependents, predsWithDisp,
             LEFT.wi=RIGHT.wi AND LEFT.id=RIGHT.id AND LEFT.number=RIGHT.number,
             pred_v_act(LEFT, RIGHT));
  null_mu := TABLE(dependents, {wi, number, REAL8 mu:=AVE(GROUP, value)},
                   wi, number, FEW, UNSORTED);
  Obs_Deviance null_mod(Obs_Dev_withDisp od, RECORDOF(null_mu) m):=TRANSFORM
    sgn := IF(od.actual < m.mu, -1, 1);
    disp := od.disp;
    SELF.nil_ll := ll(od.actual, m.mu, disp);
    SELF.nil_dev_component := 2 * (ll(od.actual, od.actual, disp) - SELF.nil_ll);
    SELF.nil_dev_residual := SQRT(SELF.nil_dev_component) * sgn;
    SELF := od;
  END;
  rslt := JOIN(avp, null_mu,
             LEFT.wi=RIGHT.wi AND LEFT.model=RIGHT.number,
             null_mod(LEFT, RIGHT), LOOKUP);
  RETURN rslt;
END;