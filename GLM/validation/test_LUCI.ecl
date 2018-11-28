IMPORT $.^ AS GLMmod;
IMPORT GLMmod.Types AS Types;
IMPORT GLMmod.Family;
IMPORT ML_Core;

num_recs := 300;
Work1 := RECORD
  UNSIGNED2 rid;
  STRING wi_str;
  INTEGER8 dep;
  REAL8 v1;
  REAL8 v2;
END;
Work1 gen(UNSIGNED c) := TRANSFORM
  SELF.rid := c;
  SELF.wi_str := 'Grp-'+INTFORMAT((c-1) % 4, 2, 1);
  SELF.dep := IF(c<=0.4*num_recs, 1, 0);
  SELF.v1 := IF(c<=0.4*num_recs, 25, 75) * (0.5 + (RANDOM() %100)/100);
  SELF.v2 := IF(c<=0.4*num_recs, 80, 20) * (0.5 + (RANDOM() %100)/100);
END;
ds := DATASET(num_recs, gen(COUNTER));
// Enumerate the work items
GLMmod.enum_workitems(ds, ds_w_wi, wi_str, wi);
// Create dep and dep_map for response variable
ML_Core.ToField(ds_w_wi, dep, rid, wi,, 'dep');
dep_df := PROJECT(dep, Types.NumericField);
// Create indep and indep_map for explanatory data
ML_Core.ToField(ds_w_wi, indep, rid, wi,, 'v1, v2');
//
model := GLMmod.GLM(indep, dep_df, Family.Binomial).GetModel();
int_model := GLMmod.ExtractBeta_full(model);
ext_model := GLMmod.Named_Model(model, indep_map, dep_map, ds_w_wi_map);
//
luci_rq := DATASET([{'$_mod','$ Model','dep', ALL,'$'}
                   ,{'mod1','Model 1', 'dep', ALL,''}
                   ,{'mod2','Model 2', 'dep', ['Grp-02', 'Grp-00'],'Q'}
                   ], Types.LUCI_Model_Rqst);
luci := GLMmod.LUCI_Model(luci_rq, ext_model, 'wi_str', Family.Binomial);
//
t0 := TABLE(ds_w_wi, {wi_str, wi, dep, c:=COUNT(GROUP),
                 av_v1:=AVE(GROUP,v1), av_v2:=AVE(GROUP,v2),
                 mn_v1:=MIN(GROUP,v1), mn_v2:=MIN(GROUP,v2),
                 mx_v1:=MAX(GROUP,v1), mx_v2:=MAX(GROUP,v2)},
            wi_str, wi, dep, FEW, UNSORTED);
EXPORT test_LUCI := OUTPUT(luci, NAMED('luci_text'));
