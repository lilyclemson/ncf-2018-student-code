IMPORT $.^ AS GLMmod;
IMPORT GLMmod.Family;
IMPORT ML_Core.Types AS Core_Types;
IMPORT GLMmod.validation.unit_test_getModel_Rdata as rdata;
NumericField := Core_Types.NumericField;
Layout_Model := Core_types.Layout_Model;

dep_bn := DATASET(
       [{1, 1, 1, 0}, {1, 1, 2, 1},
        {1, 2, 1, 0}, {1, 2, 2, 1},
        {1, 3, 1, 1}, {1, 3, 2, 0},
        {1, 4, 1, 0}, {1, 4, 2, 1},
        {1, 5, 1, 0}, {1, 5, 2, 1},
        {1, 6, 1, 0}, {1, 6, 2, 1},
        {1, 7, 1, 1}, {1, 7, 2, 0},
        {1, 8, 1, 0}, {1, 8, 2, 1},
        {1, 9, 1, 1}, {1, 9, 2, 0},
        {2, 1, 1, 0}, {2, 1, 2, 0}, {2, 1, 3, 0},
        {2, 2, 1, 0}, {2, 2, 2, 0}, {2, 2, 3, 0},
        {2, 3, 1, 1}, {2, 3, 2, 1}, {2, 3, 3, 0},
        {2, 4, 1, 0}, {2, 4, 2, 0}, {2, 4, 3, 1},
        {2, 5, 1, 0}, {2, 5, 2, 0}, {2, 5, 3, 0},
        {2, 6, 1, 0}, {2, 6, 2, 1}, {2, 6, 3, 1},
        {2, 7, 1, 1}, {2, 7, 2, 0}, {2, 7, 3, 0},
        {2, 8, 1, 0}, {2, 8, 2, 1}, {2, 8, 3, 1},
        {2, 9, 1, 1}, {2, 9, 2, 0}, {2, 9, 3, 0},
        {3, 1, 1, 0},
        {3, 2, 1, 0},
        {3, 3, 1, 1},
        {3, 4, 1, 0},
        {3, 5, 1, 0},
        {3, 6, 1, 0},
        {3, 7, 1, 1},
        {3, 8, 1, 0},
        {3, 9, 1, 1}], NumericField);
dep_cnt := DATASET(
       [{1, 1, 1, 1}, {1, 1, 2, 6},
        {1, 2, 1, 1}, {1, 2, 2, 6},
        {1, 3, 1, 2}, {1, 3, 2, 5},
        {1, 4, 1, 2}, {1, 4, 2, 5},
        {1, 5, 1, 3}, {1, 5, 2, 4},
        {1, 6, 1, 3}, {1, 6, 2, 4},
        {1, 7, 1, 4}, {1, 7, 2, 3},
        {1, 8, 1, 5}, {1, 8, 2, 2},
        {1, 9, 1, 6}, {1, 9, 2, 1},
        {2, 1, 1, 1}, {2, 1, 2, 6}, {2, 1, 3, 1},
        {2, 2, 1, 1}, {2, 2, 2, 5}, {2, 2, 3, 1},
        {2, 3, 1, 2}, {2, 3, 2, 4}, {2, 3, 3, 2},
        {2, 4, 1, 2}, {2, 4, 2, 3}, {2, 4, 3, 2},
        {2, 5, 1, 3}, {2, 5, 2, 3}, {2, 5, 3, 3},
        {2, 6, 1, 3}, {2, 6, 2, 2}, {2, 6, 3, 3},
        {2, 7, 1, 4}, {2, 7, 2, 2}, {2, 7, 3, 4},
        {2, 8, 1, 5}, {2, 8, 2, 1}, {2, 8, 3, 5},
        {2, 9, 1, 6}, {2, 9, 2, 1}, {2, 9, 3, 6},
        {3, 1, 1, 1},
        {3, 2, 1, 1},
        {3, 3, 1, 1},
        {3, 4, 1, 1},
        {3, 5, 1, 1},
        {3, 6, 1, 1},
        {3, 7, 1, 2},
        {3, 8, 1, 2},
        {3, 9, 1, 2}], NumericField);
wgt := DATASET(
       [{1, 1, 1, 1},
        {1, 2, 1, 2},
        {1, 3, 1, 3},
        {1, 4, 1, 4},
        {1, 5, 1, 5},
        {1, 6, 1, 4},
        {1, 7, 1, 3},
        {1, 8, 1, 2},
        {1, 9, 1, 1},
        {2, 1, 1, 1},
        {2, 2, 1, 2},
        {2, 3, 1, 3},
        {2, 4, 1, 4},
        {2, 5, 1, 5},
        {2, 6, 1, 4},
        {2, 7, 1, 3},
        {2, 8, 1, 2},
        {2, 9, 1, 1},
        {3, 1, 1, 1},
        {3, 2, 1, 2},
        {3, 3, 1, 3},
        {3, 4, 1, 4},
        {3, 5, 1, 5},
        {3, 6, 1, 4},
        {3, 7, 1, 3},
        {3, 8, 1, 2},
        {3, 9, 1, 1}], NumericField);
ind := DATASET(
    [{1, 1, 1, .6}, {1, 1, 2, .7}, {1, 1, 3, .8},
     {1, 2, 1, .8}, {1, 2, 2, .7}, {1, 2, 3, .7},
     {1, 3, 1, .7}, {1, 3, 2, .8}, {1, 3, 3, .6},
     {1, 4, 1, .9}, {1, 4, 2, .7}, {1, 4, 3, .9},
     {1, 5, 1, .8}, {1, 5, 2, .9}, {1, 5, 3, .6},
     {1, 6, 1, .8}, {1, 6, 2, .5}, {1, 6, 3, .8},
     {1, 7, 1, .2}, {1, 7, 2,  0}, {1, 7, 3, .3},
     {1, 8, 1, .3}, {1, 8, 2, .4}, {1, 8, 3, .4},
     {1, 9, 1, .4}, {1, 9, 2, .7}, {1, 9, 3,  0},
     {2, 1, 1, .9}, {2, 1, 2, .7}, {2, 1, 3, .8},
     {2, 2, 1, .8}, {2, 2, 2, .7}, {2, 2, 3, .7},
     {2, 3, 1, .7}, {2, 3, 2, .8}, {2, 3, 3, .9},
     {2, 4, 1, .6}, {2, 4, 2, .5}, {2, 4, 3, .6},
     {2, 5, 1, .6}, {2, 5, 2, .6}, {2, 5, 3, .6},
     {2, 6, 1, .6}, {2, 6, 2, .5}, {2, 6, 3, .5},
     {2, 7, 1, .2}, {2, 7, 2, .1}, {2, 7, 3, .3},
     {2, 8, 1, .3}, {2, 8, 2, .4}, {2, 8, 3, .4},
     {2, 9, 1, .4}, {2, 9, 2, .7}, {2, 9, 3, .3},
     {3, 1, 1, .6}, {3, 1, 2, .7}, {3, 1, 3, .8},
     {3, 2, 1, .8}, {3, 2, 2, .7}, {3, 2, 3, .7},
     {3, 3, 1, .7}, {3, 3, 2, .8}, {3, 3, 3, .6},
     {3, 4, 1, .9}, {3, 4, 2, .7}, {3, 4, 3, .9},
     {3, 5, 1, .8}, {3, 5, 2, .9}, {3, 5, 3, .6},
     {3, 6, 1, .8}, {3, 6, 2, .5}, {3, 6, 3, .8},
     {3, 7, 1, .2}, {3, 7, 2, .1}, {3, 7, 3, .3},
     {3, 8, 1, .3}, {3, 8, 2, .4}, {3, 8, 3, .4},
     {3, 9, 1, .4}, {3, 9, 2, .7}, {3, 9, 3, .3}], NumericField);
dep_bn_nz :=  dep_bn(value <> 0);
dep_cnt_nz := dep_cnt(value <> 0);
ind_nz := ind(value <> 0);
wgt_nz := wgt(value <> 0);

maxit := 100;
eps := 0.0000001;
rdg := GLMmod.Constants.default_ridge;

mod_g_bn := GLMmod.IRLS.getModel_global(ind_nz, dep_bn_nz, Family.Quasibinomial, maxit, eps, rdg, wgt_nz);
mod_l_bn := GLMmod.IRLS.getModel_local(ind_nz, dep_bn_nz, Family.Quasibinomial, maxit, eps, rdg, wgt_nz);
mod_r_bn := rdata.Quasibinomial;

mod_g_ps := GLMmod.IRLS.getModel_global(ind_nz, dep_cnt_nz, Family.Quasipoisson, maxit, eps, rdg, wgt_nz);
mod_l_ps := GLMmod.IRLS.getModel_local(ind_nz, dep_cnt_nz, Family.Quasipoisson, maxit, eps, rdg, wgt_nz);
mod_r_ps := rdata.Quasipoisson;

mod_g_ga := GLMmod.IRLS.getModel_global(ind_nz, dep_cnt_nz, Family.Gamma, maxit, eps, rdg, wgt_nz);
mod_l_ga := GLMmod.IRLS.getModel_local(ind_nz, dep_cnt_nz, Family.Gamma, maxit, eps, rdg, wgt_nz);
mod_r_ga := rdata.Gamma;

mod_g_gs := GLMmod.IRLS.getModel_global(ind_nz, dep_cnt_nz, Family.Gaussian, maxit, eps, rdg, wgt_nz);
mod_l_gs := GLMmod.IRLS.getModel_local(ind_nz, dep_cnt_nz, Family.Gaussian, maxit, eps, rdg, wgt_nz);
mod_r_gs := rdata.Gaussian;

mod_g_ig := GLMmod.IRLS.getModel_global(ind_nz, dep_cnt_nz, Family.InvGauss, maxit, eps, rdg, wgt_nz);
mod_l_ig := GLMmod.IRLS.getModel_local(ind_nz, dep_cnt_nz, Family.InvGauss, maxit, eps, rdg, wgt_nz);
mod_r_ig := rdata.InvGauss;

cmpr := RECORD
  UNSIGNED wi;
  UNSIGNED id;
  UNSIGNED number;
  REAL8 v_left;
  REAL8 v_right;
  VARSTRING family;
END;
DATASET(cmpr) getDiff(DATASET(Layout_Model) m1, DATASET(Layout_Model) m2, INTEGER ignore, VARSTRING f) := FUNCTION
  rslt := JOIN(m1, m2,
              LEFT.wi=RIGHT.wi AND LEFT.id=RIGHT.id AND LEFT.number=RIGHT.number AND LEFT.id <> ignore
              AND ABS((LEFT.value-RIGHT.value) / LEFT.value) > 0.05 AND ABS(LEFT.value-RIGHT.value) > 0.0000001,
              TRANSFORM(cmpr, SELF.v_right:=RIGHT.value, SELF.v_left:=LEFT.value, SELF.family:=f, SELF:=LEFT));
  RETURN(rslt);
END;
DATASET(cmpr) getAll(DATASET(Layout_Model) m1, DATASET(Layout_Model) m2, VARSTRING f) := FUNCTION
  rslt := JOIN(m1, m2,
              LEFT.wi=RIGHT.wi AND LEFT.id=RIGHT.id AND LEFT.number=RIGHT.number,
              TRANSFORM(cmpr, SELF.v_right:=RIGHT.value, SELF.v_left:=LEFT.value, SELF.family:=f, SELF:=LEFT));
  RETURN(rslt);
END;

diff_h := getDiff(mod_g_bn, mod_l_bn, -1, 'bn')
        + getDiff(mod_g_ps, mod_l_ps, -1, 'ps')
        + getDiff(mod_g_ga, mod_l_ga, -1, 'ga')
        + getDiff(mod_g_gs, mod_l_gs, -1, 'gs')
        + getDiff(mod_g_ig, mod_l_ig, -1, 'ig');
diff_r := getDiff(mod_l_bn, mod_r_bn,  2, 'bn')
        + getDiff(mod_l_ps, mod_r_ps,  2, 'ps')
        + getDiff(mod_l_ga, mod_r_ga,  2, 'ga')
        + getDiff(mod_l_gs, mod_r_gs,  2, 'gs')
        + getDiff(mod_l_ig, mod_r_ig,  2, 'ig');
all_h  := getAll(mod_g_bn, mod_l_bn, 'bn')
        + getAll(mod_g_ps, mod_r_ps, 'ps')
        + getAll(mod_g_ga, mod_r_ga, 'ga')
        + getAll(mod_g_gs, mod_r_gs, 'gs')
        + getAll(mod_g_ig, mod_r_ig, 'ig');
all_r  := getAll(mod_l_bn, mod_r_bn, 'bn')
        + getAll(mod_l_ps, mod_r_ps, 'ps')
        + getAll(mod_l_ga, mod_r_ga, 'ga')
        + getAll(mod_l_gs, mod_r_gs, 'gs')
        + getAll(mod_l_ig, mod_r_ig, 'ig');

diff_rpt_h := OUTPUT(SORT(diff_h, wi, id, number), NAMED('Differences_HPCC'));
diff_rpt_r := OUTPUT(SORT(diff_r, wi, id, number), NAMED('Differences_R'));
all_rpt_h := OUTPUT(SORT(all_h, wi, id, number), NAMED('All_H'));
all_rpt_r := OUTPUT(SORT(all_r, wi, id, number), NAMED('All_R'));
EXPORT unit_test_getModel := SEQUENTIAL(diff_rpt_h, diff_rpt_r, all_rpt_h, all_rpt_r);
