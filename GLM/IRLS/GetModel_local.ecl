IMPORT ML_Core;
IMPORT ML_Core.Types AS Core_Types;
IMPORT $.^ AS GLMmod;
IMPORT GLMmod.Constants;
IMPORT GLMmod.Types;
IMPORT GLMmod.Family;
IMPORT $ AS IRLS;
IMPORT Std;
IMPORT Std.BLAS AS BLAS;
//Aliases for convenience
NumericField := Core_Types.NumericField;
Layout_Model := Core_Types.Layout_Model;
t_work_item  := Core_Types.t_work_item;
t_RecordID   := Core_Types.t_RecordID;
t_FieldNumber:= Core_Types.t_FieldNumber;
t_FieldReal  := Core_Types.t_FieldReal;
value_t      := BLAS.Types.value_t;
dimension_t  := BLAS.Types.dimension_t;
matrix_t     := BLAS.Types.matrix_t;
triangle     := BLAS.Types.triangle;
diagonal     := BLAS.Types.diagonal;
side         := BLAS.Types.side;
gemm         := BLAS.dgemm;
scal         := BLAS.dscal;
trsm         := BLAS.dtrsm;
getf2        := BLAS.dgetf2;
axpy         := BLAS.daxpy;
asum         := BLAS.dasum;
Apply2Cells  := BLAS.Apply2Cells;
make_diag    := BLAS.make_diag;
dimm         := GLMmod.dimm;
make_vector  := BLAS.make_vector;
extract_diag := BLAS.extract_diag;
t_part       := UNSIGNED2;
Apply2CellsBinary  := GLMmod.Apply2CellsBinary;

Part := RECORD
  t_work_item wi;
  dimension_t part_rows;
  dimension_t part_cols;
  matrix_t mat;
  REAL8 max_delta := 0.0;
  UNSIGNED2 iterations := 0;
  REAL8 mse := 0.0;
  REAL8 dispersion := 1.0;
END;
Ext_NumFld := RECORD(NumericField)
  dimension_t part_rows;
  dimension_t part_cols;
  UNSIGNED4 inserts;
  BOOLEAN dropMe;
END;
wi_info := RECORD
  t_work_item orig_wi;
  t_work_item wi;
  UNSIGNED4 col;
  UNSIGNED4 dep_cols;
  UNSIGNED4 dep_rows;
  UNSIGNED4 ind_cols;
  UNSIGNED4 ind_rows;
  UNSIGNED4 orig_col;
END;

/**
 * Internal function to determine values for the model co-efficients
 * and selected stats from building the model.
 * @param independents the independent values
 * @param dependents the dependent values.
 * @param fam a module defining the error distribution and link of the response
 * @param max_iter maximum number of iterations to try
 * @param epsilon the minimum change in the Beta value estimate to continue
 * @param ridge a value to populate a diagonal matrix that is added to
 * a matrix help assure that the matrix is invertible.
 * @param weights A set of observation weights (one per dependent value).
 * @return coefficient matrix plus model building stats
 */
EXPORT DATASET(Layout_Model) GetModel_local(
  DATASET(NumericField)   independents,
   DATASET(NumericField)  dependents,
   Family.FamilyInterface fam,
   UNSIGNED2              max_iter = 200,
   REAL8                  epsilon  = Constants.default_epsilon,
   REAL8                  ridge    = Constants.default_ridge,
   DATASET(NumericField)  weights  = DATASET([], NumericField)) := FUNCTION

  // Get definition of mean and variance functions
  REAL8 link(REAL8 v) := fam.link(v);
  REAL8 mu(REAL8 v) := fam.mu(v);
  REAL8 var(REAL8 v) := fam.var(v);
  REAL8 deta(REAL8 v) := fam.deta(v);
  REAL8 init(REAL8 v, REAL8 w = 1.0) := fam.init(v, w);
  value_t init_uni(value_t v,
                 dimension_t r,
                 dimension_t c) := init(v);
  value_t init_bin(value_t v,
                 value_t w,
                 dimension_t r,
                 dimension_t c) := init(v, w);
  value_t link_i(value_t v,
                 dimension_t r,
                 dimension_t c) := link(v);
  value_t u_i(value_t v,
              dimension_t r,
              dimension_t c) := mu(v);
  value_t w_i(value_t v,
              dimension_t r,
              dimension_t c) := 1.0 / var(v) / POWER(deta(v), 2);

  // Define utility functions
  value_t abs_v(value_t v,
                dimension_t r,
                dimension_t c) := ABS(v);
  value_t sq_cells(value_t v,
                   dimension_t r,
                   dimension_t c) := POWER(v, 2);
  value_t resi_uy(value_t u,
                  value_t y_u,
                  dimension_t r,
                  dimension_t c) := y_u * deta(u);
  value_t disp_uy(value_t u,
                  value_t res,
                  dimension_t r,
                  dimension_t c) := w_i(u, r, c) * POWER(res, 2);
  value_t mult_ab(value_t a,
                  value_t b,
                  dimension_t r,
                  dimension_t c) := a * b;

  // check for user-defined weights
  doWeights := EXISTS(weights);

  // check that id and number are 1 and up
  ind_screen := ASSERT(independents, id>0 AND number>0,
                       'Column left of 1 in Ind or a row 0', FAIL);
  dep_screen := ASSERT(dependents, id>0 and number>0,
                       'Column left of 1 in Dep or a row 0', FAIL);
  wgt_screen := ASSERT(weights, id>0 and number>0,
                       'Column left of 1 in Wgt or a row 0', FAIL);
  // work item re-map for multiple column dependents and replicate
  dcols := TABLE(dep_screen, {wi, number, max_id:=MAX(GROUP, id), col:=1},
                 wi, number, FEW, UNSORTED);
  dep_map := PROJECT(GROUP(SORT(dcols, wi, number), wi),
                     TRANSFORM(RECORDOF(dcols),
                               SELF.col := COUNTER,
                               SELF:=LEFT));
  dep_cols := TABLE(dep_map,
                   {wi,r:=MAX(GROUP,max_id), c:=MAX(GROUP,col)},
                   wi, FEW, UNSORTED);
  ind_cols := TABLE(ind_screen,
                    {wi, r:=MAX(GROUP,id), c:=MAX(GROUP,number)},
                    wi, FEW, UNSORTED);
  wi_info cmb(RECORDOF(ind_cols) ind, RECORDOF(dep_cols) dep):=TRANSFORM
    SELF.orig_wi := ind.wi;
    SELF.wi := ind.wi;
    SELF.col := 1;    // place holder
    SELF.dep_cols := dep.c;
    SELF.dep_rows := MAX(dep.r, ind.r);
    SELF.ind_cols := ind.c + 1; // to be inserted
    SELF.ind_rows := MAX(ind.r, dep.r);
    SELF.orig_col := 0;   // fill in after NORMALIZE
  END;
  cmb_cols := JOIN(ind_cols, dep_cols, LEFT.wi=RIGHT.wi,
                   cmb(LEFT, RIGHT), FEW);
  wi_info mark_wi(wi_info prev, wi_info curr) := TRANSFORM
    SELF.wi := IF(prev.wi=0, 1, prev.wi + prev.dep_cols);
    SELF := curr;
  END;
  marked_wi := ITERATE(SORT(cmb_cols,wi), mark_wi(LEFT, RIGHT));
  exp_wi := NORMALIZE(marked_wi, LEFT.dep_cols,
                      TRANSFORM(wi_info,
                                SELF.wi := LEFT.wi - 1 + COUNTER,
                                SELF.col := COUNTER,
                                SELF := LEFT));
  map_wi := JOIN(exp_wi, dep_map,
                 LEFT.orig_wi=RIGHT.wi AND LEFT.col=RIGHT.col,
                 TRANSFORM(wi_info,
                           SELF.orig_col:=RIGHT.number,
                           SELF:=LEFT),
                 LOOKUP, FEW);
  dist_wi := SORT(DISTRIBUTE(map_wi, wi), wi, LOCAL);
  // generate end records for inserts of missing values
  Ext_NumFld gen_end(wi_info wif, UNSIGNED c, BOOLEAN isDep):=TRANSFORM
    SELF.wi := wif.wi;
    SELF.id := MAX(wif.dep_rows, wif.ind_rows);
    SELF.value := 0;
    SELF.number := c;
    SELF.dropMe := TRUE;
    SELF.inserts := 0;
    SELF.part_rows := MAX(wif.dep_rows, wif.ind_rows);
    SELF.part_cols := IF(isDep, 1, wif.ind_cols);
  END;
  end_dep := PROJECT(dist_wi, gen_end(LEFT, 1, TRUE));
  end_ind := NORMALIZE(dist_wi, LEFT.ind_cols-1, gen_end(LEFT, COUNTER, FALSE));
  // transforms to expand records, find inserts, insert missing zeros, and roll to Partition
  Ext_NumFld exp_nf(NumericField nf, wi_info wif, BOOLEAN isDep):=TRANSFORM
    SELF.wi := wif.wi;
    SELF.part_rows := MAX(wif.dep_rows, wif.ind_rows);
    SELF.part_cols := IF(isDep, 1, wif.ind_cols);
    SELF.number := IF(isDep, 1, nf.number);
    SELF.inserts := 0;
    SELF.dropMe := FALSE;
    SELF := nf;
  END;
  Ext_NumFld inserts(Ext_NumFld prev, Ext_NumFld curr, BOOLEAN isDep):=TRANSFORM
    SELF.inserts := IF(prev.id=curr.id, 0, curr.id - prev.id - 1);
    SELF.dropMe := prev.id=curr.id;
    SELF := curr;
  END;
  Ext_NumFld insert_zeros(Ext_NumFld base, UNSIGNED c):=TRANSFORM
    insertZero := c <= base.inserts;
    SELF.value := IF(insertZero, 0.0, base.value);
    SELF.id := base.id - base.inserts + c - 1;
    SELF := base;
  END;
  Part roll_nf(Ext_NumFld nf, DATASET(Ext_NumFld) rws, BOOLEAN isDep):=TRANSFORM
    ones := make_vector(nf.part_rows, 1.0);
    SELF.mat := IF(isDep, SET(rws, value), ones+SET(rws, value));
    SELF := nf;
  END;
  // replicate independents for multiple dependents
  repl_ind := JOIN(ind_screen, map_wi, LEFT.wi=RIGHT.orig_wi,
                    exp_nf(LEFT, RIGHT, FALSE), LOOKUP, MANY, FEW);
  dist_ind := DISTRIBUTE(repl_ind, wi);
  srtd_ind := SORT(dist_ind+end_ind, wi, number, id, dropMe, LOCAL);
  grpd_ind := GROUP(srtd_ind, wi, number, LOCAL);
  mrkd_ind := ITERATE(grpd_ind, inserts(LEFT, RIGHT, FALSE));
  full_ind := NORMALIZE(mrkd_ind(NOT dropMe), LEFT.inserts+1,
                        insert_zeros(LEFT, COUNTER));
  rr_ind := GROUP(full_ind, wi, LOCAL);
  ind_mat := ROLLUP(rr_ind, GROUP, roll_nf(LEFT, ROWS(LEFT), FALSE));
  // replicate weights for multiple dependents
  repl_wgt := JOIN(wgt_screen, map_wi, LEFT.wi=RIGHT.orig_wi,
                    exp_nf(LEFT, RIGHT, TRUE), LOOKUP, MANY, FEW);
  dist_wgt := DISTRIBUTE(repl_wgt, wi);
  srtd_wgt := SORT(dist_wgt+end_dep, wi, number, id, dropMe, LOCAL);
  grpd_wgt := GROUP(srtd_wgt, wi, LOCAL);
  mrkd_wgt := ITERATE(grpd_wgt, inserts(LEFT, RIGHT, TRUE));
  full_wgt := NORMALIZE(mrkd_wgt(NOT dropMe), LEFT.inserts+1,
                        insert_zeros(LEFT, COUNTER));
  rr_wgt := GROUP(full_wgt, wi, LOCAL);
  wgt_mat := ROLLUP(rr_wgt, GROUP, roll_nf(LEFT, ROWS(LEFT), TRUE));
  // re-map Y and weights matrices to multiple vectors and fluff with zeros
  expd_dep := JOIN(dep_screen, map_wi,
                   LEFT.wi=RIGHT.orig_wi AND LEFT.number=RIGHT.orig_col,
                   exp_nf(LEFT, RIGHT, TRUE), LOOKUP, FEW);
  dist_dep := DISTRIBUTE(expd_dep, wi);
  srtd_dep := SORT(dist_dep+end_dep, wi, number, id, dropMe, LOCAL);
  grpd_dep := GROUP(srtd_dep, wi, LOCAL);
  mrkd_dep := ITERATE(grpd_dep, inserts(LEFT, RIGHT, TRUE));
  full_dep := NORMALIZE(mrkd_dep(NOT dropMe), LEFT.inserts+1,
                        insert_zeros(LEFT, COUNTER));
  rr_dep := GROUP(full_dep, wi, LOCAL);
  dep_mat := ROLLUP(rr_dep, GROUP, roll_nf(LEFT, ROWS(LEFT), TRUE));
  // Define Ridge diagonal matrix
  Part makeRidge(wi_info wif) := TRANSFORM
    SELF.mat := make_diag(wif.ind_cols, ridge);
    SELF.wi := wif.wi;
    SELF.part_rows := wif.ind_cols;
    SELF.part_cols := wif.ind_cols;
  END;
  R_mat := PROJECT(dist_wi, makeRidge(LEFT));
  // Initial beta estimate
  Part init_beta(Part x, Part y) := TRANSFORM
    SELF.mat := make_vector(x.part_cols, 0.0); // overwritten with family's init function in iter 1
    SELF.iterations := 0;
    SELF.max_delta := 2*epsilon;
    SELF.part_rows := x.part_cols;
    SELF.part_cols := 1;
    SELF.wi := x.wi;
  END;
  init_B := JOIN(ind_mat, dep_mat, LEFT.wi=RIGHT.wi,
                 init_beta(LEFT, RIGHT), LOCAL);
  // iterative least squares to converge Beta
  DATASET(Part) iter0(DATASET(Part) Beta, UNSIGNED c):=FUNCTION
    list := [Beta, ind_mat, dep_mat, R_mat, wgt_mat];
    Part stp0(DATASET(Part) d) := TRANSFORM
      obs := d[2].part_rows;
      dims := d[2].part_cols;
      B := d[1].mat[1..dims];
      X := d[2].mat;
      Y := d[3].mat;
      R := d[4].mat;
      wf:= d[5].mat;
      U0 := IF(~doWeights,
        Apply2Cells(obs, 1, Y, init_uni),
        Apply2CellsBinary(obs, 1, Y, wf, init_bin)
      );
      XB0 := Apply2Cells(obs, 1, U0, link_i);
      XB := IF(c <> 1,
        gemm(FALSE, FALSE, obs, 1, dims, 1.0, X, B),
        XB0
      );
      U := IF(c <> 1, Apply2Cells(obs, 1, XB, u_i), U0);
      W_uw := Apply2Cells(obs, 1, U, w_i);
      W := IF(
        doWeights,
        Apply2CellsBinary(obs, 1, W_uw, wf, mult_ab),
        W_uw
      );
      XtW := dimm(TRUE, FALSE, FALSE, TRUE, dims, obs, obs, 1.0, X, W);
      XtWX_R := gemm(FALSE, FALSE, dims, dims, obs, 1.0, XtW, X, 1.0, R);
      Y_U := axpy(obs, -1.0, U, 1, Y, 1);
      res := Apply2CellsBinary(obs, 1, U, Y_U, resi_uy);
      XB_res := axpy(obs, 1.0, XB, 1, res, 1);
      WXB_res := dimm(FALSE, FALSE, TRUE, FALSE, obs, 1, obs, 1.0, W, XB_res);
      XtWXB_res := gemm(TRUE, FALSE, dims, 1, obs, 1.0, X, WXB_res);
      LU_XtWX_R := getf2(dims, dims, XtWX_R);
      identity := make_diag(dims);
      inner := trsm(Side.Ax, Triangle.Lower, FALSE, Diagonal.UnitTri,
                    dims, dims, dims, 1.0, LU_XtWX_R, identity);
      inv_m := trsm(Side.Ax, Triangle.Upper, FALSE, Diagonal.NotUnitTri,
                    dims, dims, dims, 1.0, LU_XtWX_R, inner);
      new_B := gemm(FALSE, FALSE, dims, 1, dims, 1.0, inv_m, XtWXB_res);
      SE := extract_diag(dims, dims, inv_M);
      XB1:= gemm(FALSE, FALSE, obs, 1, dims, 1.0, X, new_B);
      U1 := Apply2Cells(obs, 1, XB1, u_i);
      Y_U1 := axpy(obs, -1, U1, 1, Y, 1);
      res1 := Apply2CellsBinary(obs, 1, U1, Y_U1, resi_uy);
      sqY_U1 := Apply2Cells(obs, 1, Y_U1, sq_cells);
      mse := asum(obs, sqY_U1, 1) / obs;
      B_new_B := axpy(dims, -1.0, new_B, 1, B, 1);
      delta_B := Apply2Cells(dims, 1, B_new_B, abs_v);
						disp_unif := Apply2CellsBinary(obs, 1, U1, res1, disp_uy);
						disp_weig := Apply2CellsBinary(obs, 1, disp_unif, wf, mult_ab);
      dispersion := MAP(
        ~fam.dispersion => 1.0,
        ~doWeights => asum(obs, disp_unif, 1) / (obs - dims),
        asum(obs, disp_weig, 1) / (obs - dims)
      );
      SE_adj := IF(fam.dispersion,
        scal(dims, dispersion, SE, 1),
        SE);
      SELF.mat := new_B + SE_adj;
      SELF.iterations := c;
      SELF.max_delta := MAX(delta_B);
      SELF.part_cols := 2;
      SELF.mse := mse;
      SELF.dispersion := dispersion;
      SELF := d[1];
    END;
    rslt := JOIN(list, LEFT.wi=RIGHT.wi, stp0(ROWS(LEFT)),
                 SORTED(wi), LOCAL);
    RETURN rslt;
  END;
  calc_B := LOOP(init_B,
                 epsilon<LEFT.max_delta AND max_iter>LEFT.iterations,
                 iter0(ROWS(LEFT), COUNTER));
  // Capture model statistics, multiple responses are still multiple wi
  // Betas and SE for betas, Iterations, last delta, mse, dispersion
  NumericField extBetas(Part p, UNSIGNED subscript) := TRANSFORM
    beta := p.mat[subscript];
    se := SQRT(p.mat[subscript]);
    SELF.wi := p.wi;
    SELF.id := Constants.id_betas + subscript - 1;
    SELF.number := 1;
    SELF.value := IF(subscript>p.part_rows, se, beta);
  END;
  b_se := NORMALIZE(calc_B, LEFT.part_rows*2, extBetas(LEFT,COUNTER));
  NumericField extStats(Part p, UNSIGNED fld) := TRANSFORM
    SELF.wi := p.wi;
    SELF.id := CHOOSE(fld, Constants.id_iters, Constants.id_delta,
                      Constants.id_mse, Constants.id_dispersion);
    SELF.number := 1;
    SELF.value := CHOOSE(fld, p.iterations, p.max_delta, p.mse,
                          p.dispersion);
  END;
  stats := NORMALIZE(calc_B, 4, extStats(LEFT, COUNTER));
  // return multiple wi to single wi and multiple columns
  Layout_Model wi_reset(NumericField nf, wi_info w) := TRANSFORM
    SELF.wi := w.orig_wi;
    SELF.number := w.orig_col;
    SELF := nf;
  END;
  var_data := JOIN(b_se+stats, dist_wi, LEFT.wi=RIGHT.wi,
                   wi_reset(LEFT, RIGHT), LOOKUP, FEW) : ONWARNING(1005,ignore);
  // get base data for the training session
  Layout_Model extBase(wi_info w, UNSIGNED f) := TRANSFORM
    SELF.wi := w.orig_wi;
    SELF.id := Constants.id_base;
    SELF.number := CHOOSE(f, Constants.base_builder,
                             Constants.base_max_iter,
                             Constants.base_epsilon,
                             Constants.base_ind_vars,
                             Constants.base_dep_vars,
                             Constants.base_obs);
    SELF.value := CHOOSE(f, Constants.builder_irls_local,
                            max_iter,
                            epsilon,
                            w.ind_cols-1,
                            w.dep_cols,
                            MAX(w.dep_rows, w.ind_rows));
  END;
  base_data := NORMALIZE(cmb_cols, 6, extBase(LEFT, COUNTER));
  RETURN base_data + var_data;
END;
