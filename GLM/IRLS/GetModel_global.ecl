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
trsm         := BLAS.dtrsm;
getf2        := BLAS.dgetf2;
axpy         := BLAS.daxpy;
asum         := BLAS.dasum;
Apply2Cells  := BLAS.Apply2Cells;
make_diag    := BLAS.make_diag;
make_vector  := BLAS.make_vector;
extract_diag := BLAS.extract_diag;
dimm         := GLMmod.dimm;
Apply2CellsBinary  := GLMmod.Apply2CellsBinary;

Part := RECORD
  t_work_item wi;
  dimension_t part_rows;
  dimension_t part_cols;
  UNSIGNED4 obs;
  UNSIGNED4 dims;
  UNSIGNED4 this_addr;
  UNSIGNED4 frst_addr;
  UNSIGNED4 parts;
  matrix_t mat;
END;
Ext_Part := RECORD(Part)
  REAL8 max_delta;
  REAL8 part_sse;
  REAL8 part_nsse;
  REAL8 part_dispersion;
  UNSIGNED2 iterations;
END;
Ext_Part_C := RECORD(Part)
  REAL8 max_delta;
  REAL8 part_mse;
  REAL8 part_dispersion;
  UNSIGNED2 iterations;
END;
t_term_type := ENUM(UNSIGNED1, OTHER=0, LHS, RHS);
Summand := RECORD(Part)
  t_term_type typ := t_term_type.OTHER;
END;
wi_info := RECORD
  t_work_item wi;
  t_work_item orig_wi;
  UNSIGNED4 obs;
  UNSIGNED4 dims;
  UNSIGNED4 col;
  UNSIGNED4 orig_col;
  UNSIGNED4 dep_cols;
  UNSIGNED4 dep_rows;
  UNSIGNED4 ind_cols;
  UNSIGNED4 ind_rows;
  dimension_t parts;
  dimension_t num_rows;
  UNSIGNED4 frst_addr;
END;
Ext_NFld := RECORD(NumericField)
  UNSIGNED4 addr;
  UNSIGNED4 frst;
  UNSIGNED4 parts;
  UNSIGNED4 mat_cols;
  UNSIGNED4 mat_rows;
  UNSIGNED4 part_rows;
  UNSIGNED4 part;
  UNSIGNED4 num_rows;
  UNSIGNED4 obs;
  UNSIGNED4 dims;
  UNSIGNED4 inserts;
  BOOLEAN dropMe;
  REAL8 weight := 1.0;
END;

Cell := {value_t v};
value_t prototype(value_t v) := 0.0;
matrix_t Apply2Mat(matrix_t mat, prototype f) := FUNCTION
  cells := DATASET(mat, Cell);
  Cell apply_f(Cell in) := TRANSFORM
    SELF.v := f(in.v);
  END;
  rslt := SET(PROJECT(cells, apply_f(LEFT)), v);
  RETURN rslt;
END;
Part apply_transform(Part p, prototype f) := TRANSFORM
  SELF.mat := Apply2Mat(p.mat, f);
  SELF := p;
END;
value_t prototype_bin(value_t v, value_t u) := 0.0;
matrix_t Apply2MatBinary(matrix_t mat1, matrix_t mat2, prototype_bin f) := FUNCTION
  cells1 := DATASET(mat1, Cell);
  cells2 := DATASET(mat2, Cell);
  Cell apply_f(Cell in1, Cell in2) := TRANSFORM
    SELF.v := f(in1.v, in2.v);
  END;
  rslt := SET(COMBINE(cells1, cells2, apply_f(LEFT, RIGHT)), v);
  RETURN rslt;
END;
Part apply_transform_bin(Part p, Part q, prototype_bin f) := TRANSFORM
  SELF.mat := Apply2MatBinary(p.mat, q.mat, f);
  SELF := p;
END;

//
/**
 * Internal function to determine values for the model coefficients
 * and selected statistics from building the model.  Function works
 * with a vector of blocks for X and Y, and a single block for the
 * XtWX matrix.
 * @param independents the independent values
 * @param dependents the dependent values
 * @param fam a module defining the error distribution and link of the response
 * @param max_iter maximum number of iterations to try
 * @param epsilon the minimum change in the Beta value estimate to continue
 * @param ridge a value to pupulate a diagonal matrix that is added to
 * a matrix help assure that the matrix is invertible.
 * @param weights A set of observation weights (one per dependent value).
 * @return coefficient matrix plus model building statistics
 */
EXPORT DATASET(Layout_Model) GetModel_global(
  DATASET(NumericField)  independents,
  DATASET(NumericField)  dependents,
  Family.FamilyInterface fam,
  UNSIGNED               max_iter = 200,
  REAL8                  epsilon  = Constants.default_epsilon,
  REAL8                  ridge    = Constants.default_ridge,
  DATASET(NumericField)  weights  = DATASET([], NumericField)) := FUNCTION

  // Get definition of mean and variance functions
  REAL8 link(REAL8 v) := fam.link(v);
  REAL8 mu(REAL8 v) := fam.mu(v);
  REAL8 var(REAL8 v) := fam.var(v);
  REAL8 deta(REAL8 v) := fam.deta(v);
  REAL8 init(REAL8 v, REAL8 w = 1.0) := fam.init(v, w);
  value_t init_uni(value_t v) := init(v);
  value_t init_bin(value_t v, value_t w) := init(v, w);
  value_t link_i(value_t v) := link(v);
  value_t u_i(value_t v) := mu(v);
  value_t w_i(value_t v) := 1.0 / var(v) / POWER(deta(v), 2);

  // Define utility functions
  value_t abs_v(value_t v) := ABS(v);
  value_t mult_ab(value_t a, value_t b) := a * b;
  value_t mult_mat(value_t a,
                   value_t b,
                   dimension_t r,
                   dimension_t c) := a * b;
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
                  dimension_t c) := w_i(u) * POWER(res, 2);

  // check for user-defined weights
  doWeights := EXISTS(weights);

  // X cols, need to be dense
  ind_screen := ASSERT(independents, number>0 AND id>0,
                       'Column left of 1 in Ind or a row 0', FAIL);
  dep_screen := ASSERT(dependents, number>0 AND id>0,
                       'Column left of 1 in Dep or a row 0', FAIL);
  wgt_screen := ASSERT(weights, id>0 and number>0,
                       'Column left of 1 in Wgt or a row 0', FAIL);
  icols := TABLE(ind_screen, {wi, number, max_id:=MAX(GROUP, id), col:=1},
                 wi, number, FEW, UNSORTED);
  ind_cols_r := {UNSIGNED4 wi, UNSIGNED4 r, UNSIGNED4 c, SET OF UNSIGNED4 cols};
  ind_cols := ROLLUP(GROUP(SORT(icols, wi, number), wi), GROUP,
                     TRANSFORM(ind_cols_r,
                               SELF.r := MAX(ROWS(LEFT), max_id),
                               SELF.c := COUNT(ROWS(LEFT)),
                               SELF.cols:=SET(ROWS(LEFT), number),  //wi-number unique
                               SELF.wi:=LEFT.wi));
  // ind_cols := TABLE(ind_screened,
  //                   {wi, r:=MAX(GROUP,id), c:=MAX(GROUP,number)},
  //                   wi, FEW, UNSORTED);
  // Y columns need to be dense, so re-map as necessary
  dcols := TABLE(dep_screen, {wi, number, max_id:=MAX(GROUP, id), col:=1},
                 wi, number, FEW, UNSORTED);
  dep_map := PROJECT(GROUP(SORT(dcols, wi, number), wi),
                     TRANSFORM(RECORDOF(dcols),
                               SELF.col := COUNTER,
                               SELF:=LEFT));
  dep_cols := TABLE(dep_map,
                   {wi,r:=MAX(GROUP,max_id), c:=MAX(GROUP,col)},
                   wi, FEW, UNSORTED);
  // work item information
  wi_info cmb(RECORDOF(ind_cols) ind, RECORDOF(dep_cols) dep):=TRANSFORM
    SELF.orig_wi := ind.wi;
    SELF.wi := ind.wi;
    SELF.col := 1;        // place holder
    SELF.orig_col := 0;   // fill in after NORMALIZE
    SELF.obs := MAX(dep.r, ind.r);
    SELF.dims := ind.c + 1;
    SELF.dep_cols := dep.c;
    SELF.dep_rows := dep.r;
    SELF.ind_cols := ind.c + 1; // to be inserted
    SELF.ind_rows := ind.r;
    SELF.parts := (((MAX(ind.r,dep.r)*(ind.c+1))-1) DIV Constants.local_cap) + 1;
    SELF.num_rows := ((MAX(ind.r,dep.r)-1) DIV SELF.parts) + 1;
    SELF.frst_addr := 1; // place holder, assign after expansion
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
  w_dep  := JOIN(exp_wi, dep_map,
                 LEFT.orig_wi=RIGHT.wi AND LEFT.col=RIGHT.col,
                 TRANSFORM(wi_info,
                           SELF.orig_col:=RIGHT.number,
                           SELF:=LEFT),
                 LOOKUP, FEW);
  wi_info assign_addr(wi_info prev, wi_info curr) := TRANSFORM
    SELF.frst_addr := IF(prev.frst_addr=0, 1, prev.frst_addr+prev.parts);
    SELF := curr;
  END;
  map_wi := ITERATE(w_dep, assign_addr(LEFT, RIGHT));
  // now to extend independent and dependent records
  Ext_Nfld dummy_end(wi_info inf, BOOLEAN isDep, UNSIGNED c) := TRANSFORM
    this_col := ((c-1) DIV inf.parts) + 1;
    part := (c-1) % inf.parts;
    SELF.part := part;
    SELF.addr := part + inf.frst_addr;
    SELF.mat_rows := IF(isDep, inf.dep_rows, inf.ind_rows);
    SELF.mat_cols := IF(isDep, 1, inf.dims);
    SELF.part_rows:= MIN(inf.num_rows, inf.obs - inf.num_rows*(part));
    SELF.wi := inf.wi;
    SELF.frst := inf.frst_addr;
    SELF.parts := inf.parts;
    SELF.num_rows := inf.num_rows;
    SELF.number := this_col;
    SELF.obs := inf.obs;
    SELF.dims := inf.dims;
    SELF.inserts := 0;
    SELF.id := MIN((part+1)*inf.num_rows, inf.obs);
    SELF.value := 0;
    SELF.dropMe := TRUE;
  END;
  dep_ends := NORMALIZE(map_wi, LEFT.parts, dummy_end(LEFT, TRUE, COUNTER));
  ind_ends := NORMALIZE(map_wi, LEFT.parts*(LEFT.dims-1),
                        dummy_end(LEFT, FALSE, COUNTER));
  // replicate for multiple dep cols and partition
  Ext_NFld mark_addr(NumericField nf, wi_info inf, BOOLEAN isDep) := TRANSFORM
    part := ((nf.id-1) DIV inf.num_rows);
    SELF.part := part;
    SELF.addr :=  part + inf.frst_addr;
    SELF.mat_rows := IF(isDep, inf.dep_rows, inf.ind_rows);
    SELF.mat_cols := IF(isDep, 1, inf.dims);
    SELF.part_rows:= MIN(inf.num_rows, inf.obs - inf.num_rows*(part));
    SELF.wi := inf.wi;
    SELF.frst := inf.frst_addr;
    SELF.parts := inf.parts;
    SELF.num_rows := inf.num_rows;
    SELF.number := IF(isDep, 1, nf.number);
    SELF.obs := inf.obs;
    SELF.dims := inf.dims;
    SELF.inserts := 0;
    SELF.dropMe := FALSE;
    SELF := nf;
  END;
  Ext_NFld find_inserts(Ext_NFld prev, Ext_NFld curr, BOOLEAN isDep) := TRANSFORM
    prev_id := IF(prev.id=0, (curr.part*curr.num_rows), prev.id);
    SELF.inserts := IF(curr.id=prev_id, 0, curr.id - prev_id - 1);
    SELF.dropMe := prev.id=curr.id;
    SELF := curr;
  END;
  Ext_NFld insert_zeros(Ext_NFld base, UNSIGNED c) := TRANSFORM
    insertZero := c <= base.inserts;
    SELF.value := IF(insertZero, 0, base.value);
    SELF.id := base.id - base.inserts + c - 1;
    SELF := base;
  END;
  Part roll_part(Ext_NFld par, DATASET(Ext_NFld) rws, BOOLEAN isDep) := TRANSFORM
    ones := make_vector(par.part_rows, 1.0);
    SELF.wi := par.wi;
    SELF.part_rows := par.part_rows;
    SELF.part_cols := par.mat_cols;
    SELF.this_addr := par.addr;
    SELF.frst_addr := par.frst;
    SELF.parts := par.parts;
    SELF.mat  := IF(isDep, SET(rws, value), ones+SET(rws, value));
    SELF.obs := par.obs;
    SELF.dims := par.dims;
  END;
  // replicate independents for multiple dependents
  repl_ind := JOIN(ind_screen, map_wi, LEFT.wi=RIGHT.orig_wi,
                   mark_addr(LEFT, RIGHT, FALSE), LOOKUP, MANY);
  dist_ind := DISTRIBUTE(repl_ind+ind_ends, addr);
  sorted_ind := SORT(dist_ind, wi, part, number, id, dropMe, LOCAL);
  grpd_ind := GROUP(sorted_ind, wi, part, number, LOCAL);
  mrkd_ind := ITERATE(grpd_ind, find_inserts(LEFT, RIGHT, FALSE));
  full_ind := NORMALIZE(mrkd_ind(NOT dropMe), LEFT.inserts+1,
                        insert_zeros(LEFT, COUNTER));
  rr_ind := GROUP(full_ind, wi, part, LOCAL);
  ind_mat := ROLLUP(rr_ind, GROUP, roll_part(LEFT, ROWS(LEFT), FALSE));
  // replicate weights for multiple dependents
  repl_wgt := JOIN(wgt_screen, map_wi, LEFT.wi=RIGHT.orig_wi,
                   mark_addr(LEFT, RIGHT, TRUE), LOOKUP, MANY);
  dist_wgt := DISTRIBUTE(repl_wgt+dep_ends, addr);
  sorted_wgt := SORT(dist_wgt, wi, part, number, id, dropMe, LOCAL);
  grpd_wgt := GROUP(sorted_wgt, wi, part, number, LOCAL);
  mrkd_wgt := ITERATE(grpd_wgt, find_inserts(LEFT, RIGHT, TRUE));
  full_wgt := NORMALIZE(mrkd_wgt(NOT dropMe), LEFT.inserts+1,
                        insert_zeros(LEFT, COUNTER));
  rr_wgt := GROUP(full_wgt, wi, part, LOCAL);
  wgt_mat := ROLLUP(rr_wgt, GROUP, roll_part(LEFT, ROWS(LEFT), TRUE));
  // re-map the columns of the dep matrix and partition
  exp_dep := JOIN(dep_screen, map_wi,
                  LEFT.wi=RIGHT.orig_wi AND LEFT.number=RIGHT.orig_col,
                  mark_addr(LEFT, RIGHT, TRUE), LOOKUP);
  dist_dep := DISTRIBUTE(exp_dep+dep_ends, addr);
  srtd_dep := SORT(dist_dep, wi, part, number, id, dropMe, LOCAL);
  grpd_dep := GROUP(srtd_dep, wi, part, number, LOCAL);
  // add in the missing rows
  mrkd_dep := ITERATE(grpd_dep, find_inserts(LEFT, RIGHT, TRUE));
  full_dep := NORMALIZE(mrkd_dep(NOT dropMe), LEFT.inserts+1,
                        insert_zeros(LEFT, COUNTER));
  rr_dep := GROUP(full_dep, wi, part, LOCAL);
  dep_mat := ROLLUP(rr_dep, GROUP, roll_part(LEFT, ROWS(LEFT), TRUE));
  // generate initial values for Betas
  Ext_Part initial_beta(wi_info inf, UNSIGNED c) := TRANSFORM
    SELF.wi := inf.wi;
    SELF.part_rows := inf.dims;
    SELF.part_cols := 1;
    SELF.this_addr := inf.frst_addr + c - 1;
    SELF.frst_addr := inf.frst_addr;
    SELF.parts := inf.parts;
    SELF.mat := make_vector(inf.dims, 0.0); // overwritten with family's init function in iter 1
    SELF.obs := inf.obs;
    SELF.dims := inf.dims;
    SELF.max_delta := 2*epsilon;
    SELF.part_sse := 0.0;
    SELF.part_nsse := 0.0;
    SELF.part_dispersion := 0.0;
    SELF.iterations := 0;
  END;
  B_1node := NORMALIZE(map_wi, LEFT.parts, initial_beta(LEFT, COUNTER));
  B_dist := DISTRIBUTE(B_1node, this_addr);
  B_init := SORT(B_dist, wi, this_addr, LOCAL);
  // generate an m element ridge vector
  Part make_ridge(Part p) := TRANSFORM
    SELF.part_cols := p.dims;
    SELF.mat :=  make_diag(p.dims, ridge);
    SELF := p;
  END;
  ridge_mat := PROJECT(B_init, make_ridge(LEFT));
  // iterate Beta to get convergence
  DATASET(Ext_Part) iter0(DATASET(Ext_Part) B_SE, UNSIGNED iter):=FUNCTION
    U0 := IF(~doWeights,
      PROJECT(dep_mat, apply_transform(LEFT, init_uni)),
      JOIN(dep_mat, wgt_mat, LEFT.wi=RIGHT.wi AND LEFT.this_addr=RIGHT.this_addr,
      apply_transform_bin(LEFT, RIGHT, init_bin), LOCAL)
    );
    XB0 := PROJECT(U0, apply_transform(LEFT, link_i));
    // Need XB ==> vector of partitions, all local
    Part XB_prod(Part x, Part b) := TRANSFORM
      SELF.mat := gemm(FALSE, FALSE, x.part_rows, 1, x.part_cols, 1.0, x.mat, b.mat);
      SELF.part_cols := 1;
      SELF := x;
    END;
    XB := IF(iter <> 1,
      JOIN(ind_mat, B_SE,
           LEFT.wi=RIGHT.wi AND LEFT.this_addr=RIGHT.this_addr,
           XB_prod(LEFT, RIGHT), LOCAL),
      XB0
    );
    U := IF(iter <> 1, PROJECT(XB, apply_transform(LEFT, u_i)), U0);
    W_uw := PROJECT(U, apply_transform(LEFT, w_i));   // Block Vector
    W := IF(
      doWeights,
      JOIN(W_uw, wgt_mat, LEFT.wi=RIGHT.wi AND LEFT.this_addr=RIGHT.this_addr,
      apply_transform_bin(LEFT, RIGHT, mult_ab), LOCAL),
      W_uw
	   );
    // XtWX ==> Single partition of partial sums
    Summand XtWX_prod(Part x, Part w) := TRANSFORM
      XtW := dimm(TRUE, FALSE, FALSE, TRUE, x.dims, x.part_rows,
                  x.part_rows, 1.0, x.mat, w.mat);
      SELF.mat := gemm(FALSE, FALSE, x.dims, x.dims, x.part_rows,
                       1.0, XtW, x.mat);
      SELF.part_rows := x.dims;
      SELF.part_cols := x.dims;
      SELF.typ := t_term_type.LHS;
      SELF := x;
    END;
    XtWX_parts := JOIN(ind_mat, W,
                       LEFT.wi=RIGHT.wi AND LEFT.this_addr=RIGHT.this_addr,
                       XtWX_prod(LEFT,RIGHT), LOCAL);
    // Xt(WXB + Y - U) ==> single partition of partial sums
    ds_set := [PROJECT(W, Summand), PROJECT(XB, Summand),
               PROJECT(dep_mat, Summand), PROJECT(U, Summand),
               PROJECT(ind_mat, Summand)];
    Summand XtWXB_res_term(DATASET(Summand) parts) := TRANSFORM
      part_obs := parts[1].part_rows;
      dims := parts[1].dims;
      W_mat := parts[1].mat;
      Y_mat := parts[3].mat;
      U_mat := parts[4].mat;
      XB_mat := parts[2].mat;
      X_mat := parts[5].mat;
      Y_U := axpy(part_obs, -1.0, U_mat, 1, Y_mat, 1);
      res := Apply2CellsBinary(part_obs, 1, U_mat, Y_U, resi_uy);
      XB_res := axpy(part_obs, 1.0, XB_mat, 1, res, 1);
      WXB_res := dimm(FALSE, FALSE, TRUE, FALSE, part_obs, 1, part_obs,
                       1.0, W_mat, XB_res);
      XtWXB_res := gemm(TRUE, FALSE, dims, 1, part_obs, 1.0, X_mat, WXB_res);
      SELF.mat := XtWXB_res;
      SELF.part_rows := dims;
      SELF.part_cols := 1;
      SELF.dims := 1;
      SELF.typ := t_term_type.RHS;
      SELF := parts[2];
    END;
    XtWXB_res_parts := JOIN(ds_set,
                            LEFT.wi=RIGHT.wi AND LEFT.this_addr=RIGHT.this_addr,
                            XtWXB_res_term(ROWS(LEFT)),
                            SORTED(wi, this_addr), LOCAL);
    // replicate XtWX and Xt(WXB + Y - U) partials to each node for local sum
    replicants := NORMALIZE(XtWXB_res_parts + XtWX_parts, MAX(CLUSTERSIZE, LEFT.parts),
                      TRANSFORM(Summand,
                                SELF.this_addr:=LEFT.frst_addr+COUNTER-1,
                                SELF := LEFT));
    Term_home := DISTRIBUTE(replicants, this_addr);
    // Sum LHS terms to get XtWX_R on every node
    LHS_terms := PROJECT(Term_home(typ=t_term_type.LHS), Part);
    XtWX_R_terms := SORT(LHS_terms+ridge_mat, wi, LOCAL);
    Part sum_part(Part base, Part incr) := TRANSFORM
      SELF.mat := axpy(base.part_rows*base.part_cols, 1.0, base.mat, 1, incr.mat, 1);
      SELF := base;
    END;
    XtWX_R := ROLLUP(XtWX_R_terms, sum_part(LEFT, RIGHT), wi, LOCAL);
    // Sum RHS to get Xt(WXB + Y - U) on each node
    RHS_terms := PROJECT(Term_home(typ=t_term_type.RHS), Part);
    XtWXB_res_terms := SORT(RHS_terms, wi, LOCAL);
    XtWXB_res := ROLLUP(XtWXB_res_terms, sum_part(LEFT,RIGHT), wi, LOCAL);
    // Calc INV(XtWX + R) ==> single partition, calc on all nodes
    Part calc_inv(Part p) := TRANSFORM
      LU := getf2(p.dims, p.dims, p.mat);
      identity := make_diag(p.dims);
      inner := trsm(Side.Ax, Triangle.Lower, FALSE, Diagonal.UnitTri,
                    p.dims, p.dims, p.dims, 1.0, LU, identity);
      SELF.mat := trsm(Side.Ax, Triangle.Upper, FALSE, Diagonal.NotUnitTri,
                       p.dims, p.dims, p.dims, 1.0, LU, inner);
      SELF.part_rows := p.dims;
      SELF.part_cols := p.dims;
      SELF := p;
    END;
    inv_XtWX_R := PROJECT(XtWX_R, calc_inv(LEFT));
    // B1 = INV(XtWX+R) * XtWXB_res ==> single partition, each node
    // SE = diag of INV(XtWX+R)
    Part calc_beta_se(Part inv, Part rhs) := TRANSFORM
      betas := gemm(FALSE, FALSE, inv.dims, 1, inv.dims, 1.0, inv.mat, rhs.mat);
      SE := extract_diag(inv.dims, inv.dims, inv.mat);
      SELF.mat := betas + SE;
      SELF.part_rows := inv.dims;
      SELF.part_cols := 2;
      SELF := inv;
    END;
    b_se_dist := JOIN(inv_XtWX_R, XtWXB_res,
                      LEFT.wi=RIGHT.wi AND LEFT.this_addr=RIGHT.this_addr,
                      calc_beta_se(LEFT,RIGHT), LOCAL);
    // XB1 ==> vector of partitions
    XB1 := JOIN(ind_mat, b_se_dist,
                LEFT.wi=RIGHT.wi AND LEFT.this_addr=RIGHT.this_addr,
                XB_prod(LEFT, RIGHT), LOCAL);
    // U1 ==> from XB1 product, vector of partitions
    U1 := PROJECT(XB1, apply_transform(LEFT, u_i));
    Score_Rec_DM := RECORD
      t_work_item wi;
      UNSIGNED2 this_addr;
      REAL8 sse;
      REAL8 nsse;
      matrix_t disp_mat;
    END;
    Score_Rec := RECORD
      t_work_item wi;
      UNSIGNED2 this_addr;
      REAL8 sse;
      REAL8 nsse;
      REAL8 dispersion;
    END;
    Score_Rec_DM compare_Y(Part Y, Part U1) := TRANSFORM
      part_obs := Y.part_rows;
      dims := Y.dims;
      Y_mat := Y.mat;
      U1_mat := U1.mat;
      Y_U1 := axpy(part_obs, -1.0, U1_mat, 1, Y_mat, 1);
      sqdiff := Apply2Cells(part_obs, 1, Y_U1, sq_cells);
      res1 := Apply2CellsBinary(part_obs, 1, U1_mat, Y_U1, resi_uy);
      SELF.sse := asum(part_obs, sqdiff, 1);
      SELF.nsse := part_obs;
      SELF.disp_mat := Apply2CellsBinary(part_obs, 1, U1_mat, res1, disp_uy);
      SELF.wi := Y.wi;
      SELF.this_addr := Y.this_addr;
    END;
    Score_Rec get_disp(Score_Rec_DM sc, Part wf) := TRANSFORM
      part_obs := wf.part_rows;
      dims := wf.dims;
      disp_weig := Apply2CellsBinary(part_obs, 1, sc.disp_mat, wf.mat, mult_mat);
      SELF.dispersion := MAP(
        ~fam.dispersion => 1.0,
        ~doWeights => asum(part_obs, sc.disp_mat, 1),
        asum(part_obs, disp_weig, 1)
      );
      SELF := sc;
    END;
    scored_parts_DM := JOIN(dep_mat, U1,
                         LEFT.wi=RIGHT.wi AND LEFT.this_addr=RIGHT.this_addr,
                         compare_Y(LEFT, RIGHT), LOCAL);
    scored_parts := JOIN(scored_parts_DM, wgt_mat,
                         LEFT.wi=RIGHT.wi AND LEFT.this_addr=RIGHT.this_addr,
                         get_disp(LEFT, RIGHT), LOCAL);
    // delta B = B-B1, get MAX    ==> replicated single partition.
    Ext_Part get_max_delta(Part b1, Part b) := TRANSFORM
      diff := axpy(b.dims, -1.0, b1.mat, 1, b.mat[1..b.dims], 1);
      abs_diff := Apply2Mat(diff, abs_v);
      SELF.max_delta := MAX(abs_diff);
      SELF.part_sse := 0.0;
      SELF.part_nsse := 0.0;
      SELF.part_dispersion := 0.0;
      SELF.iterations := iter;
      SELF := b1;
    END;
    max_delta := JOIN(b_se_dist, B_SE,
                     LEFT.wi=RIGHT.wi AND LEFT.this_addr=RIGHT.this_addr,
                     get_max_delta(LEFT, RIGHT), LOCAL);
    // update new B_SE with sse and iteration
    Ext_Part update_score(Ext_Part b1, Score_Rec sc) := TRANSFORM
      SELF.part_sse := sc.sse;
      SELF.part_nsse := sc.nsse;
      SELF.part_dispersion := sc.dispersion;
      SELF.iterations := iter;
      SELF := b1;
    END;
    scored := JOIN(max_delta, scored_parts,
                 LEFT.wi=RIGHT.wi AND LEFT.this_addr=RIGHT.this_addr,
                 update_score(LEFT, RIGHT), LOCAL);
    rslt := SORT(scored, wi, this_addr, LOCAL);
    RETURN rslt;
  END;
  B_work := LOOP(B_init, epsilon<LEFT.max_delta AND max_iter>LEFT.iterations,
                 iter0(ROWS(LEFT), COUNTER));
  // Capture model statistics, multiple responses are still multiple wi
  // Betas and SE for betas, Iterations, last delta, correct, incorrect
  // First, combine the beta blocks to get totals for correct/incorrect
  B_wrk0 := DISTRIBUTE(B_work, frst_addr);
  B_wrk1 := GROUP(SORT(b_wrk0, wi, LOCAL), wi, LOCAL);
  Ext_Part_C roll_bwork(Ext_Part b, DATASET(Ext_Part) rws) := TRANSFORM
    obs := SUM(rws, part_nsse);
    SELF.part_mse := SUM(rws, part_sse) / obs;
    SELF.part_dispersion := IF(fam.dispersion,
      SUM(rws, part_dispersion) / (obs - b.dims),
      1.0);
    SELF := b;
  END;
  B_calc := ROLLUP(B_wrk1, GROUP, roll_bwork(LEFT, ROWS(LEFT)));
  NumericField extBetas(Ext_Part_C p, UNSIGNED subscript) := TRANSFORM
    beta := p.mat[subscript];
    se := SQRT(p.mat[subscript] * p.part_dispersion);
    SELF.wi := p.wi;
    SELF.id := Constants.id_betas + subscript - 1;
    SELF.number := 1;
    SELF.value := IF(subscript>p.part_rows, se, beta);
  END;
  b_se := NORMALIZE(B_calc, LEFT.part_rows*2, extBetas(LEFT,COUNTER));
  NumericField extStats(Ext_Part_C p, UNSIGNED fld) := TRANSFORM
    SELF.wi := p.wi;
    SELF.id := CHOOSE(fld, Constants.id_iters, Constants.id_delta,
                      Constants.id_mse, Constants.id_dispersion);
    SELF.number := 1;
    SELF.value := CHOOSE(fld, p.iterations, p.max_delta, p.part_mse,
                          p.part_dispersion);
  END;
  stats := NORMALIZE(B_calc, 4, extStats(LEFT, COUNTER));

  // return multiple wi to single wi and multiple columns
  Layout_Model wi_reset(NumericField nf, wi_info w) := TRANSFORM
    SELF.wi := w.orig_wi;
    SELF.number := w.orig_col;
    SELF.id := nf.id;
    SELF := nf;
  END;
  var_data := JOIN(b_se+stats, map_wi, LEFT.wi=RIGHT.wi,
                   wi_reset(LEFT, RIGHT), LOOKUP, FEW); // : ONWARNING(1005,ignore);
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
    SELF.value := CHOOSE(f, Constants.builder_irls_global,
                            max_iter,
                            epsilon,
                            w.ind_cols-1,
                            w.dep_cols,
                            w.obs);
  END;
  base_data := NORMALIZE(cmb_cols, 6, extBase(LEFT, COUNTER));
  RETURN base_data + var_data;
END;
