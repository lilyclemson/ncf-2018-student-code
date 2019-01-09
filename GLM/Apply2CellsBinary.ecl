IMPORT Std.BLAS;
IMPORT Std.BLAS.Types AS Types;

/**
 * Function prototype for Apply2CellsBinary.
 * @param x the first value
 * @param y the second value
 * @param r the row ordinal
 * @param c the column ordinal
 * @return the updated value
 */
 Types.value_t ICellFuncBinary(Types.value_t x,
                               Types.value_t y,
                               Types.dimension_t r,
                               Types.dimension_t c) := x;

/**
 * Iterate matrix and apply function to each pair of cells.
 *@param m number of rows
 *@param n number of columns
 *@param x matrix
 *@param y matrix
 *@param f function to apply
 *@return updated matrix
 */
EXPORT Types.matrix_t Apply2CellsBinary(Types.dimension_t m = 1,
                                        Types.dimension_t n = 1,
                                        Types.matrix_t x    = [],
                                        Types.matrix_t y    = [],
                                        ICellFuncBinary f   = ICellFuncBinary) := FUNCTION
  Cell := {Types.value_t v};
  CellBin := {Types.value_t x, Types.value_t y};
  Cell applyFunc(CellBin d, UNSIGNED pos) := TRANSFORM
    r := ((pos-1)  %  m) + 1;
    c := ((pos-1) DIV m) + 1;
    SELF.v := f(d.x, d.y, r, c);
  END;
  dInx := DATASET(x, Cell);
  dIny := DATASET(y, Cell);
  dIn := COMBINE(dInx, dIny, TRANSFORM(CellBin, SELF.x := LEFT.v, SELF.y := RIGHT.v));
  dOut := PROJECT(dIn, applyFunc(LEFT, COUNTER));
  RETURN SET(dOut, v);
END;