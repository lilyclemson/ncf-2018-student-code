IMPORT ML_Core;
IMPORT ML_Core.Types AS Core_Types;
IMPORT $ AS GLMmod;
IMPORT GLMmod.Types;
IMPORT GLMmod.Family;

/**
 * Internal function to determine whether the dependent variable is appropriate
 * for the specified error distribution. Eg. A continuous dependent varying over
 * [0, 100] is not valid for the binomial family of GLMs.
 * @internal
 * @param stats Set of data statistics returned by the DataStats function.
 * @param fam A module defining the error distribution and link of the response.
 * @return Dataset indicating validity for each work item and warning messages
 * for any found to be invalid.
 */
EXPORT DATASET(Types.Data_Diagnostic) DataCheck(
  DATASET(Types.Data_Info) stats,
  Family.FamilyInterface fam) := FUNCTION

  // Get family distribution check values
  cardinality := fam.cardinality;
  values      := fam.values;
  isInteger   := fam.isInteger;

  Types.Data_Diagnostic checkWIs(Types.Data_Info di) := TRANSFORM
    dep_stats := di.dependent_stats;
    cardValid := MIN(dep_stats, cardinality) >= cardinality[1] AND
      MAX(dep_stats, cardinality) <= cardinality[2];
    valValid  := MIN(dep_stats, min_value) >= values[1] AND
      MAX(dep_stats, max_value) <= values[2];
    typeValid := ~isInteger OR (isInteger AND MIN(dep_stats, is_integer));
    SELF.wi := di.wi;
    SELF.valid := cardValid AND valValid AND typeValid;
    msg_prefix := '';
    SELF.message_text := [
      IF(~cardValid, msg_prefix + 'Dependent variable cardinality out of range for family', ''),
      IF(~valValid, msg_prefix + 'Dependent variable values out of range for family', ''),
      IF(~typeValid, msg_prefix + 'Dependent variable should contain only integers', '')
    ];
  END;

  ValidByWI := PROJECT(stats, checkWIs(LEFT));

  RETURN(ValidByWI);
END;