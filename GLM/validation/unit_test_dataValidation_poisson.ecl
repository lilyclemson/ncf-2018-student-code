﻿IMPORT $.^ AS GLMmod;
IMPORT GLMmod.Family;
IMPORT ML_Core.Types AS Core_Types;
NumericField := Core_Types.NumericField;

dep := DATASET(
       [{1, 1, 1, 1}, {1, 1, 2, 6},
        {1, 2, 1, 1}, {1, 2, 2, 6},
        {1, 3, 1, 2}, {1, 3, 2, 5},
        {1, 4, 1, 2}, {1, 4, 2, 5},
        {1, 5, 1, 3}, {1, 5, 2, 4.5},
        {1, 6, 1, 3}, {1, 6, 2, 4},
        {1, 7, 1, 4}, {1, 7, 2, 3},
        {1, 8, 1, 5}, {1, 8, 2, 2},
        {1, 9, 1, 6}, {1, 9, 2, 1},
        {2, 1, 1, 1}, {2, 1, 2, 6}, {2, 1, 3, -1},
        {2, 2, 1, 1}, {2, 2, 2, 5}, {2, 2, 3, 1},
        {2, 3, 1, 2}, {2, 3, 2, 4}, {2, 3, 3, 2},
        {2, 4, 1, 2}, {2, 4, 2, 3}, {2, 4, 3, 2},
        {2, 5, 1, 3}, {2, 5, 2, 3}, {2, 5, 3, 3},
        {2, 6, 1, 3}, {2, 6, 2, 2}, {2, 6, 3, 3},
        {2, 7, 1, 4}, {2, 7, 2, 2}, {2, 7, 3, 4},
        {2, 8, 1, 5}, {2, 8, 2, 1}, {2, 8, 3, 5},
        {2, 9, 1, 6}, {2, 9, 2, 1}, {2, 9, 3, 6},
        {3, 1, 1, -0.0001},
        {3, 2, 1, 0},
        {3, 3, 1, 0},
        {3, 4, 1, 0},
        {3, 5, 1, 0},
        {3, 6, 1, 0},
        {3, 7, 1, 1},
        {3, 8, 1, 1},
        {3, 9, 1, 1}], NumericField);
ind0 := DATASET(
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
ind := WHEN(ind0, #STORED('GLM_LOCAL_MATRIX_CAP', 20));
stats := GLMmod.DataStats(ind, dep, TRUE, FALSE, Family.Poisson);

stats_out := OUTPUT(stats, NAMED('stats'));
EXPORT unit_test_dataValidation_poisson := SEQUENTIAL(stats_out);
