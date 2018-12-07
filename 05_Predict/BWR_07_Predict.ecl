IMPORT Taxi;
IMPORT ML_Core;
IMPORT ML_Core.Types AS Types;
IMPORT ML_Core.Analysis AS Analysis;
IMPORT LogisticRegression AS LR;
IMPORT Std;

//Import Validated Data
//------------------------------------------------------------------------------

FlatWeatherRec := RECORD
    UNSIGNED4       date;
    INTEGER8        minutes_after_midnight;
    STRING          summary;
    DECIMAL6_3      temperature;
    UDECIMAL6_3     precipintensity;
    STRING          preciptype;
    UDECIMAL4_2     windspeed;
    UDECIMAL4_2     visibility;
    UDECIMAL4_2     cloudcover;
    UNSIGNED1       preciptypeid;
END;

YellowLayout := RECORD
    UNSIGNED4       record_id;
    UNSIGNED1       vendorid;
    STRING19        tpep_pickup_datetime;
    STRING19        tpep_dropoff_datetime;
    UNSIGNED1       passenger_count;
    DECIMAL10_2     trip_distance;
    DECIMAL9_6      pickup_longitude;
    DECIMAL9_6      pickup_latitude;
    UNSIGNED1       rate_code_id;
    STRING1         store_and_fwd_flag;
    DECIMAL9_6      dropoff_longitude;
    DECIMAL9_6      dropoff_latitude;
    UNSIGNED1       payment_type;
    DECIMAL8_2      fare_amount;
    DECIMAL8_2      extra;
    DECIMAL8_2      mta_tax;
    DECIMAL8_2      tip_amount;
    DECIMAL8_2      tolls_amount;
    DECIMAL8_2      improvement_surcharge;
    DECIMAL8_2      total_amount;
    BOOLEAN         bad_trip_distance;
    BOOLEAN         bad_passenger_count;
    BOOLEAN         bad_pickup_coordinates;
    BOOLEAN         bad_dropoff_coordinates;
    BOOLEAN         bad_fare_amount;
    BOOLEAN         bad_tip_amount;
    BOOLEAN         bad_tolls_amount;
    BOOLEAN         bad_improvement_surcharge;
    BOOLEAN         bad_total_amount;
    BOOLEAN         is_valid_record;
    UNSIGNED4       pickup_date;
    UNSIGNED3       pickup_time;
    UNSIGNED2       pickup_minutes_after_midnight;
    UNSIGNED2       pickup_time_window;
    UNSIGNED1       pickup_time_hour;
    UNSIGNED1       pickup_day_of_week;
    BOOLEAN         pickup_date_is_holiday;
    UNSIGNED4       dropoff_date;
    UNSIGNED3       dropoff_time;
    UNSIGNED2       dropoff_minutes_after_midnight;
    UNSIGNED1       dropoff_time_window;
    UNSIGNED1       dropoff_time_hour;
    UNSIGNED1       dropoff_day_of_week;
    UNSIGNED2       trip_duration_minutes;
    UNSIGNED2       trip_distance_bucket;
    UNSIGNED1       pickup_borough_id;
    UNSIGNED1       dropoff_borough_id;
    FlatWeatherRec  weather;
END;

taxiData := DATASET
    (
        '~taxi_data::data_validated_enriched_appended',
        YellowLayout,
        FLAT
    );

// Restrict our analysis to only valid records
ds := taxiData(is_valid_record);

//------------------------------------------------------------------------------

//----------------------------------Preprocessing---------------------------------------------------
//Define desired Layout/Fields 1
Layout1 := RECORD       
  ds.pickup_date;
  ds.pickup_time;
  ds.weather.preciptypeid;
  INTEGER rainytrips := 0; //Total trips in the rain Per day
  INTEGER totaltrips := 0; //Total Trips Per day
  DECIMAL9_2 pct := 0;     //The percentage of Rainy trips Per day 
END;
// TRANSFORM ds/dataset into desired Layout/Fields 
ds1 := PROJECT(ds(pickup_borough_id <4),TRANSFORM(Layout1, SELF.preciptypeid := LEFT.weather.preciptypeid, SELF := LEFT));
//Aggregate data t1: t: Summrize the total rainytrips/day; cnt: Summrize the total trips/day
t1 := TABLE(ds1, {pickup_date, t:= SUM(GROUP, preciptypeid), cnt:= COUNT(GROUP)}, pickup_date);
//Aggregate data t2: pct:  the percentage of Rainy trips Per day
t2 := TABLE(t1, {pickup_date, cnt,  REAL pct := t/cnt}, pickup_date);
//Define desired Layout/Fields 2
Layout2 := RECORD
  ds.pickup_date;
  ds.weather.preciptypeid;
  INTEGER period := 1;
  DECIMAL9_2 avgtrips := 0;
  INTEGER totaltrips := 0;
  DECIMAL9_2 rainpct := 0.0;
  DECIMAL9_2 trippct := 0.0;
  INTEGER dir := 0;
END;
ds2 := PROJECT(t2, TRANSFORM(Layout2, SELF.preciptypeid := IF(LEFT.pct = 0 , 0, 1), 
                                      SELF.totaltrips := LEFT.cnt,
                                      SELF.rainpct := LEFT.pct;
                                      SELF := LEFT));
//Aggregate the Data
layout2 doRollup(layout2 l, layout2 r):= TRANSFORM
  tt := l.totaltrips + r.totaltrips;
  p :=l.period + 1;
  SELF.pickup_date := r.pickup_date;
  SELF.period := p;                      //Total number of days in the same weather condition: either not rainy or rainy
  SELF.totaltrips := tt;                 //Total number of trips in the same weather condition
  SELF.rainpct := l.rainpct + r.rainpct; //Total number of percentage of rainy trips
  SELF := l;
END;
ds3 := ROLLUP(SORT(DISTRIBUTE(ds2,HASH32(pickup_date)),pickup_date, LOCAL),doRollup(LEFT, RIGHT), preciptypeid, LOCAL );
//Aggregate the Data -- Cont.,
ds4 := PROJECT(ds3, TRANSFORM(layout2, SELF.avgtrips := LEFT.totaltrips/LEFT.period,
                                       SELF.rainpct := LEFT.rainpct/LEFT.period,
                                       SELF := LEFT));
//Define the Classes
ds5 := ITERATE
    (
        SORT(ds4, pickup_date),
        TRANSFORM
            (
                Layout2,
                SELF.trippct := (RIGHT.avgtrips - LEFT.avgtrips)/LEFT.avgtrips,
                //Define the Classes: 0 --> decrease, 1 --> increase, 2 --> invalid
                SELF.dir := IF(LEFT.preciptypeid < RIGHT.preciptypeid,IF(SELF.trippct>0 , 1, 0), 2),
                SELF := RIGHT
            )
    );
//Filter out invalid data
ds6 := ds5(dir = 0 OR dir = 1);
//Add RANDOM() for later
ds7 := PROJECT(ds6, TRANSFORM(layout2, SELF.pickup_date := RANDOM(), SELF := LEFT));
//Add ID for each record
ML_Core.AppendID(ds7, id, ds8);
//Random Sampling
dstrain := ENTH(SORT(ds8, pickup_date),1, 1, 1);
dstest := ENTH(SORT(ds8, pickup_date),1,8, 1);

//----------------------------------Preprocessing END---------------------------------------------------
//----------------------------------Machine Learning---------------------------------------------------
//Scaler: a Helper function to scale Fields
Scaler(DATASET(Types.NumericField) ds) := FUNCTION
  //ML_Core.FieldAggregates()
  scale:= ML_Core.FieldAggregates(ds).simple;
  rst := JOIN(ds, scale, LEFT.wi = RIGHT.wi AND LEFT.number = RIGHT.number,  TRANSFORM(Types.NumericField, 
              SELF.value := (LEFT.value - RIGHT.minval)/(RIGHT.maxval- RIGHT.minval), SELF := LEFT), LOOKUP);
  RETURN rst;
END;
//Transform baseData to NF format.
ML_Core.ToField(dstrain, NFtrain, id, , , 'rainpct,dir');
ML_Core.ToField(dstest, NFtest, id, , , 'rainpct,dir');
//Field hold the class label
pnumber := 2;
//Trainset
DStrainInd := NFtrain(number <pnumber );
DStrainInd_scaled:= scaler(dstrainind); 
DStrainDpt := PROJECT(NFtrain(number = pnumber  ), TRANSFORM(Types.DiscreteField, SELF.number := 1, SELF := LEFT));
//Testset
DStestInd := NFtest(number <pnumber );
DStestInd_scaled := scaler(DStestInd); 
DStestDpt :=  PROJECT(NFtest(number = pnumber  ), TRANSFORM(Types.DiscreteField, SELF.number := 1, SELF := LEFT));
//LogisticRegression
//SETUP Model Parameters
//max number of iterations
max_itr_bi := 100;
//converge rate
threshold_bi := 0.0000001;
//Run the model and see the prediction result
mod_bi := LR.BinomialLogisticRegression(max_itr_bi, threshold_bi).getModel(DStrainInd_scaled, DStrainDpt);
predict_bi := LR.BinomialLogisticRegression(max_itr_bi, threshold_bi).Classify(mod_bi, DSTestInd_scaled);
//The classstate of trainset
DSTrainStat := Analysis.Classification.classstats(DStrainDpt);
OUTPUT(DSTrainStat, NAMED('DSTrainStat'));
DSTestStat := Analysis.Classification.classstats(DSTestDpt);
OUTPUT(DSTestStat, NAMED('DSTestStat'));
evaluation := Analysis.Classification.AccuracyByClass(predict_bi, DSTestDpt);
OUTPUT(evaluation, NAMED('evaluation'));

