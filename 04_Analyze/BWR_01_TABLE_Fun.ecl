IMPORT Std;

#WORKUNIT('name', 'Taxi Data: TABLE Fun');

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
validData := taxiData(is_valid_record);

//------------------------------------------------------------------------------

// Base aggregation -- several functions below rely on this as a
// starting dataset
baseAggregation := TABLE
    (
        validData,
        {
            pickup_day_of_week,
            pickup_time_hour,
            UNSIGNED4   cnt := COUNT(GROUP),
            DECIMAL10_2 total_trip_distance := SUM(GROUP, trip_distance)
        },
        pickup_day_of_week, pickup_time_hour,
        FEW
    ) : PERSIST('~cache::daily_pickups_baseaggregation');

//------------------------------------------------------------------------------

// Pickups per hour by day of week
perHourPerDayCount := TABLE
    (
        baseAggregation,
        {
            pickup_time_hour,
            UNSIGNED4   cnt_total := SUM(GROUP, cnt),
            UNSIGNED4   cnt_sunday := SUM(GROUP, IF(pickup_day_of_week = 1, cnt, 0)),
            UNSIGNED4   cnt_monday := SUM(GROUP, IF(pickup_day_of_week = 2, cnt, 0)),
            UNSIGNED4   cnt_tuesday := SUM(GROUP, IF(pickup_day_of_week = 3, cnt, 0)),
            UNSIGNED4   cnt_wednesday := SUM(GROUP, IF(pickup_day_of_week = 4, cnt, 0)),
            UNSIGNED4   cnt_thursday := SUM(GROUP, IF(pickup_day_of_week = 5, cnt, 0)),
            UNSIGNED4   cnt_friday := SUM(GROUP, IF(pickup_day_of_week = 6, cnt, 0)),
            UNSIGNED4   cnt_saturday := SUM(GROUP, IF(pickup_day_of_week = 7, cnt, 0)),
        },
        pickup_time_hour,
        FEW
    );

OUTPUT(SORT(perHourPerDayCount, pickup_time_hour), NAMED('perHourPerDayCount'), ALL);

//------------------------------------------------------------------------------

// Average trip distance per hour by day of week
perHourPerDayAveDistance := TABLE
    (
        baseAggregation,
        {
            pickup_time_hour,
            DECIMAL10_2 dist_total := SUM(GROUP, total_trip_distance) / SUM(GROUP, cnt),
            DECIMAL10_2 dist_sunday := SUM(GROUP, IF(pickup_day_of_week = 1, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_monday := SUM(GROUP, IF(pickup_day_of_week = 2, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_tuesday := SUM(GROUP, IF(pickup_day_of_week = 3, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_wednesday := SUM(GROUP, IF(pickup_day_of_week = 4, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_thursday := SUM(GROUP, IF(pickup_day_of_week = 5, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_friday := SUM(GROUP, IF(pickup_day_of_week = 6, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_saturday := SUM(GROUP, IF(pickup_day_of_week = 7, total_trip_distance / cnt, 0)),
        },
        pickup_time_hour,
        FEW
    );
OUTPUT(SORT(perHourPerDayAveDistance, pickup_time_hour), NAMED('perHourPerDayAveDistance'), ALL);

//------------------------------------------------------------------------------

// Helper function for the next two aggregations
DescribeDayOfWeek(UNSIGNED1 dayOfWeekNum) := CHOOSE
    (
        dayOfWeekNum,
        'Sunday',
        'Monday',
        'Tuesday',
        'Wednesday',
        'Thursday',
        'Friday',
        'Saturday',
        '<Unknown>'
    );

// Pickups per day by hour
perDayPerHourCount := TABLE
    (
        baseAggregation,
        {
            pickup_day_of_week,
            STRING      pickup_day := DescribeDayOfWeek(pickup_day_of_week),
            UNSIGNED4   cnt_total := SUM(GROUP, cnt),
            UNSIGNED4   cnt_00 := SUM(GROUP, IF(pickup_time_hour = 0, cnt, 0)),
            UNSIGNED4   cnt_01 := SUM(GROUP, IF(pickup_time_hour = 1, cnt, 0)),
            UNSIGNED4   cnt_02 := SUM(GROUP, IF(pickup_time_hour = 2, cnt, 0)),
            UNSIGNED4   cnt_03 := SUM(GROUP, IF(pickup_time_hour = 3, cnt, 0)),
            UNSIGNED4   cnt_04 := SUM(GROUP, IF(pickup_time_hour = 4, cnt, 0)),
            UNSIGNED4   cnt_05 := SUM(GROUP, IF(pickup_time_hour = 5, cnt, 0)),
            UNSIGNED4   cnt_06 := SUM(GROUP, IF(pickup_time_hour = 6, cnt, 0)),
            UNSIGNED4   cnt_07 := SUM(GROUP, IF(pickup_time_hour = 7, cnt, 0)),
            UNSIGNED4   cnt_08 := SUM(GROUP, IF(pickup_time_hour = 8, cnt, 0)),
            UNSIGNED4   cnt_09 := SUM(GROUP, IF(pickup_time_hour = 9, cnt, 0)),
            UNSIGNED4   cnt_10 := SUM(GROUP, IF(pickup_time_hour = 10, cnt, 0)),
            UNSIGNED4   cnt_11 := SUM(GROUP, IF(pickup_time_hour = 11, cnt, 0)),
            UNSIGNED4   cnt_12 := SUM(GROUP, IF(pickup_time_hour = 12, cnt, 0)),
            UNSIGNED4   cnt_13 := SUM(GROUP, IF(pickup_time_hour = 13, cnt, 0)),
            UNSIGNED4   cnt_14 := SUM(GROUP, IF(pickup_time_hour = 14, cnt, 0)),
            UNSIGNED4   cnt_15 := SUM(GROUP, IF(pickup_time_hour = 15, cnt, 0)),
            UNSIGNED4   cnt_16 := SUM(GROUP, IF(pickup_time_hour = 16, cnt, 0)),
            UNSIGNED4   cnt_17 := SUM(GROUP, IF(pickup_time_hour = 17, cnt, 0)),
            UNSIGNED4   cnt_18 := SUM(GROUP, IF(pickup_time_hour = 18, cnt, 0)),
            UNSIGNED4   cnt_19 := SUM(GROUP, IF(pickup_time_hour = 19, cnt, 0)),
            UNSIGNED4   cnt_20 := SUM(GROUP, IF(pickup_time_hour = 20, cnt, 0)),
            UNSIGNED4   cnt_21 := SUM(GROUP, IF(pickup_time_hour = 21, cnt, 0)),
            UNSIGNED4   cnt_22 := SUM(GROUP, IF(pickup_time_hour = 22, cnt, 0)),
            UNSIGNED4   cnt_23 := SUM(GROUP, IF(pickup_time_hour = 23, cnt, 0))
        },
        pickup_day_of_week,
        FEW
    );

OUTPUT(SORT(perDayPerHourCount, pickup_day_of_week), NAMED('perDayPerHourCount'), ALL);

//------------------------------------------------------------------------------

// Pickups per hour by day
perDayPerHourAveDistance := TABLE
    (
        baseAggregation,
        {
            pickup_day_of_week,
            STRING      pickup_day := DescribeDayOfWeek(pickup_day_of_week),
            DECIMAL10_2 dist_total := SUM(GROUP, total_trip_distance) / SUM(GROUP, cnt),
            DECIMAL10_2 dist_00 := SUM(GROUP, IF(pickup_time_hour = 0, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_01 := SUM(GROUP, IF(pickup_time_hour = 1, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_02 := SUM(GROUP, IF(pickup_time_hour = 2, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_03 := SUM(GROUP, IF(pickup_time_hour = 3, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_04 := SUM(GROUP, IF(pickup_time_hour = 4, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_05 := SUM(GROUP, IF(pickup_time_hour = 5, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_06 := SUM(GROUP, IF(pickup_time_hour = 6, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_07 := SUM(GROUP, IF(pickup_time_hour = 7, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_08 := SUM(GROUP, IF(pickup_time_hour = 8, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_09 := SUM(GROUP, IF(pickup_time_hour = 9, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_10 := SUM(GROUP, IF(pickup_time_hour = 10, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_11 := SUM(GROUP, IF(pickup_time_hour = 11, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_12 := SUM(GROUP, IF(pickup_time_hour = 12, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_13 := SUM(GROUP, IF(pickup_time_hour = 13, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_14 := SUM(GROUP, IF(pickup_time_hour = 14, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_15 := SUM(GROUP, IF(pickup_time_hour = 15, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_16 := SUM(GROUP, IF(pickup_time_hour = 16, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_17 := SUM(GROUP, IF(pickup_time_hour = 17, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_18 := SUM(GROUP, IF(pickup_time_hour = 18, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_19 := SUM(GROUP, IF(pickup_time_hour = 19, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_20 := SUM(GROUP, IF(pickup_time_hour = 20, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_21 := SUM(GROUP, IF(pickup_time_hour = 21, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_22 := SUM(GROUP, IF(pickup_time_hour = 22, total_trip_distance / cnt, 0)),
            DECIMAL10_2 dist_23 := SUM(GROUP, IF(pickup_time_hour = 23, total_trip_distance / cnt, 0))
        },
        pickup_day_of_week,
        FEW
    );

OUTPUT(SORT(perDayPerHourAveDistance, pickup_day_of_week), NAMED('perDayPerHourAveDistance'), ALL);

//------------------------------------------------------------------------------

/******************************************************************************
 * TODO
 *
 * Create new aggregations by applying TABLE against either the baseAggregation
 * dataset or the full dataset (validData).
 *
 * ECL functions that will accept the GROUP keyword instead of a dataset --
 * which means they can be used within TABLE -- are as follows:
 *
 *      AVE()
 *      COUNT()
 *      MAX()
 *      MIN()
 *      SUM()
 *      VARIANCE()
 *      COVARIANCE()
 *      CORRELATION()
 ******************************************************************************/
