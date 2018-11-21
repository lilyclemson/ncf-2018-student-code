IMPORT Std;

#WORKUNIT('name', 'Taxi Data: GROUP and more');

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

// Helper function
DescribeBoroughID(UNSIGNED1 boroughID) := CHOOSE
    (
        boroughID,
        'The Bronx',
        'Manhattan',
        'Brooklyn',
        'Staten Island',
        'Queens',
        '<Unknown>'
    );

// Further restrict our data to only those fares that traveled between
// known boroughs
boroughTrips := validData((pickup_borough_id BETWEEN 1 AND 5) AND (dropoff_borough_id BETWEEN 1 AND 5) AND (pickup_borough_id != dropoff_borough_id));

// We actually don't need all those fields; let's slim things down
// to just the fields we need
slimTrips := TABLE(boroughTrips, {pickup_borough_id, dropoff_borough_id, tpep_pickup_datetime, total_amount});

// Distribute our data to put all records with matching borough pairs on one
// node; this allows us to use LOCAL to direct the compiler to operate on only
// the data local to each node, independently
distributedTrips := DISTRIBUTE(slimTrips, HASH32(pickup_borough_id, dropoff_borough_id));

// Group the data around the pickup/dropoff boroughs; must be sorted first;
// note the use of LOCAL
sortedTrips := SORT(distributedTrips, pickup_borough_id, dropoff_borough_id, LOCAL);
groupedTrips := GROUP(sortedTrips, pickup_borough_id, dropoff_borough_id, LOCAL);

// Find the top 10 highest-fare trips between each borough-to-borough combination;
// in case of a tie, the earlier trip wins
topTrips := TOPN(groupedTrips, 10, -total_amount, tpep_pickup_datetime);

// Change the borough IDs to names and number the top total amounts; note
// that the data is still grouped; COUNTER will reset to one at the beginning
// of each group
topTripsRewrite := PROJECT
    (
        topTrips,
        TRANSFORM
            (
                {
                    STRING      pickup_borough,
                    STRING      dropoff_borough,
                    UNSIGNED1   rank,
                    DECIMAL8_2  total_amount
                },
                SELF.pickup_borough := DescribeBoroughID(LEFT.pickup_borough_id),
                SELF.dropoff_borough := DescribeBoroughID(LEFT.dropoff_borough_id),
                SELF.rank := COUNTER,
                SELF.total_amount := LEFT.total_amount
            )
    );

// Ungroup the data
topTenBoroughTrips := UNGROUP(topTripsRewrite);

OUTPUT(topTenBoroughTrips, NAMED('topTenBoroughTrips'), ALL);

// Now find the highest average of the top 10
topTenAverage := TABLE
    (
        topTenBoroughTrips,
        {
            pickup_borough,
            dropoff_borough,
            DECIMAL8_2  ave_total_amount := AVE(GROUP, total_amount)
        },
        pickup_borough, dropoff_borough
    );

OUTPUT(SORT(topTenAverage, -ave_total_amount), NAMED('BestAverageFromTop10'));
