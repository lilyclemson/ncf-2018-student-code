IMPORT Std;

#WORKUNIT('name', 'Validate Taxi Data');

//------------------------------------------------------------------------------

// Record definition for the transformed data
YellowLayout := RECORD
    UNSIGNED4   record_id;
    UNSIGNED1   vendorid;
    STRING19    tpep_pickup_datetime;
    STRING19    tpep_dropoff_datetime;
    UNSIGNED1   passenger_count;
    DECIMAL10_2 trip_distance;
    DECIMAL9_6  pickup_longitude;
    DECIMAL9_6  pickup_latitude;
    UNSIGNED1   rate_code_id;
    STRING1     store_and_fwd_flag;
    DECIMAL9_6  dropoff_longitude;
    DECIMAL9_6  dropoff_latitude;
    UNSIGNED1   payment_type;
    DECIMAL8_2  fare_amount;
    DECIMAL8_2  extra;
    DECIMAL8_2  mta_tax;
    DECIMAL8_2  tip_amount;
    DECIMAL8_2  tolls_amount;
    DECIMAL8_2  improvement_surcharge;
    DECIMAL8_2  total_amount;
END;

taxiData := DATASET
    (
        '~taxi_data::data',
        YellowLayout,
        FLAT
    );

//------------------------------------------------------------------------------

// Record definition that contains validation indicators
ValidatedLayout := RECORD
    YellowLayout;
    BOOLEAN     bad_trip_distance;
    BOOLEAN     bad_passenger_count;
    // TODO: Add more indicators
    BOOLEAN     is_valid_record;
END;

// Rewrite the data into the new layout, adding validation check results
// along the way
validatedData := PROJECT
    (
        taxiData,
        TRANSFORM
            (
                ValidatedLayout,
                SELF.bad_trip_distance := LEFT.trip_distance <= 0 OR LEFT.trip_distance > 200,
                SELF.bad_passenger_count := LEFT.passenger_count < 1 OR LEFT.passenger_count > 6,
                // TODO: Evaluate additional indicators
                // TODO: Don't forget to add indicators to next assignment
                SELF.is_valid_record := NOT (SELF.bad_trip_distance OR SELF.bad_passenger_count),
                SELF := LEFT
            )
    );

OUTPUT(validatedData(is_valid_record), NAMED('ValidRecordSample'));
OUTPUT(validatedData(NOT is_valid_record), NAMED('InvalidRecordSample'));
