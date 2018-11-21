IMPORT Std;

#WORKUNIT('name', 'Transform Raw Data');

//------------------------------------------------------------------------------

// Layout of Taxi data using more precise datatypes; HPCC Systems will
// automatically perform the necessary conversions while reading the raw data
CoercedYellowLayout := RECORD
    UNSIGNED1   VendorID;
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

// Constructing a "temporary superfile" path in order to combine the separate
// raw files in one step
rawTaxiTempSuperfilePath := '~{'
    + 'taxi_data::raw::yellow_tripdata_2015-01.csv,'
    + 'taxi_data::raw::yellow_tripdata_2015-02.csv,'
    + 'taxi_data::raw::yellow_tripdata_2015-03.csv,'
    + 'taxi_data::raw::yellow_tripdata_2015-04.csv,'
    + 'taxi_data::raw::yellow_tripdata_2015-05.csv,'
    + 'taxi_data::raw::yellow_tripdata_2015-06.csv,'
    + 'taxi_data::raw::yellow_tripdata_2015-07.csv,'
    + 'taxi_data::raw::yellow_tripdata_2015-08.csv,'
    + 'taxi_data::raw::yellow_tripdata_2015-09.csv,'
    + 'taxi_data::raw::yellow_tripdata_2015-10.csv,'
    + 'taxi_data::raw::yellow_tripdata_2015-11.csv,'
    + 'taxi_data::raw::yellow_tripdata_2015-12.csv,'
    + 'taxi_data::raw::yellow_tripdata_2016-01.csv,'
    + 'taxi_data::raw::yellow_tripdata_2016-02.csv,'
    + 'taxi_data::raw::yellow_tripdata_2016-03.csv,'
    + 'taxi_data::raw::yellow_tripdata_2016-04.csv,'
    + 'taxi_data::raw::yellow_tripdata_2016-05.csv,'
    + 'taxi_data::raw::yellow_tripdata_2016-06.csv'
    +'}';

// Get all of the raw data, with better datatypes, into one dataset
rawTaxiData := DATASET
    (
        rawTaxiTempSuperfilePath,
        CoercedYellowLayout,
        CSV(HEADING(1))
    );

// Define a record_id field
NewYellowLayout := RECORD
    UNSIGNED4   record_id;
    CoercedYellowLayout;
END;

// Assign record_id values
eltTaxiData := PROJECT
    (
        rawTaxiData,
        TRANSFORM
            (
                NewYellowLayout,
                SELF.record_id := COUNTER,
                SELF := LEFT
            )
    );

// In real life, you would create a new file containing this transformed data
OUTPUT(eltTaxiData, NAMED('eltTaxiData'));

//------------------------------------------------------------------------------

// Because we were given precise datatypes for the weather data, and we are
// not appending record_id values to the dataset, this really just a repeat
// of reading the raw file
FlatWeatherRec := RECORD
    Std.Date.Date_t         date;
    Std.Date.Seconds_t      minutes_after_midnight;
    STRING                  summary;
    DECIMAL6_3              temperature;
    UDECIMAL6_3             precipIntensity;
    STRING                  precipType;
    UDECIMAL4_2             windSpeed;
    UDECIMAL4_2             visibility;
    UDECIMAL4_2             cloudCover;
END;

eltWeatherData := DATASET
    (
        '~taxi_data::weather_new_york_city.txt',
        FlatWeatherRec,
        CSV(SEPARATOR('\t'), QUOTE(''))
    );

// In real life, you would create a new file containing this transformed data
OUTPUT(eltWeatherData, NAMED('eltWeatherData'));
