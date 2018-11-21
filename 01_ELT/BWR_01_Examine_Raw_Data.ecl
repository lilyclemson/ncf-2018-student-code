IMPORT Std;

#WORKUNIT('name', 'Examine Raw Data');

//------------------------------------------------------------------------------

// Layout of the raw data; everything is a STRING
YellowLayout := RECORD
    STRING  VendorID;
    STRING  tpep_pickup_datetime;
    STRING  tpep_dropoff_datetime;
    STRING  passenger_count;
    STRING  trip_distance;
    STRING  pickup_longitude;
    STRING  pickup_latitude;
    STRING  rate_code_id;
    STRING  store_and_fwd_flag;
    STRING  dropoff_longitude;
    STRING  dropoff_latitude;
    STRING  payment_type;
    STRING  fare_amount;
    STRING  extra;
    STRING  mta_tax;
    STRING  tip_amount;
    STRING  tolls_amount;
    STRING  improvement_surcharge;
    STRING  total_amount;
END;

sampleTaxiData := DATASET
    (
        '~taxi_data::raw::yellow_tripdata_2015-01.csv',
        YellowLayout,
        CSV(HEADING(1))
    );

OUTPUT(sampleTaxiData, NAMED('sampleTaxiData'));

//------------------------------------------------------------------------------

// We were given the precise datatypes for this raw data, so we'll use them
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

sampleWeatherData := DATASET
    (
        '~taxi_data::weather_new_york_city.txt',
        FlatWeatherRec,
        CSV(SEPARATOR('\t'), QUOTE(''))
    );

OUTPUT(sampleWeatherData, NAMED('sampleWeatherData'));
