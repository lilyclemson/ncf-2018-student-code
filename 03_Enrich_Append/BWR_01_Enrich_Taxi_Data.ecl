IMPORT Std;

#WORKUNIT('name', 'Enrich Taxi Data');

//------------------------------------------------------------------------------

// Record definition for the transformed and validated data
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
END;

taxiData := DATASET
    (
        '~taxi_data::data_validated',
        YellowLayout,
        FLAT
    );

//------------------------------------------------------------------------------

/*******************************************************************************
 * Std.Date datatypes and functions cited in presentation
 *
 * Std.Date.Date_t - numeric date in YYYYMMDD format
 * Std.Date.Time_t - numeric time in HHMMSS format
 *
 * Std.Date.FromStringToDate(STRING s, STRING format)
 * Std.Date.FromStringToTime(STRING s, STRING format)
 * Std.Date.Year(Date_t d)
 * Std.Date.Month(Date_t d)
 * Std.Date.Day(Date_t d)
 * Std.Date.Hour(Time_t t)
 * Std.Date.Minute(Time_t t)
 * Std.Date.Second(Time_t t)
 * Std.Date.DayOfWeek(Date_t d)
 * Std.Date.DaysBetween(Date_t d)
 ******************************************************************************/

// Record definition that contains new fields extracted from elsewhere
// in each record
EnrichedLayout := RECORD
    YellowLayout;
    Std.Date.Date_t         pickup_date;
    Std.Date.Time_t         pickup_time;
    UNSIGNED1               pickup_day_of_week;
    // TODO: Add more fields representing data to extract
END;

// Rewrite the data into the new layout, adding extraction results
// along the way
enrichedTaxiData := PROJECT
    (
        taxiData,
        TRANSFORM
            (
                EnrichedLayout,
                SELF.pickup_date := Std.Date.FromStringToDate(LEFT.tpep_pickup_datetime[..10], '%Y-%m-%d'),
                SELF.pickup_time := Std.Date.FromStringToTime(LEFT.tpep_pickup_datetime[12..], '%H:%M:%S'),
                SELF.pickup_day_of_week := Std.Date.DayOfWeek(SELF.pickup_date),
                // TODO: Compute values for the added fields
                SELF := LEFT
            )
    );

OUTPUT(enrichedTaxiData, NAMED('enrichedTaxiData'));
