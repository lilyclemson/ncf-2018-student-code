IMPORT Std;

#WORKUNIT('name', 'Taxi Data: Append');

//------------------------------------------------------------------------------

// Record definition for the enriched taxi data
YellowLayout := RECORD
	UNSIGNED4		record_id;
	UNSIGNED1		vendorid;
	STRING19		tpep_pickup_datetime;
	STRING19		tpep_dropoff_datetime;
	UNSIGNED1		passenger_count;
	DECIMAL10_2		trip_distance;
	DECIMAL9_6		pickup_longitude;
	DECIMAL9_6		pickup_latitude;
	UNSIGNED1		rate_code_id;
	STRING1			store_and_fwd_flag;
	DECIMAL9_6		dropoff_longitude;
	DECIMAL9_6		dropoff_latitude;
	UNSIGNED1		payment_type;
	DECIMAL8_2		fare_amount;
	DECIMAL8_2		extra;
	DECIMAL8_2		mta_tax;
	DECIMAL8_2		tip_amount;
	DECIMAL8_2		tolls_amount;
	DECIMAL8_2		improvement_surcharge;
	DECIMAL8_2		total_amount;
	BOOLEAN			bad_trip_distance;
	BOOLEAN			bad_passenger_count;
	BOOLEAN			bad_pickup_coordinates;
	BOOLEAN			bad_dropoff_coordinates;
	BOOLEAN			bad_fare_amount;
	BOOLEAN			bad_tip_amount;
	BOOLEAN			bad_tolls_amount;
	BOOLEAN			bad_improvement_surcharge;
	BOOLEAN			bad_total_amount;
	BOOLEAN			is_valid_record;
	UNSIGNED4		pickup_date;
	UNSIGNED3		pickup_time;
	UNSIGNED2		pickup_minutes_after_midnight;
	UNSIGNED2		pickup_time_window;
	UNSIGNED1		pickup_time_hour;
	UNSIGNED1		pickup_day_of_week;
	BOOLEAN			pickup_date_is_holiday;
	UNSIGNED4		dropoff_date;
	UNSIGNED3		dropoff_time;
	UNSIGNED2		dropoff_minutes_after_midnight;
	UNSIGNED1		dropoff_time_window;
	UNSIGNED1		dropoff_time_hour;
	UNSIGNED1		dropoff_day_of_week;
	UNSIGNED2		trip_duration_minutes;
	UNSIGNED2		trip_distance_bucket;
END;

taxiData := DATASET
    (
        '~taxi_data::data_validated_enriched',
        YellowLayout,
        FLAT
    );

OUTPUT(taxiData, NAMED('EnrichedTaxiData'));

//------------------------------------------------------------------------------

WeatherLayout := RECORD
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

weatherData := DATASET
    (
        '~taxi_data::weather_new_york_city',
        WeatherLayout,
        FLAT
    );

OUTPUT(weatherData, NAMED('WeatherData'));

//------------------------------------------------------------------------------

TaxiWeatherLayout := RECORD
    YellowLayout;
    WeatherLayout   weather; // Embedded child record
END;

taxiWithWeather := JOIN
    (
        taxiData,
        weatherData,
        LEFT.pickup_date = RIGHT.Date
            AND RIGHT.minutes_after_midnight BETWEEN LEFT.pickup_minutes_after_midnight - 30 AND LEFT.pickup_minutes_after_midnight + 30,
        TRANSFORM
            (
                TaxiWeatherLayout,
                SELF.weather := RIGHT,
                SELF := LEFT
            ),
        LOOKUP, LEFT OUTER
    );

OUTPUT(taxiWithWeather, NAMED('TaxiWithWeather'));

//------------------------------------------------------------------------------

// Inline dataset defining NYC boroughs and their GPS-based bounding boxes
boroughsBoundingBoxes := DATASET
    (
        [
            {1, 'The Bronx', 40.917577, 40.785743, -73.748060, -73.933808},
            {2, 'Manhattan', 40.882214, 40.680396, -73.907000, -74.047285},
            {3, 'Brooklyn', 40.739446, 40.551042, -73.833365, -74.056630},
            {4, 'Staten Island', 40.651812, 40.477399, -74.034547, -74.259090},
            {5, 'Queens', 40.812242, 40.489794, -73.700272, -73.833365}
        ],
        {
            UNSIGNED1   id;
            STRING      burroughs_name;
            DECIMAL9_6  north;
            DECIMAL9_6  south;
            DECIMAL9_6  east;
            DECIMAL9_6  west;
        }
    );

TaxiBoroughLayout := RECORD
    TaxiWeatherLayout;
    UNSIGNED1               pickup_borough_id;
    UNSIGNED1               dropoff_borough_id;
END;

/******************************************************************************
 * TODO
 *
 * Create JOIN functions that append the ID of the borough from which a taxi
 * fare was picked up and in which the fare was dropped off.  Populate the
 * TaxiBoroughLayout record with the results.
 *
 * Hint #1:  This will take two JOINs.  The second will use the result of
 *           the first.
 * Hint #2:  Both JOINs should use the ALL option.
 ******************************************************************************/
