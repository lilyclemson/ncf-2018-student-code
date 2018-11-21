IMPORT Std;

#WORKUNIT('name', 'Examine Profiling Results');

//------------------------------------------------------------------------------

// Record definition of saved profiling results

ModeRec := RECORD
    STRING                      value;
    UNSIGNED4                   rec_count;
END;

PatternCountRec := RECORD
    STRING                      data_pattern;
    UNSIGNED4                   rec_count;
    STRING                      example;
END;

ProfileLayout := RECORD
    STRING                      attribute;
    STRING                      given_attribute_type;
    STRING                      best_attribute_type;
    UNSIGNED4                   rec_count;
    UNSIGNED4                   fill_count;
    DECIMAL9_6                  fill_rate;
    UNSIGNED4                   cardinality;
    DATASET(ModeRec)            modes{MAXCOUNT(5)};
    UNSIGNED4                   min_length;
    UNSIGNED4                   max_length;
    UNSIGNED4                   ave_length;
    DATASET(PatternCountRec)    popular_patterns{MAXCOUNT(100)};
    DATASET(PatternCountRec)    rare_patterns{MAXCOUNT(100)};
END;

//------------------------------------------------------------------------------

taxiProfile := DATASET
    (
        '~taxi_data::raw_taxi_data_profile',
        ProfileLayout,
        FLAT
    );

OUTPUT(taxiProfile, NAMED('taxiProfile'));

//------------------------------------------------------------------------------

// The profile results for both taxi and weather datasets use the same record
// definition, so we'll reuse it here
weatherProfile := DATASET
    (
        '~taxi_data::weather_new_york_city_data_profile',
        ProfileLayout,
        FLAT
    );

OUTPUT(weatherProfile, NAMED('weatherProfile'));
