import pandas as pd
from pandas.core.common import flatten
import numpy as np
# import os
# import scipy.stats as stats

# Can't get this to work for some reason... will analyze in R for now
# from statsmodels.miscmodels.ordinal_model import OrderedModel

new_colnames = {'Timestamp': 'TIMESTAMP',
                'Username': 'USERNAME',
                'CRITERIA RANK: [Structural Integrity]': 'RANK_CRIT_STRUCTURAL',
                'CRITERIA RANK: [Functionality]': 'RANK_CRIT_FUNCTIONAL',
                'CRITERIA RANK: [Maintenance]': 'RANK_CRIT_MAINTENANCE',
                'VISIBILITY RANK: [Ellery]': 'RANK_VIS_ELLERY',
                'VISIBILITY RANK: [Embarcadero]': 'RANK_VIS_EMBARCADERO',
                'VISIBILITY RANK: [Hampshire]': 'RANK_VIS_HAMPSHIRE',
                'VISIBILITY RANK: [Broadway]': 'RANK_VIS_BROADWAY',
                'VISIBILITY RANK: [Potrero]': 'RANK_VIS_POTRERO',
                'VISIBILITY RANK: [Meridian]': 'RANK_VIS_MERIDIAN',
                'DEBRIS RATING [Non-puncture hazards]': 'RANK_DEBRIS_NON_PUNCT',
                'DEBRIS RATING [Particulate debris]': 'RANK_DEBRIS_PARTICLES',
                'DEBRIS RATING [Precipitation Snow/Ice/Puddles]': 'RANK_DEBRIS_PRECIP',
                'DEBRIS RATING [Slip hazards]': 'RANK_DEBRIS_SLIP',
                'DEBRIS RATING [Puncture hazards]': 'RANK_DEBRIS_PUNCT',
                'DEBRIS MEASUREMENT: [Non-puncture hazards]': 'DEBRIS_MEAS_NON_PUNCT',
                'DEBRIS MEASUREMENT: [Particulate debris]': 'DEBRIS_MEAS_PARTICLES',
                'DEBRIS MEASUREMENT: [Precipitation Snow/Ice]': 'DEBRIS_MEAS_PRECIP',
                'DEBRIS MEASUREMENT: [Slip hazards]': 'DEBRIS_MEAS_SLIP',
                'DEBRIS MEASUREMENT: [Puncture hazards]': 'DEBRIS_MEAS_PUNCT',
                'BIKE LANE RANKING: [Post protected]': 'RANK_LANE_POSTPROTECT',
                'BIKE LANE RANKING: [Buffered]': 'RANK_LANE_BUFFERED',
                'BIKE LANE RANKING: [Raised]': 'RANK_LANE_RAISED',
                'BIKE LANE RANKING: [Standard]': 'RANK_LANE_STANDARD',
                'BIKE LANE RANKING: [Parking protected]': 'RANK_LANE_PARKINGPROTECT',
                'BIKE LANE RANKING: [Curb protected]': 'RANK_LANE_CURBPROTECT',
                'Are there other pavement features missing that you think are especially'
                ' important to you? (leave blank otherwise)': 'OR_FEATURES',
                'Do you have another suggested measurement? or a specific refinement?'
                ' (leave blank otherwise)': 'OR_MEASUREMENTS',
                'If road space wasn\'t a problem, how much horizontal distance you would'
                ' put between the bike lane and car lanes before it doesn\'t make you feel'
                ' any safer or comfortable? For reference, we\'ll measure in "car widths",'
                ' where 1 car width is about 9ft (~2.7m) and 5 car widths is 45 ft': 'MAX_BUFFER',
                'What is your age? (type a number in years)': 'AGE',
                'What is your gender?': 'GENDER',
                'Do you consider yourself a cyclist?': 'CYCLIST_TYPE',
                'How often do you bike?': 'CYCLIST_FREQ'
                }

col_dict = {
    'IDS': ['TIMESTAMP', 'USERNAME'],
    'RANK_CRIT': ['RANK_CRIT_STRUCTURAL', 'RANK_CRIT_FUNCTIONAL', 'RANK_CRIT_MAINTENANCE'],
    'RANK_VIS': ['RANK_VIS_ELLERY', 'RANK_VIS_EMBARCADERO', 'RANK_VIS_HAMPSHIRE',
                 'RANK_VIS_BROADWAY', 'RANK_VIS_POTRERO', 'RANK_VIS_MERIDIAN'],
    'RANK_DEBRIS': ['RANK_DEBRIS_NON_PUNCT', 'RANK_DEBRIS_PARTICLES',
                    'RANK_DEBRIS_PRECIP', 'RANK_DEBRIS_SLIP', 'RANK_DEBRIS_PUNCT'],
    'RANK_LANE': ['RANK_LANE_POSTPROTECT', 'RANK_LANE_BUFFERED', 'RANK_LANE_RAISED',
                  'RANK_LANE_STANDARD', 'RANK_LANE_PARKINGPROTECT', 'RANK_LANE_CURBPROTECT'],
    'PREF_MEAS': ['DEBRIS_MEAS_NON_PUNCT', 'DEBRIS_MEAS_PUNCT', 'DEBRIS_MEAS_SLIP',
                  'DEBRIS_MEAS_PARTICLES', 'DEBRIS_MEAS_PRECIP'],
    'OPEN_RESPONSE': ['OR_MEASUREMENTS', 'OR_FEATURES', 'MAX_BUFFER'],
    'DEMOGS': ['AGE', 'GENDER', 'CYCLIST_TYPE', 'CYCLIST_FREQ']
}

cyclist_type = {'Commuter (even if only pre-pandemic)':                 'COMMUTER',
                'Wouldn\'t go on my own, but don\'t mind biking':       'SOCIAL',
                'Recreational for fitness':                             'RECREATIONAL',
                'Recreational as casual activity with family/friends':  'SOCIAL-REC',
                'You couldn\'t get me on a bike':                       'NONCYCLIST'
                }

cyclist_freq = {'Almost daily, at least once a week if possible':   'DAILY',
                'More than once a month but not most days':         'WEEKLY',
                'A few times a year but less than once a month':    'MONTHLY',
                'Rarely, if ever.':                                 'RARELY'
                }


def clean_data(path):
    data = pd.read_csv(path)

    # Check for duplicates & rename columns for clarity
    cleaned = data.loc[~data.iloc[:, 1::].duplicated(), ].rename(columns=new_colnames)

    # Cols to clean convert to integer
    cols = list(flatten([col_dict[k] for k in ['RANK_CRIT', 'RANK_VIS', 'RANK_DEBRIS', 'RANK_LANE']]))

    # Convert ranks to integer
    cleaned = cleaned.apply(lambda x: x.str.slice(0, 1).astype(int) if x.name in cols else x)

    # Clean up measurement response
    cleaned = cleaned.apply(lambda x: x.str.upper().str.replace('BY ', '') if x.name in col_dict['PREF_MEAS'] else x)

    # Clean up cyclist type & cycling frequency
    cleaned.CYCLIST_TYPE = cleaned.CYCLIST_TYPE.replace(cyclist_type)
    cleaned.CYCLIST_FREQ = cleaned.CYCLIST_FREQ.replace(cyclist_freq)

    # choice id
    cleaned.index = np.arange(len(cleaned))
    cleaned.index.name = 'chid'

    # Saves a copy in the same folder
    fname = '/'.join(path.split('/')[:-1] + ['/cleaned_' + path.split('/')[-1]])
    cleaned.to_csv(fname)


if __name__ == "__main__":
    clean_data('../data/survey_data_download_latest.csv')
