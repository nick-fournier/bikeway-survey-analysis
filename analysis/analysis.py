import pandas as pd
import os
import numpy as np
import json
from scipy import stats
from pandas.core.common import flatten
from rpy2 import robjects as ro
from rpy2.robjects import pandas2ri
from rpy2.robjects.packages import importr

# Install MASS package, contains polr
# utils = importr('utils')
# utils.chooseCRANmirror(ind=1)
# utils.install_packages('MASS')
# utils.install_packages('broom')

# Initialize packages
MASS = importr('MASS')
broom = importr('broom')
perf = importr('performance')
R = ro.r
pandas2ri.activate()


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
    # 'GROUP': ['STEADFAST', 'RELUCTANT'],
    'DEMOGS': ['AGE', 'GENDER', 'CYCLIST_TYPE', 'CYCLIST_FREQ'] #, 'GROUP']
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

width_height = {
    'POSTPROTECT':      {'WIDTH': 0.5, 'HEIGHT': 3},
    'BUFFERED':         {'WIDTH': 3,   'HEIGHT': 0},
    'RAISED':           {'WIDTH': 1,   'HEIGHT': 0.5},
    'STANDARD':         {'WIDTH': 0.5, 'HEIGHT': 0},
    'PARKINGPROTECT':   {'WIDTH': 12,  'HEIGHT': 5},
    'CURBPROTECT':      {'WIDTH': 3,   'HEIGHT': 0.5}
}

class BikeAnalysis:
    def __init__(self, data_path, out_path):
        self.data_path = self.pathcheck(data_path)
        self.outpath = self.pathcheck(out_path)

        # Cleans up data a bit
        self.data = self.clean_data(self.data_path)

        # Estimates width and height coefficients
        self.buffer_coef_dict, self.buffer_r2_dict, self.dim_data = self.buffer_fit()

        # Gets mean aggregate ranking scores
        self.rank_scores = self.ranking_scores()

        # Get frequency counts for mea
        self.measure_prefs = self.measure_counts()

        # Saving output
        # self.save()
        #
        # # Generate plots
        # self.plot()

    def pathcheck(self, path):
        if os.path.isdir('.' + '/'.join(path.split('/')[:-1])):
            path = '.' + path

        if path[-1] != '/' and not os.path.isfile(path):
            return path + '/'
        else:
            return path

    def clean_data(self, path):
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

        # # Two types of bicyclists
        # typefreq = pd.crosstab(index=cleaned.CYCLIST_TYPE, columns=cleaned.CYCLIST_FREQ)
        # cleaned['GROUP'] = np.where(cleaned.CYCLIST_FREQ.isin(['DAILY', 'WEEKLY']), 'STEADFAST', 'RELUCTANT')

        # choice id
        cleaned.index = np.arange(len(cleaned))
        cleaned.index.name = 'chid'

        return cleaned

    def buffer_fit(self):
        # Reshape to long form
        rank_data = pd.melt(self.data.reset_index(),
                            id_vars=['chid'] + col_dict['DEMOGS'],
                            value_vars=col_dict['RANK_LANE'])

        # Remove prefix
        rank_data.variable = rank_data.variable.str.replace('RANK_LANE_', '')

        # Turn width-height dict to DataFrame
        df_heightwidth = pd.DataFrame.from_records(width_height).T.reset_index().rename(columns={'index': 'variable'})

        # Merge width and height
        rank_data = rank_data.merge(df_heightwidth, on='variable')

        # Pass pandas DF to R DF and run MASS::polr()
        f_list = {'simple': 'factor(value) ~ WIDTH + HEIGHT',
                  'all': 'factor(value) ~ WIDTH + HEIGHT + AGE + CYCLIST_TYPE + CYCLIST_FREQ + GENDER',
                  'byfreq': 'factor(value) ~ (WIDTH + HEIGHT):CYCLIST_FREQ',
                  'bytype': 'factor(value) ~ (WIDTH + HEIGHT):CYCLIST_TYPE',
                  'bytypefreq': 'factor(value) ~ (WIDTH + HEIGHT):(CYCLIST_TYPE+CYCLIST_FREQ)'
                  }

        df_dict = {}
        r2_dict = {}
        for f in f_list:
            print(f_list[f])
            null_model = MASS.polr(formula='factor(value) ~ 1', data=rank_data, Hess=False)
            model_results = MASS.polr(formula=f_list[f], data=rank_data, Hess=False)
            df_results = R.tidy(model_results)
            # rsquared = float(1 - R.logLik(model_results) / R.logLik(null_model))
            rsquared = float(R.r2(model_results).rx2['R2_Nagelkerke'])

            # Get Degrees of Freedom
            n = rank_data.shape[0]
            nvars = df_results.loc[df_results['coef.type'] == 'coefficient'].shape[0]
            DegFree = n - nvars - 1

            # Calc p-value
            df_results['p.value'] = stats.t.sf(np.abs(df_results.statistic), DegFree) * 2

            # Add to output list
            df_dict[f] = df_results
            r2_dict[f] = rsquared

        return df_dict, r2_dict, rank_data

    def ranking_scores(self):
        agg_cols = {}
        for var in ['RANK_CRIT', 'RANK_VIS', 'RANK_DEBRIS', 'RANK_LANE']:
            agg_cols[var] = self.data[col_dict[var]].mean(axis=0).to_dict()

        return agg_cols

    def measure_counts(self):
        agg_meas = {}
        for c in col_dict['PREF_MEAS']:
            count = {'AREA': 0, 'VOLUME': 0, 'WEIGHT': 0, 'DEPTH': 0}
            count.update(self.data[c].value_counts().to_dict())
            agg_meas[c] = count

        return pd.DataFrame(agg_meas).T

    def save(self):
        # Save to CSV
        self.data.to_csv(self.outpath + '/cleaned_survey_data.csv')
        self.dim_data.to_csv(self.outpath + '/buffer_dim_data.csv')
        self.measure_prefs.to_csv(self.outpath + '/measurement_prefs.csv')
        # self.buffer_coef.to_csv(self.outpath + '/buffer_coefs.csv')

        for df in self.buffer_coef_dict:
            self.buffer_coef_dict[df].to_csv(self.outpath + '/buffer_coefs' + df + '.csv')


        # Serializing to json string
        json_object = json.dumps(self.rank_scores, indent=4)
        # Writing to .json
        with open(self.outpath + '/avg_ranks.json', 'w') as f:
            f.write(json_object)

        # Serializing to json string
        json_object = json.dumps(self.buffer_r2_dict, indent=4)
        # Writing to .json
        with open(self.outpath + '/buffer_r2.json', 'w') as f:
            f.write(json_object)

    def plot(self):
        R.source('R_plotting.R')

if __name__ == "__main__":
    path = './data/QUALITY OF RIDE _ BICYCLE EDITION.csv.zip'
    results = BikeAnalysis(data_path=path, out_path='./output/')

    results.save()
    # results.plot()
