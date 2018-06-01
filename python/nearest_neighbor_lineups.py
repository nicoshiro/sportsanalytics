import math
import copy
from itertools import combinations
from collections import defaultdict

import pandas as pd
import numpy as np
from scipy.special import binom
from scipy.spatial.distance import pdist, squareform
from progress.bar import Bar
from sklearn.model_selection import KFold
import matplotlib.pyplot as plt

from stint import Stint


def normalize_df(df):
    '''
    Normalize a pandas DataFrame object column-wise.

    Parameters
    --------------------
        df              -- pandas.DataFrame, data frame to normalize
    Returns
    --------------------
        df_normalized   -- pandas.DataFrame, normalized data frame

    '''
    return (df-df.mean())/df.std()


def compute_similarity_matrix(df, distance_metric):
    '''
    Compute a pairwise (by row) similarity matrix from a pandas DataFrame.

    Parameters
    --------------------
        df                  -- pandas.DataFrame
        distance_metric     -- str, distance metric for pdist function
    Returns
    --------------------
        similarity_matrix   -- pandas.DataFrame

    '''
    # get distance matrix
    distance_matrix = pd.DataFrame(squareform(pdist(df,
                                                    metric=distance_metric)),
                                   columns=df.index,
                                   index=df.index)

    # turn distance matrix into similarity matrix by adding 1
    # TODO: assumes metric used computes distance as (1-similarity)
    similarity_matrix = 1 - distance_matrix

    return similarity_matrix


def get_similarity_matrix(player_data, distance_metric, normalize):
    '''
    Get similarity matrix for players in player_data.

    Parameters
    --------------------
        player_data         -- pandas.DataFrame, individual player data
        distance_metric     -- str, distance metric for pdist function
        normalize           -- bool, whether or not to normalize the data before
                               applying the distance metric
    Returns
    --------------------
        similarity_matrix   -- pandas.DataFrame, similarity matrix
    '''
    # get list of player names
    player_names = pd.Series.tolist(player_data['Player'])

    # set names as row indices
    idx_rename_dict = dict(zip(player_data.index, player_names))
    player_data = player_data.rename(index=idx_rename_dict)

    # select stats of interest
    stats_of_interest = ['Age', 'FG', 'FGA', '2P', '2PA', '3P', '3PA', 'FT',
                         'FTA', 'ORB', 'DRB', 'TRB', 'AST', 'STL', 'BLK', 'TOV',
                         'PF']
    player_stats = player_data[stats_of_interest]

    if normalize:
        # normalize player stats
        player_stats_norm = normalize_df(player_stats)

    # create similarity matrix from normalized stats
    similarity_matrix =  compute_similarity_matrix(player_stats_norm,
                                                   distance_metric)

    return similarity_matrix


def create_adjacency_matrix(player_names, similarity_matrix, group_size=5):
    '''
    Get adjacency matrix for all possible stints of size group_size from players
    in player_data where weights are the similarity between stints.

    Parameters
    --------------------
        player_names         -- list of str, names of relevant players
        similarity_matrix   -- pandas.DataFrame, similarity matrix for all
                               players in player_data
        group_size          -- int, size of the combinations to consider,
                               default 5 for a regular lineup

    Returns
    --------------------
        adjacency_matrix    -- pandas.DataFrame, adjacency matrix for all stints
    '''
    # get all combinations of size group_size
    player_combinations = list(combinations(player_names, group_size))
    num_combos = len(player_combinations)

    # initialize adjacency matrix with zeros
    adjacency_matrix = np.zeros((num_combos, num_combos))
    # adjacency_matrix = lil_matrix((num_combos, num_combos))

    # loop through all pairs of combinations and store similarity
    progress_bar = Bar('Processing', max=num_combos*num_combos)
    for row, player_combo_1 in enumerate(player_combinations):
        for i, player_combo_2 in enumerate(player_combinations[row:]):
            col = row + i
            stint_1 = Stint(player_combo_1)
            stint_2 = Stint(player_combo_2)
            similarity = stint_1.similarity(stint_2, similarity_matrix)
            adjacency_matrix[row, col] = similarity
            progress_bar.next()
    progress_bar.finish()

    # get rid of ones on the diagonal for taking higher powers
    adjacency_matrix -= np.eye(num_combos)

    # fill in bottom half of matrix (matrix is symmetric)
    adjacency_matrix += np.transpose(adjacency_matrix)

    # turn in to pandas DataFrame to set names
    names = [','.join(group) for group in player_combinations]
    adjacency_matrix = pd.DataFrame(adjacency_matrix,
                                    columns=names,
                                    index=names)

    return adjacency_matrix


def get_player_combo_indicator(player_combo, stint_data):
    '''
    Get indicator column representing when all players in player_combo are
    observed together and whether they are the home or away team for each
    observation.

    Parameters
    --------------------
        player_combo    -- iterable of str, names of players of interest
        stint_data      -- pandas.DataFrame, stint table for relevant
                           players

    Returns
    --------------------
        home_or_away    -- pandas.Series of int, indicator with +1 for home
                           appearances and -1 for away
    '''
    # add up player columns
    indicator = np.zeros(len(stint_data))
    for player_name in player_combo:
        try:
            indicator += stint_data.loc[:,player_name]
        except KeyError:
            # print('Warning: {} not in stint data.'.format(player_name))
            pass

    # store indicator get if they are home or away for plus-minus calculation
    # later
    home_or_away = indicator

    # if magnitude is group_size then all were on the court together
    group_size = len(player_combo)
    indicator = abs(indicator) == group_size

    # make home or away 1 for home and -1 for away
    home_or_away = (home_or_away * indicator) / group_size

    return home_or_away


def get_plus_minus_vector(player_names, stint_data, group_size=5):
    '''
    Get vector of plus-minus values for all possible combinations of size
    group_size of players in player_names.

    Parameters
    --------------------
        player_names    -- iterable of str, names of relevant players
        stint_data      -- pandas.DataFrame, stint table for relevant
                           players
        group_size      -- int, size of the combinations to consider,
                           default 5 for a regular lineup

    Returns
    --------------------
        pm_vector       -- pandas.Series, plus-minus score or NaN if never
                           observed for each group
    '''
    # get all combinations of size group_size
    player_combinations = list(combinations(player_names, group_size))
    num_combos = len(player_combinations)

    # initialize pm_vector with NaN
    pm_vector = np.empty(num_combos)
    pm_vector[:] = np.nan

    # loop through each combination and store plus-minus
    for i, player_combo in enumerate(player_combinations):
        # get indicator for when all players on the court
        home_or_away = get_player_combo_indicator(player_combo, stint_data)
        # make boolean version for subsetting stint_data
        indicator = home_or_away.astype(bool)
        # only look for plus-minus if players seen together
        if any(indicator):
            # grab plus minus scaled by home or away indicator value
            plus_minus_values = stint_data.loc[:, 'pt.diff.per.min'][indicator] * home_or_away[indicator]
            # TODO: could just grab 'pt.differential' and not have to scale by minutes_played
            # take weighted sum of plus minus using minutes played -- sum(MP_i * PM_i)/sum(MP_i)
            minutes_played = stint_data.loc[:, 'minutes_played'][indicator]
            plus_minus_values *= minutes_played
            pm_vector[i] = plus_minus_values.values.sum() / minutes_played.values.sum()

    group_names = [','.join(group) for group in player_combinations]
    pm_vector = pd.Series(pm_vector, index=group_names)

    return pm_vector


def predict_plus_minus(adjacency_matrix, plus_minus_vector):
    '''
    Predict the plus-minus of a player combination using adjacency matrix and
    observed plus-minus values.

    Parameters
    --------------------
        player_combos       -- iterable of iterable of str, combos of players
                               for which to predict plus-minus
        adjacency_matrix    -- pandas.DataFrame, weighted adjacency matrix for
                               similarity measure between groups (should be in
                               final form i.e. already raised to a power etc.)
        plus_minus_vector   -- pandas.Series, observed plus-minus scores for all
                               possible player combinations
    Returns
    --------------------
        plus_minus_pred     -- pandas.Series, plus-minus prediction for each
                               combo in player_combos
    '''
    # # TODO: add 1 back to diagonal for adjacency matrix to incorporate observed value if already seen
    # adjacency_matrix += np.eye(len(adjacency_matrix))

    # multiply each row of adjacency matrix by pm vector
    plus_minus_pred = adjacency_matrix * plus_minus_vector

    # # compute row means ignoring NaN values
    # TODO: should not be mean with probability matrix
    # plus_minus_pred = plus_minus_pred.mean(axis=1, skipna=True)

    # compute row sums ignoring NaN values
    # TODO: row sums instead of means now assumes we have a probability matrix
    plus_minus_pred = plus_minus_pred.sum(axis=1, skipna=True)

    # ignore imputed value if there is an observed value
    # TODO: try this
    # plus_minus_pred *= np.isnan(plus_minus_vector)
    # plus_minus_pred = np.nansum(
    #     np.dstack((plus_minus_pred, plus_minus_vector)),
    #                2).flatten()

    # turn into named pandas Series
    plus_minus_pred = pd.Series(plus_minus_pred, index=adjacency_matrix.index)

    return plus_minus_pred


def load_adjacency_matrix(team_id):
    '''Load adjacency matrix for team with ID team_id.'''
    filename = '../data/adj_mats/adj_mat_{}.tsv'.format(team_id)
    adjacency_matrix = pd.read_csv(filename,
                                   header=0,
                                   index_col=0,
                                   sep='\t')

    return adjacency_matrix


def performance(player_names, stints_train, stints_test, adjacency_matrix,
                metric, group_size=5, verbose=False):
    '''
    Measure prediction performance via metric on player combos of size
    group_size from stints_test using observed plus-minus values from
    stints_train only and adjacency matrix constructed from stints_train.


    Parameters
    --------------------
        player_names        -- iterable of str, names of relevant players
                               for which to predict plus-minus
        stints_train        -- pandas.DataFrame, stint table (training data)
        stints_test         -- pandas.DataFrame, stint table (test data)
        adjacency_matrix    -- pandas.DataFrame, weighted adjacency matrix for
                               similarity measure between groups (should be in
                               final form i.e. already raised to a power etc.)
        metric              -- str, accepted: 'RMSE', 'MAE', 'both'
        group_size          -- int, size of player combinations to consider,
                               default 5. Note: must agree with group_size used
                               to create adjacency_matrix
    Returns
    --------------------
        score               -- float, performance as measured by metric (if
                               single metric given)
                               dict, performance dictionary (if 'all' given as
                               metric)
    '''
    if verbose:
        print('\t\tGetting observed plus-minus values in training data')
    # get observed plus-minus values from stints_train for all possible
    # player combinations
    pm_vector = get_plus_minus_vector(
        player_names=player_names,
        stint_data=stints_train,
        group_size=group_size)

    # print('Observed PM vector sparsity:', compute_matrix_sparsity(pd.DataFrame(pm_vector)))

    if verbose:
        print('\t\tGetting observed plus-minus values in test data')
    # get observed plus-minus values from stints_train for all possible
    # player combinations
    # TODO: this is saying we want to predict their minutes_played-weighted average plus-minus (NOT INDIVIDUAL OBSERVATIONS)
    actual_values = get_plus_minus_vector(
        player_names=player_names,
        stint_data=stints_test,
        group_size=group_size)

    if verbose:
        num_combos = int(binom(len(player_names), group_size))
        print('\t\tPredicting plus-minus for {} stints...'.format(num_combos))
    # compare observed values to predicted values, where prediction uses
    # only plus-minus values from stints_train
    predicted_values = predict_plus_minus(adjacency_matrix,
                                          pm_vector)
    # plot results
    # plt.plot(actual_values, actual_values)
    # plt.scatter(actual_values, predicted_values)
    # plt.show()

    if metric == 'RMSE':
        MSE = np.nanmean((predicted_values - actual_values) ** 2)
        RMSE = math.sqrt(MSE)
        return RMSE
    elif metric == 'MAE':
        MAE = np.nanmean(abs(predicted_values - actual_values))
        return MAE
    elif metric == 'all':
        MAE = np.nanmean(abs(predicted_values - actual_values))
        MSE = np.nanmean((predicted_values - actual_values) ** 2)
        RMSE = math.sqrt(MSE)
        return {'RMSE': RMSE, 'MAE': MAE}
    else:
        return None


def cv_performance(player_names, stint_table, adjacency_matrix, metric,
                   num_folds, group_size=5, verbose=False):
    '''
    Get cross-validation performance as measured by metric using k-fold
    cross-validation.

    Parameters
    --------------------
        player_names        -- iterable of str, names of relevant players
                               for which to predict plus-minus
        stint_table         -- pandas.DataFrame, stint table (training data)
        adjacency_matrix    -- pandas.DataFrame, weighted adjacency matrix for
                               similarity measure between groups (should be in
                               final form i.e. already raised to a power etc.)
        metric              -- str, accepted: 'RMSE', 'MAE', 'both'
        num_folds           -- int, number of folds for k-fold CV
        group_size          -- int, size of player combinations to consider,
                               default 5. Note: must agree with group_size used
                               to create adjacency_matrix
    Returns
    --------------------
        scores              -- list of float, performance as measured by metric
                               (if single metric given)
                               list of dict, performance dictionary (if 'all'
                               given as metric)
    '''
    # split data into k-folds
    if verbose:
        print('{}-fold CV performance...'.format(num_folds))
    kf = KFold(n_splits=num_folds)

    # loop through folds and store scores
    scores = []
    for i, (train_indices, test_indices) in enumerate(kf.split(stint_table)):
        if verbose:
            print('\tFold {}'.format(i))
        # split into test and train
        X_train = stint_table.iloc[train_indices]
        X_test = stint_table.iloc[test_indices]

        # compute score
        score = performance(player_names=player_names,
                            stints_train=X_train,
                            stints_test=X_test,
                            adjacency_matrix=adjacency_matrix,
                            metric=metric,
                            group_size=group_size,
                            verbose=verbose)
        scores.append(score)

    return scores


def make_probability_matrix(adjacency_matrix_, shift=1.0):
    '''
    Turn adjacency matrix into a transition probability matrix by normalizing
    each row to sum to one (dividing each row by the corresponding row-sum).
    Default parameters assume matrix entries are correlations between -1 and 1.

    Parameters
    --------------------
        adjacency_matrix    -- pandas.DataFrame, weighted adjacency matrix for
                               similarity measure between groups
        shift_by            -- float, amount to shift each nonzero value by
                               before dividing by the row sum

    Returns
    --------------------
        normalized_matrix   -- pandas.DataFrame, weighted adjacency matrix
                                after normalization
    '''
    # make deep copy to avoid changing adjacency matrix
    adjacency_matrix = copy.deepcopy(adjacency_matrix_)
    # TODO: this currently assumes entries are correlations between -1 and 1

    # TODO: add one and divide by two

    # shift nonzero values from [-1,1] to [0,2] to make positive
    n, m = adjacency_matrix.shape
    adjacency_matrix += shift * np.ones((n,m)) * (adjacency_matrix != 0)
    # divide each row by the row-sum to make a probability
    row_sums = adjacency_matrix.sum(axis=1)
    normalized_matrix = adjacency_matrix.div(row_sums, axis=0)

    return normalized_matrix


def compute_matrix_sparsity(adjacency_matrix):
    '''
    Compute the sparsity of an adjacency matrix.

    Parameters
    --------------------
        adjacency_matrix    -- pandas.DataFrame, weighted adjacency matrix for
                               similarity measure between groups

    Returns
    --------------------
        sparsity            -- float, sparsity is computed as the percentage of
                               elements in the matrix that are nonzero
    '''
    # TODO: try other measures of sparsity
    # count number nonzero minus number NaN to get the effective number nonzero
    num_nonzero = adjacency_matrix.values.astype(bool).sum() - np.isnan(adjacency_matrix.values).sum()
    n, m = adjacency_matrix.shape
    num_elements = n*m
    sparsity = float(num_nonzero) / num_elements

    return sparsity


def exponentiate_matrix(adjacency_matrix_, sparsity, falloff='none'):
    '''
    Raise an adjacency matrix to the successively higher powers until it has
    requested sparsity, filling in elements with the first nonzero value to
    appear at the lowest power possible.

    Parameters
    --------------------
        adjacency_matrix_    -- pandas.DataFrame, weighted adjacency matrix for
                               similarity measure between groups
        sparsity            -- float, desired sparsity. Sparsity is computed as
                               the percentage of elements in the matrix that are
                               nonzero.
                               None, original adjacency_matrix_ is returned.
                               This option is for compatibility.
        falloff             -- str, method for down-weighting higher powers
                               accepted: 'none', 'exponential', 'inverse'
    Returns
    --------------------
        sparsity_matrix     -- pandas.DataFrame, weighted adjacency matrix
                               with requested sparsity after exponentiating
    '''
    # make deep copy to avoid changing adjacency matrix
    adjacency_matrix = copy.deepcopy(adjacency_matrix_)


    if sparsity is None:
        # if we don't want to increase sparsity return as a probability matrix
        # after adding ones to the diagonal

        # add identity matrix to sparsity_matrix (note it's a square matrix)
        n, _ = adjacency_matrix.shape
        # TODO: think about making k something other than 1 here
        k = 2
        adjacency_matrix += k*np.eye(n)

        return make_probability_matrix(adjacency_matrix, shift=1.0)


    # make adjacency matrix a probability matrix before taking powers
    # shift by one because it is input with correlations between -1 and 1
    adjacency_matrix = make_probability_matrix(adjacency_matrix, shift=1.0)

    # initialize sparsity_matrix as original adjacency_matrix
    sparsity_matrix = copy.deepcopy(adjacency_matrix)

    # add identity matrix to sparsity_matrix (note it's a square matrix)
    n, _ = sparsity_matrix.shape
    sparsity_matrix += np.eye(n)

    # update until desired sparsity is reached
    current_sparsity = compute_matrix_sparsity(sparsity_matrix)
    current_power = 1
    while current_sparsity < sparsity:
        # increase power by one
        # TODO: zero out diagonal!!
        adjacency_matrix = adjacency_matrix.dot(adjacency_matrix)
        current_power += 1

        # TODO: should we zero out the diagonal for next power?
        # adjacency_matrix *= (np.ones((n,n)) - np.eye(n))

        # make adjacency_matrix a probability matrix without shifting values
        # because adjacency_matrix started as a probability matrix, all values
        # of higher powers are between 0 and 1
        # TODO: don't need to normalize here because it's just a k-step transition probability matrix now
        # adjacency_matrix = make_probability_matrix(adjacency_matrix, shift=0)

        # down-weight result based on falloff term
        if falloff == 'exponential':
            weight = np.exp(-current_power)
        elif falloff == 'none':
            weight = 1
        elif falloff == 'inverse':
            weight = float(1) / current_power
        else:
            pass

        # TODO: don't multiply by weight otherwise no longer transition probabilities
        # adjacency_matrix *= weight

        # zero out any elements in adjacency_matrix that have corresponding
        # nonzero elements in current sparsity_matrix and add to existing
        # sparsity_matrix, down-weighting accordingly
        sparsity_matrix += weight * adjacency_matrix * (sparsity_matrix == 0)

        # measure new sparsity
        current_sparsity = compute_matrix_sparsity(sparsity_matrix)

    # once sparsity is fixed make it a probability matrix without shifting values
    # TODO: should this be turned into a transition probability matrix here or somewhere else?
    sparsity_matrix = make_probability_matrix(sparsity_matrix, shift=0)

    return sparsity_matrix


def cv_performance_power(player_names, stint_table, adjacency_matrix, metric,
                         num_folds, sparsity_range, falloff, group_size=5,
                         verbose=False):
    '''
    Get cross-validation performance as measured by metric using k-fold
    cross-validation.

    Parameters
    --------------------
        player_names        -- iterable of str, names of relevant players
                               for which to predict plus-minus
        stint_table         -- pandas.DataFrame, stint table (training data)
        adjacency_matrix    -- pandas.DataFrame, weighted adjacency matrix for
                               similarity measure between groups (should be in
                               final form i.e. already raised to a power etc.)
        metric              -- str, accepted: 'RMSE', 'MAE', 'both'
        num_folds           -- int, number of folds for k-fold CV
        sparsity_range      -- list of float, list of sparsities of the
                               adjacency matrix to grid search over. Sparsity is
                               computed as the percentage of elements in the
                               matrix that are nonzero
        falloff             -- str, method for down-weighting higher matrix
                               powers accepted: 'none', 'exponential', 'inverse'
        group_size          -- int, size of player combinations to consider,
                               default 5. Note: must agree with group_size used
                               to create adjacency_matrix
    Returns
    --------------------
        scores              -- dict of list of float, performance as measured by
                               metric for each sparsity (if single metric given)
                               dict of list of dict, performance dictionary
                               (if 'all' given as metric) for each sparsity
    '''
    scores = {}
    for sparsity in sparsity_range:
        sparsity_matrix = exponentiate_matrix(
            adjacency_matrix_=adjacency_matrix,
            sparsity=sparsity,
            falloff=falloff
        )
        # get CV training performance
        score = cv_performance(
            player_names=player_names,
            stint_table=stint_table,
            adjacency_matrix=sparsity_matrix,
            metric=metric,
            num_folds=num_folds,
            group_size=group_size,
            verbose=verbose
        )
        scores[sparsity] = score

    return scores


def run_test_data():
    '''Run trained model on test data.'''
    print('Loading data...')
    all_player_data = pd.read_csv(
        '../data/individual_2015_2016/standardized_aggregate_individuals_2015_2016.csv')

    # use filtered stints_train and stints_test
    stints_train = pd.read_csv('../data/stints_train_filtered.csv')
    stints_test = pd.read_csv('../data/stints_test_filtered.csv')

    # define teams
    all_team_ids = list(pd.Series.unique(all_player_data['Tm']))

    # define stint size (<5 helpful for debugging and examples)
    group_size = 5

    # define sparsity and falloff for adjacency matrix
    sparsity = 0.25
    falloff = 'exponential'

    # get test RMSE and MAE for each team
    test_RMSEs = []
    test_MAEs = []
    for i, team_id in enumerate(all_team_ids):
        # print progress update
        print('\tTeam {}/{}'.format(i, len(all_team_ids)))
        # skip invalid team_id
        if team_id == 'nan':
            continue

        # get data for given team only
        player_data = all_player_data[all_player_data.Tm == team_id]
        player_names = player_data['Player'].unique()

        # if group size <5 then matrices are not precomputed
        if group_size < 5:
            ## create similarity matrix
            print('Creating similarity matrix for team {}...'.format(team_id))
            # create similarity matrix with normalization and cosine distance
            # i.e. pearson correlation
            player_similarity_matrix = get_similarity_matrix(
                player_data=player_data,
                distance_metric='cosine',
                normalize=True)

            ## create adjacency matrix
            print('Creating adjacency matrix for team {}...'.format(team_id))
            adjacency_matrix = create_adjacency_matrix(
                player_names=player_names,
                similarity_matrix=player_similarity_matrix,
                group_size=group_size)

            # # write out to tsv file (tsv due to commas in the row/col names)
            filename = '../data/adj_mats/adj_mat_'
            adjacency_matrix.to_csv(filename + team_id + '.tsv', sep='\t')

        else:
            ## load adjacency matrix or create if necessary
            print('Loading adjacency matrix for team {}...'.format(team_id))
            try:
                adjacency_matrix = load_adjacency_matrix(team_id)
            except:
                # print('No adjacency matrix')
                # continue
                print(
                    'Creating similarity matrix for team {}...'.format(team_id))
                similarity_matrix = get_similarity_matrix(
                    player_data=player_data,
                    distance_metric='cosine',
                    normalize=True)
                print(
                    'Creating adjacency matrix for team {}...'.format(team_id))
                adjacency_matrix = create_adjacency_matrix(
                    player_names=player_names,
                    similarity_matrix=similarity_matrix,
                    group_size=group_size)

                # save adjacency matrix for next time it's needed
                filename = '../data/adj_mats/adj_mat_{}.tsv'.format(team_id)
                adjacency_matrix.to_csv(filename, sep='\t')

        # describe loaded matrix
        print('\tNum Players: {}'.format(len(player_names)))
        print('\tMatrix Dimensions: {}'.format(adjacency_matrix.shape))

        ## prediction

        # create matrix for prediction
        sparsity_matrix = exponentiate_matrix(adjacency_matrix,
                                              sparsity=sparsity,
                                              falloff=falloff)
        # get test performance
        score = performance(
            player_names=player_names,
            stints_train=stints_train,
            stints_test=stints_test,
            adjacency_matrix=sparsity_matrix,
            metric='all',
            group_size=group_size,
            verbose=True)

        # print and store RMSE and MAE
        print('Test error: {}'.format(score))
        test_RMSEs.append(score['RMSE'])
        test_MAEs.append(score['MAE'])

    # print mean RMSE and MAE across all teams
    mean_rmse = np.nanmean(test_RMSEs)
    mean_mae = np.nanmean(test_MAEs)
    print('Sparsity: {}, Falloff: {}'.format(sparsity, falloff))
    print('Mean Test RMSE: {}'.format(mean_rmse))
    print('Mean Test MAE: {}'.format(mean_mae))


def run_cv_training_error():
    '''Run model and predict training data.'''
    print('Loading data...')
    all_player_data = pd.read_csv(
        '../data/individual_2015_2016/standardized_aggregate_individuals_2015_2016.csv')

    # use filtered stints_train and stints_test
    stints_train = pd.read_csv('../data/stints_train_filtered.csv')
    stints_test = pd.read_csv('../data/stints_test_filtered.csv')

    # define teams
    all_team_ids = list(pd.Series.unique(all_player_data['Tm']))

    # define stint size (<5 helpful for debugging and examples)
    group_size = 5

    # store test RMSE and MAE for each team for each sparsity
    # defaultdicts for extensibility (in case dict of lists needed)
    RMSEs = defaultdict(lambda: defaultdict(float))
    MAEs = defaultdict(lambda: defaultdict(float))

    # define sparsity and falloff for adjacency matrix
    sparsity_range = [None, 0.01, 0.5, 0.75, 1]
    falloff = 'inverse'

    for i, team_id in enumerate(all_team_ids):
        print('\tTeam {}/{}'.format(i, len(all_team_ids)))
        # skip invalid team_id
        if team_id == 'nan':
            continue

        # get data for given team only
        player_data = all_player_data[all_player_data.Tm == team_id]
        player_names = player_data['Player'].unique()

        # if group size <5 then matrices are not precomputed
        if group_size < 5:
            ## create similarity matrix
            print('Creating similarity matrix for team {}...'.format(team_id))
            # create similarity matrix with normalization and cosine distance
            # i.e. pearson correlation
            player_similarity_matrix = get_similarity_matrix(
                player_data=player_data,
                distance_metric='cosine',
                normalize=True)

            ## create adjacency matrix
            print('Creating adjacency matrix for team {}...'.format(team_id))
            adjacency_matrix = create_adjacency_matrix(
                player_names=player_names,
                similarity_matrix=player_similarity_matrix,
                group_size=group_size)

            # # write out to tsv file (tsv due to commas in the row/col names)
            filename = '../data/adj_mats/adj_mat_'
            adjacency_matrix.to_csv(filename + team_id + '.tsv', sep='\t')

        else:
            ## load adjacency matrix or create if necessary
            print('Loading adjacency matrix for team {}...'.format(team_id))
            try:
                adjacency_matrix = load_adjacency_matrix(team_id)
            except:
                # print('No adjacency matrix')
                # continue
                print(
                    'Creating similarity matrix for team {}...'.format(team_id))
                similarity_matrix = get_similarity_matrix(
                    player_data=player_data,
                    distance_metric='cosine',
                    normalize=True)
                print(
                    'Creating adjacency matrix for team {}...'.format(team_id))
                adjacency_matrix = create_adjacency_matrix(
                    player_names=player_names,
                    similarity_matrix=similarity_matrix,
                    group_size=group_size)

                # save adjacency matrix for next time it's needed
                filename = '../data/adj_mats/adj_mat_{}.tsv'.format(team_id)
                adjacency_matrix.to_csv(filename, sep='\t')

        # describe loaded matrix
        print('\tNum Players: {}'.format(len(player_names)))
        print('\tMatrix Dimensions: {}'.format(adjacency_matrix.shape))

        # get CV training performance over sparsity range
        performance_dict = cv_performance_power(
            sparsity_range=sparsity_range,
            falloff=falloff,
            player_names=player_names,
            stint_table=stints_train,
            adjacency_matrix=adjacency_matrix,
            metric='all',
            num_folds=3,
            group_size=group_size,
            verbose=True
        )

        # print results
        best_sparsity_rmse = sparsity_range[0]
        best_sparsity_mae = sparsity_range[0]
        best_rmse = np.nanmean([score['RMSE'] for score
                                in performance_dict[best_sparsity_rmse]])
        best_mae = np.nanmean([score['MAE'] for score
                               in performance_dict[best_sparsity_mae]])
        for sparsity in sparsity_range:
            mean_rmse = np.nanmean([score['RMSE'] for score
                                    in performance_dict[sparsity]])
            mean_mae = np.nanmean([score['MAE'] for score
                                   in performance_dict[sparsity]])

            # store mean value for this sparsity for given team
            RMSEs[team_id][sparsity] = mean_rmse
            MAEs[team_id][sparsity] = mean_mae

            # print('Sparsity: {}'.format(sparsity))
            # print('\tMean RMSE: {}'.format(mean_rmse))
            # print('\tMean MAE: {}'.format(mean_mae))

            # store best one
            if mean_rmse < best_rmse:
                best_sparsity_rmse = sparsity
                best_rmse = mean_rmse
            if mean_mae < best_mae:
                best_sparsity_mae = sparsity
                best_mae = mean_mae

        # print best result for current team
        print('Falloff: {}'.format(falloff))
        print('Best sparsity (RMSE): {}'.format(best_sparsity_rmse))
        print('\tMean RMSE: {}'.format(best_rmse))
        print('Best sparsity (MAE): {}'.format(best_sparsity_mae))
        print('\tMean MAE: {}'.format(best_mae))

    # print results across all teams
    print('RMSEs', RMSEs)
    print('MAEs', MAEs)

    # for team, RMSE_dict in RMSEs.items():
    #     print(team, dict(RMSE_dict))
    #
    # for team, MAE_dict in MAEs.items():
    #     print(team, dict(MAE_dict))

def run_training_data():
    '''Run model and predict on training data to show if learning or not.'''
    print('Loading data...')
    all_player_data = pd.read_csv(
        '../data/individual_2015_2016/standardized_aggregate_individuals_2015_2016.csv')

    # use filtered stints_train and stints_test
    stints_train = pd.read_csv('../data/stints_train_filtered.csv')
    stints_test = pd.read_csv('../data/stints_test_filtered.csv')

    # define teams
    all_team_ids = list(pd.Series.unique(all_player_data['Tm']))

    # define stint size (<5 helpful for debugging and examples)
    group_size = 5

    # define sparsity and falloff for adjacency matrix
    sparsity = None
    falloff = 'inverse'

    for i, team_id in enumerate(all_team_ids):
        print('\tTeam {}/{}'.format(i, len(all_team_ids)))
        # skip invalid team_id
        if team_id == 'nan':
            continue

        # get data for given team only
        player_data = all_player_data[all_player_data.Tm == team_id]
        player_names = player_data['Player'].unique()

        # if group size <5 then matrices are not precomputed
        if group_size < 5:
            ## create similarity matrix
            print('Creating similarity matrix for team {}...'.format(team_id))
            # create similarity matrix with normalization and cosine distance
            # i.e. pearson correlation
            player_similarity_matrix = get_similarity_matrix(
                player_data=player_data,
                distance_metric='cosine',
                normalize=True)

            ## create adjacency matrix
            print('Creating adjacency matrix for team {}...'.format(team_id))
            adjacency_matrix = create_adjacency_matrix(
                player_names=player_names,
                similarity_matrix=player_similarity_matrix,
                group_size=group_size)

            # # write out to tsv file (tsv due to commas in the row/col names)
            filename = '../data/adj_mats/adj_mat_'
            adjacency_matrix.to_csv(filename + team_id + '.tsv', sep='\t')

        else:
            ## load adjacency matrix or create if necessary
            print('Loading adjacency matrix for team {}...'.format(team_id))
            try:
                adjacency_matrix = load_adjacency_matrix(team_id)
            except:
                # print('No adjacency matrix')
                # continue
                print(
                    'Creating similarity matrix for team {}...'.format(team_id))
                similarity_matrix = get_similarity_matrix(
                    player_data=player_data,
                    distance_metric='cosine',
                    normalize=True)
                print(
                    'Creating adjacency matrix for team {}...'.format(team_id))
                adjacency_matrix = create_adjacency_matrix(
                    player_names=player_names,
                    similarity_matrix=similarity_matrix,
                    group_size=group_size)

                # save adjacency matrix for next time it's needed
                filename = '../data/adj_mats/adj_mat_{}.tsv'.format(team_id)
                adjacency_matrix.to_csv(filename, sep='\t')

        # describe loaded matrix
        print('\tNum Players: {}'.format(len(player_names)))
        print('\tMatrix Dimensions: {}'.format(adjacency_matrix.shape))

        # create matrix for prediction
        sparsity_matrix = exponentiate_matrix(adjacency_matrix,
                                              sparsity=sparsity,
                                              falloff=falloff)

        # plot performance
        score = performance(
            player_names=player_names,
            stints_train=stints_train,
            stints_test=stints_train,
            adjacency_matrix=sparsity_matrix,
            metric='all',
            group_size=group_size,
            verbose=True)






def main():
    # TODO: write tests
    # TODO: try creating similarity matrix using correlation distance or other /
    # TODO: measure to account for higher order similarity

    # TODO: tune over power instead of sparsity

    # run_training_data()
    run_test_data()
    # run_cv_training_error()


if __name__ == '__main__':
    main()
