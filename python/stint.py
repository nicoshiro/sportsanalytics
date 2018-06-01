

class Stint:
    '''
    Stint class for a group of players on the court.
    '''
    def __init__(self, names):
        '''
        Normalize a pandas DataFrame object column-wise.

        Parameters
        --------------------
            names              -- iterable of str, names of players in the stint

        Attributes
        --------------------
            names              -- set of str, names of players in the stint
        '''
        self.names = set(names)

    def __str__(self):
        '''
        Return string representation of Stint.
        '''
        # comma separated player names
        return ','.join(self.names)

    def similarity(self, other, player_similarity_matrix):
        '''
        Measure similarity to other stint.

        Parameters
        --------------------
            other               -- Stint, stint against which similarity is
                                   measured
            player_similarity_matrix   -- pandas DataFrame, similarity matrix
                                          corresponding players

        Returns
        --------------------
            similarity  -- float, similarity between stints
        '''
        if self.equivalent(other):
            return 1.0

        # get substitutions between stints
        substitutions = self.get_substitutions(other)

        if len(substitutions) != 2:
            # more than one substitution apart
            return 0.0
        else:
            player_1 = substitutions[0]
            player_2 = substitutions[1]
            similarity = player_similarity_matrix.loc[player_1, player_2]
            return similarity

    def equivalent(self, other):
        '''
        Determine if equivalent to another Stint. Two Stints are equivalent if
        they contain the same player names.

        Parameters
        --------------------
            other               -- Stint, stint against which equivalence is
                                   measured

        Returns
        --------------------
            flag  -- bool, True if both Stints are equivalent, False otherwise
        '''
        # compare name sets
        return self.names == other.names

    def get_substitutions(self, other):
        '''
        Determine substituted players between Stint and other Stint.

        Parameters
        --------------------
            other          -- Stint, stint against which self is compared

        Returns
        --------------------
            substitutions  -- list of str, player names for substituted players
                              to get from self to other Stint
        '''
        return list((set.union(self.names, other.names) -
                set.intersection(self.names, other.names)))