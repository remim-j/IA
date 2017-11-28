# Required Python Packages

import csv
import pandas as pd
import numpy as np
import plotly.plotly as py
import plotly.graph_objs as go

from sklearn.cross_validation import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn import metrics

py.sign_in('remim-j', 'bgyA2QoltHfaOls5QQne')

# Files
DATA_SET_PATH = "datatest.csv"

def dataset_headers(dataset):
    """
    To get the dataset header names
    :param dataset: loaded dataset into pandas DataFrame
    :return: list of header names
    """
    return list(dataset.columns.values)

def unique_observations(dataset, header, method=1):
    """
    To get unique observations in the loaded pandas DataFrame column
    :param dataset:
    :param header:
    :param method: Method to perform the unique (default method=1 for pandas and method=0 for numpy )
    :return:
    """
    try:
        if method == 0:
            # With Numpy
            observations = np.unique(dataset[[header]])
        elif method == 1:
            # With Pandas
            observations = pd.unique(dataset[header].values.ravel())
        else:
            observations = None
            print ("Wrong method type, Use 1 for pandas and 0 for numpy")
    except Exception as e:
        observations = None
        print ("Error: {error_msg} /n Please check the inputs once..!".format(error_msg=e.message))
    return observations


def feature_target_frequency_relation(dataset, f_t_headers):

    """
    To get the frequency relation between targets and the unique feature observations
    :param dataset:
    :param f_t_headers: feature and target header
    :return: feature unique observations dictionary of frequency count dictionary
    """

    feature_unique_observations = unique_observations(dataset, f_t_headers[0])
    unique_targets = unique_observations(dataset, f_t_headers[1])

    frequencies = {}
    for feature in feature_unique_observations:
        frequencies[feature] = {unique_targets[0]: len(
            dataset[(dataset[f_t_headers[0]] == feature) & (dataset[f_t_headers[1]] == unique_targets[0])]),
            unique_targets[1]: len(
                dataset[(dataset[f_t_headers[0]] == feature) & (dataset[f_t_headers[1]] == unique_targets[1])])}
    return frequencies
 
def main():
    """
    with open(DATA_SET_PATH, 'r') as csvfile:
     spamreader = csv.reader(csvfile, delimiter=' ', quotechar='|')
     for row in spamreader:
         print (', '.join(row))
    Logistic Regression classifier main
    :return:
    """
    # Load the data set for training and testing the logistic regression classifier
    dataset = pd.read_csv(DATA_SET_PATH)
    print ("Number of Observations :: ", len(dataset))
    headers = dataset_headers(dataset)
    print ("Data set headers :: {headers}".format(headers=headers)) 
    
 
if __name__ == "__main__":
    main()
