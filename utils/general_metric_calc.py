import numpy as np

def calculate_matrix_percentile(matrix):
    ten_percentile_array = []
    fifty_percentile_array = []
    ninty_percentile_array = []

    index = 0
    for flow in matrix[0]:
        ten_percentile_array.append(np.nanpercentile(matrix[:, index], 10))
        fifty_percentile_array.append(np.nanpercentile(matrix[:, index], 50))
        ninty_percentile_array.append(np.nanpercentile(matrix[:, index], 90))
        index = index + 1
    return ten_percentile_array, fifty_percentile_array, ninty_percentile_array

def calculate_average_each_column(matrix):
    average = []

    index=0
    for flow in matrix[0]:
        average.append(np.nanmean(matrix[:,index]))
        index = index + 1

    return average

def calculate_std_each_column(matrix):
    std = []

    index=0
    for flow in matrix[0]:
        std.append(np.nanstd(matrix[:,index]))
        index = index + 1

    return std

def calculate_cov_each_column(std_array, average_array):
    cov = []

    index=0
    for average in average_array:
        cov.append(std_array[index] / average)
        index = index + 1
    return cov

def calculate_percent_exceedance(matrix, percentile):
    return np.nanpercentile(matrix, percentile)
