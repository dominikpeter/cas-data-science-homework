
# coding: utf-8

# In[2]:

import os
import pandas as pd
import numpy as np
from collections import defaultdict
from numpy import zeros, log, array
from scipy.sparse import coo_matrix, csr_matrix
from sklearn.preprocessing import normalize
from sklearn.metrics.pairwise import pairwise_distances
import scipy.spatial.distance as dist
from sklearn.metrics.pairwise import cosine_similarity


# In[3]:

path = os.path.expanduser("~/Google Drive/CSVs/item_customer_matrix.csv")


# In[4]:

def read_data(filename):
    """ Reads in the last.fm dataset, and returns a tuple of a pandas dataframe
    and a sparse matrix of artist/user/playcount """
    # read in triples of user/artist/playcount from the input dataset


    # map each artist and user to a unique numeric value
    data['user'] = data['user'].astype("category")
    data['artist'] = data['artist'].astype("category")

    # create a sparse matrix of all the users/plays
    plays = coo_matrix((data['plays'].astype(float),
                       (data['artist'].cat.codes.copy(),
                        data['user'].cat.codes.copy())))

    return data, plays


def cosine1(matrix):
    normalized = normalize(matrix)
    return normalized.dot(normalized.T)


# In[12]:

def bm25_weight(data, K1=100, B=0.8):
    """ Weighs each row of the matrix data by BM25 weighting """
    # calculate idf per term (user)
    N = float(data.shape[0])
    idf = np.log(N / (1 + np.bincount(data.col)))

    # calculate length_norm per document (artist)
    row_sums = np.squeeze(np.asarray(data.sum(1)))
    average_length = row_sums.sum() / N
    length_norm = (1.0 - B) + B * row_sums / average_length

    # weight matrix rows by bm25
    ret = coo_matrix(data)
    ret.data = ret.data * (K1 + 1.0) / (K1 * length_norm[ret.row] + ret.data) * idf[ret.col]
    return ret


# In[13]:

def bm25(matrix):
    plays = bm25_weight(matrix)
    return plays.dot(matrix.T)


def get_largest(row, N=10):
    if N >= row.nnz:
        best = zip(row.data, row.indices)
    else:
        ind = np.argpartition(row.data, -N)[-N:]
        best = zip(row.data[ind], row.indices[ind])
    return sorted(best, reverse=True)


def calculate_similar_items(similarity, item, idItem):
    neighbours = similarity[idItem]
    top = get_largest(neighbours)
    return [(item[other], score, i) for i, (score, other) in enumerate(top)]


# In[ ]:


def main():
    # In[5]:

    data = pd.read_csv(path,
                       delimiter=";",
                       usecols=[0, 1, 2, 3],
                       names=["idClient", "idItem", "idUser", "sales"],
                       dtype={"idClient": np.int16, "idItem": np.str0, "idUser": np.str0, "sales": np.float64})

    # In[7]:

    data['idUser'] = data['idUser'].astype("category")
    data['idItem'] = data['idItem'].astype("category")

    # In[8]:

    data.head()

    # In[9]:

    sales = coo_matrix((data['sales'].astype(float),
                        (data['idItem'].cat.codes.copy(),
                         data['idUser'].cat.codes.copy())))

    # similarity = cosine1(sales)
    similarity = bm25(sales)

    # In[15]:

    items = dict(enumerate(data['idItem'].cat.categories))
    user_count = data.groupby('idItem').size()
    to_generate = sorted(list(items), key=lambda x: -user_count[x])


    l = []
    for item in to_generate:
        name = items[item]
        for other, score, rank in calculate_similar_items(similarity, items, item):
            l.append([name, other, score, rank])

    # In[ ]:

    similarity_DF = pd.DataFrame(l, columns=['name', 'other', 'score', 'rank'])

    # In[ ]:

    similarity_DF = similarity_DF[similarity_DF['rank'] != 0]

    # In[ ]:

    # similarity_DF.to_html("test.html")


    # In[ ]:

    similarity_DF.to_json("cosine_distance_item_based.json")

    # In[ ]:

    similarity_DF.to_csv("cosine_distance_item_based.csv", sep=";")

    # In[ ]:

    similarity_DF.head()

    # In[ ]:

    sales_user = coo_matrix((data['sales'].astype(float),
                             (data['idUser'].cat.codes.copy(),
                              data['idItem'].cat.codes.copy())))

    # In[ ]:

    users = dict(enumerate(data['idUser'].cat.categories))
    item_count = data.groupby('idUser').size()
    to_generate = sorted(list(users), key=lambda x: -item_count[x])

    # In[ ]:

    similarity = bm25(sales_user)

    # In[ ]:

    l = []
    for user in to_generate:
        name = users[user]
        for other, score, rank in calculate_similar_items(similarity, users, user):
            l.append([name, other, score, rank])

    # In[ ]:

    similarity_DF = pd.DataFrame(l, columns=['name', 'other', 'score', 'rank'])

    # In[ ]:

    similarity_DF = similarity_DF[similarity_DF['rank'] != 0]

    # In[ ]:

    similarity_DF.head()

    # In[ ]:

    similarity_DF.to_json("cosine_distance_user_based.json")
    similarity_DF.to_csv("cosine_distance_user_based.csv", sep=";")


if __name__ == "__main__":
    main()