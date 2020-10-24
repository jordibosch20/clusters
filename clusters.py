import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from sklearn.cluster import KMeans
from sklearn.datasets import make_blobs

players = pd.read_csv('players.csv', index_col=0)
# print(players.iloc[0, 2][0])
# print(type(players.iloc[0, 2]))

# players.loc[(players['player_positions'][0] in [
#              "LB", "LWB", "RB", "RWB"])] = "defensor"


players2d = pd.DataFrame(players).copy(deep=False)
players2d.drop(columns=['short_name', 'overall',
                        'player_positions', 'preferred_foot'], inplace=True)

print(players2d)
res = KMeans(n_clusters=4).fit_predict(players2d)


plt.figure(figsize=(12, 12))
plt.subplot(121)
plt.scatter(players2d.iloc[1: 8000, players2d.columns.get_loc('defending_standing_tackle')],
            players2d.iloc[1:8000, players2d.columns.get_loc('skill_dribbling')], c=res[1: 8000], s=4)
plt.title("Cluster")
plt.show()
