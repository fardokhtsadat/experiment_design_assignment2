import pandas as pd
import numpy as np
import pickle

data = pd.read_csv("ratings_data.txt", header=None, sep=" ")
data.columns = ["user_id", "item_id", "rating_value"]

unique_item_ids = data["item_id"].unique().tolist()
unique_user_ids = data["user_id"].unique().tolist()

test_data = {}

np.random.seed(0)

for user_id in unique_user_ids:
	if len(data.loc[data["user_id"] == user_id]) > 1:
		temp = data.loc[data["user_id"] == user_id]
		all_positive_interaction = temp["item_id"].tolist()
		all_negative_interaction = list(set(unique_item_ids) - set(all_positive_interaction))
		sampled_positive_interaction = np.random.choice(all_positive_interaction, 1)[0]
		test_data[user_id] = (sampled_positive_interaction, list(np.random.choice(all_negative_interaction, 100, replace=False)))
		data.drop(data[(data["user_id"] == user_id) & (data["item_id"] == sampled_positive_interaction)].index, inplace=True)

with open('test_set.pickle', 'wb') as handle:
	pickle.dump(test_data, handle, protocol=pickle.HIGHEST_PROTOCOL)

data.to_csv('train_set.csv', sep=",")

final_test_data = np.asarray(test_data)
final_train_data = np.asarray(data[["user_id", "item_id"]].values.tolist())

np.savez('../data/epinions.npz', train_data=final_train_data, test_data=final_test_data)
