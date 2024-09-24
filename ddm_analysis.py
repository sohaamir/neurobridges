import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import hddm
from scipy import stats

# Read the CSV file
data = pd.read_csv('processed_data/stop_signal_preprocessed_int_rumination.csv')

# Data preparation
data['IntrusiveSum_z'] = (data['IntrusiveSum'] - data['IntrusiveSum'].mean()) / data['IntrusiveSum'].std()
data['responded'] = np.where((data['task'] == 'Stop') & (data['incorrect'] == 0), 0, 1)
data['rt'] = np.where(data['responded'] == 0, np.nan, data['rt'] / 1000)
data['ssd'] = data['ssd'] / 1000

# Filter for Go trials only
go_data = data[data['task'] == 'NoStop'].copy()

# Prepare data for modeling
model_data = go_data[['id', 'rt', 'correct', 'IntrusiveSum']].copy()
model_data['response'] = np.where(model_data['correct'] == 1, 1, 0)
model_data = model_data.dropna()

# Fit the HDDM model
model = hddm.HDDM(model_data, depends_on={'v': 'IntrusiveSum', 'a': 'IntrusiveSum', 't': 'IntrusiveSum'})
model.find_starting_values()
model.sample(2000, burn=1000, dbname='traces.db', db='pickle')

# Extract parameter estimates
params = model.get_group_nodes()
a_samples = model.nodes_db.node[['a(IntrusiveSum)']]
v_samples = model.nodes_db.node[['v(IntrusiveSum)']]
t_samples = model.nodes_db.node[['t(IntrusiveSum)']]

# Correlate model parameters with IntrusiveSum
unique_intrusive_sum = model_data['IntrusiveSum'].unique()

a_means = [np.mean(a_samples.loc[a_samples.index.str.contains(f'a({i})')]) for i in unique_intrusive_sum]
v_means = [np.mean(v_samples.loc[v_samples.index.str.contains(f'v({i})')]) for i in unique_intrusive_sum]
t_means = [np.mean(t_samples.loc[t_samples.index.str.contains(f't({i})')]) for i in unique_intrusive_sum]

corr_a = stats.pearsonr(unique_intrusive_sum, a_means)
corr_v = stats.pearsonr(unique_intrusive_sum, v_means)
corr_t = stats.pearsonr(unique_intrusive_sum, t_means)

print(f"Correlation between IntrusiveSum and boundary separation (a): {corr_a[0]:.3f}, p={corr_a[1]:.3f}")
print(f"Correlation between IntrusiveSum and drift rate (v): {corr_v[0]:.3f}, p={corr_v[1]:.3f}")
print(f"Correlation between IntrusiveSum and non-decision time (t): {corr_t[0]:.3f}, p={corr_t[1]:.3f}")

# Scatter plots for IntrusiveSum vs. model parameters
def plot_param(x, y, xlabel, ylabel, title):
    plt.figure(figsize=(8, 6))
    sns.scatterplot(x=x, y=y)
    sns.regplot(x=x, y=y, scatter=False)
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.title(title)
    plt.savefig(f"{title.replace(' ', '_').lower()}.png")
    plt.close()

plot_param(unique_intrusive_sum, a_means, "Intrusive Rumination Score", "Boundary Separation (a)", "IntrusiveSum vs Boundary Separation")
plot_param(unique_intrusive_sum, v_means, "Intrusive Rumination Score", "Drift Rate (v)", "IntrusiveSum vs Drift Rate")
plot_param(unique_intrusive_sum, t_means, "Intrusive Rumination Score", "Non-decision Time (t)", "IntrusiveSum vs Non-decision Time")