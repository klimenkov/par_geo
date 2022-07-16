import logging
import matplotlib.pyplot as plt
import numpy as np
import os
import sys

logging.basicConfig(
    stream=sys.stdout,
    level=logging.INFO)

config = {
    "rcParams_custom": {
        "font.family": "Times New Roman",
        "mathtext.default": "regular",
        "xaxis.labellocation": "right",
        "yaxis.labellocation": "top"},
    "file_columns_map_dict": {
        "sily.txt":  {4: "$R_x$", 6: "$R_z$"},
        "perem.txt": {4: "$V_x$"}},
    "save_plot": False }

file_name = "sily.txt"
output_dir_name = "output"

rcParams_custom = config["rcParams_custom"]
file_columns_map_dict = config["file_columns_map_dict"]

plt.rcParams.update(rcParams_custom)

save_plot = config["save_plot"]

file_path = os.path.join(output_dir_name, file_name)

data = np.loadtxt(file_path)
columns_map_dict = file_columns_map_dict[file_name]

plt.suptitle(file_name)
axes = []
for i, (c, c_name) in enumerate(columns_map_dict.items(), 1):
    ax = plt.subplot(len(columns_map_dict), 1, i)
    axes.append(ax)

    plt.grid()

    plt.xlabel("Т, с")
    plt.ylabel(f"{c_name}", rotation="horizontal")

    plt.plot(data[:, 0], data[:, c])

plt.subplots_adjust(hspace=0.5)
plt.gca().get_shared_x_axes().join(*axes)

if save_plot:
    plots_dir_path = os.path.join(output_dir_name, "plots")

    if not os.path.exists(plots_dir_path):
        os.makedirs(plots_dir_path)

    plot_file_name = "{}.png".format(file_name)
    plot_file_path = os.path.join(plots_dir_path, plot_file_name)
    plt.savefig(plot_file_path)

    logging.info(
        "File {} saved to {}".format(plot_file_name, plot_file_path))

plt.show()
