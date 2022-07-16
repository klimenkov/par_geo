import logging
import matplotlib.pyplot as plt
import numpy as np
import os


config = {
    "rcParams_custom": {
        "font.family": "Times New Roman",
        "mathtext.default": "regular",
        "xaxis.labellocation": "right",
        "yaxis.labellocation": "top",
        "figure.figsize": (7, 9)},
    "file_columns_map_dict": {
        "sily.txt":  [{"col_i": 4, "col_str": "$R_x$"}],
        "perem.txt": [{"col_i": 4, "col_str": "$V_x$"}] },
    "save_plot": True }

plt.rcParams.update(config["rcParams_custom"])

output_dir_name = "output"
file_name = "sily.txt"
col_info = config["file_columns_map_dict"][file_name][0]

dir_names = os.listdir(output_dir_name)

plt.suptitle(file_name)

for i, dir_name in enumerate(dir_names, 1):
    ax = plt.subplot(len(dir_names), 1, i)

    omega_ratio, _, _, _ = dir_name.split("_")

    file_path = os.path.join(output_dir_name, dir_name, file_name)
    data = np.genfromtxt(file_path, invalid_raise=False)

    plt.grid()
    plt.title(rf'$\omega_1/\omega_2$: {omega_ratio}')

    plt.xlabel("Т, с")
    plt.ylabel(f"{col_info['col_str']}", rotation="horizontal")

    plt.plot(data[:, 0], data[:, col_info["col_i"]])

plt.subplots_adjust(hspace=0.5)

if config["save_plot"]:
    plots_dir_path = "plots"

    if not os.path.exists(plots_dir_path):
        os.makedirs(plots_dir_path)

    plot_file_name = "{}.png".format(file_name)
    plot_file_path = os.path.join(plots_dir_path, plot_file_name)
    plt.savefig(plot_file_path)

    logging.info(
        "File {} saved to {}".format(plot_file_name, plot_file_path))

plt.show()
