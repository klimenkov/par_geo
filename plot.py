import matplotlib.pyplot as plt
import numpy as np
import os

config_dict = {
    "sily.txt":  {4: "RX", 6: "RZ"},
    "perem.txt": {4: "VX"} }

output_dir = "output"
file_name = "perem.txt"
file_path = os.path.join(output_dir, file_name)

data = np.loadtxt(file_path)
columns_map_dict = config_dict[file_name]


plt.suptitle(file_name)
axes = []
for i, (c, c_name) in enumerate(columns_map_dict.items(), 1):
    ax = plt.subplot(len(columns_map_dict), 1, i)
    axes.append(ax)

    plt.grid()

    plt.xlabel("T")
    plt.ylabel(f"{c_name}")

    plt.plot(data[:, 0], data[:, c])

plt.subplots_adjust(hspace=0.5)
plt.gca().get_shared_x_axes().join(*axes)

plt.show()


