from distutils.command.config import config
import numpy as np
import matplotlib.pyplot as plt


config_dict = {
    "sily.txt":  {4: "RX", 6: "RZ"},
    "perem.txt": {4: "VX"} }


file_name = "sily.txt"
data = np.loadtxt(file_name)
columns_map_dict = config_dict[file_name]

plt.suptitle(file_name)
axes = []
for i, (c, c_name) in enumerate(columns_map_dict.items(), 1):
    axes.append(plt.subplot(len(columns_map_dict), 1, i))
    plt.gca().set_aspect(aspect=30)
    plt.grid()

    plt.xlabel("T")
    plt.ylabel(f"{c_name}")
    # plt.gca().set_ylim(auto=True)

    plt.plot(data[:, 0], data[:, c])

plt.gca().get_shared_x_axes().join(*axes)
plt.show()
# plt.grid()
# 


# plt.xlabel("T")
# plt.ylabel(f"{column_name}")

# plt.plot(data[:, 0], data[:, 4])
# # plt.plot(data[:, 0], data[:, 6])


