output_dir_name = "output"
file_name = "sily.txt"
file_path = sprintf("%s/%s", output_dir_name, file_name)

set terminal png size 1024,768
set output file_name.'.png'
plot file_path using 1:5 with lines
