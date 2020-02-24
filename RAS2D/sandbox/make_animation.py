import os

fps = 10
speed = 'fast' # ultrafast, superfast, veryfast, faster, fast, medium, slow, slower, veryslow
quality = 20 # range 20 - 40 ??
start_number = 30

cmd = f'ffmpeg -r {fps} -y -start_number {start_number} -i 2D_Interior_Area_HDF_depth_%03d.png -vf "scale=710:804" -c:v libx264 -profile:v baseline -preset {speed} -tune animation -crf {quality} -pix_fmt yuv420p 2D_Interior_Area_HDF_depth.mp4'
os.system(cmd)