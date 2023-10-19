import Metashape
import os, sys, time

# Checking compatibility
compatible_major_version = "1.8"
found_major_version = ".".join(Metashape.app.version.split('.')[:2])
if found_major_version != compatible_major_version:
    raise Exception("Incompatible Metashape version: {} != {}".format(found_major_version, compatible_major_version))

def find_files(folder, types):
    return [entry.path for entry in os.scandir(folder) if (entry.is_file() and os.path.splitext(entry.name)[1].lower() in types)]

if len(sys.argv) < 3:
    print("Usage: general_workflow.py <image_folder> <output_folder>")
    sys.exit(1)

image_folder = sys.argv[1]
output_folder = sys.argv[2]

photos = find_files(image_folder, [".jpg", ".jpeg", ".tif", ".tiff"])

doc = Metashape.Document()
doc.save(output_folder + '/project.psx')

chunk = doc.addChunk()

chunk.addPhotos(photos)
doc.save()

print(str(len(chunk.cameras)) + " images loaded")

