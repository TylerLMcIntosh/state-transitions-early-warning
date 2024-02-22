
import os
import glob
import time
import geopandas as gpd
import rioxarray as rxr
import xarray as xr
from rioxarray.merge import merge_arrays
import rasterio.crs

begin = time.time()

maindir = os.path.join(os.getcwd(),'data/spatial/mod/MODIS')

proj = 'EPSG:5070'

srme = gpd.read_file(os.path.join(os.getcwd(),'data/spatial/raw/us_eco_l3_srme.gpkg')).to_crs(proj)


#############
# Functions #
#############

def list_files(path, ext):
    return glob.glob(os.path.join(path, '**', '*{}'.format(ext)), recursive=True)


def mosaic_tifs(tif_list, output_filename, clip):
    tiles = []
    for tif in tif_list:
        # print(os.path.basename(tif))
        tile = rxr.open_rasterio(tif, masked=True, cache=False).squeeze()
        tile = tile.rio.reproject(proj)
        nodata = tile.rio.nodata

        # Set nodata values if not already set
        if nodata is None:
            print("Writing NoData")
            tile.rio.write_nodata(-9999.0, inplace=True)
        else:
            nodata = nodata
            # print(nodata)

        tiles.append(tile)

        del tif, tile

    # Merge the rasters
    print("Merging arrays.")

    stack = xr.concat(tiles, dim='band')
    out_mosaic = stack.mean(dim='band', skipna=True)
    out_mosaic = out_mosaic.to_dataset(name=ind).to_array()

    # Clip to the SRME
    print("Clipping ...")
    clipped = out_mosaic.rio.clip(clip.geometry)

    clipped.rio.to_raster(
        output_filename, compress='zstd', zstd_level=9, driver='GTiff')

    del out_mosaic, tiles, clipped, stack


# Get a list of the TIFF files
tifs = list_files(os.path.join(maindir,'tiles'),'.tif')
print(len(tifs))

# Loop through indices and blocks, create a mosaic

indices = ['EVI_md','LST_md','NDWI_md','LAI_md','FPAR_md']
years = range(2000,2022)

missing = {}

for ind in indices:

    print(f"Processing the {ind} mosaic.")

    # Filter by the index
    tifs_ind = [tif for tif in tifs if ind in os.path.basename(tif)]
    print(f"Number of files: {len(tifs_ind)}")
    for year in years:
        print(year)

        # Filter the list of tif files to the year
        tifs_annual = [tif for tif in tifs_ind if str(year) in os.path.basename(tif)]

        print(f"Number of files ({year}): {len(tifs_annual)}")

        if len(tifs_annual) < 15:

            missing[ind] = []
            missing[ind].append([year,len(tifs_annual)])

            print("Missing tiles for mosaic operation, skipping ...")

        else:

            out_path = os.path.join(maindir,f'mosaics/{ind}')
            if not os.path.exists(out_path):
                os.makedirs(out_path)

            out_file_path = os.path.join(out_path, f"Southern_Rockies_MODIS_{ind}_{year}.tif")

            # Mosaic the annual tif files and save out
            print(f"Creating mosaic for {ind} {year}...")
            print(f"Saving to: {out_path}")

            mosaic_tifs(tifs_annual, out_file_path, clip=srme)

print(f"Missing data for \n {missing}")
print(f"Total elapsed time: {round((time.time() - begin)/60,1)} minutes.")